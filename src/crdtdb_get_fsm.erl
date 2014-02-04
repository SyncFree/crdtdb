%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc coordination of CRDTDB write requests

-module(crdtdb_get_fsm).

-behaviour(gen_fsm).

-include("crdtdb.hrl").

-export([start_link/2]).
%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).
%% State-event callbacks
-export([execute/2, await_responses/2]).

-record(state, {from :: {raw, integer(), pid()},
                key, %% The key for the CRDT
                tref, %% A timer, so we don't wait for ever
                results=[], %% success responses
                errors=[] %% error responses
               }).

-define(BUCKET, <<"crdtdb">>).
-define(N, 3). %% Store 3 replicas of each key, value
-define(R, 2). %% Require 2 response before success

-define(DEFAULT_TIMEOUT, 60000). %% Don't wait for ever for replies

%% ===================================================================
%% Public API
%% ===================================================================

start_link(From, Key) ->
    gen_fsm:start_link(?MODULE, [From, Key], []).

%% ====================================================================
%% gen_fsm callbacks
%% ====================================================================

%% @private
init([From, Key]) ->
    BKey = {?BUCKET, Key},
    StateData = #state{from = From,
                       key = BKey},
    %% Move to state prepare at once (0 timeout) and trigger
    %% prepare's `timeout' event.
    {ok, execute, StateData, 0}.

%% @private
execute(timeout, StateData=#state{key = BKey}) ->
    %% We can't wait for ever, so start a timer to notify this FSM
    %% when the time is up
    TRef = schedule_timeout(?DEFAULT_TIMEOUT),
    DocIdx = riak_core_util:chash_key(BKey),
    UpNodes = riak_core_node_watcher:nodes(crdtdb),
    Preflist = [IndexNode || {IndexNode, _Type} <- riak_core_apl:get_apl_ann(DocIdx, ?N, UpNodes)],
    case length(Preflist) of
        0 ->
            %% Empty preflist
            client_reply({error, empty_preflist}, StateData);
        VN when VN < ?R ->
            client_reply({error, {insufficient_vnodes, VN, ?R}}, StateData);
        _ ->
            crdtdb_vnode:get(Preflist, BKey),
            {next_state, await_responses, StateData#state{tref=TRef}}
    end.

%% @private
await_responses(request_timeout, StateData) ->
    client_reply({error,timeout}, StateData);
await_responses({error, notfound}, StateData = #state{results=Results0}) ->
    Results = [notfound | Results0],
    case length(Results) of
        ?R ->
            Reply = merge_results(Results),
            client_reply(Reply, StateData#state{results=Results});
        _ ->
            {next_state, await_responses, StateData#state{results=Results}}
    end;
await_responses({error, _E}=Res, StateData = #state{errors=Errors0}) ->
    Errors = [Res | Errors0],
    case length(Errors) of %% Can't get ?R successes now
        ?R ->
            client_reply({error, r_unsatisfied}, StateData#state{errors = Errors});
        _ ->
            {next_state, await_responses, StateData#state{errors = Errors}}
    end;
await_responses({ok, CRDT}, StateData=#state{results=Results0}) ->
    Results = [CRDT | Results0],
    case length(Results) of
        ?R ->
            Reply = merge_results(Results),
            client_reply(Reply, StateData#state{results=Results});
        _ ->
            {next_state, await_responses, StateData#state{results=Results}}
    end.

%% @private
handle_event(_Event, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
handle_info(request_timeout, StateName, StateData) ->
    ?MODULE:StateName(request_timeout, StateData);
handle_info(_Info, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
terminate(Reason, _StateName, _State) ->
    Reason.

%% @private
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Send a reply
client_reply(Reply, StateData = #state{from = {raw, ReqId, Pid}}) ->
    Pid ! {ReqId, Reply},
    {stop, normal, StateData}.

merge_results([Mergedest | Rest]) ->
    merge_results(Rest, Mergedest).

merge_results([], Mergedest) ->
    {ok, Mergedest};
merge_results([#crdt{mod=Mod, value=V1} | Rest], #crdt{mod=Mod, value=V2}) ->
    merge_results(Rest, #crdt{mod=Mod, value=Mod:merge(V1, V2)});
merge_results([#crdt{mod=Mod1} | _Rest], #crdt{mod=Mod2}) ->
    {error, {type_conflict, Mod1, Mod2}};
merge_results([Res | Rest], notfound) ->
    merge_results(Rest, Res).

schedule_timeout(infinity) ->
    undefined;
schedule_timeout(Timeout) ->
    erlang:send_after(Timeout, self(), request_timeout).


