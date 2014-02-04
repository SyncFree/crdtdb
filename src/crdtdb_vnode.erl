-module(crdtdb_vnode).
-behaviour(riak_core_vnode).
-include("crdtdb.hrl").

%% CRDT API
-export([get/2, put/3]).

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-ignore_xref([
             start_vnode/1
             ]).

-record(state, {partition, dets}).

-define(SYNC(PrefList, Command),
        riak_core_vnode_master:sync_command(PrefList, Command, crdtdb_vnode_master)).

%% CRDT API

%% @doc get the CRDT stored under Key
get(Preflist, Key) ->
  ?SYNC(Preflist, {get, Key}).

%% @doc put the given CRDT as the value under Key
put(Preflist, Key, CRDT) ->
    ?SYNC(Preflist, {put, Key, CRDT}).

%% Vnode API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    File = filename:join(app_helper:get_env(riak_core, platform_data_dir),
                         integer_to_list(Partition)),
    {ok, Dets} = dets:open_file(Partition, [{file, File}, {type, set}]),
    {ok, #state { partition=Partition, dets=Dets }}.

%% Get the CRDT stored at Key
handle_command({get, Key}, _Sender, State=#state{dets=Dets}) ->
    Reply = case dets:lookup(Dets, Key) of
                [] -> {error, notfound};
                [{Key, CRDT}] -> {ok, CRDT};
                Error -> Error
            end,
    {reply, Reply, State};
handle_command({put, Key, #crdt{mod=Mod, value=Val}=CRDT}, _Sender, State=#state{dets=Dets}) ->
    Reply = case dets:lookup(Dets, Key) of
                [] -> dets:insert(Dets, {Key, CRDT});
                [{Key, #crdt{mod=Mod, value=LocalValue}}] ->
                    Merged = Mod:merge(Val, LocalValue),
                    dets:insert(Dets, {Key, #crdt{mod=Mod, value=Merged}});
                [{Key, #crdt{mod=Other}}] ->
                    {error, {type_conflict, Mod, Other}};
                Error ->
                    Error
            end,
    {reply, Reply, State};
handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, #state{dets=Dets}) ->
    dets:close(Dets).
