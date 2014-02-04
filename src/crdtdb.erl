-module(crdtdb).
-include("crdtdb.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         get/1,
         put/3
        ]).

-define(TIMEOUT, 60000).

%% Public API
get(Key) ->
    Me = self(),
    ReqId = mk_reqid(),
    crdtdb_get_fsm:start_link({raw, ReqId, Me}, Key),
    wait_for_reqid(ReqId, ?TIMEOUT).

put(Key, Mod, Value) ->
    Me = self(),
    ReqId = mk_reqid(),
    crdtdb_put_fsm:start_link({raw, ReqId, Me}, Key, #crdt{mod=Mod, value=Value}),
    wait_for_reqid(ReqId, ?TIMEOUT).

%% @private
mk_reqid() ->
    erlang:phash2({self(), os:timestamp()}).

%% @private
wait_for_reqid(ReqId, Timeout) ->
    receive
        {ReqId, Response} -> Response
    after Timeout ->
            {error, timeout}
    end.
