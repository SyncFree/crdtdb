-module(crdtdb).
-include("crdtdb.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0,
         get/1,
         put/3
        ]).

-define(BUCKET, <<"crdtdb">>).


%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, crdtdb_vnode_master).

get(Key) ->
    DocIdx = riak_core_util:chash_key({?BUCKET, Key}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
    [{IndexNode, _Type}] = PrefList,
    case crdtdb_vnode:get(IndexNode, Key) of
        {ok, #crdt{mod=M, value=V}} ->
            {ok, {M, V}};
        E ->
            E
    end.

put(Key, Mod, Value) ->
    DocIdx = riak_core_util:chash_key({?BUCKET, Key}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
    [{IndexNode, _Type}] = PrefList,
    crdtdb_vnode:put(IndexNode, Key, #crdt{mod=Mod, value=Value}).
