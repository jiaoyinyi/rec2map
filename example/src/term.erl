%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% erlang数据和bson数据转换
%%% @end
%%% Created : 2022-12-24 00:00:00
%%%-------------------------------------------------------------------
-module(term).
-author("jiaoyinyi").

-export([
    term_to_bson/1,
    bson_to_term/1
]).

-define(TERM_TYPE, <<"_t">>).
-define(TERM_DATA, <<"_d">>).

-define(TERM_TYPE_INTEGER, 1).
-define(TERM_TYPE_FLOAT, 2).
-define(TERM_TYPE_BOOLEAN, 3).
-define(TERM_TYPE_ATOM, 4).
-define(TERM_TYPE_BINARY, 5).
-define(TERM_TYPE_RECORD, 6).
-define(TERM_TYPE_TUPLE, 7).
-define(TERM_TYPE_MAP, 8).
-define(TERM_TYPE_LIST, 9).
-define(TERM_TYPE_OTHER, 10).

%% @doc erlang数据转bson数据
-spec term_to_bson(term()) -> map().
term_to_bson(Term) when is_integer(Term) ->
    #{?TERM_TYPE => ?TERM_TYPE_INTEGER, ?TERM_DATA => Term};
term_to_bson(Term) when is_float(Term) ->
    #{?TERM_TYPE => ?TERM_TYPE_FLOAT, ?TERM_DATA => Term};
term_to_bson(Term) when is_boolean(Term) ->
    #{?TERM_TYPE => ?TERM_TYPE_BOOLEAN, ?TERM_DATA => Term};
term_to_bson(Term) when is_atom(Term) ->
    #{?TERM_TYPE => ?TERM_TYPE_ATOM, ?TERM_DATA => atom_to_binary(Term, utf8)};
term_to_bson(Term) when is_binary(Term) ->
    #{?TERM_TYPE => ?TERM_TYPE_BINARY, ?TERM_DATA => Term};
term_to_bson(Term) when is_tuple(Term) ->
    case tuple_size(Term) > 0 andalso is_atom(element(1, Term)) andalso is_record(Term, element(1, Term)) of
        true ->
            #{?TERM_TYPE => ?TERM_TYPE_RECORD, ?TERM_DATA => begin {ok, Map} = rec_term:rec_to_map(Term), Map end};
        false ->
            #{?TERM_TYPE => ?TERM_TYPE_TUPLE, ?TERM_DATA => tuple_to_bson(Term)}
    end;
term_to_bson(Term) when is_map(Term) ->
    #{?TERM_TYPE => ?TERM_TYPE_MAP, ?TERM_DATA => maps:fold(fun(K, V, Acc) -> maps:put(erlang:term_to_binary(K), term_to_bson(V), Acc) end, #{}, Term)};
term_to_bson(Term) when is_list(Term) ->
    #{?TERM_TYPE => ?TERM_TYPE_LIST, ?TERM_DATA => lists:map(fun term_to_bson/1, Term)};
term_to_bson(Term) ->
    #{?TERM_TYPE => ?TERM_TYPE_OTHER, ?TERM_DATA => erlang:term_to_binary(Term)}.

%% @doc bson数据转erlang数据
-spec bson_to_term(map()) -> term().
bson_to_term(#{?TERM_TYPE := ?TERM_TYPE_INTEGER, ?TERM_DATA := Data}) -> Data;
bson_to_term(#{?TERM_TYPE := ?TERM_TYPE_FLOAT, ?TERM_DATA := Data}) -> Data;
bson_to_term(#{?TERM_TYPE := ?TERM_TYPE_BOOLEAN, ?TERM_DATA := Data}) -> Data;
bson_to_term(#{?TERM_TYPE := ?TERM_TYPE_ATOM, ?TERM_DATA := Data}) -> binary_to_atom(Data, utf8);
bson_to_term(#{?TERM_TYPE := ?TERM_TYPE_BINARY, ?TERM_DATA := Data}) -> Data;
bson_to_term(#{?TERM_TYPE := ?TERM_TYPE_RECORD, ?TERM_DATA := Data}) -> begin {ok, Rec} = rec_term:map_to_rec(Data), Rec end;
bson_to_term(#{?TERM_TYPE := ?TERM_TYPE_TUPLE, ?TERM_DATA := Data}) -> bson_to_tuple(Data);
bson_to_term(#{?TERM_TYPE := ?TERM_TYPE_MAP, ?TERM_DATA := Data}) -> maps:fold(fun(K, V, Acc) -> maps:put(erlang:binary_to_term(K), bson_to_term(V), Acc) end, #{}, Data);
bson_to_term(#{?TERM_TYPE := ?TERM_TYPE_LIST, ?TERM_DATA := Data}) -> lists:map(fun bson_to_term/1, Data);
bson_to_term(#{?TERM_TYPE := ?TERM_TYPE_OTHER, ?TERM_DATA := Data}) -> erlang:binary_to_term(Data).

tuple_to_bson(Tuple) ->
    do_tuple_to_bson(Tuple, 1, #{}).
do_tuple_to_bson(Tuple, Idx, BsonMap) when Idx > tuple_size(Tuple) ->
    BsonMap;
do_tuple_to_bson(Tuple, Idx, BsonMap) ->
    NewBsonMap = maps:put(integer_to_binary(Idx), term_to_bson(element(Idx, Tuple)), BsonMap),
    do_tuple_to_bson(Tuple, Idx + 1, NewBsonMap).

bson_to_tuple(Bson) ->
    Map =
        maps:fold(
            fun(K, V, Acc) ->
                maps:put(binary_to_integer(K), bson_to_term(V), Acc)
            end, #{}, Bson
        ),
    list_to_tuple(maps:values(Map)).