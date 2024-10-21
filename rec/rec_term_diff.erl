%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% record数据对比
%%% @end
%%% Created : 2022-12-24 00:00:00
%%%-------------------------------------------------------------------
-module(rec_term_diff).
-author("jiaoyinyi").

-export([
    diff/3
    , diff/4
]).

-include("rec_term.hrl").

%% 对比中间数据
-record(diff_state, {
    max_layer                 :: non_neg_integer() |  infinity               %% 比较最大层数
    , layer                   :: pos_integer()                               %% 当前层数
}).

%% @doc 对比差异
-spec diff(atom(), tuple(), tuple()) -> {ok, map()} | {error, term()}.
diff(RecName, OldRec, NewRec) ->
    diff(RecName, OldRec, NewRec, #{}).
diff(RecName, OldRec, NewRec, Args) when is_map(Args) ->
    MaxLayer = maps:get(max_layer, Args, infinity),
    State = #diff_state{max_layer = MaxLayer, layer = 0},
    diff(RecName, OldRec, NewRec, State);
diff(RecName, OldRec, NewRec, State = #diff_state{}) ->
    diff_rec(RecName, OldRec, NewRec, State).

diff_rec(RecName, OldRec, NewRec, State) when is_record(OldRec, RecName) andalso is_record(NewRec, RecName) ->
    case is_same_hash(OldRec, NewRec) of
        true ->
            same;
        false ->
            diff_rec_field(RecName, OldRec, NewRec, State, 2, tuple_size(NewRec), #{})
    end;
diff_rec(RecName, _OldRec, _NewRec, _State) ->
    {error, {not_record, RecName}}.

diff_rec_field(_RecName, _OldRec, _NewRec, _State, Pos, Size, DiffMap) when Pos > Size ->
    case map_size(DiffMap) > 0 of
        true ->
            {ok, DiffMap};
        false ->
            same
    end;
diff_rec_field(RecName, OldRec, NewRec, State, Pos, Size, DiffMap) ->
    FieldInfo = #rec_term_field{name = FieldName, type = FieldType} = rec_term_data:rec_field({RecName, Pos}),
    OldTerm = element(Pos, OldRec),
    NewTerm = element(Pos, NewRec),
    case diff_term(RecName, FieldName, FieldType, OldTerm, NewTerm, State) of
        same ->
            diff_rec_field(RecName, OldRec, NewRec, State, Pos + 1, Size, DiffMap);
        {ok, Diff} ->
            FieldBinName = FieldInfo#rec_term_field.bin_name,
            diff_rec_field(RecName, OldRec, NewRec, State, Pos + 1, Size, maps:put(FieldBinName, Diff, DiffMap));
        {error, Reason} ->
            {error, Reason}
    end.

diff_term(_RecName, _FieldName, {record, FieldRecName}, OldTerm, NewTerm, State) ->
    diff_rec(FieldRecName, OldTerm, NewTerm, State);
diff_term(RecName, FieldName, FieldType = {map, _KeyType, _ValType}, OldTerm, NewTerm, State) ->
    diff_map(RecName, FieldName, FieldType, OldTerm, NewTerm, State);
diff_term(RecName, FieldName, FieldType = {list, _ValType}, OldTerm, NewTerm, State) ->
    diff_list(RecName, FieldName, FieldType, OldTerm, NewTerm, State);
diff_term(RecName, FieldName, FieldType, OldTerm, NewTerm, State) ->
    diff_base(RecName, FieldName, FieldType, OldTerm, NewTerm, State).

diff_base(RecName, FieldName, FieldType, OldTerm, NewTerm, _State) ->
    case is_same_base(OldTerm, NewTerm) of
        true ->
            same;
        false ->
            {ok, OldMap} = rec_term:to_bson_term(RecName, FieldName, FieldType, OldTerm),
            {ok, NewMap} = rec_term:to_bson_term(RecName, FieldName, FieldType, NewTerm),
            Diff = #rec_term_diff{op = ?REC_TERM_DIFF_OP_UPDATE, old = OldMap, new = NewMap},
            {ok, Diff}
    end.

diff_map(RecName, FieldName, FieldType, OldMap, NewMap, State) when is_map(OldMap) andalso is_map(NewMap) ->
    case is_same_map(OldMap, NewMap) of
        true ->
            same;
        false ->
            DiffMap = diff_map_delete(RecName, FieldName, FieldType, OldMap, NewMap, State, #{}),
            NewDiffMap = diff_map_add_update(RecName, FieldName, FieldType, OldMap, NewMap, State, DiffMap),
            {ok, NewDiffMap}
    end;
diff_map(_RecName, _FieldName, _FieldType, _OldMap, _NewMap, _State) ->
    {error, not_map}.

diff_map_delete(RecName, FieldName, {map, KeyType, ValType}, OldMap, NewMap, _State, DiffMap) ->
    maps:fold(
        fun(Key, Val, Acc) ->
            true = rec_term:is_type(KeyType, Key),
            case maps:is_key(Key, NewMap) of
                false ->
                    BinKey = rec_term:to_binary(Key),
                    {ok, ValMap} = rec_term:to_bson_term(RecName, FieldName, ValType, Val),
                    Diff = #rec_term_diff{op = ?REC_TERM_DIFF_OP_DELETE, old = ValMap},
                    maps:put(BinKey, Diff, Acc);
                true ->
                    Acc
            end
        end, DiffMap, OldMap
    ).

diff_map_add_update(RecName, FieldName, {map, KeyType, ValType}, OldMap, NewMap, State, DiffMap) ->
    maps:fold(
        fun(Key, Val, Acc) ->
            true = rec_term:is_type(KeyType, Key),
            case maps:find(Key, OldMap) of
                error -> %% 新增
                    BinKey = rec_term:to_binary(Key),
                    {ok, ValMap} = rec_term:to_bson_term(RecName, FieldName, ValType, Val),
                    Diff = #rec_term_diff{op = ?REC_TERM_DIFF_OP_ADD, new = ValMap},
                    maps:put(BinKey, Diff, Acc);
                {ok, OldVal} ->
                    case diff_term(RecName, FieldName, ValType, OldVal, Val, State) of %% TODO 异常待处理
                        same ->
                            Acc;
                        {ok, Diff} ->
                            BinKey = rec_term:to_binary(Key),
                            maps:put(BinKey, Diff, Acc)
                    end
            end
        end, DiffMap, NewMap
    ).

diff_list(RecName, FieldName, FieldType, OldList, NewList, State) when is_list(OldList) andalso is_list(NewList)->
    case is_same_list(OldList, NewList) of
        true ->
            same;
        false ->
            do_diff_list(RecName, FieldName, FieldType, OldList, NewList, State, 0, #{})
    end;
diff_list(_RecName, _FieldName, _FieldType, _OldList, _NewList, _State) ->
    {error, not_list}.

do_diff_list(_RecName, _FieldName, _FieldType, [], [], _State, _Idx, DiffMap) ->
    {ok, DiffMap};
do_diff_list(RecName, FieldName, FieldType = {list, Type}, [OldI | OldList], [NewI | NewList], State, Idx, DiffMap) ->
    case diff_term(RecName, FieldName, Type, OldI, NewI, State) of
        same ->
            do_diff_list(RecName, FieldName, FieldType, OldList, NewList, State, Idx + 1, DiffMap);
        {ok, Diff} ->
            do_diff_list(RecName, FieldName, FieldType, OldList, NewList, State, Idx + 1, maps:put(Idx, Diff, DiffMap));
        {error, Reason} ->
            {error, Reason}
    end;
do_diff_list(RecName, FieldName, FieldType = {list, Type}, [OldI | OldList], [], State, Idx, DiffMap) ->
    {ok, OldMap} = rec_term:to_bson_term(RecName, FieldName, Type, OldI),
    Diff = #rec_term_diff{op = ?REC_TERM_DIFF_OP_DELETE, old = OldMap},
    do_diff_list(RecName, FieldName, FieldType, OldList, [], State, Idx + 1, maps:put(Idx, Diff, DiffMap));
do_diff_list(RecName, FieldName, FieldType = {list, Type}, [], [NewI | NewList], State, Idx, DiffMap) ->
    {ok, NewMap} = rec_term:to_bson_term(RecName, FieldName, Type, NewI),
    Diff = #rec_term_diff{op = ?REC_TERM_DIFF_OP_ADD, old = NewMap},
    do_diff_list(RecName, FieldName, FieldType, [], NewList, State, Idx + 1, maps:put(Idx, Diff, DiffMap)).

%% 累加层数
acc_layer(State) ->
    State#diff_state{layer = State#diff_state.layer + 1}.

is_same_base(OldVal, NewVal) ->
    OldVal =:= NewVal.

is_same_map(OldMap, NewMap) ->
    map_size(OldMap) == map_size(NewMap) andalso is_same_hash(OldMap, NewMap).

is_same_list([], []) ->
    true;
is_same_list([], [_ | _]) ->
    false;
is_same_list([_ | _], []) ->
    false;
is_same_list(OldList, NewList) ->
    is_same_hash(OldList, NewList).

is_same_hash(OldTerm, NewTerm) ->
    erlang:phash2(OldTerm) == erlang:phash2(NewTerm).