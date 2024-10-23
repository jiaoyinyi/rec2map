%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% record数据对比
%%% @end
%%% Created : 2022-12-24 00:00:00
%%%-------------------------------------------------------------------
-module(rec_diff).
-author("jiaoyinyi").

-export([
    diff/3, diff/4
    , merge/1, merge/2
    , mongo_update/1
]).

-include("rec_term.hrl").
-include("rec_diff.hrl").

%% 中间数据
-record(state, {
    max_layer                 :: non_neg_integer() |  infinity               %% 比较最大层数
    , layer                   :: pos_integer()                               %% 当前层数
}).

%% @doc 对比差异
-spec diff(atom(), tuple(), tuple()) -> {ok, #rec_diff{} | map()} | {error, term()}.
diff(RecName, OldRec, NewRec) ->
    diff(RecName, OldRec, NewRec, #{}).
diff(RecName, OldRec, NewRec, Args) when is_map(Args) ->
    MaxLayer = maps:get(max_layer, Args, infinity),
    State = #state{max_layer = MaxLayer, layer = 1},
    diff(RecName, OldRec, NewRec, State);
diff(RecName, OldRec, NewRec, State = #state{}) ->
    case diff_rec(RecName, OldRec, NewRec, State) of
        same ->
            {ok, #{}};
        Result ->
            Result
    end.

diff_rec(RecName, OldRec, NewRec, State = #state{layer = Layer, max_layer = MaxLayer}) ->
    case is_record(OldRec, RecName) andalso is_record(NewRec, RecName) of
        true ->
            try
                case OldRec =:= NewRec of
                    true ->
                        same;
                    false when Layer >= MaxLayer ->
                        {ok, OldRecMap} = rec_term:rec_to_map(OldRec),
                        {ok, NewRecMap} = rec_term:rec_to_map(NewRec),
                        Diff = #rec_diff{op = ?REC_DIFF_OP_UPDATE, old = OldRecMap, new = NewRecMap},
                        {ok, Diff};
                    false ->
                        diff_rec_field(RecName, OldRec, NewRec, acc_layer(State), 2, tuple_size(NewRec), #{})
                end
            catch
                _Class:_Reason:_Stacktrace ->
                    {error, {diff_rec_err, RecName, OldRec, NewRec, {_Reason, _Stacktrace}}}
            end;
        false ->
            {error, {diff_rec_err, RecName, OldRec, NewRec}}
    end.

diff_rec_field(_RecName, _OldRec, _NewRec, _State, Pos, Size, DiffMap) when Pos > Size ->
    {ok, DiffMap};
diff_rec_field(RecName, OldRec, NewRec, State, Pos, Size, DiffMap) ->
    FieldInfo = rec_term_data:rec_field({RecName, Pos}),
    OldTerm = element(Pos, OldRec),
    NewTerm = element(Pos, NewRec),
    case diff_term(RecName, FieldInfo, OldTerm, NewTerm, State) of
        same ->
            diff_rec_field(RecName, OldRec, NewRec, State, Pos + 1, Size, DiffMap);
        {ok, Diff} ->
            FieldBinName = FieldInfo#rec_term_field.bin_name,
            diff_rec_field(RecName, OldRec, NewRec, State, Pos + 1, Size, maps:put(FieldBinName, Diff, DiffMap));
        {error, Reason} ->
            {error, Reason}
    end.

diff_term(_RecName, _FieldInfo, undefined, undefined, _State) ->
    same;
diff_term(RecName, FieldInfo = #rec_term_field{default = Default}, OldTerm0, NewTerm0, State) ->
    OldTerm = rec_term:get_default(OldTerm0, Default),
    NewTerm = rec_term:get_default(NewTerm0, Default),
    do_diff_term(RecName, FieldInfo, OldTerm, NewTerm, State).

do_diff_term(_RecName, #rec_term_field{type = {record, FieldRecName}}, OldTerm, NewTerm, State) ->
    diff_rec(FieldRecName, OldTerm, NewTerm, State);
do_diff_term(RecName, FieldInfo = #rec_term_field{type = {map, _KeyType, _ValType}}, OldTerm, NewTerm, State) ->
    diff_map(RecName, FieldInfo, OldTerm, NewTerm, State);
do_diff_term(RecName, FieldInfo = #rec_term_field{type = {list, _ValType}}, OldTerm, NewTerm, State) ->
    diff_list(RecName, FieldInfo, OldTerm, NewTerm, State);
do_diff_term(RecName, FieldInfo, OldTerm, NewTerm, State) ->
    diff_base(RecName, FieldInfo, OldTerm, NewTerm, State).

diff_base(RecName, #rec_term_field{name = FieldName, type = FieldType}, OldTerm, NewTerm, _State) ->
    try
        case OldTerm =:= NewTerm of
            true ->
                same;
            false ->
                {ok, OldMap} = rec_term:to_bson_term(RecName, FieldName, FieldType, OldTerm),
                {ok, NewMap} = rec_term:to_bson_term(RecName, FieldName, FieldType, NewTerm),
                Diff = #rec_diff{op = ?REC_DIFF_OP_UPDATE, old = OldMap, new = NewMap},
                {ok, Diff}
        end
    catch
        _Class:_Reason:_Stacktrace ->
            {error, {diff_base_err, RecName, FieldName, FieldType, OldTerm, NewTerm, {_Reason, _Stacktrace}}}
    end.

diff_map(RecName, FieldInfo = #rec_term_field{name = FieldName, type = FieldType}, OldMap, NewMap, State = #state{layer = Layer, max_layer = MaxLayer}) when is_map(OldMap) andalso is_map(NewMap) ->
    try
        case OldMap =:= NewMap of
            true ->
                same;
            false when Layer >= MaxLayer ->
                {ok, OldMapMap} = rec_term:to_bson_term(RecName, FieldName, FieldType, OldMap),
                {ok, NewMapMap} = rec_term:to_bson_term(RecName, FieldName, FieldType, NewMap),
                Diff = #rec_diff{op = ?REC_DIFF_OP_UPDATE, old = OldMapMap, new = NewMapMap},
                {ok, Diff};
            false ->
                NewState = acc_layer(State),
                DiffMap = diff_map_delete(RecName, FieldInfo, OldMap, NewMap, NewState, #{}),
                NewDiffMap = diff_map_add_update(RecName, FieldInfo, OldMap, NewMap, NewState, DiffMap),
                {ok, NewDiffMap}
        end
    catch
        _Class:_Reason:_Stacktrace ->
            {error, {diff_map_err, RecName, FieldName, FieldType, OldMap, NewMap, {_Reason, _Stacktrace}}}
    end;
diff_map(RecName, #rec_term_field{name = FieldName, type = FieldType}, OldMap, NewMap, _State) ->
    {error, {diff_map_err, RecName, FieldName, FieldType, OldMap, NewMap}}.

diff_map_delete(RecName, #rec_term_field{name = FieldName, type = {map, KeyType, ValType}}, OldMap, NewMap, _State, DiffMap) ->
    maps:fold(
        fun(Key, Val, Acc) ->
            true = rec_term:is_type(KeyType, Key),
            case maps:is_key(Key, NewMap) of
                false ->
                    BinKey = rec_term:to_binary(Key),
                    {ok, ValMap} = rec_term:to_bson_term(RecName, FieldName, ValType, Val),
                    Diff = #rec_diff{op = ?REC_DIFF_OP_DELETE, old = ValMap},
                    maps:put(BinKey, Diff, Acc);
                true ->
                    Acc
            end
        end, DiffMap, OldMap
    ).

diff_map_add_update(RecName, FieldInfo = #rec_term_field{name = FieldName, type = {map, KeyType, ValType}}, OldMap, NewMap, State, DiffMap) ->
    maps:fold(
        fun(Key, Val, Acc) ->
            true = rec_term:is_type(KeyType, Key),
            case maps:find(Key, OldMap) of
                error ->
                    BinKey = rec_term:to_binary(Key),
                    {ok, ValMap} = rec_term:to_bson_term(RecName, FieldName, ValType, Val),
                    Diff = #rec_diff{op = ?REC_DIFF_OP_ADD, new = ValMap},
                    maps:put(BinKey, Diff, Acc);
                {ok, OldVal} ->
                    case diff_term(RecName, FieldInfo#rec_term_field{type = ValType}, OldVal, Val, State) of
                        same ->
                            Acc;
                        {ok, Diff} ->
                            BinKey = rec_term:to_binary(Key),
                            maps:put(BinKey, Diff, Acc)
                    end
            end
        end, DiffMap, NewMap
    ).

diff_list(RecName, FieldInfo = #rec_term_field{name = FieldName, type = FieldType}, OldList, NewList, State = #state{layer = Layer, max_layer = MaxLayer}) when is_list(OldList) andalso is_list(NewList) ->
    try
        case OldList =:= NewList of
            true ->
                same;
            false when Layer >= MaxLayer ->
                {ok, OldListMap} = rec_term:to_bson_term(RecName, FieldName, FieldType, OldList),
                {ok, NewListMap} = rec_term:to_bson_term(RecName, FieldName, FieldType, NewList),
                Diff = #rec_diff{op = ?REC_DIFF_OP_UPDATE, old = OldListMap, new = NewListMap},
                {ok, Diff};
            false ->
                NewState = acc_layer(State),
                do_diff_list(RecName, FieldInfo, OldList, NewList, NewState, 0, #{})
        end
    catch
        _Class:_Reason:_Stacktrace ->
            {error, {diff_list_err, RecName, FieldName, FieldType, OldList, NewList, {_Reason, _Stacktrace}}}
    end;
diff_list(RecName, #rec_term_field{name = FieldName, type = FieldType}, OldList, NewList, _State) ->
    {error, {diff_list_err, RecName, FieldName, FieldType, OldList, NewList}}.

do_diff_list(_RecName, _FieldInfo, [], [], _State, _Idx, DiffMap) ->
    {ok, DiffMap};
do_diff_list(RecName, FieldInfo = #rec_term_field{type = {list, Type}}, [OldI | OldList], [NewI | NewList], State, Idx, DiffMap) ->
    case diff_term(RecName, FieldInfo#rec_term_field{type = Type}, OldI, NewI, State) of
        same ->
            do_diff_list(RecName, FieldInfo, OldList, NewList, State, Idx + 1, DiffMap);
        {ok, Diff} ->
            do_diff_list(RecName, FieldInfo, OldList, NewList, State, Idx + 1, maps:put(Idx, Diff, DiffMap));
        {error, Reason} ->
            {error, Reason}
    end;
do_diff_list(RecName, FieldInfo = #rec_term_field{name = FieldName, type = {list, Type}}, [OldI | OldList], [], State, Idx, DiffMap) ->
    {ok, OldMap} = rec_term:to_bson_term(RecName, FieldName, Type, OldI),
    Diff = #rec_diff{op = ?REC_DIFF_OP_DELETE, old = OldMap},
    do_diff_list(RecName, FieldInfo, OldList, [], State, Idx + 1, maps:put(Idx, Diff, DiffMap));
do_diff_list(RecName, FieldInfo = #rec_term_field{name = FieldName, type = {list, Type}}, [], [NewI | NewList], State, Idx, DiffMap) ->
    {ok, NewMap} = rec_term:to_bson_term(RecName, FieldName, Type, NewI),
    Diff = #rec_diff{op = ?REC_DIFF_OP_ADD, new = NewMap},
    do_diff_list(RecName, FieldInfo, [], NewList, State, Idx + 1, maps:put(Idx, Diff, DiffMap)).

%% 累加层数
acc_layer(State) ->
    State#state{layer = State#state.layer + 1}.

%% @doc 合并对比
-spec merge([#rec_diff{} | map()]) -> #rec_diff{} | map().
merge([DiffMap]) ->
    DiffMap;
merge([OldDiffMap, NewDiffMap | DiffMaps]) ->
    merge([merge(OldDiffMap, NewDiffMap) | DiffMaps]).

merge(OldDiff = #rec_diff{}, NewDiff = #rec_diff{}) ->
    merge_diff(OldDiff, NewDiff);
merge(OldDiffMap, NewDiffMap) when is_map(OldDiffMap) andalso map_size(OldDiffMap) == 0 ->
    NewDiffMap;
merge(OldDiffMap, NewDiffMap) when is_map(NewDiffMap) andalso map_size(NewDiffMap) == 0 ->
    OldDiffMap;
merge(OldDiff = #rec_diff{}, NewDiffMap) ->
    merge_diff(OldDiff, NewDiffMap);
merge(OldDiffMap, NewDiff = #rec_diff{}) ->
    merge_diff(OldDiffMap, NewDiff);
merge(OldDiffMap, NewDiffMap) ->
    maps:fold(
        fun(Key, Diff, Acc) ->
            case maps:find(Key, Acc) of
                {ok, NewDiff0} ->
                    case merge_diff(Diff, NewDiff0) of
                        remove ->
                            maps:remove(Key, Acc);
                        NewDiff ->
                            maps:put(Key, NewDiff, Acc)
                    end;
                error ->
                    maps:put(Key, Diff, Acc)
            end
        end, NewDiffMap, OldDiffMap
    ).

merge_diff(#rec_diff{op = ?REC_DIFF_OP_ADD}, #rec_diff{op = ?REC_DIFF_OP_UPDATE, new = NewVal}) -> %% 新增操作合并更新操作，更新新增操作的值
    #rec_diff{op = ?REC_DIFF_OP_ADD, new = NewVal};
merge_diff(#rec_diff{op = ?REC_DIFF_OP_ADD}, #rec_diff{op = ?REC_DIFF_OP_DELETE}) -> %% 新增操作合并删除操作，移除该操作
    remove;
merge_diff(#rec_diff{op = ?REC_DIFF_OP_UPDATE, old = OldVal}, #rec_diff{op = ?REC_DIFF_OP_UPDATE, new = NewVal}) when OldVal =:= NewVal -> %% 更新操作合并更新操作，值相同
    remove;
merge_diff(#rec_diff{op = ?REC_DIFF_OP_UPDATE, old = OldVal}, #rec_diff{op = ?REC_DIFF_OP_UPDATE, new = NewVal}) -> %% 更新操作合并更新操作，更新更新操作的值
    #rec_diff{op = ?REC_DIFF_OP_UPDATE, old = OldVal, new = NewVal};
merge_diff(#rec_diff{op = ?REC_DIFF_OP_UPDATE, old = OldVal}, #rec_diff{op = ?REC_DIFF_OP_DELETE}) -> %% 更新操作合并删除操作，更新为删除操作
    #rec_diff{op = ?REC_DIFF_OP_DELETE, old = OldVal};
merge_diff(#rec_diff{op = ?REC_DIFF_OP_DELETE, old = OldVal}, #rec_diff{op = ?REC_DIFF_OP_ADD, new = NewVal}) when OldVal =:= NewVal -> %% 删除操作合并新增操作，值相同
    remove;
merge_diff(#rec_diff{op = ?REC_DIFF_OP_DELETE, old = OldVal}, #rec_diff{op = ?REC_DIFF_OP_ADD, new = NewVal}) -> %% 删除操作合并新增操作，更新为更新操作
    #rec_diff{op = ?REC_DIFF_OP_UPDATE, old = OldVal, new = NewVal};
merge_diff(OldDiff, #rec_diff{op = ?REC_DIFF_OP_UPDATE, old = OldVal, new = NewVal}) when is_map(OldDiff) -> %% 子值操作合并更新操作，更新更新操作的值
    #rec_diff{op = ?REC_DIFF_OP_UPDATE, old = OldVal, new = NewVal};
merge_diff(OldDiff, #rec_diff{op = ?REC_DIFF_OP_DELETE, old = OldVal}) when is_map(OldDiff) -> %% 子值操作合并删除操作，更新为删除操作
    #rec_diff{op = ?REC_DIFF_OP_DELETE, old = OldVal};
merge_diff(#rec_diff{op = ?REC_DIFF_OP_ADD, new = NewVal0}, NewDiff) when (is_map(NewVal0) orelse is_list(NewVal0)) andalso is_map(NewDiff) -> %% 新增操作合并子值操作，更新新增操作的值
    Ret =
        case merge_diff_vals(NewVal0, NewDiff) of
            remove ->
                remove;
            NewVal ->
                #rec_diff{op = ?REC_DIFF_OP_ADD, new = NewVal}
        end,
    Ret;
merge_diff(#rec_diff{op = ?REC_DIFF_OP_UPDATE, old = OldVal, new = NewVal0}, NewDiff) when (is_map(NewVal0) orelse is_list(NewVal0)) andalso is_map(NewDiff) -> %% 更新操作合并子值操作，更新更新操作的值
    case merge_diff_vals(NewVal0, NewDiff) of
        remove ->
            remove;
        NewVal ->
            #rec_diff{op = ?REC_DIFF_OP_UPDATE, old = OldVal, new = NewVal}
    end;
merge_diff(OldDiff, NewDiff) when is_map(OldDiff) andalso is_map(NewDiff) -> %% 子值操作合并子值操作，递归按前面规则合并处理
    merge(OldDiff, NewDiff).

merge_diff_vals(ValMap, DiffMap) when is_map(ValMap) ->
    Ret = maps:fold(
        fun(Key, Diff, Acc) ->
            merge_diff_val(Acc, Key, Diff)
        end, ValMap, DiffMap
    ),
    case map_size(Ret) == 0 of
        true ->
            remove;
        _ ->
            Ret
    end;
merge_diff_vals(ValList, DiffMap) when is_list(ValList) ->
    ValMap = list_to_map(ValList),
    Ret = maps:fold(
        fun(Key, Diff, Acc) ->
            merge_diff_val(Acc, Key, Diff)
        end, ValMap, DiffMap
    ),
    maps:values(Ret).

merge_diff_val(ValMap, Key, #rec_diff{op = ?REC_DIFF_OP_ADD, new = Val}) ->
    maps:put(Key, Val, ValMap);
merge_diff_val(ValMap, Key, #rec_diff{op = ?REC_DIFF_OP_UPDATE, new = Val}) ->
    maps:put(Key, Val, ValMap);
merge_diff_val(ValMap, Key, #rec_diff{op = ?REC_DIFF_OP_DELETE}) ->
    maps:remove(Key, ValMap);
merge_diff_val(ValMap, Key, DiffMap) when is_map(DiffMap) -> %% 子值还是maps格式，递归处理 #{<<"data">> => #{<<"int">> => 1},<<"msg">> => <<"hello">>} #{<<"data">> => #{<<"float">> => {rec_diff,add,undefined,1.2}, <<"int">> => {rec_diff,update,1,2}}}
    Val = maps:get(Key, ValMap),
    case merge_diff_vals(Val, DiffMap) of
        remove ->
            maps:remove(Key, ValMap);
        NewVal ->
            maps:put(Key, NewVal, ValMap)
    end.

list_to_map(List) ->
    list_to_map(List, 0, #{}).
list_to_map([], _Idx, Map) ->
    Map;
list_to_map([I | List], Idx, Map) ->
    list_to_map(List, Idx + 1, maps:put(Idx, I, Map)).

%% @doc 差异数据转mongodb的更新操作值
-spec mongo_update(map()) -> map().
mongo_update(DiffMap) ->
    mongo_update(DiffMap, #{}).
mongo_update(DiffMap, UpdateMap) ->
    maps:fold(
        fun(Key, Val, Acc) ->
            do_mongo_update(Key, Val, Acc)
        end, UpdateMap, DiffMap
    ).

do_mongo_update(Key, #rec_diff{op = ?REC_DIFF_OP_ADD, new = New}, UpdateMap) ->
    set_update_map(<<"$set">>, Key, New, UpdateMap);
do_mongo_update(Key, #rec_diff{op = ?REC_DIFF_OP_UPDATE, new = New}, UpdateMap) ->
    set_update_map(<<"$set">>, Key, New, UpdateMap);
do_mongo_update(Key, #rec_diff{op = ?REC_DIFF_OP_DELETE}, UpdateMap) ->
    set_update_map(<<"$unset">>, Key, <<>>, UpdateMap);
do_mongo_update(Key, ValMap, UpdateMap) when is_map(ValMap) -> %% 内嵌子项处理
    maps:fold(
        fun(SubKey, SubVal, Acc) ->
            NewKey = key_join(Key, SubKey),
            do_mongo_update(NewKey, SubVal, Acc)
        end, UpdateMap, ValMap
    ).

set_update_map(Label, Key, Val, UpdateMap) ->
    LabelMap = maps:get(Label, UpdateMap, #{}),
    NewLabelMap = maps:put(Key, Val, LabelMap),
    maps:put(Label, NewLabelMap, UpdateMap).

key_join(Key, SubKey) ->
    <<(rec_term:to_binary(Key))/binary, ".", (rec_term:to_binary(SubKey))/binary>>.