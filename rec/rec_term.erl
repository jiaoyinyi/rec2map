%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% record数据转换
%%% @end
%%% Created : 2022-12-24 00:00:00
%%%-------------------------------------------------------------------
-module(rec_term).
-author("jiaoyinyi").

-export([
    rec_to_map/1
    , rec_field_to_map_field/3
    , map_to_rec/1, map_to_rec/2
    , map_field_to_rec_field/3
]).

%% @doc record转成bson的map结构
-spec rec_to_map(tuple()) -> {ok, map()} | {error, term()}.
rec_to_map(Rec) when is_tuple(Rec) andalso tuple_size(Rec) > 0 ->
    RecName = element(1, Rec),
    rec_to_map(rec_term_data:rec_info(RecName), RecName, rec_term_data:bin_name(RecName), Rec, #{}).
rec_to_map([], _RecName, RecBinName, _Rec, Map) ->
    NewMap = maps:put(<<"_rec_name">>, RecBinName, Map),
    {ok, NewMap};
rec_to_map([Info | RecInfo], RecName, RecBinName, Rec, Map) ->
    FieldName = element(1, Info),
    FieldPos = element(2, Info),
    FieldBinName = element(3, Info),
    FieldType = element(4, Info),
    Default = element(5, Info),
    FieldTerm = get_default(element(FieldPos, Rec), Default),
    case to_bson_term(RecName, FieldName, FieldType, FieldTerm) of
        {ok, BsonTerm} ->
            NewMap = maps:put(FieldBinName, BsonTerm, Map),
            rec_to_map(RecInfo, RecName, RecBinName, Rec, NewMap);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc record字段转成bson的map结构字段
-spec rec_field_to_map_field(atom(), pos_integer(), term()) -> {ok, map()} | {error, term()}.
rec_field_to_map_field(RecName, Pos, RecField) ->
    case Pos > 1 andalso Pos =< rec_term_data:rec_size(RecName) of
        true ->
            Info = rec_term_data:rec_field({RecName, Pos}),
            FieldName = element(1, Info),
            FieldBinName = element(3, Info),
            FieldType = element(4, Info),
            Default = element(5, Info),
            FieldTerm = get_default(RecField, Default),
            case to_bson_term(RecName, FieldName, FieldType, FieldTerm) of
                {ok, BsonTerm} ->
                    Map = #{FieldBinName => BsonTerm},
                    {ok, Map};
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, {rec_pos_err, RecName, Pos}}
    end.

%% @doc bson的map结构转成record
-spec map_to_rec(map()) -> {ok, tuple()} | {error, term()}.
map_to_rec(Map) ->
    case maps:find(<<"_rec_name">>, Map) of
        {ok, RecBinName} ->
            RecName = binary_to_atom(RecBinName, utf8),
            map_to_rec(rec_term_data:rec_info(RecName), RecName, Map, rec_term_data:rec(RecName));
        _ ->
            {error, not_rec_map}
    end.
%% @doc bson的map结构转成record
-spec map_to_rec(atom(), map()) -> {ok, tuple()} | {error, term()}.
map_to_rec(RecName, Map) ->
    RecBinName = rec_term_data:bin_name(RecName),
    case maps:find(<<"_rec_name">>, Map) of
        {ok, RecBinName} ->
            map_to_rec(rec_term_data:rec_info(RecName), RecName, Map, rec_term_data:rec(RecName));
        _ ->
            {error, {rec_err, RecBinName, Map}}
    end.
map_to_rec([], _RecName, _Map, Rec) ->
    {ok, Rec};
map_to_rec([Info | RecInfo], RecName, Map, Rec) ->
    FieldName = element(1, Info),
    FieldPos = element(2, Info),
    FieldBinName = element(3, Info),
    FieldType = element(4, Info),
    case maps:find(FieldBinName, Map) of
        {ok, BsonTerm} ->
            case to_erl_term(RecName, FieldName, FieldType, BsonTerm) of
                {ok, ErlTerm} ->
                    NewRec = setelement(FieldPos, Rec, ErlTerm),
                    map_to_rec(RecInfo, RecName, Map, NewRec);
                {error, Reason} ->
                    {error, Reason}
            end;
        error ->
            Default = element(5, Info),
            ErlTerm = get_default(element(FieldPos, Rec), Default),
            NewRec = setelement(FieldPos, Rec, ErlTerm),
            map_to_rec(RecInfo, RecName, Map, NewRec)
    end.

%% @doc bson的map结构字段转成record字段
-spec map_field_to_rec_field(atom(), pos_integer(), map()) -> {ok, term()} | {error, term()}.
map_field_to_rec_field(RecName, Pos, MapField) ->
    Info = rec_term_data:rec_field({RecName, Pos}),
    FieldName = element(1, Info),
    FieldBinName = element(3, Info),
    FieldType = element(4, Info),
    case maps:find(FieldBinName, MapField) of
        {ok, BsonTerm} ->
            to_erl_term(RecName, FieldName, FieldType, BsonTerm);
        error  ->
            {error, {map_field_err, RecName, Pos, MapField}}
    end.

%% @doc 转成bson的项
to_bson_term(_RecName, _FieldName, atom, Data) when is_atom(Data) ->
    {ok, atom_to_binary(Data)};
to_bson_term(_RecName, _FieldName, integer, Data) when is_integer(Data) ->
    {ok, Data};
to_bson_term(_RecName, _FieldName, pos_integer, Data) when is_integer(Data) andalso Data > 0 ->
    {ok, Data};
to_bson_term(_RecName, _FieldName, neg_integer, Data) when is_integer(Data) andalso Data < 0 ->
    {ok, Data};
to_bson_term(_RecName, _FieldName, non_neg_integer, Data) when is_integer(Data) andalso Data >= 0 ->
    {ok, Data};
to_bson_term(_RecName, _FieldName, float, Data) when is_float(Data) ->
    {ok, Data};
to_bson_term(_RecName, _FieldName, boolean, Data) when is_boolean(Data) ->
    {ok, Data};
to_bson_term(_RecName, _FieldName, binary, Data) when is_binary(Data) ->
    {ok, Data};
to_bson_term(RecName, FieldName, {record, FieldRecName}, Data) ->
    try
        true = is_record(Data, FieldRecName),
        rec_to_map(Data)
    catch
        _Class:_Reason ->
            {error, {field_err, RecName, FieldName, {record, FieldRecName}, Data, {_Class, _Reason}}}
    end;
to_bson_term(RecName, FieldName, {list, Type}, Data) when is_list(Data) ->
    try
        BsonTerm =
            [begin
                 true = is_type(Type, D),
                 {ok, Term} = to_bson_term(RecName, FieldName, Type, D),
                 Term
             end || D <- Data],
        {ok, BsonTerm}
    catch
        _Class:_Reason ->
            {error, {field_err, RecName, FieldName, {list, Type}, Data, {_Class, _Reason}}}
    end;
to_bson_term(RecName, FieldName, {map, KeyType, ValType}, Data) when is_map(Data) ->
    try
        BsonTerm =
            maps:fold(
                fun(Key, Val, Acc) ->
                    true = is_type(KeyType, Key),
                    {ok, Term} = to_bson_term(RecName, FieldName, ValType, Val),
                    maps:put(to_binary(Key), Term, Acc)
                end, #{}, Data
            ),
        {ok, BsonTerm}
    catch
        _Class:_Reason ->
            {error, {field_err, RecName, FieldName, {map, KeyType, ValType}, Data, {_Class, _Reason}}}
    end;
to_bson_term(RecName, FieldName, {tuple, ElemTypes}, Data) when is_tuple(Data) andalso length(ElemTypes) =:= tuple_size(Data) ->
    try
        {_, BsonTerm} =
            lists:foldl(
                fun(ElemType, {Pos, Elems}) ->
                    {ok, Elem} = to_bson_term(RecName, FieldName, ElemType, element(Pos, Data)),
                    NewElems = maps:put(integer_to_binary(Pos), Elem, Elems),
                    {Pos + 1, NewElems}
                end, {1, #{}}, ElemTypes
            ),
        {ok, BsonTerm}
    catch
        _Class:_Reason ->
            {error, {field_err, RecName, FieldName, {tuple, ElemTypes}, Data, {_Class, _Reason}}}
    end;
to_bson_term(RecName, FieldName, {union, Type, Values}, Data) ->
    try
        true = lists:member(Data, Values),
        to_bson_term(RecName, FieldName, Type, Data)
    catch
        _Class:_Reason ->
            {error, {field_err, RecName, FieldName, {union, Type, Values}, Data, {_Class, _Reason}}}
    end;
to_bson_term(RecName, FieldName, Type, Data) ->
    {error, {field_err, RecName, FieldName, Type, Data}}.

%% 转成erlang的项
to_erl_term(_RecName, _FieldName, atom, Data) when is_binary(Data) ->
    {ok, binary_to_atom(Data)};
to_erl_term(_RecName, _FieldName, integer, Data) when is_integer(Data) ->
    {ok, Data};
to_erl_term(RecName, FieldName, integer, Data) when is_binary(Data) ->
    to_erl_term(RecName, FieldName, integer, binary_to_integer(Data));
to_erl_term(_RecName, _FieldName, pos_integer, Data) when is_integer(Data) andalso Data > 0 ->
    {ok, Data};
to_erl_term(RecName, FieldName, pos_integer, Data) when is_binary(Data) ->
    to_erl_term(RecName, FieldName, pos_integer, binary_to_integer(Data));
to_erl_term(_RecName, _FieldName, neg_integer, Data) when is_integer(Data) andalso Data < 0 ->
    {ok, Data};
to_erl_term(RecName, FieldName, neg_integer, Data) when is_binary(Data) ->
    to_erl_term(RecName, FieldName, neg_integer, binary_to_integer(Data));
to_erl_term(_RecName, _FieldName, non_neg_integer, Data) when is_integer(Data) andalso Data >= 0 ->
    {ok, Data};
to_erl_term(RecName, FieldName, non_neg_integer, Data) when is_binary(Data) ->
    to_erl_term(RecName, FieldName, non_neg_integer, binary_to_integer(Data));
to_erl_term(_RecName, _FieldName, float, Data) when is_float(Data) ->
    {ok, Data};
to_erl_term(RecName, FieldName, float, Data) when is_binary(Data) ->
    to_erl_term(RecName, FieldName, float, binary_to_float(Data));
to_erl_term(_RecName, _FieldName, boolean, Data) when is_boolean(Data) ->
    {ok, Data};
to_erl_term(_RecName, _FieldName, binary, Data) when is_binary(Data) ->
    {ok, Data};
to_erl_term(_RecName, _FieldName, {record, FieldRecName}, Data) when is_map(Data) ->
    map_to_rec(FieldRecName, Data);
to_erl_term(RecName, FieldName, {list, Type}, Data) when is_list(Data) ->
    try
        ErlTerm =
            [begin
                 {ok, Term} = to_erl_term(RecName, FieldName, Type, D),
                 Term
             end || D <- Data],
        {ok, ErlTerm}
    catch
        _Class:_Reason ->
            {error, {field_err, RecName, FieldName, {list, Type}, Data, {_Class, _Reason}}}
    end;
to_erl_term(RecName, FieldName, {map, KeyType, ValType}, Data) when is_map(Data) ->
    try
        ErlTerm =
            maps:fold(
                fun(Key, Val, Acc) ->
                    {ok, KeyTerm} = to_erl_term(RecName, FieldName, KeyType, Key),
                    {ok, ValTerm} = to_erl_term(RecName, FieldName, ValType, Val),
                    maps:put(KeyTerm, ValTerm, Acc)
                end, #{}, Data
            ),
        {ok, ErlTerm}
    catch
        _Class:_Reason ->
            {error, {field_err, RecName, FieldName, {map, KeyType, ValType}, Data, {_Class, _Reason}}}
    end;
to_erl_term(RecName, FieldName, {tuple, ElemTypes}, Data) when is_map(Data) andalso length(ElemTypes) =:= map_size(Data) ->
    try
        {_, NewElems} =
            lists:foldl(
                fun(ElemType, {Pos, Elems}) ->
                    BsonElem = maps:get(integer_to_binary(Pos), Data),
                    {ok, Elem} = to_erl_term(RecName, FieldName, ElemType, BsonElem),
                    {Pos + 1, [Elem | Elems]}
                end, {1, []}, ElemTypes
            ),
        ErlTerm = list_to_tuple(lists:reverse(NewElems)),
        {ok, ErlTerm}
    catch
        _Class:_Reason ->
            {error, {field_err, RecName, FieldName, {tuple, ElemTypes}, Data, {_Class, _Reason}}}
    end;
to_erl_term(RecName, FieldName, {union, Type, Values}, Data) ->
    try
        {ok, ErlTerm} = to_erl_term(RecName, FieldName, Type, Data),
        true = lists:member(ErlTerm, Values),
        {ok, ErlTerm}
    catch
        _Class:_Reason ->
            {error, {field_err, RecName, FieldName, {union, Type, Values}, Data, {_Class, _Reason}}}
    end;
to_erl_term(RecName, FieldName, Type, Data) ->
    {error, {field_err, RecName, FieldName, Type, Data}}.

%% @doc 数据是否为对应的类型
is_type(atom, Data) when is_atom(Data) ->
    true;
is_type(integer, Data) when is_integer(Data) ->
    true;
is_type(pos_integer, Data) when is_integer(Data) andalso Data > 0 ->
    true;
is_type(neg_integer, Data) when is_integer(Data) andalso Data < 0 ->
    true;
is_type(non_neg_integer, Data) when is_integer(Data) andalso Data >= 0 ->
    true;
is_type(float, Data) when is_float(Data) ->
    true;
is_type(boolean, Data) when is_boolean(Data) ->
    true;
is_type(binary, Data) when is_binary(Data) ->
    true;
is_type({record, RecName}, Data) ->
    is_record(Data, RecName);
is_type({union, Type, _Values}, Data) ->
    is_type(Type, Data);
is_type(_Type, _Data) ->
    false.

%% @doc 转成二进制
to_binary(Data) when is_atom(Data) ->
    atom_to_binary(Data);
to_binary(Data) when is_integer(Data) ->
    integer_to_binary(Data);
to_binary(Data) when is_float(Data) ->
    float_to_binary(Data);
to_binary(Data) when is_binary(Data) ->
    Data.

%% @doc 获取默认值
get_default(undefined, {record, RecName}) ->
    get_rec_default(rec_term_data:rec_info(RecName), rec_term_data:rec(RecName));
get_default(Rec, {record, RecName}) ->
    case is_record(Rec, RecName) of
        true ->
            get_rec_default(rec_term_data:rec_info(RecName), Rec);
        _ ->
            error({get_default_rec_err, RecName, Rec})
    end;
get_default(undefined, Default) ->
    Default;
get_default(Data, _Default) ->
    Data.

%% @doc 获取记录默认值
get_rec_default([], Rec) ->
    Rec;
get_rec_default([Info | RecInfo], Rec) ->
    Pos = element(2, Info),
    Default = element(5, Info),
    Term = element(Pos, Rec),
    NewTerm = get_default(Term, Default),
    NewRec = setelement(Pos, Rec, NewTerm),
    get_rec_default(RecInfo, NewRec).