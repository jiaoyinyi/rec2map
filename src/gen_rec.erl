%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% 生成记录转换数据
%%% @end
%%% Created : 2022-12-24 00:00:00
%%%-------------------------------------------------------------------
-module(gen_rec).
-author("jiaoyinyi").

-include("gen_rec.hrl").

-export([
    main/2
]).

%% @doc 主入口
-spec main(string(), string()) -> ok.
main(IncPath, OutPath) ->
    FileNames = get_filenames(IncPath),
    Recs = parse(FileNames, IncPath),
    dump(FileNames, Recs, OutPath).

%% @doc 获取要解析的文件名
-spec get_filenames(string()) -> [string()].
get_filenames(IncPath) ->
    [FileName || FileName <- filelib:wildcard("*.hrl", IncPath), not lists:member(FileName, ?EXCLUDE_HRLS)].

%% @doc 解析每个文件的宏定义
parse(FileNames, IncPath) ->
    parse(FileNames, IncPath, []).
parse([], _IncPath, Recs) ->
    lists:keysort(#rec.name, Recs);
parse([FileName | FileNames], IncPath, Recs) ->
    {ok, Epp} = epp:open(filename:join(IncPath, FileName), IncPath),
    AddRecs = parse_recs(Epp),
    parse(FileNames, IncPath, AddRecs ++ Recs).

%% @doc 解析所有记录
parse_recs(Epp) ->
    parse_recs(Epp, []).
parse_recs(Epp, Acc) ->
    case epp:parse_erl_form(Epp) of
        {ok, {attribute, _, record, {RecName, RecFields}}} ->
            case not lists:member(RecName, ?EXCLUDE_RECORDS) of
                true ->
                    Rec = parse_rec(RecName, RecFields),
                    parse_recs(Epp, [Rec | Acc]);
                _ ->
                    parse_recs(Epp, Acc)
            end;
        {eof, _} ->
            Acc;
        _ ->
            parse_recs(Epp, Acc)
    end.

%% @doc 解析记录
parse_rec(RecName, RecFields) ->
    Fields = parse_rec_fields(RecName, 2, RecFields, []),
    #rec{name = RecName, fields = Fields, size = length(Fields) + 1}.

%% @doc 解析记录所有字段
parse_rec_fields(_RecName, _NextPos, [], Fields) ->
    lists:reverse(Fields);
parse_rec_fields(RecName, NextPos, [RecField | RecFields], Fields) ->
    Field = parse_rec_field(RecName, NextPos, RecField),
    parse_rec_fields(RecName, NextPos + 1, RecFields, [Field | Fields]).

%% @doc 解析记录字段
parse_rec_field(RecName, Pos, {typed_record_field, {record_field, _, AfFieldName}, AbstractType}) -> %% 不带默认值
    FieldName = get_field_name(AfFieldName),
    FieldType = get_field_type(RecName, FieldName, AbstractType),
    FieldDefault = get_field_default(RecName, FieldName, FieldType, undefined),
    #rec_field{name = FieldName, pos = Pos, type = FieldType, default = FieldDefault};
parse_rec_field(RecName, Pos, {typed_record_field, {record_field, _, AfFieldName, AbstractExpr}, AbstractType}) -> %% 带默认值
    FieldName = get_field_name(AfFieldName),
    FieldType = get_field_type(RecName, FieldName, AbstractType),
    FieldDefault = get_field_default(RecName, FieldName, FieldType, AbstractExpr),
    #rec_field{name = FieldName, pos = Pos, type = FieldType, default = FieldDefault}.

%% @doc 获取字段名
get_field_name({_, _, Name}) ->
    Name.

%% @doc 获取字段类型
get_field_type(RecName, FieldName, Type) ->
    case do_get_field_type(Type) of
        {error, _Reason} ->
            ?PRINT("记录~w的~w字段为非法字段类型：~w", [RecName, FieldName, Type]),
            exit(1);
        FieldType ->
            FieldType
    end.

do_get_field_type({type, _, Type, _}) when ?IS_BASE_TYPE(Type) -> %% 基础类型
    Type;
do_get_field_type({type, _, record, [{_, _, RecName}]}) -> %% 记录类型 #RecName{}
    {record, RecName};
do_get_field_type({type, _, tuple, [{atom, _, record}, {atom, _, RecName}]}) -> %% 记录类型 {record,RecName}
    {record, RecName};
do_get_field_type({type, _, list, [{type, _, Type, _}]}) when ?IS_BASE_TYPE(Type) -> %% 基础类型列表 [Type]
    {list, Type};
do_get_field_type({type, _, list, [{type, _, record, [{_, _, RecName}]}]}) -> %% 记录类型列表 [#RecName{}]
    {list, {record, RecName}};
do_get_field_type({type, _, list, [{type, _, tuple, [{atom, _, record}, {atom, _, RecName}]}]}) -> %% 记录类型列表 [{record,RecName}]
    {list, {record, RecName}};
do_get_field_type({type, _, map, [{type, _, map_field_assoc, [{type, _, KeyType, _}, {type, _, ValType, _}]}]}) when ?IS_BASE_TYPE(KeyType) andalso ?IS_BASE_TYPE(ValType) -> %% key为基础类型，val为基础类型的map #{KeyType => ValType}
    {map, KeyType, ValType};
do_get_field_type({type, _, map, [{type, _, map_field_assoc, [{type, _, KeyType, _}, {type, _, record, [{_, _, RecName}]}]}]}) when ?IS_BASE_TYPE(KeyType) -> %% key为基础类型，val为记录的map #{KeyType => #RecName{}}
    {map, KeyType, {record, RecName}};
do_get_field_type({type, _, map, [{type, _, map_field_assoc, [{type, _, KeyType, _}, {type, _, tuple, [{atom, _, record}, {atom, _, RecName}]}]}]}) when ?IS_BASE_TYPE(KeyType) -> %% key为基础类型，val为记录的map #{KeyType => {record,RecName}}
    {map, KeyType, {record, RecName}};
do_get_field_type({type, _, tuple, Types}) -> %% 元组类型 {ElemType1,ElemType2}
    case get_tuple_elem_types(Types) of
        ElemTypes = [_ | _] ->
            {tuple, ElemTypes};
        {error, Reason} ->
            {error, Reason}
    end;
do_get_field_type({type, _, union, Unions}) -> %% 范围 worker | supervisor
    case get_union_type_values(Unions) of
        {error, Reason} ->
            {error, Reason};
        {Type, Values} ->
            {union, Type, Values}
    end;
do_get_field_type(_Type) ->
    {error, type_err}.

%% @doc 获取元组每项类型
get_tuple_elem_types(Types) ->
    get_tuple_elem_types(Types, []).
get_tuple_elem_types([], ElemTypes) ->
    lists:reverse(ElemTypes);
get_tuple_elem_types([{type, _, ElemType, _} | Types], ElemTypes) when ?IS_BASE_TYPE(ElemType) ->
    get_tuple_elem_types(Types, [ElemType | ElemTypes]);
get_tuple_elem_types(_Types, _ElemTypes) ->
    {error, tuple_elem_type_err}.

%% @doc 获取范围的类型和值
get_union_type_values([{Type, _, Value} | Unions]) when ?IS_BASE_TYPE(Type) ->
    get_union_type_values(Unions, Type, [Value]);
get_union_type_values(_Unions) ->
    {error, union_type_value_err}.
get_union_type_values([], Type, Values) ->
    {Type, lists:reverse(Values)};
get_union_type_values([{Type, _, Value} | Unions], Type, Values) when ?IS_BASE_TYPE(Type) ->
    get_union_type_values(Unions, Type, [Value | Values]);
get_union_type_values(_Unions, _Type, _Values) ->
    {error, union_type_value_err}.

%% @doc 获取字段默认值
get_field_default(RecName, FieldName, Type, Default) ->
    case do_get_field_default(Type, Default) of
        {error, _Reason} ->
            ?PRINT("记录~w的~w字段~w类型为非法默认值：~w", [RecName, FieldName, Type, Default]),
            exit(1);
        FieldDefault ->
            FieldDefault
    end.

%% 基础数据类型默认值
do_get_field_default(atom, undefined) ->
    "undefined";
do_get_field_default(integer, undefined) ->
    "0";
do_get_field_default(pos_integer, undefined) ->
    "1";
do_get_field_default(neg_integer, undefined) ->
    "-1";
do_get_field_default(non_neg_integer, undefined) ->
    "0";
do_get_field_default(float, undefined) ->
    "0.0";
do_get_field_default(boolean, undefined) ->
    "false";
do_get_field_default(binary, undefined) ->
    "<<>>";
do_get_field_default(integer, {integer, _, Default}) ->
    ?STR("~w", [Default]);
do_get_field_default(pos_integer, {integer, _, Default}) when Default > 0 ->
    ?STR("~w", [Default]);
do_get_field_default(neg_integer, {integer, _, Default}) when Default < 0 ->
    ?STR("~w", [Default]);
do_get_field_default(non_neg_integer, {integer, _, Default}) when Default >= 0 ->
    ?STR("~w", [Default]);
do_get_field_default(binary, {bin, _, []}) ->
    "<<>>";
do_get_field_default(binary, {bin, _, [{bin_element, _, {string, _, Str}, default, default}]}) ->
    ?STR("<<\"~ts\">>", [Str]);
do_get_field_default(binary, {bin, _, [{bin_element, _, {string, _, Str}, default, [utf8]}]}) ->
    ?STR("<<\"~ts\"/utf8>>", [Str]);
do_get_field_default(Type, {Type, _, Default}) when ?IS_BASE_TYPE(Default) ->
    ?STR("~w", [Default]);
do_get_field_default({record, RecName}, undefined) -> %% 记录默认值 undefined
    ?STR("{record,~w}", [RecName]);
do_get_field_default({record, RecName}, {record, _, RecName, []}) -> %% 记录默认值 #RecName{}
    ?STR("{record,~w}", [RecName]);
do_get_field_default({record, RecName}, {call, _, {remote, _, {atom, _, rec_term_data}, {atom, _, rec}}, [{atom, _, RecName}]}) -> %% 记录默认值 rec_term_data:rec(RecName)
    ?STR("{record,~w}", [RecName]);
do_get_field_default({list, _}, undefined) -> %% 列表默认值为空列表
    "[]";
do_get_field_default({list, _}, {nil, _}) -> %% 列表默认值为空列表
    "[]";
do_get_field_default({map, _, _}, undefined) -> %% map默认值为空map
    "#{}";
do_get_field_default({map, _, _}, {map, _, []}) -> %% map默认值为空map
    "#{}";
do_get_field_default({tuple, ElemTypes}, undefined) -> %% 元组默认值
    get_tuple_default(ElemTypes, undefined);
do_get_field_default({tuple, ElemTypes}, {tuple, _, ElemDefaults}) -> %% 元组默认值
    get_tuple_default(ElemTypes, ElemDefaults);
do_get_field_default({union, _, [Default | _]}, undefined) -> %% 范围默认值
    ?STR("~w", [Default]);
do_get_field_default({union, _, Values}, {_, _, Default}) -> %% 范围默认值
    case lists:member(Default, Values) of
        true ->
            ?STR("~w", [Default]);
        _ ->
            {error, default_err}
    end;
do_get_field_default(_Type, _Default) ->
    {error, default_err}.

%% 获取元组默认值
get_tuple_default(ElemTypes, ElemDefaults) ->
    get_tuple_default(ElemTypes, ElemDefaults, []).
get_tuple_default([], undefined, Defaults) ->
    ?STR("{~ts}", [string:join(lists:reverse(Defaults), ",")]);
get_tuple_default([], [], Defaults) ->
    ?STR("{~ts}", [string:join(lists:reverse(Defaults), ",")]);
get_tuple_default([ElemType | ElemTypes], undefined, Defaults) ->
    case do_get_field_default(ElemType, undefined) of
        {error, Reason} ->
            {error, Reason};
        Default ->
            get_tuple_default(ElemTypes, undefined, [Default | Defaults])
    end;
get_tuple_default([ElemType | ElemTypes], [ElemDefault | ElemDefaults], Defaults) ->
    case do_get_field_default(ElemType, ElemDefault) of
        {error, Reason} ->
            {error, Reason};
        Default ->
            get_tuple_default(ElemTypes, ElemDefaults, [Default | Defaults])
    end;
get_tuple_default(_ElemTypes, _ElemDefaults, _Defaults) ->
    {error, tuple_default_err}.


%% @doc 写入文件数据
dump(FileNames, Recs, OutPath) ->
    Header = dump_header(),
    Hrl = dump_hrl(FileNames),
    BinName = dump_bin_name(Recs),
    Rec = dump_rec(Recs),
    RecSize = dump_rec_size(Recs),
    RecInfo = dump_rec_info(Recs),
    RecField = dump_rec_field(Recs),
    Data = string:join([Header, Hrl, BinName, Rec, RecSize, RecInfo, RecField], "\n\n"),
    file:write_file(filename:join(OutPath, ?OUT_FILE), unicode:characters_to_binary(Data, utf8)).

%% @doc 写入模块头部信息
dump_header() ->
    Format =
        """%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @doc
%%% record转换数据
%%% @end
%%%-------------------------------------------------------------------
-module(~ts).
-author(\"jiaoyinyi\").

-export([
    bin_name/1
    , rec/1
    , rec_size/1
    , rec_info/1
    , rec_field/1
]).""",
    ?STR(Format, [filename:basename(?OUT_FILE, ".erl")]).

%% @doc 写入头文件
dump_hrl(FileNames) ->
    dump_hrl(FileNames, []).
dump_hrl([], Acc) ->
    string:join(lists:reverse(Acc), "\n");
dump_hrl([FileName | FileNames], Acc) ->
    Mod = filename:basename(FileName, ".hrl"),
    Str = ?STR("-include(\"~ts.hrl\").", [Mod]),
    dump_hrl(FileNames, [Str | Acc]).

%% @doc 写入二进制名字
dump_bin_name(Recs) ->
    dump_bin_name(Recs, []).
dump_bin_name([], Acc) ->
    Str = "bin_name(_RecName) -> exit({bin_name_err, _RecName}).",
    string:join(lists:reverse([Str | Acc]), "\n");
dump_bin_name([#rec{name = Name} | Recs], Acc) ->
    Str = ?STR("bin_name(~w) -> <<\"~w\">>;", [Name, Name]),
    dump_bin_name(Recs, [Str | Acc]).

%% @doc 写入记录初始化方法
dump_rec(Recs) ->
    dump_rec(Recs, []).
dump_rec([], Acc) ->
    Str = "rec(_RecName) -> exit({rec_err, _RecName}).",
    string:join(lists:reverse([Str | Acc]), "\n");
dump_rec([#rec{name = Name} | Recs], Acc) ->
    Str = ?STR("rec(~w) -> #~w{};", [Name, Name]),
    dump_rec(Recs, [Str | Acc]).

%% @doc 写入记录大小
dump_rec_size(Recs) ->
    dump_rec_size(Recs, []).
dump_rec_size([], Acc) ->
    Str = "rec_size(_RecName) -> exit({rec_size_err, _RecName}).",
    string:join(lists:reverse([Str | Acc]), "\n");
dump_rec_size([#rec{name = Name, size = Size} | Recs], Acc) ->
    Str = ?STR("rec_size(~w) -> ~w;", [Name, Size]),
    dump_rec_size(Recs, [Str | Acc]).

%% @doc 写入记录信息
dump_rec_info(Recs) ->
    dump_rec_info(Recs, []).
dump_rec_info([], Acc) ->
    Str = "rec_info(_RecName) ->\n    exit({rec_info_err, _RecName}).",
    string:join(lists:reverse([Str | Acc]), "\n");
dump_rec_info([#rec{name = RecName, fields = Fields} | Recs], Acc) ->
    Str = ?STR("rec_info(~w) ->~n    ~ts;", [RecName, dump_rec_fields(RecName, Fields)]),
    dump_rec_info(Recs, [Str | Acc]).

%% @doc 写入记录字段信息
dump_rec_fields(RecName, Fields) ->
    dump_rec_fields(RecName, Fields, []).
dump_rec_fields(_RecName, [], Acc) ->
    ?STR("[~ts]", [string:join(lists:reverse(Acc), ", ")]);
dump_rec_fields(RecName, [#rec_field{name = Name, pos = Pos, type = Type, default = Default} | Fields], Acc) ->
    Str = ?STR("{~w,~w,<<\"~w\">>,~w,~ts}", [Name, Pos, Name, Type, Default]),
    dump_rec_fields(RecName, Fields, [Str | Acc]).

%% @doc 写入记录字段信息
dump_rec_field(Recs) ->
    dump_rec_field(Recs, []).
dump_rec_field([], Acc) ->
    Str = "rec_field(_Key) ->\n    exit({rec_field_err, _Key}).",
    string:join(lists:reverse([Str | Acc]), "\n");
dump_rec_field([#rec{name = RecName, fields = Fields} | Recs], Acc) ->
    NewAcc = dump_rec_field(RecName, Fields, Acc),
    dump_rec_field(Recs, NewAcc).
dump_rec_field(_RecName, [], Acc) ->
    Acc;
dump_rec_field(RecName, [#rec_field{name = Name, pos = Pos, type = Type, default = Default} | Fields], Acc) ->
    Str = ?STR("rec_field({~w,~w}) ->\n    {~w,~w,<<\"~w\">>,~w,~ts};", [RecName, Pos, Name, Pos, Name, Type, Default]),
    dump_rec_field(RecName, Fields, [Str | Acc]).