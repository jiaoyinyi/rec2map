%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% 生成记录信息头文件
%%% @end
%%% Created : 2022-12-16 00:00:00
%%%-------------------------------------------------------------------

-ifndef(GEN_REC_HRL).
-define(GEN_REC_HRL, 1).

%% 支持的类型
%% base_type() :: atom() | integer() | pos_integer() | neg_integer() | non_neg_integer() | float() | binary()
%%
%% 原子 atom()
%% 整形 integer(), pos_integer(), neg_integer(), non_neg_integer()
%% 浮点形 float()
%% Bool boolean()
%% 二进制 binary()
%% 记录 #record{}, {record,RecName}
%% 基础类型列表 [base_type()]
%% 记录类型列表 [#record{}], [{record,RecName}]
%% Key为基础类型，Val为基础类型的Map #{base_type() => base_type()}
%% Key为基础类型，Val为记录类型的Map #{base_type() => #record{}}, #{base_type() => {record,RecName}}
%% 基础类型元组 {base_type(),base_type(),...}
%% 基础类型范围类型 base_type() | base_type()

-define(OUT_FILE, "rec_term_data.erl").                 %% 生成目标文件

-define(STR(Format, Args), lists:flatten(io_lib:format(Format, Args))).
-define(PRINT(Format, Args), io:format(unicode:characters_to_binary(?STR(Format, Args), utf8))).

%% 是否为基础类型
-define(BASE_TYPES, [
    atom
    , integer, pos_integer, neg_integer, non_neg_integer
    , float
    , boolean
    , binary
]).

-define(IS_BASE_TYPE(T), (T == atom orelse T == integer orelse T == pos_integer orelse T == neg_integer orelse T == non_neg_integer orelse T == float orelse T == boolean orelse T == binary)).

%% 要去除的头文件
-define(EXCLUDE_HRLS, [
]).
%% 要去除的记录
-define(EXCLUDE_RECORDS, [
]).

%% 记录字段
-record(rec_field, {
    name                    :: atom()                                                          %% 名字
    , pos                   :: pos_integer()                                                   %% 位置
    , type                  :: term()                                                          %% 类型
    , default               :: string()                                                        %% 默认值
}).

%% 记录
-record(rec, {
    name                    :: atom()                                                          %% 记录名
    , fields = []           :: [#rec_field{}]                                                  %% 记录字段
    , size = 0              :: pos_integer()                                                   %% 记录大小
}).

-endif.