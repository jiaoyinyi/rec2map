%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% rec_term_data记录信息头文件
%%% @end
%%% Created : 2022-12-16 00:00:00
%%%-------------------------------------------------------------------

-ifndef(REC_TERM_HRL).
-define(REC_TERM_HRL, 1).

-record(rec_term_field, {
    name                     :: atom()                     %% 名称
    , pos                    :: pos_integer()              %% 位置
    , bin_name               :: binary()                   %% 二进制名称
    , type                   :: term()                     %% 类型
    , default                :: term()                     %% 默认值
}).

-define(REC_TERM_DIFF_OP_ADD, add).
-define(REC_TERM_DIFF_OP_DELETE, delete).
-define(REC_TERM_DIFF_OP_UPDATE, update).

-define(REC_TERM_IS_BASE_TYPE(Term), (is_integer(Term) orelse is_float(Term) orelse is_boolean(Term) orelse is_atom(Term) orelse is_binary(Term))).

-record(rec_term_diff, {
    op                       :: add | delete | update      %% 操作
    , old                    :: term()                     %% 旧值
    , new                    :: term()                     %% 新值
}).

-endif.