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

-endif.