%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% rec_term_data记录信息头文件
%%% @end
%%% Created : 2022-12-16 00:00:00
%%%-------------------------------------------------------------------

-ifndef(REC_DIFF_HRL).
-define(REC_DIFF_HRL, 1).

%% record差异操作值
-define(REC_DIFF_OP_ADD, add).          %% 新增
-define(REC_DIFF_OP_DELETE, delete).    %% 删除
-define(REC_DIFF_OP_UPDATE, update).    %% 更新

%% record差异
-record(rec_diff, {
    op                       :: add | delete | update      %% 操作
    , old                    :: term()                     %% 旧值
    , new                    :: term()                     %% 新值
}).

-endif.