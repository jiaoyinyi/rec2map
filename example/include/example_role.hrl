%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% 生成记录信息头文件
%%% @end
%%% Created : 2022-12-16 00:00:00
%%%-------------------------------------------------------------------

-ifndef(EXAMPLE_ROLE_HRL).
-define(EXAMPLE_ROLE_HRL, 1).

-record(role, {
    rid                     :: non_neg_integer()
    , srv_id                :: binary()
    , name = <<>>           :: binary()
    , lev = 1               :: term()
    , partners = []         :: [{record, partner}]
    , m_package             :: {record, m_package}
}).

-record(partner, {
    id                       :: pos_integer()
    , bid                    :: pos_integer()
}).

-record(package, {
    type                     :: pos_integer()
    , items = []             :: [{record, item}]
    , capacity               :: pos_integer()
    , size                   :: non_neg_integer()
}).

-record(m_package, {
    packages                 :: #{pos_integer() => #package{}}
}).

-record(item, {
    id                       :: pos_integer()
    , bid                    :: pos_integer()
}).

-endif.