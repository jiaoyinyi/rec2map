%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @doc
%%% record转换数据
%%% @end
%%%-------------------------------------------------------------------
-module(rec_term_data).
-author("jiaoyinyi").

-export([
    bin_name/1
    , rec/1
    , rec_info/1
]).

-include("example_role.hrl").

bin_name(item) -> <<"item">>;
bin_name(m_package) -> <<"m_package">>;
bin_name(package) -> <<"package">>;
bin_name(partner) -> <<"partner">>;
bin_name(role) -> <<"role">>;
bin_name(_RecName) -> exit({bin_name_err, _RecName}).

rec(item) -> #item{};
rec(m_package) -> #m_package{};
rec(package) -> #package{};
rec(partner) -> #partner{};
rec(role) -> #role{};
rec(_RecName) -> exit({rec_err, _RecName}).

rec_info(item) ->
    [{id,2,<<"id">>,pos_integer,1}, {bid,3,<<"bid">>,pos_integer,1}];
rec_info(m_package) ->
    [{packages,2,<<"packages">>,{map,pos_integer,{record,package}},#{}}];
rec_info(package) ->
    [{type,2,<<"type">>,pos_integer,1}, {items,3,<<"items">>,{list,{record,item}},[]}, {capacity,4,<<"capacity">>,pos_integer,1}, {size,5,<<"size">>,non_neg_integer,0}];
rec_info(partner) ->
    [{id,2,<<"id">>,pos_integer,1}, {bid,3,<<"bid">>,pos_integer,1}];
rec_info(role) ->
    [{rid,2,<<"rid">>,non_neg_integer,0}, {srv_id,3,<<"srv_id">>,binary,<<>>}, {name,4,<<"name">>,binary,<<>>}, {lev,5,<<"lev">>,pos_integer,1}, {partners,6,<<"partners">>,{list,{record,partner}},[]}, {m_package,7,<<"m_package">>,{record,m_package},{record,m_package}}];
rec_info(_RecName) ->
    exit({rec_info_err, _RecName}).