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
    , rec_size/1
    , rec_info/1
    , rec_field/1
]).

-include("rec_term.hrl").


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

rec_size(item) -> 3;
rec_size(m_package) -> 2;
rec_size(package) -> 5;
rec_size(partner) -> 3;
rec_size(role) -> 7;
rec_size(_RecName) -> exit({rec_size_err, _RecName}).

rec_info(item) ->
    [#rec_term_field{name = id, pos = 2, bin_name = <<"id">>, type = pos_integer, default = 1}, #rec_term_field{name = bid, pos = 3, bin_name = <<"bid">>, type = pos_integer, default = 1}];
rec_info(m_package) ->
    [#rec_term_field{name = packages, pos = 2, bin_name = <<"packages">>, type = {map,pos_integer,{record,package}}, default = #{}}];
rec_info(package) ->
    [#rec_term_field{name = type, pos = 2, bin_name = <<"type">>, type = pos_integer, default = 1}, #rec_term_field{name = items, pos = 3, bin_name = <<"items">>, type = {list,{record,item}}, default = []}, #rec_term_field{name = capacity, pos = 4, bin_name = <<"capacity">>, type = pos_integer, default = 1}, #rec_term_field{name = size, pos = 5, bin_name = <<"size">>, type = non_neg_integer, default = 0}];
rec_info(partner) ->
    [#rec_term_field{name = id, pos = 2, bin_name = <<"id">>, type = pos_integer, default = 1}, #rec_term_field{name = bid, pos = 3, bin_name = <<"bid">>, type = pos_integer, default = 1}];
rec_info(role) ->
    [#rec_term_field{name = rid, pos = 2, bin_name = <<"rid">>, type = non_neg_integer, default = 0}, #rec_term_field{name = srv_id, pos = 3, bin_name = <<"srv_id">>, type = binary, default = <<>>}, #rec_term_field{name = name, pos = 4, bin_name = <<"name">>, type = binary, default = <<>>}, #rec_term_field{name = lev, pos = 5, bin_name = <<"lev">>, type = pos_integer, default = 1}, #rec_term_field{name = partners, pos = 6, bin_name = <<"partners">>, type = {list,{record,partner}}, default = []}, #rec_term_field{name = m_package, pos = 7, bin_name = <<"m_package">>, type = {record,m_package}, default = {record,m_package}}];
rec_info(_RecName) ->
    exit({rec_info_err, _RecName}).

rec_field({item,2}) ->
    #rec_term_field{name = id, pos = 2, bin_name = <<"id">>, type = pos_integer, default = 1};
rec_field({item,3}) ->
    #rec_term_field{name = bid, pos = 3, bin_name = <<"bid">>, type = pos_integer, default = 1};
rec_field({m_package,2}) ->
    #rec_term_field{name = packages, pos = 2, bin_name = <<"packages">>, type = {map,pos_integer,{record,package}}, default = #{}};
rec_field({package,2}) ->
    #rec_term_field{name = type, pos = 2, bin_name = <<"type">>, type = pos_integer, default = 1};
rec_field({package,3}) ->
    #rec_term_field{name = items, pos = 3, bin_name = <<"items">>, type = {list,{record,item}}, default = []};
rec_field({package,4}) ->
    #rec_term_field{name = capacity, pos = 4, bin_name = <<"capacity">>, type = pos_integer, default = 1};
rec_field({package,5}) ->
    #rec_term_field{name = size, pos = 5, bin_name = <<"size">>, type = non_neg_integer, default = 0};
rec_field({partner,2}) ->
    #rec_term_field{name = id, pos = 2, bin_name = <<"id">>, type = pos_integer, default = 1};
rec_field({partner,3}) ->
    #rec_term_field{name = bid, pos = 3, bin_name = <<"bid">>, type = pos_integer, default = 1};
rec_field({role,2}) ->
    #rec_term_field{name = rid, pos = 2, bin_name = <<"rid">>, type = non_neg_integer, default = 0};
rec_field({role,3}) ->
    #rec_term_field{name = srv_id, pos = 3, bin_name = <<"srv_id">>, type = binary, default = <<>>};
rec_field({role,4}) ->
    #rec_term_field{name = name, pos = 4, bin_name = <<"name">>, type = binary, default = <<>>};
rec_field({role,5}) ->
    #rec_term_field{name = lev, pos = 5, bin_name = <<"lev">>, type = pos_integer, default = 1};
rec_field({role,6}) ->
    #rec_term_field{name = partners, pos = 6, bin_name = <<"partners">>, type = {list,{record,partner}}, default = []};
rec_field({role,7}) ->
    #rec_term_field{name = m_package, pos = 7, bin_name = <<"m_package">>, type = {record,m_package}, default = {record,m_package}};
rec_field(_Key) ->
    exit({rec_field_err, _Key}).