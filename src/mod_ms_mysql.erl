%%%-------------------------------------------------------------------
%%% File    : mod_mstech_mysql.erl
%%% Author  : Takao <takao.matsumura@mstech.com.br>
%%% Purpose : Module to handle mysql query for ejabberd server.
%%% Created :  25 August 2015 by Takao <takao.matsumura@mstech.com.br>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

%debugging mode
%-compile([debug_info, export_all]).

-module(mod_ms_mysql).

-behaviour(gen_mod).

% gen_mod exports
-export([start/2, stop/1, mod_opt_type/1]).

% mstech_log functions exports
- export([messages_to_all/2, messages_to_from/4]).

-include("logger.hrl").

-include("jlib.hrl").

%%%----------------------------------------------------------------------
%%% gen_mod callbacks
%%%----------------------------------------------------------------------

start(_Host, _Opts) ->
    %% case gen_mod:get_opt(docroot, Opts, fun(A) -> A end, undefined) of
    ok.

stop(_Host) -> ok.


%%%----------------------------------------------------------------------
%%% Mstech - Util Functions
%%%----------------------------------------------------------------------



%%%----------------------------------------------------------------------
%%% Mstech - Exported Functions
%%%----------------------------------------------------------------------

messages_to_all(Host,To) ->
    StrQuery = "select distinct tmp.dt as date, tmp.user as user from (select date(created_at) as dt, to_user as user from archive where (from_user = '"++To++"') union " ++ " select date(created_at) as dt, from_user as user from archive where " ++ "(to_user = '"++To++"') ) as tmp",
    Query = list_to_bitstring(StrQuery),
    io:format("Query=~p.~n", [Query]),
    mysql_query(Host, Query).

messages_to_from(Host,Date,To,From) ->
    StrQuery = "select distinct created_at as date, to_user, from_user, txt, time(created_at) from archive where created_at between '"++Date++" 00:00:00' and '"++Date++" 23:59:59' and (from_user in ('"++To++"', '"++From++"') and to_user in ('"++To++"','"++From++"')) order by created_at",
    Query = list_to_bitstring(StrQuery),
    io:format("Query=~p.~n", [Query]),
    mysql_query(Host, Query).


%%%----------------------------------------------------------------------
%%% Mstech - Module Internal Functions
%%%----------------------------------------------------------------------

mysql_query(Host,Query) ->
  case ejabberd_odbc:sql_query(Host,Query) of
    {selected, Metadata, []} ->
      {ok, Metadata, no_results};
    {selected, Metadata, Result} ->
      {ok, Metadata, Result};
    {error,Reason} ->
      {error,Reason}
  end.

%%%----------------------------------------------------------------------
%%% Info texts
%%%----------------------------------------------------------------------

get_info_text({info, query_mysql}) ->
  <<"Sent query mysql.">>;
get_info_text({info, query_mysql, {Host}}) ->
  list_to_binary([<<"Sent mysql query for host: ">>, Host]);
get_info_text({info, result_list_exp, {File,Exp}}) ->
  list_to_binary([<<"List of all files named ">>, File, <<" containing ">>, Exp, <<".">>]).


%%%----------------------------------------------------------------------
%%% Error texts
%%%----------------------------------------------------------------------

get_error_text({error, enoent}) ->
  list_to_binary([<<"The file does not exist.">>]);
get_error_text({error, eisdir}) ->
  list_to_binary([<<"The file is a directory.">>]);
get_error_text({error, _}) ->
  list_to_binary([<<"An error occured.">>]).

mod_opt_type(_) -> [].