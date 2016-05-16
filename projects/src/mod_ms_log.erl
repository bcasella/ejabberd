%%%-------------------------------------------------------------------
%%% File    : mod_ms_log.erl
%%% Author  : Takao <takao.matsumura@mstech.com.br>
%%% Purpose : Module to read (log) files of ejabberd server.
%%% Created :  03 July 2015 by Takao <takao.matsumura@mstech.com.br>
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

-module(mod_ms_log).

-behaviour(gen_mod).

% gen_mod exports
-export([start/2, stop/1, mod_opt_type/1]).

% mstech_log functions exports
-export([list_dir/2]).

-include("ejabberd.hrl").

-include("logger.hrl").

-include("jlib.hrl").

-include("ejabberd_http.hrl").

-include("ejabberd_web_admin.hrl").

-define(DIR, "/var/log/ejabberd/").

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

order(L) -> lists:foldl(fun insert/2, [], L).
insert(X,[]) -> [X];
insert(X,L=[H|_]) when X =< H -> [X|L];
insert(X,[H|T]) -> [H|insert(X, T)].


%%%----------------------------------------------------------------------
%%% Mstech - Util Functions
%%%----------------------------------------------------------------------

% list all files inside the ?DIR directory
list_dir() ->
  case file:list_dir(?DIR) of
    {ok, Filenames} ->
      Ordered = order(Filenames),
      lists:map(
        fun(Elem) ->
          erlang:append_element({"file"}, Elem)
        end,
        Ordered);
    {error, Reason} ->
      io:format(get_error_text({error, Reason, [?DIR]})),
      ng
  end.
% list all the content of files named as File located in ?DIR
% and logfile/1 will read all files that matches Exp (its name)
list_dir(File) ->
  case file:list_dir(?DIR) of
    {ok, ListAllFiles} ->
      Ordered = lists:sort(ListAllFiles),
      Matches = lists:filter(
        fun(Elem) ->
          string:str(Elem,File) > 0
        end,
        Ordered),
      FileContents = lists:map(
        fun(Match) ->
          readfile(Match)
        end,
        Matches),
      lists:append(FileContents);
    {error, Reason} ->
      io:format(get_error_text({error, Reason, [?DIR]})),
      ng
  end.
% Matches is a list of all files in ?DIR that matches name with File
% FileContent is all lines of matched files that contains Expression Exp
list_dir(File,Exp) ->
  case file:list_dir(?DIR) of
    {ok, ListAllFiles} ->
      Ordered = lists:sort(ListAllFiles),
      Matches = lists:filter(
        fun(Elem) ->
          string:str(Elem,File) > 0
        end,
        Ordered),
      FileContents = lists:map(
        fun(Match) ->
          readfile(Match,Exp)
        end,
        Matches),
      lists:append(FileContents);
    {error, Reason} ->
      io:format(get_error_text({error, Reason, [?DIR]})),
      ng
  end.


%%%----------------------------------------------------------------------
%%% Mstech - Module Internal Functions
%%%----------------------------------------------------------------------

%% readfile open FileName if exists and
%% calls readline/1 (without Exp)
%% calls readline/2 (with Exp)
readfile(FileName) ->
  case file:open([?DIR, FileName], [read]) of
    {ok,_} ->
      {ok, Device} = file:open([?DIR, FileName], [read]),
      try readline(Device)
      after file:close(Device)
      end;
    {error,Reason} ->
      io:format(get_error_text({error, Reason})),
      file:close(FileName)
  end.
readfile(FileName,Exp) ->
  case file:open([?DIR, FileName], [read]) of
    {ok,_} ->
      {ok, Device} = file:open([?DIR, FileName], [read]),
      try readline(Device,Exp)
      after file:close(Device)
      end;
    {error,Reason} ->
      io:format(get_error_text({error, Reason})),
      file:close(FileName)
  end.

%% print actual line if Exp is not specified, (it could return the line to logweb)
%% print actual line if part of line matches Exp (it could return the line to logweb)
readline(Device) ->
  case file:read_line(Device) of
    {ok, Line} ->
      [{"line",Line}|readline(Device)];
    eof ->
      []
  end.
readline(Device,Exp) ->
  case file:read_line(Device) of
    {ok, Line} ->
%      io:format("~p",[string:str(Line, Exp)]),
      case string:str(Line, Exp) of
        0 ->
%          io:format("No ~s matching in this line.~n", [Exp]),
          readline(Device,Exp);
%          [{"line",Line}|readline(Device)];
        _ ->
          [{"line",Line}|readline(Device,Exp)]
%          io:format("~s", [Line]),
%          readline(Device,Exp)
      end;
    eof -> []
  end.


%%%----------------------------------------------------------------------
%%% Mstech Formulary logview POST
%%% function form_logview_post to handle and check parameters
%%%----------------------------------------------------------------------

form_logview_post(Q) ->
  case catch get_logview_parameters(Q) of
    [Filename] ->
      readfile(Filename);
    _ -> {error, wrong_parameters}
  end.

get_logview_parameters(Q) ->
  lists:map(fun (Key) ->
    case lists:keysearch(Key, 1, Q) of
      {value, {_Key, Value}} -> Value;
      false -> false
    end
  end,
    [<<"filename">>]).


%%%----------------------------------------------------------------------
%%% Info texts
%%%----------------------------------------------------------------------

get_info_text({info, list_directory}) ->
  <<"List directory content.">>;
get_info_text({info, result_list, {File}}) ->
  list_to_binary([<<"List of all files named ">>, File]);
get_info_text({info, result_list_exp, {File,Exp}}) ->
  list_to_binary([<<"List of all files named ">>, File, <<" containing ">>, Exp, <<".">>]).


%%%----------------------------------------------------------------------
%%% Error texts
%%%----------------------------------------------------------------------

get_error_text({error, enoent}) ->
  list_to_binary([<<"The file does not exist.">>]);
get_error_text({error, eisdir}) ->
  list_to_binary([<<"The file is a directory.">>]);
get_error_text({error, enoent,{?DIR}}) ->
  list_to_binary(["The directory does not exist.", ?DIR]);
get_error_text({error, _}) ->
  list_to_binary([<<"An error occured.">>]).

mod_opt_type(_) -> [].