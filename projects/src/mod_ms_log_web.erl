%%%-------------------------------------------------------------------
%%% File    : mod_ms_log_web.erl
%%% Author  : Isabella <isabella.santos@mstech.com.br>
%%% Purpose : Interface http module to read log files of ejabberd server.
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

-module(mod_ms_log_web).

-behaviour(gen_mod).

-export([start/2, stop/1, process/2, mod_opt_type/1]).

-include("ejabberd.hrl").

-include("logger.hrl").

-include("jlib.hrl").

-include("ejabberd_http.hrl").

-include("ejabberd_web_admin.hrl").

-define(xml_prolog, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").
-define(CT_XML, {<<"Content-Type">>, <<"text/xml; charset=utf-8">>}).
-define(CT_AXML, {<<"Content-Type">>, <<"application/xml">>}).
-define(CT_PLAIN, {<<"Content-Type">>, <<"text/plain">>}).
-define(SERVER, {<<"Server">>, <<"MSTECH Bluetalk Admin">>}).
-define(AC_ALLOW_ORIGIN, {<<"Access-Control-Allow-Origin">>, <<"*">>}).
-define(AC_ALLOW_METHODS, {<<"Access-Control-Allow-Methods">>, <<"POST, OPTIONS">>}).
-define(AC_ALLOW_HEADERS, {<<"Access-Control-Allow-Headers">>, <<"Content-Type">>}).
-define(AC_MAX_AGE, {<<"Access-Control-Max-Age">>, <<"86400">>}).
-define(OPTIONS_HEADER, [?CT_PLAIN, ?AC_ALLOW_ORIGIN, ?AC_ALLOW_METHODS,
                          ?AC_ALLOW_HEADERS, ?AC_MAX_AGE]).
-define(HEADER, [?CT_XML, ?AC_ALLOW_ORIGIN, ?AC_ALLOW_HEADERS]).
-define(RESPONSE_HEADER, [?SERVER, ?CT_AXML, ?AC_ALLOW_ORIGIN]).


%%%----------------------------------------------------------------------
%%% gen_mod callbacks
%%%----------------------------------------------------------------------

start(_Host, _Opts) ->
    %% case gen_mod:get_opt(docroot, Opts, fun(A) -> A end, undefined) of
    ok.

stop(_Host) -> ok.

%%%----------------------------------------------------------------------
%%% HTTP HANDLERS - HEAD, OPTIONS, POST, GET
%%% Mstech - All internal methods of mod_ms_log_web are accessible
%%% through process/2 (handlers).
%%%----------------------------------------------------------------------
%%% For the best integration, the return code range should not exceed the list below:
%%% 500 - internal server error
%%% 409 - conflict
%%% 404 - not found
%%% 403 - not allowed
%%% 401 - not authorised
%%% 400 - other error, should be sent in response body
%%% 204 - success, no return data
%%% 201 - created
%%% 200 - success, return value in response body
%%%----------------------------------------------------------------------
process([], #request{method = 'HEAD'}) ->
    {200, ?HEADER, []};

process([], #request{method = 'OPTIONS', data = <<>>}) ->
    {200, ?OPTIONS_HEADER, []};

process([], #request{method = 'GET', lang = Lang}) ->
    {404, ?RESPONSE_HEADER, []};

process([<<"filter_log">>], #request{method = 'OPTIONS', data = <<>>}) ->
    io:format("options"),
    {200, ?OPTIONS_HEADER, []};

process([<<"filter_log">>], #request{method = 'GET', lang = Lang}) ->
    {404, ?RESPONSE_HEADER, []};

process([<<"filter_log">>],
	#request{method = 'POST', data = Data}) ->

    [F|[E]] = string:tokens(bitstring_to_list(Data), "&"),
    BitFilename = list_to_bitstring(F -- "filename="),
    BitExpression = list_to_bitstring(E --"expression="),
    StrFilename = bitstring_to_list(BitFilename),
    StrExpression = bitstring_to_list(BitExpression),

    Text =
        list_to_binary([<<"Mstech:Info:Admin:Operation on Server:">>,
          get_info_text({info, file_for_exp, {StrFilename, StrExpression}})]),
    error_logger:info_msg(Text),

    Result = mod_ms_log:list_dir("ejabberd.log", StrExpression),
    XMLReturn = make_doc_xml(Result),
    {200, ?RESPONSE_HEADER, [XMLReturn]}.



%%%----------------------------------------------------------------------
%%% Mstech - Added Utils Functions
%%%----------------------------------------------------------------------

make_doc_xml(Fields) ->
    Xml = xmerl:export_simple([doc_xml_simple(Fields)], xmerl_xml,
                              [{prolog, ?xml_prolog}]),
    unicode:characters_to_binary(Xml).

fields_to_xml_simple(Fields) ->
    [ {logs, [{name, K}], [V]} || {K, V} <- Fields ].

doc_xml_simple(Fields) ->
    {[{actions, fields_to_xml_simple(Fields)}]}.


%%%----------------------------------------------------------------------
%%% Info texts
%%%----------------------------------------------------------------------

get_info_text({info, list_directory}) ->
  <<"List directory content.">>;
get_info_text({info, list_dir_file, {File}}) ->
  list_to_binary([<<"List of all lines of file named: ">>, File, <<".">>]);
get_info_text({info, file_for_exp, {File,Exp}}) ->
  list_to_binary([<<"List of all files named ">>, File, <<" containing ">>, Exp, <<".">>]).


%%%----------------------------------------------------------------------
%%% Error texts
%%%----------------------------------------------------------------------

get_error_text({error, file_doesnt_exist, {File}}) ->
  list_to_binary([<<"The File ">>, File, <<" doesn't exist.">>]);
get_error_text({error, gen_error, {File,Exp}}) ->
  list_to_binary([<<"An error occurred while searching ">>, Exp, <<" on ">>, File, <<" files.">>]);


get_error_text({error, wrong_parameters}) ->
    <<"Wrong parameters in the web formulary">>.

mod_opt_type(_) -> [].