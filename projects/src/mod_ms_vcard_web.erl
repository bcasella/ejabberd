%%%-------------------------------------------------------------------
%%% File    : mod_ms_vcard_web.erl
%%% Author  : Isabella Fontes <isabella.santos@mstech.com.br>
%%% Purpose : Interface module to set and get vcard information.
%%% Created :  23 September 2015 by Isabella Fontes <isabella.santos@mstech.com.br>
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

-module(mod_ms_vcard_web).

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
%%% Mstech - All internal methods of mod_mstech_mysql_web are accessible
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

process([], #request{method = 'GET'}) ->
    {404, ?RESPONSE_HEADER, []};

process([<<"set_vcard">>], #request{method = 'OPTIONS', data = <<>>}) ->
    {200, ?OPTIONS_HEADER, []};

process([<<"set_vcard">>], #request{method = 'GET'}) ->
    {404, ?RESPONSE_HEADER, []};

%%   args = [{user, binary}, {host, binary}, {name, binary}, {content, binary}],
%%   result = {res, rescode}}
process([<<"set_vcard">>],
    #request{method = 'POST', data = Data}) ->
    [U,H,FN,E,FirstN,LastN] = string:tokens(bitstring_to_list(Data), "&"),
    BitUser = list_to_bitstring(U -- "username="),  % remove string "host" from post
    BitHost = list_to_bitstring(H -- "host="),
    BitFullName = list_to_bitstring(FN -- "fullname="),
    BitEmail = list_to_bitstring(E -- "email="),
    BitFirstName = list_to_bitstring(FirstN -- "firstname="),
    BitLastName = list_to_bitstring(LastN -- "lastaname="),

    %io:format("User:~p.~n Host:~p.~n FN:~p.~n EM: ~p.~n", [BitUser,BitHost,BitFullName,BitEmail]),

    mod_admin_extra:set_vcard(BitUser,BitHost,<<"FN">>,BitFullName),
    FNText = list_to_binary([<<"Mstech:Info:Admin:Operation on Server:">>,
                        get_info_text({info, set_vcard_success, {BitUser, BitHost, "FullName", BitFullName}})]),
    error_logger:info_msg(FNText),

    mod_admin_extra:set_vcard(BitUser,BitHost,<<"EMAIL">>, <<"USERID">>,BitEmail),
    EmailText = list_to_binary([<<"Mstech:Info:Admin:Operation on Server:">>,
                        get_info_text({info, set_vcard_success, {BitUser, BitHost, "Email", BitEmail}})]),
    error_logger:info_msg(EmailText),

    mod_admin_extra:set_vcard(BitUser,BitHost,<<"N">>, <<"GIVEN">>,BitFirstName),
    FirstNameText = list_to_binary([<<"Mstech:Info:Admin:Operation on Server:">>,
                        get_info_text({info, set_vcard_success, {BitUser, BitHost, "First Name", BitFirstName}})]),
    error_logger:info_msg(FirstNameText),


    mod_admin_extra:set_vcard(BitUser,BitHost,<<"N">>, <<"FAMILY">>,BitLastName),
    LastNameText = list_to_binary([<<"Mstech:Info:Admin:Operation on Server:">>,
                        get_info_text({info, set_vcard_success, {BitUser, BitHost, "Last Name", BitLastName}})]),
    error_logger:info_msg(LastNameText),

    {200, ?RESPONSE_HEADER, [<<"ok!">>]};

process([<<"set_vcard_multi">>], #request{method = 'OPTIONS', data = <<>>}) ->
    io:format("options"),
    {200, ?OPTIONS_HEADER, []};

process([<<"set_vcard_multi">>], #request{method = 'GET'}) ->
    {404, ?RESPONSE_HEADER, []};

%% args = [{user, binary}, {host, binary}, {name, binary}, {subname, binary}, {content, binary}],
%% result = {res, rescode}},
process([<<"set_vcard_multi">>],
    #request{method = 'POST', data = Data}) ->
    [User,Host,Field, SubField, Value] = string:tokens(bitstring_to_list(Data), "&"),
    io:format("U:~p.~n H:~p.~n N: ~p.~n SN:~p.~n C:~p.~n", [User,Host,Field,SubField, Value]),

    Text = list_to_binary([<<"Mstech:Info:Admin:Operation on Server:">>,
                    get_info_text({info, set_vcard_multi_success, {User,Host,Field,SubField,Value}})]),
    error_logger:info_msg(Text),
    {200, ?RESPONSE_HEADER, [<<"ok!">>]}.

%%%----------------------------------------------------------------------
%%% Mstech - Utils Functions
%%%----------------------------------------------------------------------


%%%----------------------------------------------------------------------
%%% Info texts
%%%----------------------------------------------------------------------

get_info_text({info, set_vcard_success, {User, Host, Field, Value}}) ->
  list_to_binary([<<"Set vcard successfuly on host:">>, Host, <<" to user:">>, User, <<". Field name:">>, Field, <<" with value:">>, Value, <<".">>]);
get_info_text({info, set_vcard_multi_success, {User, Host, Field, SubField, Value}}) ->
  list_to_binary([<<"Set vcard multi successfuly on host:">>, Host, <<" to user:">>, User, <<". Field name:">>, Field, <<". Subfield name:">>, SubField, <<" with value:">>, Value, <<".">>]);
get_info_text({info, sintaxe_error, {Host}}) ->
  list_to_binary([<<"Change vcard on host: ">>, Host, <<". Wrong query syntax.">>]).

%%%----------------------------------------------------------------------
%%% Error texts
%%%----------------------------------------------------------------------

get_error_text({error, filter_chat_message_error, {Host}}) ->
  list_to_binary([<<"The Host ">>, Host, <<" doesn't exist.">>]);
get_error_text({error, undef_error, {Host}}) ->
  list_to_binary([<<"Sql query returned undefined error on: ">>, Host, <<".">>]);
get_error_text({error, wrong_parameters}) ->
    <<"Wrong parameters in the web formulary">>.

mod_opt_type(_) -> [].

