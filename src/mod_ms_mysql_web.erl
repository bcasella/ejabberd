%%%-------------------------------------------------------------------
%%% File    : mod_ms_mysql_web.erl
%%% Author  : Isabella Fontes <isabella.santos@mstech.com.br>
%%% Purpose : Interface module to read log files of ejabberd server.
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

-module(mod_ms_mysql_web).

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

process([<<"filter_to_from">>], #request{method = 'OPTIONS', data = <<>>}) ->
    io:format("options"),
    {200, ?OPTIONS_HEADER, []};

process([<<"filter_to_from">>], #request{method = 'GET'}) ->
    {404, ?RESPONSE_HEADER, []};

process([<<"filter_to_from">>],
    #request{method = 'POST', data = Data}) ->
    [H,T,F,D] = string:tokens(bitstring_to_list(Data), "&"),
    BitHost = list_to_bitstring(H -- "host="),  % remove string "host" from post
    BitTo = list_to_bitstring(T -- "to="),
    BitFrom = list_to_bitstring(F -- "from="),
    BitDate = list_to_bitstring(D -- "date="),

    Text = list_to_binary([<<"Mstech:Info:Admin:Operation on Server:">>,
                                  get_info_text({info, to_from, {BitHost, BitTo, BitFrom, BitDate}})]),
    error_logger:info_msg(Text),
    LstTo = bitstring_to_list(BitTo),
    LstFrom = bitstring_to_list(BitFrom),
    LstDate = bitstring_to_list(BitDate),
    case mod_ms_mysql:messages_to_from(BitHost,LstDate,LstTo,LstFrom) of
        {ok, _Metadata, no_results} ->
            InfoText = list_to_binary([<<"Mstech:Info:Admin:Operation on Server:">>,
                                  get_info_text({info, query_no_result, {BitHost}})]),
            error_logger:info_msg(InfoText),
            {200, ?RESPONSE_HEADER, []};
        {ok, _Metadata, Result} ->
            InfoText = list_to_binary([<<"Mstech:Info:Admin:Operation on Server:">>,
                                  get_info_text({info, query_success_result, {BitHost}})]),
            error_logger:info_msg(InfoText),
            XmlReturn = make_xml(Result),
            {200, ?RESPONSE_HEADER, [XmlReturn]};
        {error, _Reason} ->
            InfoText = list_to_binary([<<"Mstech:Error:Admin:Operation on Server:">>,
                                  get_info_text({info, query_error, {BitHost}})]),
            error_logger:info_msg(InfoText),
            {404, ?RESPONSE_HEADER, [InfoText]}
    end;

process([<<"filter_to_all">>], #request{method = 'OPTIONS', data = <<>>}) ->
    io:format("options"),
    {200, ?OPTIONS_HEADER, []};

process([<<"filter_to_all">>], #request{method = 'GET'}) ->
    {404, ?RESPONSE_HEADER, []};

process([<<"filter_to_all">>],
    #request{method = 'POST', data = Data}) ->
    [H,T] = string:tokens(bitstring_to_list(Data), "&"),
    BitHost = list_to_bitstring(H -- "host="),  % remove string "host" from post
    BitTo = list_to_bitstring(T -- "to="),

    Text = list_to_binary([<<"Mstech:Info:Admin:Operation on Server:">>,
                                    get_info_text({info, to_all, {BitHost,BitTo}})]),
    error_logger:info_msg(Text),

    LstTo = bitstring_to_list(BitTo),
    case mod_ms_mysql:messages_to_all(BitHost, LstTo) of
        {ok, _Metadata, no_results} ->
            InfoText = list_to_binary([<<"Mstech:Info:Admin:Operation on Server:">>,
                                  get_info_text({info, query_no_result, {BitHost}})]),
            error_logger:info_msg(InfoText),
            {200, ?RESPONSE_HEADER, []};
        {ok, _Metadata, Result} ->
            InfoText = list_to_binary([<<"Mstech:Info:Admin:Operation on Server:">>,
                                  get_info_text({info, query_success_result, {BitHost}})]),
            error_logger:info_msg(InfoText),
            io:format("Result=~p.~n", [Result]),
            io:format("_Metadata=~p.~n", [_Metadata]),
            Xml = make_xml_query_to_all(Result),
            {200, ?RESPONSE_HEADER, [Xml]};
        {error, _Reason} ->
            InfoText = list_to_binary([<<"Mstech:Error:Admin:Operation on Server:">>,
                                  get_info_text({info, query_error, {BitHost}})]),
            error_logger:info_msg(InfoText),
            {404, ?RESPONSE_HEADER, [InfoText]}
    end.



%%%----------------------------------------------------------------------
%%% Mstech - Utils Functions
%%%----------------------------------------------------------------------

%% Mstech aux function (first step): for each entry (list of bitstrings),
%% this function will return a tuple of strings
%% should be called to convert entries of sql response
entries_to_tuple_lists(Entries) ->
  list_to_tuple(
    lists:map(
      fun(Elem) ->
        bitstring_to_list(Elem)
      end, Entries)).

%% Mstech function: for each List element (list of lists),
%% this function will convert the element E into tuple of lists
%% in short: convert a tuple list to field nodes in xmerl format
element_to_tuple_list(Fields) ->
  Tmp = lists:map(
    fun(E) ->
      entries_to_tuple_lists(E)
    end, Fields),
   [{message,[{id, Time}],
             [{date,[Date]},
              {to,[To]},
              {from,[From]},
              {body,[Body]}]} || {Date,To,From,Body,Time} <- Tmp]. %date, to_user, from_user, txt

chat_to_xml(Messages) ->
  {chat, element_to_tuple_list(Messages)}.

%% transform xmelrs format into XML
make_xml(SqlReturn) ->
  Xml = xmerl:export_simple([chat_to_xml(SqlReturn)],
      xmerl_xml,[{prolog, ?xml_prolog}]),
  unicode:characters_to_binary(Xml).

element_to_tuple_list_to_all(Fields) ->
  Tmp = lists:map(
    fun(E) ->
      entries_to_tuple_lists(E)
    end, Fields),
   [{message,[{date, Date}],
             [{user,[User]}]} || {Date,User} <- Tmp]. %date, to_user, from_user, txt

chat_to_xml_to_all(Messages) ->
  {chat, element_to_tuple_list_to_all(Messages)}.

%% transform xmelrs format into XML
make_xml_query_to_all(SqlReturn) ->
  Xml = xmerl:export_simple([chat_to_xml_to_all(SqlReturn)],
      xmerl_xml,[{prolog, ?xml_prolog}]),
  unicode:characters_to_binary(Xml).

%% Mstech: not used now. The query must be like:
%% select id, from, to, txt, created_at from archive order by created_at limit N_start,Elements
sql_select_to_xml(MysqlResults) ->
  {ok,_Fields, Result} = MysqlResults,
%%  Columns = list_to_tuple(lists:map(fun(Elem) -> bitstring_to_list(Elem) end, Fields),
  TResult = lists:map(fun(Elem) -> list_to_tuple(Elem) end, Result),
  Content = [{ok, [{id, bitstring_to_list(ID)}],
                  [{from_user, [bitstring_to_list(FU)]},
                  {to_user, [bitstring_to_list(TU)]},
                  {txt, [bitstring_to_list(BD)]},
                  {created_at, [bitstring_to_list(CA)]}]} || {ID,FU,TU,BD,CA} <- TResult],
  xmerl:export_simple(Content,xmerl_xml).


%% Answer by stackoverflow
%%
%%  lists:seq/2 return a list starting from position 2 up to number of elements.
fields_to_xml_simple2(Fields) ->
  [
    fun(X) ->
      [{K, V}] = element(X, Fields),
      {field, [{name, atom_to_list(K)}], [V]}
    end(E)
    || E <- lists:seq(2, tuple_size(Fields))].

doc_xml_simple2(Fields) ->
  {chat, [{message, fields_to_xml_simple2(K)} || K <- Fields]}.

makeXml(Fields) ->
  Xml = xmerl:export_simple([doc_xml_simple2(Fields)], xmerl_xml,
    [{prolog, ?xml_prolog}]),
  unicode:characters_to_binary(Xml).

%%%----------------------------------------------------------------------
%%% Info texts
%%%----------------------------------------------------------------------

get_info_text({info, to_from, {Host, To, From, Date}}) ->
  list_to_binary([<<"Filter chat message on host:">>, Host, <<" to:">>, To, <<" and from:">>, From, <<" ordering by date:">>, Date, <<".">>]);
get_info_text({info, to_all, {Host, To}}) ->
  list_to_binary([<<"Filter chat message on host:">>, Host, <<" to:">>, To, <<".">>]);
get_info_text({info, query_no_result, {Host}}) ->
  list_to_binary([<<"Filter chat message on host:">>, Host, <<". The query returned no results.">>]);
get_info_text({info, query_success_result, {Host}}) ->
  list_to_binary([<<"Filter chat message on host:">>, Host, <<". The query returned with success">>]);
get_info_text({info, query_error, {Host}}) ->
  list_to_binary([<<"Filter chat message on Host: ">>, Host, <<". Wrong query syntax.">>]).

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

