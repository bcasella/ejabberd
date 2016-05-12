%%%-------------------------------------------------------------------
%%% File    : mod_mstech_web.erl
%%% Author  : Takao <takao.matsumura@mstech.com.br>
%%% Purpose : Web interface to manage accounts and related tasks
%%% Created :  15 June 2015 by Takao <takao.matsumura@mstech.com.br>
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

%%%----------------------------------------------------------------------
%%% Module  : mod_ms_account_web.erl
%%%
%%% Mstech Module description:
%%%
%%% About logging messages:
%%% It was decided to log all operation log (messages) as [info]
%%% instead of [warning] level. By default, erlang logs every [warning]
%%% level as [error] level, and it will write on 3 files, instead of only
%%% one. So these log files (ejabberd.log, error.log and crash.log) will
%%% register non error operations.
%%%
%%%----------------------------------------------------------------------

%debugging mode
%-compile([debug_info, export_all]).

-module(mod_ms_account_web).

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
-define(AC_ALLOW_METHODS, {<<"Access-Control-Allow-Methods">>, <<"POST, GET, OPTIONS">>}).
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
%%% Mstech - Added Utils Functions
%%%----------------------------------------------------------------------

make_doc_xml(Fields) ->
    Xml = xmerl:export_simple([doc_xml_simple(Fields)], xmerl_xml,
                              [{prolog, ?xml_prolog}]),
    unicode:characters_to_binary(Xml).

fields_to_xml_simple(Fields) ->
    [ {field, [{name, K}], [V]} || {K, V} <- Fields ].

doc_xml_simple(Fields) ->
    {[{users, fields_to_xml_simple(Fields)}]}.

% function ServerUser to List
su_to_list({Server, User}) ->
    jlib:jid_to_string({User, Server, <<"">>}).

%%%----------------------------------------------------------------------
%%% HTTP handlers
%%% Mstech - All internal methods of mod_mstech_web are accessible
%%% through process/2 (handlers).
%%%----------------------------------------------------------------------
process([], #request{method = 'OPTIONS'}) ->
    {200, ?OPTIONS_HEADER, []};

process([], #request{method = 'HEAD'}) ->
    {200, ?HEADER, []};

process([<<"get_version">>], #request{method = 'OPTIONS'}) ->
    {200, ?OPTIONS_HEADER, []};

process([<<"list_activity">>], #request{method = 'OPTIONS'}) ->
    {200, ?OPTIONS_HEADER, []};

process([<<"list_all">>], #request{method = 'OPTIONS'}) ->
    {200, ?OPTIONS_HEADER, []};

process([<<"delete_admin">>], #request{method = 'OPTIONS'}) ->
    {200, ?OPTIONS_HEADER, []};

process([<<"delete">>], #request{method = 'OPTIONS'}) ->
    {200, ?OPTIONS_HEADER, []};

process([<<"register">>], #request{method = 'OPTIONS'}) ->
    {200, ?OPTIONS_HEADER, []};

process([<<"change_password">>], #request{method = 'OPTIONS'}) ->
    {200, ?OPTIONS_HEADER, []};

process([<<"change_password_admin">>], #request{method = 'OPTIONS'}) ->
    {200, ?OPTIONS_HEADER, []};

process([<<"get_version">>], #request{method = 'GET'}) ->
    {200, ?RESPONSE_HEADER, ["2.1.1"]};

%process([], #request{method = 'GET', lang = Lang}) ->
%    index_page(Lang);

process([<<"register.css">>],
	#request{method = 'GET'}) ->
    serve_css();

%%  Mstech - list of all registered users.
process([<<"list_all">>],
	#request{method = 'GET'}) ->
    form_mstech_reg_users();

%%  Mstech - list of all users (last) activity.
process([<<"list_activity">>],
    #request{method = 'GET', lang = Lang}) ->
    form_user_act(Lang);

%%  Mstech - new user register POST request
%%  POST - body should have: username=[user]&server=[domain.com]&password=[psswd]
process([<<"register">>],
	#request{method = 'POST', q = Q, ip = {Ip, _Port},
		 lang = Lang, host = _HTTPHost}) ->
    case mstech_form_new_post(Q) of
      {success, ok, {Username, Server, _Password}} ->
	  Jid = jlib:make_jid(Username, Server, <<"">>),
          mod_register:send_registration_notifications(?MODULE, Jid, Ip),
    Text = list_to_binary([?T(<<"Mstech:Info:Admin:Operation on Server:The account ">>),
                            ?T(Username),
                            ?T(<<" at ">>),
                            ?T(Server),
                            ?T(<<" was successfully created.">>)]),
        error_logger:info_msg(Text),
      {200, ?RESPONSE_HEADER, Text};
%	  {200, [{<<"Server">>, <<"ejabberd">>},
%                                  {<<"Content-Type">>, <<"application/xml">>},
%                                  {<<"Access-Control-Allow-Origin">>, <<"*">>}], Text};
      Error ->
	  ErrorText =
                list_to_binary([?T(<<"Mstech:Error:Admin:Operation on Server:There was an error creating the account: ">>),
                                ?T(get_error_text(Error))]),
        error_logger:info_msg(ErrorText),
      {404, ?RESPONSE_HEADER, ErrorText}
%	  {404, [{<<"Server">>, <<"ejabberd">>},
%                                  {<<"Content-Type">>, <<"application/xml">>},
%                                  {<<"Access-Control-Allow-Origin">>, <<"*">>}], ErrorText}
    end;

%%  Mstech - unregister an existing user POST request by own user (requires password)
%%  POST - body should have: username=[user]&server=[domain.com]&password=[psswd]
process([<<"delete">>],
	#request{method = 'POST', q = Q, lang = Lang,
		 host = _Host}) ->
    case mstech_form_del_post(Q) of
      {atomic, ok, Username, Server} ->
            Text = list_to_binary([?T(<<"Mstech:Info:User:Operation on Server:The account ">>),
                            ?T(Username),
                            ?T(<<" at ">>),
                            ?T(Server),
                            ?T(<<" was successfully deleted.">>)]),
        error_logger:info_msg(Text),
	  {200, ?RESPONSE_HEADER, Text};
%	  {200, [{<<"Server">>, <<"ejabberd">>},
%                      {<<"Content-Type">>, <<"application/xml">>},
%                      {<<"Access-Control-Allow-Origin">>, <<"*">>}], Text};
      Error ->
	  ErrorText =
                list_to_binary([?T(<<"Mstech:Error:User:Operation on Server:There was an error deleting the account: ">>),
                                ?T(get_error_text(Error))]),
        error_logger:info_msg(ErrorText),
	  {404, ?RESPONSE_HEADER, ErrorText}
%	  {404, [{<<"Server">>, <<"ejabberd">>},
%                      {<<"Content-Type">>, <<"application/xml">>},
%                      {<<"Access-Control-Allow-Origin">>, <<"*">>}], ErrorText}
    end;

%%  Mstech - unregister an existing user POST request by admin (don't requires password)
%%  POST - body should have: username=[user]&server=[domain.com]
process([<<"delete_admin">>],
	#request{method = 'POST', q = Q, lang = Lang,
		 host = _Host}) ->

    case mstech_admin_form_del_post(Q) of
      {atomic, ok, Username, Server} ->
            Text = list_to_binary([?T(<<"Mstech:Info:Admin:Operation on Server:The account ">>),
                            ?T(Username),
                            ?T(<<" at ">>),
                            ?T(Server),
                            ?T(<<" was successfully deleted.">>)]),
        error_logger:info_msg(Text),
      {200, ?RESPONSE_HEADER, Text};
%	  {200, [{<<"Server">>, <<"ejabberd">>},
%                      {<<"Content-Type">>, <<"application/xml">>},
%                      {<<"Access-Control-Allow-Origin">>, <<"*">>}], Text};
      Error ->
	  ErrorText =
                list_to_binary([?T(<<"Mstech:Error:Admin:Operation on Server:There was an error deleting the account: ">>),
                                ?T(get_error_text(Error))]),
        error_logger:info_msg(ErrorText),
      {404, ?RESPONSE_HEADER, ErrorText}
%	  {404, [{<<"Server">>, <<"ejabberd">>},
%                      {<<"Content-Type">>, <<"application/xml">>},
%                      {<<"Access-Control-Allow-Origin">>, <<"*">>}], ErrorText}
    end;

%%  Mstech - change user's password
%%  This handler doesn't check the new password.
%%  Any verification (such writing new password twice) must be done before.
%%  POST - body should have: username=[user]&server=[domain.com]&passwordold=[old]&password1=[new]&password2=[new]
process([<<"change_password">>],
	#request{method = 'POST', q = Q, lang = Lang}) ->
    case mstech_form_change_password_post(Q) of
      {atomic, ok, Username, Server} ->
            Text = list_to_binary([?T(<<"Mstech:Info:User:Operation on Server:The password of ">>),
                            ?T(Username),
                            ?T(<<" at ">>),
                            ?T(Server),
                            ?T(<<" account was successfully changed.">>)]),
        error_logger:info_msg(Text),
	  {200, ?RESPONSE_HEADER, Text};
%	  {200, [{<<"Server">>, <<"ejabberd">>},
%                      {<<"Content-Type">>, <<"application/xml">>},
%                      {<<"Access-Control-Allow-Origin">>, <<"*">>}], Text};
      Error ->
	  ErrorText =
                list_to_binary([?T(<<"Mstech:Error:User:Operation on Server:There was an error changing the password: ">>),
                                ?T(get_error_text(Error))]),
        error_logger:info_msg(ErrorText),
      {404, ?RESPONSE_HEADER, ErrorText}
%	  {404, [{<<"Server">>, <<"ejabberd">>},
%                      {<<"Content-Type">>, <<"application/xml">>},
%                      {<<"Access-Control-Allow-Origin">>, <<"*">>}], ErrorText}
    end;

%%  Mstech - change user's password
%%  This handler doesn't check the new password.
%%  Any verification (such writing new password twice) must be done before.
%%  POST - body should have: username=[user]&server=[domain.com]&password1=[new]&password2=[new]
process([<<"change_password_admin">>],
    #request{method = 'POST', q = Q, lang = Lang}) ->
  case mstech_form_change_password_adm_post(Q) of
    {atomic, ok, Username, Server} ->
      Text = list_to_binary([?T(<<"Mstech:Info:Admin:Operation on Server:The password of ">>),
        ?T(Username),
        ?T(<<" at ">>),
        ?T(Server),
        ?T(<<" account was successfully changed.">>)]),
      error_logger:info_msg(Text),
      {200, ?RESPONSE_HEADER, Text};
%      {200, [{<<"Server">>, <<"ejabberd">>},
%        {<<"Content-Type">>, <<"application/xml">>},
%        {<<"Access-Control-Allow-Origin">>, <<"*">>}], Text};
    Error ->
      ErrorText =
        list_to_binary([?T(<<"Mstech:Error:Admin:Operation on Server:There was an error changing the password: ">>),
          ?T(get_error_text(Error))]),
      error_logger:info_msg(ErrorText),
      {404, ?RESPONSE_HEADER, ErrorText}
%      {404, [{<<"Server">>, <<"ejabberd">>},
%        {<<"Content-Type">>, <<"application/xml">>},
%        {<<"Access-Control-Allow-Origin">>, <<"*">>}], ErrorText}
  end;

process(_Path, _Request) ->
    {404, ?RESPONSE_HEADER, "Not Found"}.
%    {404, [{<<"Server">>, <<"ejabberd">>},
%                    {<<"Content-Type">>, <<"application/xml">>},
%                    {<<"Access-Control-Allow-Origin">>, <<"*">>}], "Not Found"}.

%%%----------------------------------------------------------------------
%%% CSS
%%%----------------------------------------------------------------------

serve_css() ->
    {200,
     [{<<"Content-Type">>, <<"text/css">>}, last_modified(),
      cache_control_public()],
     css()}.

last_modified() ->
    {<<"Last-Modified">>,
     <<"Mon, 25 Feb 2008 13:23:30 GMT">>}.

cache_control_public() ->
    {<<"Cache-Control">>, <<"public">>}.

css() ->
    <<"html,body {\nbackground: white;\nmargin: "
      "0;\npadding: 0;\nheight: 100%;\n}">>.

%%%----------------------------------------------------------------------
%%% Index page
%%%----------------------------------------------------------------------
%
%index_page(Lang) ->
%    HeadEls = [?XCT(<<"title">>,
%		    <<"Mstech Web Jabber Account Administration">>),
%	       ?XA(<<"link">>,
%		   [{<<"href">>, <<"/mstech/register.css">>},
%		    {<<"type">>, <<"text/css">>},
%		    {<<"rel">>, <<"stylesheet">>}])],
%    Els = [?XACT(<<"h1">>,
%		 [{<<"class">>, <<"title">>},
%		  {<<"style">>, <<"text-align:center;">>}],
%		 <<"Module MStech Web">>),
%	   ?XE(<<"ul">>,
%	    [?XE(<<"li">>,
%            [?ACT(<<"list_all">>, <<"List registered users">>)]),
%		?XE(<<"li">>,
%            [?ACT(<<"list_activity">>, <<"List all user's last activity">>)])
%        ])],
%    {200,
%         [{<<"Server">>, <<"ejabberd">>},
%                   {<<"Content-Type">>, <<"application/xml">>},
%                   {<<"Access-Control-Allow-Origin">>, <<"*">>}],
%          ejabberd_web:make_xhtml(HeadEls, Els)}.

%%%----------------------------------------------------------------------
%%% Mstech List of all registered users
%%%----------------------------------------------------------------------

form_mstech_reg_users() ->

  Text =
    list_to_binary([<<"Mstech:Info:Admin:Operation on Server:">>, get_info_text({info, list_all})]),
    error_logger:info_msg(Text),

    RegUsers = ejabberd_auth:dirty_get_registered_users(),
    SRUsers = lists:sort([{S,U}||{U,S} <- RegUsers]),
    BitList = [su_to_list(Users)|| Users <- SRUsers],
    StringList = [binary_to_list(Tmp) || Tmp <- BitList],
    TList = lists:map( fun(Elem) -> erlang:append_element({"e-mail"}, Elem) end, StringList),
    XMLReturn = make_doc_xml(TList),
    {200, ?RESPONSE_HEADER, XMLReturn}.
%    {200,
%         [{<<"Server">>, <<"ejabberd">>},
%          {<<"Content-Type">>, <<"application/xml">>},
%          {<<"Access-Control-Allow-Origin">>, <<"*">>}],
%          XMLReturn}.

%%%----------------------------------------------------------------------
%%% Formulary Mstech for a list of user's (last) activity
%%%----------------------------------------------------------------------

form_user_act(Lang) ->

  Text =
    list_to_binary([<<"Mstech:Info:Admin:Operation on Server:">>, get_info_text({info, activity})]),
    error_logger:info_msg(Text),

    BsRegUsers = ejabberd_auth:dirty_get_registered_users(),
    LLastAct = [binary_to_list(X) || X <- list_given_users(BsRegUsers, Lang)],
    LRegUsers = [binary_to_list(X) || X <-[su_to_list(Users) || Users <- [{S,U}||{U,S} <- BsRegUsers]]],
    List = lists:zip(LRegUsers, LLastAct),
    SList = lists:sort([{U,A}||{U,A} <- List]),
    XMLReturn = make_doc_xml(SList),
    {200, ?RESPONSE_HEADER, XMLReturn}.

%%% Mstech - Modified function from ejabberd_web_admin
%%% this function verify the last activity
%%% for each user from a given user list (Bitstring list of Registered Users)
%%% never - if never logged in
%%% universal date/hour format - last date/hour the user were seen logged in
%%% status - user online status (available, dnd, away, ...)
list_given_users(BsRegUsers, Lang) ->
	     (lists:map(fun (_SU = {User, Server}) ->
				case ejabberd_sm:get_user_resources(User, Server)
					    of
					  [] ->
					      case mod_last:get_last_info(User,
									  Server)
						  of
						not_found -> ?T(<<"Never">>);
						{ok, Shift, _Status} ->
						    TimeStamp = {Shift div
								   1000000,
								 Shift rem
								   1000000,
								 0},
						    {{Day, Month, Year},
						     {Hour, Minute, Second}} =
							calendar:now_to_local_time(TimeStamp),
						    iolist_to_binary(io_lib:format("~w-~.2.0w-~w ~.2.0w:~.2.0w:~.2.0w",
										   [Year,
										    Month,
										    Day,
										    Hour,
										    Minute,
										    Second]))
					      end;
                       _ ->
                            Status = mod_admin_extra:user_sessions_info(User,Server),
                            Temp = [G || {_A,_B,_C,_D,_E,_F,G,_H,_I} <- Status],
                            ?T(lists:min(Temp))
					end
			end,
			BsRegUsers)).

%%%----------------------------------------------------------------------
%%% Mstech Formulary new POST
%%% function mstech_form_new_post base on form_new_post and customized
%%%----------------------------------------------------------------------

mstech_form_new_post(Q) ->
    case catch mstech_get_register_parameters(Q) of
      [Username, Server, Password] ->
	  mstech_register_account(Username, Server, Password);
      _ -> {error, wrong_parameters}
    end.

mstech_get_register_parameters(Q) ->
    lists:map(fun (Key) ->
		      case lists:keysearch(Key, 1, Q) of
			{value, {_Key, Value}} -> Value;
			false -> false
		      end
	      end,
	      [<<"username">>, <<"server">>, <<"password">>]).

%% @spec(Username, Host, Password) -> {success, ok, {Username, Host, Password} |
%%                                    {success, exists, {Username, Host, Password}} |
%%                                    {error, not_allowed} |
%%                                    {error, invalid_jid}
mstech_register_account(Username, Server, Password) ->
    Access = gen_mod:get_module_opt(Server, mod_register, access,
                                    fun(A) when is_atom(A) -> A end,
                                    all),
    case jlib:make_jid(Username, Server, <<"">>) of
      error -> {error, invalid_jid, Username, Server};
      JID ->
%        io:format("~nServer: ~p ~nAccess: ~p ~nJID: ~p ", [Server, Access, JID]),
        case acl:match_rule(Server, Access, JID) of
          % match_rule won't return Username/Server. acl:match_rule does not return these variables.
          deny -> {error, not_allowed};    % it can't be: deny -> {error, not_allowed, Username, Server};
          allow -> register_account(Username, Server, Password)
        end
    end.

register_account(Username, Server, Password) ->
    case ejabberd_auth:try_register(Username, Server,
				    Password)
	of
      {atomic, Res} ->
	  {success, Res, {Username, Server, Password}};
      Other -> Other
    end.

change_password(Username, Server, PasswordOld, Password) ->
    account_exists = check_account_exists(Username, Server),
    password_correct = check_password(Username, Server, PasswordOld),
    ok = ejabberd_auth:set_password(Username, Server,Password),
    case check_password(Username, Server, Password) of
      password_correct -> {atomic, ok};
      password_incorrect -> {error, password_not_changed, Username, Server}
    end.

change_password(Username, Server, Password) ->
  account_exists = check_account_exists(Username, Server),
  case ejabberd_auth:set_password(Username, Server,Password) of
    ok -> {atomic, ok};
    _ -> {error, password_not_changed, Username, Server}
  end.

check_account_exists(Username, Server) ->
    case ejabberd_auth:is_user_exists(Username, Server) of
      true -> account_exists;
      false -> account_doesnt_exist
    end.

check_password(Username, Server, Password) ->
    case ejabberd_auth:check_password(Username, Server, Password) of
      true -> password_correct;
      false -> password_incorrect
    end.

%%%----------------------------------------------------------------------
%%% Mstech Formulary change password POST
%%% password1 is the new password for User
%%% password2 is the confirmation for new password
%%%----------------------------------------------------------------------

mstech_form_change_password_post(Q) ->
    case catch mstech_get_change_password_parameters(Q) of
      [Username, Server, PasswordOld, Password, Password] ->
	      mstech_try_change_password(Username, Server, PasswordOld, Password);
      [Username, Server, _PasswordOld, _Password1, _Password2] ->
        {error, passwords_not_identical,Username, Server};
      _ -> {error, wrong_parameters}
    end.

mstech_get_change_password_parameters(Q) ->
%% @spec(Username,Host,PasswordOld,Password) -> {atomic, ok} |
%%                                              {error, account_doesnt_exist} |
%%                                              {error, password_not_changed} |
%%                                              {error, password_incorrect}
    lists:map(fun (Key) ->
		      {value, {_Key, Value}} = lists:keysearch(Key, 1, Q),
		      Value
	      end,
	      [<<"username">>, <<"server">>, <<"passwordold">>, <<"password1">>, <<"password2">>]).

mstech_try_change_password(Username, Server, PasswordOld,
		    Password) ->
    try change_password(Username, Server, PasswordOld,
			Password)
    of
      {atomic, ok} -> {atomic, ok, Username, Server}
    catch
      error:{badmatch, Error} -> {error, Error, Username, Server}
    end.

%%%----------------------------------------------------------------------
%%% Mstech Formulary Admin change password POST
%%% password1 is the new password for User
%%% password2 is the confirmation for new password
%%%----------------------------------------------------------------------

mstech_form_change_password_adm_post(Q) ->
  case catch mstech_get_change_password_adm_parameters(Q) of
    [Username, Server, Password, Password] ->
      mstech_try_change_password_adm(Username, Server, Password);
    [Username, Server,  _Password1, _Password2] ->
      {error, passwords_not_identical,Username, Server};
    _ -> {error, wrong_parameters}
  end.

mstech_get_change_password_adm_parameters(Q) ->
%% @spec(Username,Host,PasswordOld,Password) -> {atomic, ok} |
%%                                              {error, account_doesnt_exist} |
%%                                              {error, password_not_changed} |
%%                                              {error, password_incorrect}
  lists:map(fun (Key) ->
    {value, {_Key, Value}} = lists:keysearch(Key, 1, Q),
    Value
  end,
    [<<"username">>, <<"server">>, <<"password1">>, <<"password2">>]).

mstech_try_change_password_adm(Username, Server, Password) ->
  try change_password(Username, Server, Password)
  of
    {atomic, ok} -> {atomic, ok, Username, Server}
  catch
    error:{badmatch, Error} -> {error, Error, Username, Server}
  end.

%%%----------------------------------------------------------------------
%%% Mstech Formulary user delete POST
%%%----------------------------------------------------------------------

mstech_form_del_post(Q) ->
    case catch mstech_get_unregister_parameters(Q) of
      [Username, Server, Password] ->
	    try_unregister_account(Username, Server, Password);
        _ -> {error, wrong_parameters}
    end.

mstech_get_unregister_parameters(Q) ->
%% @spec(Username, Host, Password) -> {atomic, ok} |
%%                                    {error, account_doesnt_exist} |
%%                                    {error, account_exists} |
%%                                    {error, password_incorrect}
%% Host is stored on IP format, so <<"server">> was added to specify the user's domain.
    lists:map(fun (Key) ->
		      {value, {_Key, Value}} = lists:keysearch(Key, 1, Q),
		      Value
	      end,
	      [<<"username">>, <<"server">>, <<"password">>]).

try_unregister_account(Username, Host, Password) ->
    try unregister_account(Username, Host, Password) of
      {atomic, ok, Username, Host} -> {atomic, ok, Username, Host}
    catch
      error:{badmatch, Error} -> {error, Error, Username, Host}
    end.

unregister_account(Username, Host, Password) ->
    account_exists = check_account_exists(Username, Host),
    password_correct = check_password(Username, Host,
				      Password),
    ok = ejabberd_auth:remove_user(Username, Host,
				   Password),
    account_doesnt_exist = check_account_exists(Username,
						Host),
    {atomic, ok, Username, Host}.

%%%----------------------------------------------------------------------
%%% Mstech Formulary admin delete POST
%%%----------------------------------------------------------------------

mstech_admin_form_del_post(Q) ->
    case catch mstech_admin_get_unregister_parameters(Q) of
      [Username, Server] ->
	    try_admin_unregister_account(Username, Server);
        _ -> {error, wrong_parameters}
    end.

mstech_admin_get_unregister_parameters(Q) ->
%% @spec(Username, Host, Password) -> {atomic, ok} |
%%                                    {error, account_doesnt_exist} |
%%                                    {error, account_exists} |
%%                                    {error, password_incorrect}
%% Host is stored on IP format, so <<"server">> was added to specify the user's domain.
    lists:map(fun (Key) ->
		      {value, {_Key, Value}} = lists:keysearch(Key, 1, Q),
		      Value
	      end,
	      [<<"username">>, <<"server">>]).

try_admin_unregister_account(Username, Host) ->
    try admin_unregister_account(Username, Host) of
      {atomic, ok, Username, Host} -> {atomic, ok, Username, Host}
    catch
      error:{badmatch, Error} -> {error, Error, Username, Host}
    end.

admin_unregister_account(Username, Host) ->
    account_exists = check_account_exists(Username, Host),
    ok = ejabberd_auth:remove_user(Username, Host),
    account_doesnt_exist = check_account_exists(Username,Host),
    {atomic, ok, Username, Host}.

%%%----------------------------------------------------------------------
%%% Info texts
%%%----------------------------------------------------------------------

get_info_text({info, activity}) ->
  <<"User's last activity / connected status.">>;
get_info_text({info, list_all}) ->
  <<"List of all registered users.">>.

%%%----------------------------------------------------------------------
%%% Error texts
%%%----------------------------------------------------------------------

get_error_text({success, exists, {Username, Server, _Password}}) ->
  get_error_text({atomic, exists, {Username, Server}});
get_error_text({success, exists, {Username, Server}}) ->
    get_error_text({atomic, exists, {Username, Server}});
get_error_text({atomic, exists, {Username, Server}}) ->
  list_to_binary([<<"The account ">>, Username, <<" at ">>, Server, <<" already exists.">>]);
get_error_text({error, password_incorrect}) ->
  list_to_binary([<<"Incorrect password.">>]);
get_error_text({error, password_incorrect, Username, Server}) ->
  list_to_binary([<<"Incorrect password for ">>, Username, <<" at ">>, Server, <<".">>]);
get_error_text({error, invalid_jid, Username, Server}) ->
  list_to_binary([<<"The username ">>, Username, <<" at ">>, Server, <<" is not valid.">>]);
get_error_text({error, not_allowed}) ->
  list_to_binary([<<"Operation not allowed.">>]);
get_error_text({error, account_doesnt_exist, Username, Server}) ->
  list_to_binary([<<"The account ">>, Username, <<" at ">>, Server, <<" doesn't exist.">>]);
get_error_text({error, account_doesnt_exist}) ->
    <<"Account doesn't exist.">>;
get_error_text({error, account_exists, Username, Server}) ->
  list_to_binary([<<"Teh account ">>, Username, <<" at ">>, Server, <<" was not deleted.">>]);
get_error_text({error, password_not_changed, Username, Server}) ->
  list_to_binary([<<"The password for ">>, Username, <<" at ">>, Server, <<" was not changed.">>]);
get_error_text({error, passwords_not_identical, Username, Server}) ->
  list_to_binary([<<"The new password for ">>, Username, <<" at ">>, Server, <<" are different.">>]);
get_error_text({error, wrong_parameters}) ->
    <<"Wrong parameters in the web formulary.">>.

mod_opt_type(_) -> [].
