%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2016, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(mod_vcard_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

-behaviour(mod_vcard).

%% API
-export([init/2, get_vcard/2, set_vcard/4, search/4, remove_user/2,
	 import/1, import/2, export/1]).

-include("jlib.hrl").
-include("mod_vcard.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(_Host, _Opts) ->
    ok.

get_vcard(LUser, LServer) ->
    case catch sql_queries:get_vcard(LServer, LUser) of
	{selected, [{SVCARD}]} ->
	    case fxml_stream:parse_element(SVCARD) of
		{error, _Reason} -> error;
		VCARD -> [VCARD]
	    end;
	{selected, []} -> [];
	_ -> error
    end.

set_vcard(LUser, LServer, VCARD,
	  #vcard_search{user = {User, _},
			fn = FN,
			lfn = LFN,
			family = Family,
			lfamily = LFamily,
			given = Given,
			lgiven = LGiven,
			middle = Middle,
			lmiddle = LMiddle,
			nickname = Nickname,
			lnickname = LNickname,
			bday = BDay,
			lbday = LBDay,
			ctry = CTRY,
			lctry = LCTRY,
			locality = Locality,
			llocality = LLocality,
			email = EMail,
			lemail = LEMail,
			orgname = OrgName,
			lorgname = LOrgName,
			orgunit = OrgUnit,
			lorgunit = LOrgUnit}) ->
    SVCARD = fxml:element_to_binary(VCARD),
    sql_queries:set_vcard(LServer, LUser, BDay, CTRY,
			   EMail, FN, Family, Given, LBDay,
			   LCTRY, LEMail, LFN, LFamily,
			   LGiven, LLocality, LMiddle,
			   LNickname, LOrgName, LOrgUnit,
			   Locality, Middle, Nickname, OrgName,
			   OrgUnit, SVCARD, User).

search(LServer, Data, AllowReturnAll, MaxMatch) ->
    MatchSpec = make_matchspec(LServer, Data),
    if (MatchSpec == <<"">>) and not AllowReturnAll -> [];
       true ->
	    Limit = case MaxMatch of
			infinity ->
			    <<"">>;
			Val ->
			    [<<" LIMIT ">>, jlib:integer_to_binary(Val)]
		    end,
	   case catch ejabberd_sql:sql_query(
			LServer,
			[<<"select username, fn, family, given, "
			   "middle,        nickname, bday, ctry, "
			   "locality,        email, orgname, orgunit "
			   "from vcard_search ">>,
			 MatchSpec, Limit, <<";">>]) of
	       {selected,
		[<<"username">>, <<"fn">>, <<"family">>, <<"given">>,
		 <<"middle">>, <<"nickname">>, <<"bday">>, <<"ctry">>,
		 <<"locality">>, <<"email">>, <<"orgname">>,
		 <<"orgunit">>], Rs} when is_list(Rs) ->
		   Rs;
	       Error ->
		   ?ERROR_MSG("~p", [Error]), []
	   end
    end.

remove_user(LUser, LServer) ->
    ejabberd_sql:sql_transaction(
      LServer,
      fun() ->
              ejabberd_sql:sql_query_t(
                ?SQL("delete from vcard where username=%(LUser)s")),
              ejabberd_sql:sql_query_t(
                ?SQL("delete from vcard_search where lusername=%(LUser)s"))
      end).

export(_Server) ->   
    [{vcard,
      fun(Host, #vcard{us = {LUser, LServer}, vcard = VCARD})
            when LServer == Host ->
              SVCARD = fxml:element_to_binary(VCARD),
              [?SQL("delete from vcard where username=%(LUser)s;"),
               ?SQL("insert into vcard(username, vcard) values ("
                    "%(LUser)s, %(SVCARD)s);")];
         (_Host, _R) ->
              []
      end},
     {vcard_search,
      fun(Host, #vcard_search{user = {User, LServer}, luser = LUser,
                              fn = FN, lfn = LFN, family = Family,
                              lfamily = LFamily, given = Given,
                              lgiven = LGiven, middle = Middle,
                              lmiddle = LMiddle, nickname = Nickname,
                              lnickname = LNickname, bday = BDay,
                              lbday = LBDay, ctry = CTRY, lctry = LCTRY,
                              locality = Locality, llocality = LLocality,
                              email = EMail, lemail = LEMail,
                              orgname = OrgName, lorgname = LOrgName,
                              orgunit = OrgUnit, lorgunit = LOrgUnit})
            when LServer == Host ->
              [?SQL("delete from vcard_search where lusername=%(LUser)s;"),
               ?SQL("insert into vcard_search(username,"
                    " lusername, fn, lfn, family, lfamily,"
                    " given, lgiven, middle, lmiddle,"
                    " nickname, lnickname, bday, lbday,"
                    " ctry, lctry, locality, llocality,"
                    " email, lemail, orgname, lorgname,"
                    " orgunit, lorgunit) values ("
                    " %(LUser)s, %(User)s,"
                    " %(FN)s, %(LFN)s,"
                    " %(Family)s, %(LFamily)s,"
                    " %(Given)s, %(LGiven)s,"
                    " %(Middle)s, %(LMiddle)s,"
                    " %(Nickname)s, %(LNickname)s,"
                    " %(BDay)s, %(LBDay)s,"
                    " %(CTRY)s, %(LCTRY)s,"
                    " %(Locality)s, %(LLocality)s,"
                    " %(EMail)s, %(LEMail)s,"
                    " %(OrgName)s, %(LOrgName)s,"
                    " %(OrgUnit)s, %(LOrgUnit)s);")];
         (_Host, _R) ->
              []
      end}].

import(LServer) ->
    [{<<"select username, vcard from vcard;">>,
      fun([LUser, SVCard]) ->
              #xmlel{} = VCARD = fxml_stream:parse_element(SVCard),
              #vcard{us = {LUser, LServer}, vcard = VCARD}
      end},
     {<<"select username, lusername, fn, lfn, family, lfamily, "
        "given, lgiven, middle, lmiddle, nickname, lnickname, "
        "bday, lbday, ctry, lctry, locality, llocality, email, "
        "lemail, orgname, lorgname, orgunit, lorgunit from vcard_search;">>,
      fun([User, LUser, FN, LFN,
           Family, LFamily, Given, LGiven,
           Middle, LMiddle, Nickname, LNickname,
           BDay, LBDay, CTRY, LCTRY, Locality, LLocality,
           EMail, LEMail, OrgName, LOrgName, OrgUnit, LOrgUnit]) ->
              #vcard_search{us = {LUser, LServer},
                            user = {User, LServer}, luser = LUser,
                            fn = FN, lfn = LFN, family = Family,
                            lfamily = LFamily, given = Given,
                            lgiven = LGiven, middle = Middle,
                            lmiddle = LMiddle, nickname = Nickname,
                            lnickname = LNickname, bday = BDay,
                            lbday = LBDay, ctry = CTRY, lctry = LCTRY,
                            locality = Locality, llocality = LLocality,
                            email = EMail, lemail = LEMail,
                            orgname = OrgName, lorgname = LOrgName,
                            orgunit = OrgUnit, lorgunit = LOrgUnit}
      end}].

import(_, _) ->
    pass.

%%%===================================================================
%%% Internal functions
%%%===================================================================
make_matchspec(LServer, Data) ->
    filter_fields(Data, <<"">>, LServer).

filter_fields([], Match, _LServer) ->
    case Match of
	<<"">> -> <<"">>;
	_ -> [<<" where ">>, Match]
    end;
filter_fields([{SVar, [Val]} | Ds], Match, LServer)
  when is_binary(Val) and (Val /= <<"">>) ->
    LVal = mod_vcard:string2lower(Val),
    NewMatch = case SVar of
		   <<"user">> -> make_val(Match, <<"lusername">>, LVal);
		   <<"fn">> -> make_val(Match, <<"lfn">>, LVal);
		   <<"last">> -> make_val(Match, <<"lfamily">>, LVal);
		   <<"first">> -> make_val(Match, <<"lgiven">>, LVal);
		   <<"middle">> -> make_val(Match, <<"lmiddle">>, LVal);
		   <<"nick">> -> make_val(Match, <<"lnickname">>, LVal);
		   <<"bday">> -> make_val(Match, <<"lbday">>, LVal);
		   <<"ctry">> -> make_val(Match, <<"lctry">>, LVal);
		   <<"locality">> ->
		       make_val(Match, <<"llocality">>, LVal);
		   <<"email">> -> make_val(Match, <<"lemail">>, LVal);
		   <<"orgname">> -> make_val(Match, <<"lorgname">>, LVal);
		   <<"orgunit">> -> make_val(Match, <<"lorgunit">>, LVal);
		   _ -> Match
	       end,
    filter_fields(Ds, NewMatch, LServer);
filter_fields([_ | Ds], Match, LServer) ->
    filter_fields(Ds, Match, LServer).

make_val(Match, Field, Val) ->
    Condition = case str:suffix(<<"*">>, Val) of
		  true ->
		      Val1 = str:substr(Val, 1, byte_size(Val) - 1),
		      SVal = <<(ejabberd_sql:escape_like(Val1))/binary,
			       "%">>,
		      [Field, <<" LIKE '">>, SVal, <<"'">>];
		  _ ->
		      SVal = ejabberd_sql:escape(Val),
		      [Field, <<" = '">>, SVal, <<"'">>]
		end,
    case Match of
      <<"">> -> Condition;
      _ -> [Match, <<" and ">>, Condition]
    end.
