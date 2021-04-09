%% -*- erlang -*-
%%
%% gpull: git repository management tool.
%%
%% Copyright 2017-2021 Jörgen Brandt <joergen@cuneiform-lang.org>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author Jörgen Brandt <joergen@cuneiform-lang.org>
%% @version 0.1.3
%% @copyright 2017-2021
%%
%% @end
%% -------------------------------------------------------------------

-module( gpull ).

-export( [main/1] ).

-define( REPOINFO, "repo_info.json" ).
-define( CLEAN_REPLY1, "nothing to commit, working tree clean" ).
-define( CLEAN_REPLY2, "Your branch is up to date with" ).

-type repo_obj() :: #{ protocol  => binary(),
                       url       => binary(),
                       repo_list => [binary()] }.

%%====================================================================
%% Escript main function
%%====================================================================

-spec main( ArgLst :: [string()] ) -> ok.

main( ArgLst )
when is_list( ArgLst ) ->

  RepoInfo = load_repo_info( ?REPOINFO ),

  Op =
    case ArgLst of
      []         -> pull;
      ["status"] -> status
    end,


  lists:foreach( fun( Repo ) -> process_repo( Op, Repo ) end, RepoInfo ).


%%====================================================================
%% Internal functions
%%====================================================================

%% Input validation --------------------------------------------------

-spec validate_json_string( B :: _ ) -> binary().

validate_json_string( B )
when is_binary( B ) ->
  B;

validate_json_string( B ) ->
  error( {bad_json_string, B} ).


-spec validate_json_string_list( BLst :: _ ) -> [binary()].

validate_json_string_list( BLst )
when is_list( BLst ) ->
  lists:foreach( fun validate_json_string/1, BLst ),
  BLst;

validate_json_string_list( BLst ) ->
  error( {bad_json_string_list, BLst} ).


-spec validate_json_repo_obj( M :: _ ) -> repo_obj().

validate_json_repo_obj( M = #{ protocol := P, url := U, repo_list := RLst } ) ->
  
  case P of
    <<"git">> -> ok;
    <<"svn">> -> ok;
    _         -> error( {bad_protocol, P} )
  end,

  validate_json_string( U ),
  validate_json_string_list( RLst ),

  M;

validate_json_repo_obj( M ) ->
  error( {bad_json_repo_object, M} ).


-spec validate_json_repo_obj_list( L :: _ ) -> [repo_obj()].

validate_json_repo_obj_list( L )
when is_list( L ) ->
  lists:foreach( fun validate_json_repo_obj/1, L ),
  L;

validate_json_repo_obj_list( L ) ->
  error( {bad_json_repo_obj_list, L} ).



%% File I/O ----------------------------------------------------------

-spec load_repo_info( InfoFile :: string() ) -> [repo_obj()].

load_repo_info( InfoFile ) when is_list( InfoFile ) ->

  case file:read_file( InfoFile ) of

    {error, Reason} ->
      error( Reason );

    {ok, B} ->
      validate_json_repo_obj_list( jsone:decode( B, [{keys, atom}] ) )

  end.


%% Generic repo processor --------------------------------------------

-spec process_repo( Op, M ) -> ok
when Op :: pull | status,
     M  :: repo_obj().

process_repo( Op, #{ protocol := P, url := BaseUrl, repo_list := RepoLst} ) ->
  F = proc_repo( binary_to_atom( P, utf8 ), Op, BaseUrl ),
  lists:foreach( F, RepoLst ).


-spec proc_repo( Protocol, Op, BaseUrl ) -> fun( ( Repo :: binary() ) -> ok )
when Protocol :: git | svn,
     Op       :: pull | status,
     BaseUrl  :: binary().

proc_repo( git, pull, BaseUrl ) ->
  fun( Repo ) ->
    git_pull( BaseUrl, Repo )
  end;

proc_repo( git, status, BaseUrl ) ->
  fun( Repo ) ->
    git_status( BaseUrl, Repo )
  end;

proc_repo( svn, pull, BaseUrl ) ->
  fun( Repo ) ->
    svn_up( BaseUrl, Repo )
  end;

proc_repo( svn, status, BaseUrl ) ->
  fun( Repo ) ->
    svn_status( BaseUrl, Repo )
  end.



%% Repo operations ---------------------------------------------------

-spec git_pull( Prefix, Suffix ) -> ok
when Prefix :: binary(),
     Suffix :: binary().

git_pull( Prefix, Suffix )
when is_binary( Prefix ),
     is_binary( Suffix ) ->

  RepoName = get_repo_name( Prefix, Suffix ),
  RepoUrl = get_repo_url( Prefix, Suffix, <<".git">> ),

  {Action, Cmd} =
    case filelib:is_dir( RepoName ) of

      true ->
        {"git pull", "(cd "++RepoName++" && git pull)"};

      false ->
        {"git clone", "git clone "++RepoUrl++" && (cd "++RepoName++" && git config pull.rebase false)"}
    end,

  InfoMap = #{ "URL" => RepoUrl },

  Reply = os:cmd( Cmd ),

  print_reply( RepoName, Action, InfoMap, Reply ).

-spec git_status( Prefix, Suffix ) -> ok
when Prefix :: binary(),
     Suffix :: binary().

git_status( Prefix, Suffix )
when is_binary( Prefix ),
     is_binary( Suffix ) ->

  RepoName = get_repo_name( Prefix, Suffix ),

  Cmd = "(cd "++RepoName++" && git status)",
  Reply = os:cmd( Cmd ),
  Action = "git status",
  
  case string:find( Reply, ?CLEAN_REPLY1 ) of

    nomatch -> print_reply( RepoName, Action, #{}, Reply );
    _       ->
      case string:find( Reply, ?CLEAN_REPLY2 ) of
        nomatch -> print_reply( RepoName, Action, #{}, Reply );
        _       -> ok
      end
  end.

-spec svn_up( Prefix, Suffix ) -> ok
when Prefix :: binary(),
     Suffix :: binary().

svn_up( Prefix, Suffix ) ->
  
  RepoName = get_repo_name( Prefix, Suffix ),
  RepoUrl = get_repo_url( Prefix, Suffix, <<>> ),

  {Action, Cmd} =
  case filelib:is_dir( RepoName ) of

    true ->
      {"svn up", "(cd "++RepoName++" && svn up)"};

    false ->
      {"svn co", "svn co -q "++RepoUrl++" "++RepoName}
  end,

  InfoMap = #{ "URL" => RepoUrl },

  Reply = os:cmd( Cmd ),

  print_reply( RepoName, Action, InfoMap, Reply ).


-spec svn_status( Prefix, Suffix ) -> ok
when Prefix :: binary(),
     Suffix :: binary().

svn_status( Prefix, Suffix ) ->
  
  RepoName = get_repo_name( Prefix, Suffix ),

  Cmd = "(cd "++RepoName++" && svn status)",
  Reply = os:cmd( Cmd ),
  Action = "svn status",

  case string:is_empty( Reply ) of
    true -> ok;
    false -> print_reply( RepoName, Action, #{}, Reply )
  end.

%% Helper functions --------------------------------------------------

-spec get_repo_name( Prefix, Suffix ) -> string()
when Prefix :: binary(),
     Suffix :: binary().

get_repo_name( Prefix, Suffix )
when is_binary( Prefix ),
     is_binary( Suffix ) ->
  RepoUrl = string:join( [binary_to_list( Prefix ),
                          binary_to_list( Suffix )], "/" ),
  NoTrunk = re:replace( RepoUrl, "/trunk", "", [global] ),
  TrimmedEnd = string:trim( NoTrunk, trailing, "/" ),
  Found = string:find( TrimmedEnd, "/", trailing ),
  TrimmedFront = string:trim( Found, leading, "/" ),
  binary_to_list( TrimmedFront ).



-spec get_repo_url( Prefix, Suffix, Add ) -> string()
when Prefix :: binary(),
     Suffix :: binary(),
     Add    :: binary().

get_repo_url( Prefix, Suffix, Add )
when is_binary( Prefix ),
     is_binary( Suffix ),
     is_binary( Add ) ->
  RepoUrl0 = string:join( [binary_to_list( Prefix ),
                           binary_to_list( Suffix )], "/" ),
  RepoUrl0++binary_to_list( Add ).


%% Printing ----------------------------------------------------------

-spec print_info( K, V ) -> ok
when K :: string(),
     V :: string().

print_info( K, V ) ->
  io:format( "~-8.s~s~n", [K++":", V] ).

-spec print_reply( RepoName, Action, InfoMap, Reply ) -> ok
when RepoName :: string(),
     Action   :: string(),
     InfoMap  :: #{ string() => string() },
     Reply    :: string().

print_reply( RepoName, Action, InfoMap, Reply )
when is_list( RepoName ),
     is_list( Reply ) ->
  io:format( "=====================================~n" ),
  io:format( "repo:   ~s~n", [RepoName] ),
  io:format( "action: ~s~n", [Action] ),
  lists:foreach( fun( {K, V} ) -> print_info( K, V ) end,
                 maps:to_list( InfoMap ) ),
  io:put_chars( Reply ).
