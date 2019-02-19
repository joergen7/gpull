%% -*- erlang -*-
%%
%% A git repository management tool.
%%
%% Copyright 2017-2019 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @version 0.1.2
%% @copyright 2017-2019
%%
%% @end
%% -------------------------------------------------------------------

-module( gpull ).

-export( [main/1] ).

-define( REPOINFO, "repo_info.json" ).
-define( CLEAN_REPLY1, "nothing to commit, working tree clean" ).
-define( CLEAN_REPLY2, "Your branch is up to date with" ).

%%====================================================================
%% Escript main function
%%====================================================================

-spec main( ArgLst :: [string()] ) -> ok.

main( ArgLst )
when is_list( ArgLst ) ->

  RepoInfo = load_repo_info( ?REPOINFO ),

  Op =
    case ArgLst of
      []         -> fun git_pull/2;
      ["status"] -> fun git_status/2
    end,

  F =
    fun( {BaseUrl, RepoLst} ) ->
      process_repo_lst( Op, BaseUrl, RepoLst )
    end,

  lists:foreach( F, maps:to_list( RepoInfo ) ).


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


-spec validate_json_map( M :: _ ) ->
  #{ binary() => [binary()] }.

validate_json_map( M )
when is_map( M ) ->
  lists:foreach( fun validate_json_string/1,
                 maps:keys( M ) ),
  lists:foreach( fun validate_json_string_list/1,
                 maps:values( M ) ),
  M;

validate_json_map( M ) ->
  error( {bad_json_map, M} ).


%% File I/O ----------------------------------------------------------

-spec load_repo_info( InfoFile :: string() ) ->
  #{ binary() => [binary()] }.

load_repo_info( InfoFile ) when is_list( InfoFile ) ->

  case file:read_file( InfoFile ) of

    {error, Reason} ->
      error( Reason );

    {ok, B} ->
      validate_json_map( jsone:decode( B ) )

  end.


%% Generic repo processor --------------------------------------------

-spec process_repo_lst( Op, BaseUrl, RepoLst ) -> ok
when Op      :: fun( ( Prefix :: binary(), Suffix :: binary() ) -> ok ),
     BaseUrl :: binary(),
     RepoLst :: [binary()].

process_repo_lst( Op, BaseUrl, RepoLst )
when is_function( Op, 2 ),
     is_binary( BaseUrl ),
     is_list( RepoLst ) ->

  F = fun( RepoName ) ->
        Op( BaseUrl, RepoName )
      end,

  lists:foreach( F, RepoLst ).


%% Repo operations ---------------------------------------------------

git_pull( Prefix, Suffix )
when is_binary( Prefix ),
     is_binary( Suffix ) ->

  RepoName = get_repo_name( Prefix, Suffix ),
  RepoUrl = get_repo_url( Prefix, Suffix ),

  {Action, Cmd} =
    case filelib:is_dir( RepoName ) of

      true ->
        {"git pull", "(cd "++RepoName++"; git pull)"};

      false ->
        {"git clone", "git clone "++RepoUrl}
    end,

  InfoMap = #{ "URL" => RepoUrl },

  Reply = os:cmd( Cmd ),

  print_reply( RepoName, Action, InfoMap, Reply ).

git_status( Prefix, Suffix )
when is_binary( Prefix ),
     is_binary( Suffix ) ->

  RepoName = get_repo_name( Prefix, Suffix ),

  Cmd = "(cd "++RepoName++"; git status)",
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

%% Helper functions --------------------------------------------------

-spec get_repo_name( Prefix, Suffix ) -> string()
when Prefix :: binary(),
     Suffix :: binary().

get_repo_name( Prefix, Suffix )
when is_binary( Prefix ),
     is_binary( Suffix ) ->
  RepoUrl = string:join( [binary_to_list( Prefix ),
                          binary_to_list( Suffix )], "/" ),
  [$/|RepoName] = string:find( RepoUrl, "/", trailing ),
  RepoName.


-spec get_repo_url( Prefix, Suffix ) -> string()
when Prefix :: binary(),
     Suffix :: binary().

get_repo_url( Prefix, Suffix )
when is_binary( Prefix ),
     is_binary( Suffix ) ->
  RepoUrl0 = string:join( [binary_to_list( Prefix ),
                           binary_to_list( Suffix )], "/" ),
  RepoUrl0++".git".


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
  io:format( Reply ).