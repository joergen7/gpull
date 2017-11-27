%% -*- erlang -*-
%%
%% A git repository management tool.
%%
%% Copyright 2017 Jörgen Brandt
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
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.0
%% @copyright 2017 Jörgen Brandt
%%
%% @end
%% -------------------------------------------------------------------

-module( gpull ).

-export( [main/1] ).

-define( REPOINFO, "repo_info.json" ).
-define( DEFAULTREPLY, "nothing to commit, working tree clean\n" ).

%%====================================================================
%% Escript main function
%%====================================================================

main( ArgLst ) ->

  FProcess =
    case ArgLst of
      []         -> fun git_pull/2;
      ["status"] -> fun git_status/2
    end,

  RepoInfo = load_repo_info( ?REPOINFO ),

  F =
    fun( {BaseUrl, RepoLst} ) ->
      process_repo_lst( FProcess, BaseUrl, RepoLst )
    end,

  lists:foreach( F, maps:to_list( RepoInfo ) ).



%%====================================================================
%% Internal functions
%%====================================================================

process_repo_lst( FProcess, BaseUrl, RepoLst )
when is_function( FProcess, 2 ),
     is_binary( BaseUrl ),
     is_list( RepoLst ) ->

  F = fun( RepoName ) ->
        FProcess( BaseUrl, RepoName )
      end,

  lists:foreach( F, RepoLst ).


load_repo_info( InfoFile ) when is_list( InfoFile ) ->

  case file:read_file( InfoFile ) of

    {error, Reason} ->
      error( Reason );

    {ok, B} ->
      jsone:decode( B )

  end.


git_pull( BaseUrl, RepoName )
when is_binary( BaseUrl ),
     is_binary( RepoName ) ->

  io:format( "=====================================~n" ),
  io:format( "repo:   ~s~n", [RepoName] ),

  RepoUrl = <<BaseUrl/binary, "/", RepoName/binary, ".git">>,

  io:format( "URL:    ~s~n", [RepoUrl] ),

  Cmd =
    case filelib:is_dir( RepoName ) of

      true ->
        io:format( "action: git pull~n" ),
        <<"(cd ", RepoName/binary, "; git pull)">>;

      false ->
        io:format( "action: git clone~n" ),
        <<"git clone ", RepoUrl/binary>>

    end,

  Reply = os:cmd( binary_to_list( Cmd ) ),
  io:format( Reply ).

git_status( BaseUrl, RepoName )
when is_binary( BaseUrl ),
     is_binary( RepoName ) ->


  Cmd = <<"(cd ", RepoName/binary, "; git status)">>,
  Reply = os:cmd( binary_to_list( Cmd ) ),
  
  case string:find( Reply, ?DEFAULTREPLY ) of

    nomatch ->
      io:format( "=====================================~n" ),
      io:format( "repo:   ~s~n", [RepoName] ),
      io:format( Reply );

    _ ->
      ok

  end.

