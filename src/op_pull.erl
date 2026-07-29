%% -*- erlang -*-
%%
%% gpull: repository management tool.
%%
%% Copyright 2017-2026 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @version 0.1.4
%% @copyright 2017-2026
%%
%% @end
%% -------------------------------------------------------------------

-module (op_pull).

-export ([op_pull/2]).

-include ("types.hrl").

-import (common, [get_repo_name/2, get_repo_url/3, print_reply/4]).

%%============================================================
%% Exported Functions
%%============================================================

-spec op_pull (Protocol, BaseUrl) -> fun ((Repo :: binary()) -> ok)
              when Protocol :: protocol(),
                   BaseUrl  :: binary().

op_pull (git, BaseUrl)
  when is_binary (BaseUrl) ->
    fun (Repo)
          when is_binary (Repo) ->
            git_pull (BaseUrl, Repo)
    end;

op_pull (svn, BaseUrl)
  when is_binary (BaseUrl) ->
    fun (Repo)
          when is_binary (Repo) ->
            svn_up (BaseUrl, Repo)
    end.


%%============================================================
%% Internal Functions
%%============================================================

-spec git_pull(Prefix, Suffix) -> ok
              when Prefix :: binary(),
                   Suffix :: binary().

git_pull(Prefix, Suffix)
  when is_binary(Prefix),
       is_binary(Suffix) ->

    RepoName = get_repo_name(Prefix, Suffix),
    RepoUrl = get_repo_url(Prefix, Suffix, <<".git">>),

    {Action, Cmd} =
        case filelib:is_dir(RepoName) of

            true ->
                {"git pull", "(cd " ++ RepoName ++ " && git pull)"};

            false ->
                {"git clone", "git clone " ++ RepoUrl ++ " && (cd " ++ RepoName ++ " && git config pull.rebase false)"}
        end,

    InfoMap = #{"URL" => RepoUrl},

    Reply = os:cmd(Cmd),

    print_reply(RepoName, Action, InfoMap, Reply).


-spec svn_up(Prefix, Suffix) -> ok
              when Prefix :: binary(),
                   Suffix :: binary().

svn_up(Prefix, Suffix)
  when is_binary (Prefix),
       is_binary (Suffix) ->

    RepoName = get_repo_name(Prefix, Suffix),
    RepoUrl = get_repo_url(Prefix, Suffix, <<>>),

    {Action, Cmd} =
        case filelib:is_dir(RepoName) of

            true ->
                {"svn up", "(cd " ++ RepoName ++ " && svn up)"};

            false ->
                {"svn co", "svn co -q " ++ RepoUrl ++ " " ++ RepoName}
        end,

    InfoMap = #{"URL" => RepoUrl},

    Reply = os:cmd(Cmd),

    print_reply(RepoName, Action, InfoMap, Reply).


