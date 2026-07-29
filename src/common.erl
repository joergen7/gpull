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

-module (common).

-export ([get_repo_name/2,
          get_repo_url/3,
          print_reply/4]).

%%============================================================
%% Exported Functions
%%============================================================


%% Helper functions ------------------------------------------

-spec get_repo_name(Prefix, Suffix) -> string()
              when Prefix :: binary(),
                   Suffix :: binary().

get_repo_name(Prefix, Suffix)
  when is_binary(Prefix),
       is_binary(Suffix) ->
    RepoUrl = string:join([binary_to_list(Prefix),
                           binary_to_list(Suffix)],
                          "/"),
    NoTrunk = re:replace(RepoUrl, "/trunk", "", [global]),
    TrimmedEnd = string:trim(NoTrunk, trailing, "/"),
    Found = string:find(TrimmedEnd, "/", trailing),
    TrimmedFront = string:trim(Found, leading, "/"),
    case is_binary(TrimmedFront) of
        true -> binary_to_list(TrimmedFront);
        false -> TrimmedFront
    end.


-spec get_repo_url(Prefix, Suffix, Add) -> string()
              when Prefix :: binary(),
                   Suffix :: binary(),
                   Add    :: binary().

get_repo_url(Prefix, Suffix, Add)
  when is_binary(Prefix),
       is_binary(Suffix),
       is_binary(Add) ->
    RepoUrl0 = string:join([binary_to_list(Prefix),
                            binary_to_list(Suffix)],
                           "/"),
    RepoUrl0 ++ binary_to_list(Add).


-spec print_reply(RepoName, Action, InfoMap, Reply) -> ok
              when RepoName :: string(),
                   Action   :: string(),
                   InfoMap  :: #{string() => string()},
                   Reply    :: string().


%% Printing --------------------------------------------------

print_reply(RepoName, Action, InfoMap, Reply)
  when is_list(RepoName),
       is_list(Reply) ->
    io:format("=====================================~n"),
    io:format("repo:   ~s~n", [RepoName]),
    io:format("action: ~s~n", [Action]),
    lists:foreach(fun({K, V}) -> print_info(K, V) end,
                  maps:to_list(InfoMap)),
    io:put_chars(Reply).

%%============================================================
%% Internal Functions
%%============================================================

-spec print_info(K, V) -> ok
              when K :: string(),
                   V :: string().

print_info(K, V) ->
    io:format("~-8.s~s~n", [K ++ ":", V]).

