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

-module(gpull).

-export([main/1]).

-include ("types.hrl").

-import (op_pull, [op_pull/2]).
-import (op_status, [op_status/2]).
-import (op_log, [op_log/2]).

-define(REPOINFO,      "repo_info.json").

%%====================================================================
%% Escript main function
%%====================================================================


-spec main(ArgLst :: [string()]) -> ok.

main(ArgLst)
  when is_list(ArgLst) ->

    RepoInfo = load_repo_info(?REPOINFO),

    Op =
        case ArgLst of
            ["pull"]   -> pull;
            ["status"] -> status;
            ["log"]    -> log;
            []         -> pull
        end,

    lists:foreach(fun(Repo) -> process_repo(Op, Repo) end, RepoInfo).


%%====================================================================
%% Internal functions
%%====================================================================

%% Input validation --------------------------------------------------


-spec validate_json_string(B :: binary()) -> binary().

validate_json_string(B)
  when is_binary(B) ->
    B;

validate_json_string(B) ->
    error({bad_json_string, B}).


-spec validate_json_string_list(BLst :: _) -> [binary()].

validate_json_string_list(BLst)
  when is_list(BLst) ->
    lists:foreach(fun validate_json_string/1, BLst),
    BLst;

validate_json_string_list(BLst) ->
    error({bad_json_string_list, BLst}).


-spec validate_json_repo_obj(M :: _) -> repo_obj().

validate_json_repo_obj(M = #{protocol := P, url := U, repo_list := RLst}) ->

    case P of
        <<"git">> -> ok;
        <<"svn">> -> ok;
        _ -> error({bad_protocol, P})
    end,

    _ = validate_json_string(U),
    _ = validate_json_string_list(RLst),

    M;

validate_json_repo_obj(M) ->
    error({bad_json_repo_object, M}).


-spec validate_json_repo_obj_list(L :: _) -> [repo_obj()].

validate_json_repo_obj_list(L)
  when is_list(L) ->
    lists:foreach(fun validate_json_repo_obj/1, L),
    L;

validate_json_repo_obj_list(L) ->
    error({bad_json_repo_obj_list, L}).


%% File I/O ----------------------------------------------------------


-spec load_repo_info(InfoFile :: string()) -> [repo_obj()].

load_repo_info(InfoFile) when is_list(InfoFile) ->

    case file:read_file(InfoFile) of

        {error, Reason} ->
            error(Reason);

        {ok, B} ->
            validate_json_repo_obj_list(jsone:decode(B, [{keys, atom}]))

    end.


%% Generic repo processor --------------------------------------------


-spec process_repo(Op, M) -> ok
              when Op :: pull | status,
                   M  :: repo_obj().

process_repo(Op, #{protocol := P, url := BaseUrl, repo_list := RepoLst}) ->
    F = proc_repo(Op, binary_to_atom(P, utf8), BaseUrl),
    lists:foreach(F, RepoLst).


-spec proc_repo(Op, Protocol, BaseUrl) -> fun((Repo :: binary()) -> ok)
              when Op       :: operation(),
                   Protocol :: protocol(),
                   BaseUrl  :: binary().

proc_repo(pull, Protocol, BaseUrl) ->
    op_pull (Protocol, BaseUrl);

proc_repo(status, Protocol, BaseUrl) ->
    op_status (Protocol, BaseUrl);

proc_repo(log, Protocol, BaseUrl) ->
    op_log (Protocol, BaseUrl).







