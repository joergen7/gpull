-module (op_status).

-export ([op_status/2]).

-include ("types.hrl").

-import (common, [get_repo_name/2, print_reply/4]).

-define(CLEAN_REPLY1,  "nothing to commit, working tree clean").
-define(CLEAN_REPLY2A, "Your branch is up to date with").
-define(CLEAN_REPLY2B, "Your branch is up-to-date with").

%%============================================================
%% Exported Functions
%%============================================================

-spec op_status (Protocol, BaseUrl) -> fun ((Repo :: binary()) -> ok)
              when Protocol :: protocol(),
                   BaseUrl  :: binary().

op_status (git, BaseUrl)
  when is_binary (BaseUrl) ->
    fun (Repo)
          when is_binary (Repo) ->
            git_status (BaseUrl, Repo)
    end;

op_status (svn, BaseUrl)
  when is_binary (BaseUrl) ->
    fun (Repo)
          when is_binary (Repo) ->
            svn_status (BaseUrl, Repo)
    end.

%%============================================================
%% Internal Functions
%%============================================================

-spec git_status(Prefix, Suffix) -> ok
              when Prefix :: binary(),
                   Suffix :: binary().

git_status(Prefix, Suffix)
  when is_binary(Prefix),
       is_binary(Suffix) ->

    RepoName = get_repo_name(Prefix, Suffix),

    Cmd = "(cd " ++ RepoName ++ " && git status)",
    Reply = os:cmd(Cmd),
    Action = "git status",

    case string:find(Reply, ?CLEAN_REPLY1) of

        nomatch -> print_reply(RepoName, Action, #{}, Reply);
        _ ->
            case string:find(Reply, ?CLEAN_REPLY2A) of
                nomatch ->
                    case string:find(Reply, ?CLEAN_REPLY2B) of
                        nomatch -> print_reply(RepoName, Action, #{}, Reply);
                        _ -> ok
                    end;
                _ -> ok
            end
    end.


-spec svn_status(Prefix, Suffix) -> ok
              when Prefix :: binary(),
                   Suffix :: binary().

svn_status(Prefix, Suffix) ->

    RepoName = get_repo_name(Prefix, Suffix),

    Cmd = "(cd " ++ RepoName ++ " && svn status)",
    Reply = os:cmd(Cmd),
    Action = "svn status",

    case string:is_empty(Reply) of
        true -> ok;
        false -> print_reply(RepoName, Action, #{}, Reply)
    end.


