-module (op_log).

-export ([op_log/2]).

-include ("types.hrl").

%%============================================================
%% Exported Functions
%%============================================================

-spec op_log (Protocol, BaseUrl) -> fun ((Repo :: binary()) -> ok)
                   when Protocol :: protocol(),
                        BaseUrl  :: binary().

op_log (git, BaseUrl)
  when is_binary (BaseUrl) ->
    fun (Repo)
          when is_binary (Repo) ->
            git_log (BaseUrl, Repo)
    end;

op_log (svn, _BaseUrl) ->
    error (nyi).

%%============================================================
%% Internal Functions
%%============================================================

-spec git_log (BaseUrl, Repo) -> ok
              when BaseUrl :: binary(),
                   Repo    :: binary().

git_log (BaseUrl, Repo)
  when is_binary (BaseUrl),
       is_binary (Repo) ->
    %% TODO
    ok.
