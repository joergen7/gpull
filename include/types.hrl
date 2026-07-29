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

-type operation() :: pull
                   | status.

-type protocol()  :: git
                   | svn.

-type repo_obj() :: #{protocol  => binary(),
                      url       => binary(),
                      repo_list => [binary()]}.

