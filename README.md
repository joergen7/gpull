# gpull
###### git repository management tool

## Usage

### Pulling Changes

This command line tool updates (or clones if necessary) a possibly large number of git repositories from different sources by entering on the command line:

    gpull

This results in an output that looks like this:

    =====================================
    repo:   variant_call_rna
    URL:    https://github.com/joergen7/variant_call_rna.git
    action: git pull
    Already up-to-date.
    =====================================
    repo:   wordcount
    URL:    https://github.com/joergen7/wordcount.git
    action: git pull
    Already up-to-date.

### The Repository Information File

The repository information is drawn from a file `repo_info.json` which is expected in the current working directory.

The `repo_info.json` file contains a list of JSON objects that each contain three keys: `protocol`, `url`, and `repo_list`. The protocol can be either `git` or `svn`. The base URL and repository list identify the repositories to be managed. Herein, the base URL and repo entry are joined with a `/` separator. The following is a valid `repo_info.json` file:

    [
      { "protocol"  : "git",
        "url"       : "git@github.com:joergen7",
        "repo_list" : ["bismark",
                       "bsmooth-align",
                       "gpull"] },
      { "protocol"  : "git",
        "url"       : "https://github.com",
        "repo_list" : ["sile/jsone",
                       "jcomellas/getopt"] },
      { "protocol"  : "svn",
        "url"       : "https://svn.win.tue.nl/repos",
        "repo_list" : ["cpntools/GUI/trunk"] }
    ]

The base URL string is prepended to each repository name to build the repository URL.

### Surveying Repository Statuses

In addition it is possible to detect which local repositories have changed by entering on the command line:

    gpull status

This results in an output that looks like this:

    =====================================
    repo:   gpull
    On branch master
    Your branch is up-to-date with 'origin/master'.
    Changes not staged for commit:
      (use "git add <file>..." to update what will be committed)
      (use "git checkout -- <file>..." to discard changes in working directory)

        modified:   README.md

    no changes added to commit (use "git add" and/or "git commit -a")

A notification is generated only for repositories that contain uncommitted changes.

## Building with Rebar3

Build the command line tool by entering

    rebar3 escriptize

Install the resulting script by copying it to a location in the executable path, e.g., by entering

    sudo cp _build/default/bin/gpull /usr/local/bin

## System Requirements

- [Erlang](http://www.erlang.org/) OTP 19.0 or higher
- [Rebar3](https://www.rebar3.org/) 3.0.0 or higher

## Authors

- Jörgen Brandt ([@joergen7](https://github.com/joergen7/)) [joergen@cuneiform-lang.org](mailto:joergen@cuneiform-lang.org)

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)
