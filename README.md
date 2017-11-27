# gpull
###### A git repository management tool.

[![hex.pm](https://img.shields.io/hexpm/v/gpull.svg?style=flat-square)](https://hex.pm/packages/gpull) [![Build Status](https://travis-ci.org/joergen7/gpull.svg?branch=master)](https://travis-ci.org/joergen7/gpull)

This command line tool updates (or clones if necessary) a possibly large number of git repositories from different sources by entering

    gpull

on the command line. The repository information is drawn from a file `repo_info.json` which is expected in the current working directory.

The `repo_info.json` file contains a JSON object associating a base URL string with a list of repository name strings. E.g., the following JSON object is a valid repo info object:

    {"https://github.com/joergen7" :
       ["bismark", "bsmooth-align", "cf-java", "cf-model", "cf_client",
        "cf_lang", "cf_reference", "cf_worker", "cfif", "cfui",
        "chef-bioinf-worker", "chef-cuneiform", "chef-misc", "chip-seq",
        "consensus-prediction", "cre", "cuneiform", "cuneiform-doc",
        "cuneiform-legacy", "effi", "gccount", "gen_pnet", "gen_pnet_examples",
        "gen_workflow", "gpull", "gruff", "gruff_example", "kmeans",
        "lib_combin", "lib_conf", "lib_dp", "metagenomics", "methylation",
        "mirdeep", "mirna-discovery", "phylogeny", "pn", "rna-seq",
        "variant-call", "variant-call-gatk", "variant_call_rna", "wordcount"
       ]}

The base URL string is prepended to each repository name to build the repository URL.

In addition it is possible to detect which local repositories have changed by entering

    gpull status

on the command line. A notification is generated only for repositories that contain uncommitted changes.

## System Requirements

- Erlang OTP 18.0 or higher
- Rebar3 3.0.0 or higher

## Authors

- Jörgen Brandt ([@joergen7](https://github.com/joergen7/)) [joergen.brandt@onlinehome.de](mailto:joergen.brandt@onlinehome.de)

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)