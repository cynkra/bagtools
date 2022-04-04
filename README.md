
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bagtools

This repo contains experimental functions to be used to navigate a code
base and refactor code.

These will stay as is here but might be updated, renamed, built upon in
other packages, like {refactor} or {flow} or {boomer}.

These are general functions, but designed to be fit to the issues BAG
encounters in their code base.

## Installation

Install with:

``` r
remotes::install_github("cynkra/bagtools")
```

## Features

We can apply the following to the whole project or a selection of files
or folders:

-   Make sure all “.R” files are valid syntactic R scripts :
    `check_files_parse()`

-   Fetch all namespaced calls in project (calls using `pkg::fun`
    syntax) and create open editor tab with script to check if all were
    installed : `use_namespace_check()`

-   Identify scripts that contain both functions and other object
    definitions : `identify_hybrid_scripts()`

-   Find all uses of a package’s functions (watch out for fals
    positives!): `find_package_funs()`

-   Detect similar code to spot places where a generalized function
    would be beneficial : `detect_similar_code()`

-   Open script to apply different linters for more robust code and
    better style : `use_lintr_template()`

-   Draw a diagram of the dependencies between sourced files, to get a
    better understanding of the flow and understand which scripts should
    be converted to functions or packages first :
    `flow_view_source_calls()`

-   Inspect variables used in a script to infer inputs and outputs
    `inspect_variables()`

-   Make a variable “chatty” so it talks when it is accessed or modified
    to say from which script or function its called, and its value there
    : `chatty()`

-   Detect all calls to `summarize()` for which the `.groups` argument
    was not set `detect_ambiguous_summarize_calls()`

## More functions from other packages

-   Refactor a function, keeping both original and updated code, to
    control they do the same thing.

``` r
# remotes::install_github("moodymudskipper/refactor")
library(refactor)
fun <- function (x) {
  # original code
} %refactor_value% {
  # updated code
}
```

-   View the variable dependencies in code to discover potential dead
    code or clusters of variable interactions that might be refactored
    into other functions: `flow::flow_view_vars()`
