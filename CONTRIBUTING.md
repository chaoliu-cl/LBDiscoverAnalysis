# Contributing to LBDiscoverAnalysis

Thank you for contributing.

## Getting Started

1.  Fork the repository and create a feature branch.
2.  Install dependencies, including suggested packages for tests and
    docs.
3.  Run checks before opening a pull request.

## Development Workflow

1.  Keep changes focused and small.
2.  Add or update tests for behavior changes.
3.  Update documentation (`R/` roxygen comments and generated
    `man/*.Rd`) when public behavior changes.
4.  Run:

``` r
devtools::document()
devtools::test()
devtools::check()
```

## Pull Request Expectations

1.  Describe the problem and the approach.
2.  Link related issues.
3.  Include reproducible examples for bug fixes when possible.
4.  Ensure CI passes before requesting review.

## Code Style

1.  Follow tidyverse style for R code.
2.  Prefer explicit, informative error messages.
3.  Keep functions pure where possible and avoid unnecessary side
    effects.
