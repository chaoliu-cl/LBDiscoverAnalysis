# LBDiscoverAnalysis

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/chaoliu-cl/LBDiscoverAnalysis/graph/badge.svg)](https://app.codecov.io/gh/chaoliu-cl/LBDiscoverAnalysis)
[![R-CMD-check](https://github.com/chaoliu-cl/LBDiscoverAnalysis/actions/workflows/r.yml/badge.svg)](https://github.com/chaoliu-cl/LBDiscoverAnalysis/actions/workflows/r.yml)
<!-- badges: end -->

`LBDiscoverAnalysis` contains the analysis-facing components of the original
`LBDiscover` workflow.

## Scope

This package focuses on:

1. Co-occurrence analysis
2. Discovery models (ABC, AnC, LSI, BITOLA-style)
3. Validation and evidence checking
4. Visualization and reporting

`LBDiscoverAnalysis` depends on `LBDiscover` for article retrieval,
preprocessing, dictionary lookup, and entity extraction.
