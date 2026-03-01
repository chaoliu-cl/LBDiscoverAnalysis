# Enhance ABC results with external knowledge

This function enhances ABC results with information from external
knowledge bases.

## Usage

``` r
enhance_abc_kb(abc_results, knowledge_base = c("umls", "mesh"), api_key = NULL)
```

## Arguments

- abc_results:

  A data frame containing ABC results.

- knowledge_base:

  Character string, the knowledge base to use ("umls" or "mesh").

- api_key:

  Character string. API key for the knowledge base (if needed).

## Value

A data frame with enhanced ABC results.
