# Comprehensive entity validation using multiple techniques

Comprehensive entity validation using multiple techniques

## Usage

``` r
validate_entity_comprehensive(
  term,
  claimed_type,
  use_nlp = TRUE,
  use_pattern = TRUE,
  use_external_api = FALSE
)
```

## Arguments

- term:

  Character string, the term to validate

- claimed_type:

  Character string, the claimed entity type

- use_nlp:

  Logical, whether to use NLP-based validation

- use_pattern:

  Logical, whether to use pattern-based validation

- use_external_api:

  Logical, whether to query external APIs

## Value

Logical indicating if the term is validated
