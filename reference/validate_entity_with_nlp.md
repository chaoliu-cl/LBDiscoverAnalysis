# Validate entity types using NLP-based entity recognition with improved accuracy

Validate entity types using NLP-based entity recognition with improved
accuracy

## Usage

``` r
validate_entity_with_nlp(term, claimed_type, nlp_model = NULL)
```

## Arguments

- term:

  Character string, the term to validate

- claimed_type:

  Character string, the claimed entity type

- nlp_model:

  The loaded NLP model to use for validation

## Value

Logical indicating if the term is likely of the claimed type
