# Filter entities to include only valid biomedical terms

This function applies validation to ensure only legitimate biomedical
entities are included, while preserving trusted terms.

## Usage

``` r
valid_entities(
  entities,
  primary_term,
  primary_term_variations = NULL,
  validation_function = NULL,
  verbose = TRUE,
  entity_col = "entity",
  type_col = "entity_type"
)
```

## Arguments

- entities:

  Data frame of entities to filter

- primary_term:

  The primary term to trust

- primary_term_variations:

  Vector of variations of the primary term to trust

- validation_function:

  Function to validate entities (default: is_valid_biomedical_entity)

- verbose:

  Logical; if TRUE, print status messages (default: TRUE)

- entity_col:

  Name of the column containing entity names (default: "entity")

- type_col:

  Name of the column containing entity types (default: "entity_type")

## Value

A data frame of filtered entities

## Examples

``` r
# Create example entities
entities <- data.frame(
  entity = c("migraine", "optimization", "receptor", "europe"),
  entity_type = c("disease", "process", "protein", "location")
)

# Validate entities
validated <- valid_entities(entities, "migraine", c("migrain", "headache"))
#> Error in valid_entities(entities, "migraine", c("migrain", "headache")): could not find function "valid_entities"
print(validated)
#> Error: object 'validated' not found
```
