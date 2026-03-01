# Create co-occurrence matrix without explicit entity type constraints

This function creates a co-occurrence matrix from entity data while
preserving entity type information as an attribute without enforcing
type constraints.

## Usage

``` r
create_comat(
  entity_data,
  doc_id_col = "doc_id",
  entity_col = "entity",
  count_col = NULL,
  type_col = "entity_type",
  normalize = TRUE,
  normalization_method = c("cosine", "jaccard", "dice")
)
```

## Arguments

- entity_data:

  A data frame with document IDs and entities.

- doc_id_col:

  Name of the column containing document IDs.

- entity_col:

  Name of the column containing entity names.

- count_col:

  Name of the column containing entity counts (optional).

- type_col:

  Name of the column containing entity types (optional).

- normalize:

  Logical. If TRUE, normalizes the co-occurrence matrix.

- normalization_method:

  Method for normalization ("cosine", "jaccard", or "dice").

## Value

A co-occurrence matrix with entity types stored as an attribute.

## Examples

``` r
if (FALSE) { # \dontrun{
co_matrix <- create_comat(entities,
                          doc_id_col = "doc_id",
                          entity_col = "entity",
                          type_col = "entity_type")
} # }
```
