# Combine and deduplicate entity datasets

This function combines custom and standard entity datasets, handling the
case where one or both might be empty, and removes duplicates.

## Usage

``` r
merge_entities(
  custom_entities,
  standard_entities,
  primary_term,
  primary_type = "disease",
  verbose = TRUE
)
```

## Arguments

- custom_entities:

  Data frame of custom entities (can be NULL)

- standard_entities:

  Data frame of standard entities (can be NULL)

- primary_term:

  The primary term of interest

- primary_type:

  The entity type of the primary term (default: "disease")

- verbose:

  Logical; if TRUE, print status messages (default: TRUE)

## Value

A data frame of combined entities

## Examples

``` r
# Create example entity datasets
custom_entities <- data.frame(
  doc_id = c(1, 1, 2),
  entity = c("migraine", "headache", "pain"),
  entity_type = c("disease", "symptom", "symptom"),
  start_pos = c(1, 10, 5),
  end_pos = c(8, 18, 9),
  sentence = c("sent1", "sent1", "sent2"),
  frequency = c(2, 1, 1)
)

standard_entities <- data.frame(
  doc_id = c(1, 2, 2),
  entity = c("serotonin", "migraine", "therapy"),
  entity_type = c("chemical", "disease", "treatment"),
  start_pos = c(20, 1, 15),
  end_pos = c(29, 8, 22),
  sentence = c("sent1", "sent2", "sent2"),
  frequency = c(1, 1, 1)
)

# Merge entities
merged <- merge_entities(custom_entities, standard_entities, "migraine")
#> Combined 3 custom entities with 3 standard entities.
print(merged)
#>   doc_id    entity entity_type start_pos end_pos sentence frequency
#> 1      1  migraine     disease         1       8    sent1         2
#> 2      1  headache     symptom        10      18    sent1         1
#> 3      2      pain     symptom         5       9    sent2         1
#> 4      1 serotonin    chemical        20      29    sent1         1
#> 5      2  migraine     disease         1       8    sent2         1
#> 6      2   therapy   treatment        15      22    sent2         1
```
