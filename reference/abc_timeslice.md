# Apply time-sliced ABC model for validation

This function implements a time-sliced ABC model for validation. It uses
historical data to predict connections that will appear in the future.

## Usage

``` r
abc_timeslice(
  entity_data,
  time_column = "publication_year",
  split_time,
  a_term,
  a_type = NULL,
  c_type = NULL,
  min_score = 0.1,
  n_results = 100
)
```

## Arguments

- entity_data:

  A data frame of entity data with time information.

- time_column:

  Name of the column containing time information.

- split_time:

  Time point to split historical and future data.

- a_term:

  Character string, the source term (A).

- a_type:

  Character string, the entity type for A terms.

- c_type:

  Character string, the entity type for C terms.

- min_score:

  Minimum score threshold for results.

- n_results:

  Maximum number of results to return.

## Value

A list with prediction results and validation metrics.

## Examples

``` r
if (FALSE) { # \dontrun{
validation <- abc_timeslice(entity_data,
                            time_column = "publication_year",
                            split_time = 2010,
                            a_term = "migraine")
} # }
```
