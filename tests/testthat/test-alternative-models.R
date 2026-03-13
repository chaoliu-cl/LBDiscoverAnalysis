sample_co_matrix <- function() {
  terms <- c("migraine", "serotonin", "cgrp", "sumatriptan", "inflammation")
  mat <- matrix(
    c(
      0.00, 0.90, 0.80, 0.70, 0.60,
      0.90, 0.00, 0.85, 0.65, 0.55,
      0.80, 0.85, 0.00, 0.75, 0.50,
      0.70, 0.65, 0.75, 0.00, 0.40,
      0.60, 0.55, 0.50, 0.40, 0.00
    ),
    nrow = 5,
    byrow = TRUE,
    dimnames = list(terms, terms)
  )
  attr(mat, "entity_types") <- c(
    migraine = "disease",
    serotonin = "chemical",
    cgrp = "protein",
    sumatriptan = "drug",
    inflammation = "biological_process"
  )
  mat
}

test_that("anc_model returns ranked results and handles no-result cases", {
  co_matrix <- sample_co_matrix()

  res <- anc_model(
    co_matrix = co_matrix,
    a_term = "migraine",
    n_b_terms = 3,
    min_score = 0.3,
    n_results = 10,
    validation_function = function(term, claimed_type) TRUE
  )

  expect_s3_class(res, "data.frame")
  expect_true(all(c("a_term", "b_terms", "c_term", "anc_score") %in% names(res)))
  expect_gt(nrow(res), 0)
  expect_equal(res$a_term[[1]], "migraine")

  empty_res <- anc_model(
    co_matrix = co_matrix,
    a_term = "migraine",
    n_b_terms = 3,
    min_score = 0.99,
    validation_function = function(term, claimed_type) TRUE
  )

  expect_s3_class(empty_res, "data.frame")
  expect_equal(nrow(empty_res), 0)
})

test_that("bitola_model validates types and returns aggregated A-C paths", {
  co_matrix <- sample_co_matrix()

  expect_error(
    bitola_model(co_matrix, a_term = "migraine"),
    "Both A and C semantic types must be provided"
  )

  expect_error(
    bitola_model(
      co_matrix,
      a_term = "migraine",
      a_semantic_type = "protein",
      c_semantic_type = "drug"
    ),
    "is not of semantic type"
  )

  res <- bitola_model(
    co_matrix = co_matrix,
    a_term = "migraine",
    a_semantic_type = "disease",
    c_semantic_type = "drug",
    min_score = 0.2,
    n_results = 5
  )

  expect_s3_class(res, "data.frame")
  expect_true(all(c("a_term", "c_term", "b_terms", "ranking_score") %in% names(res)))
  expect_gt(nrow(res), 0)
})

test_that("lsi_model supports filtered and unfiltered runs", {
  skip_if_not_installed("irlba")

  tdm <- matrix(
    c(
      5, 2, 0, 1,
      3, 1, 2, 1,
      1, 4, 3, 0,
      0, 1, 5, 4,
      2, 0, 1, 3
    ),
    nrow = 5,
    byrow = TRUE
  )
  rownames(tdm) <- c("migraine", "serotonin", "cgrp", "sumatriptan", "trpv1")

  entity_types <- c(
    migraine = "disease",
    serotonin = "chemical",
    cgrp = "protein",
    sumatriptan = "drug",
    trpv1 = "gene"
  )

  expect_error(lsi_model(tdm, a_term = "unknown"), "not found")

  res_unfiltered <- suppressWarnings(
    lsi_model(
      term_doc_matrix = tdm,
      a_term = "migraine",
      n_factors = 2,
      n_results = 3,
      enforce_biomedical_terms = FALSE,
      entity_types = entity_types
    )
  )

  expect_s3_class(res_unfiltered, "data.frame")
  expect_true(all(c("a_term", "c_term", "lsi_similarity") %in% names(res_unfiltered)))
  expect_gt(nrow(res_unfiltered), 0)

  res_filtered <- suppressWarnings(
    lsi_model(
      term_doc_matrix = tdm,
      a_term = "migraine",
      n_factors = 2,
      n_results = 4,
      enforce_biomedical_terms = TRUE,
      c_term_types = c("protein", "drug"),
      entity_types = entity_types,
      validation_function = function(term, claimed_type) TRUE,
      use_nlp = FALSE,
      nlp_threshold = 0.5
    )
  )

  expect_s3_class(res_filtered, "data.frame")
  expect_true(all(c("c_term", "c_type") %in% names(res_filtered)))
  expect_true(all(res_filtered$c_type %in% c("protein", "drug")))
})

test_that("create_tdm creates matrices and validates inputs", {
  preprocessed <- data.frame(
    doc_id = c("d1", "d2", "d3"),
    stringsAsFactors = FALSE
  )
  preprocessed$terms <- list(
    data.frame(word = c("migraine", "serotonin"), count = c(3, 1)),
    data.frame(word = c("migraine", "cgrp"), count = c(2, 4)),
    data.frame(word = c("sumatriptan", "cgrp"), count = c(1, 2))
  )

  tdm <- create_tdm(preprocessed, min_df = 1, max_df = 1)
  expect_true(is.matrix(tdm))
  expect_true(all(c("migraine", "cgrp", "serotonin", "sumatriptan") %in% rownames(tdm)))

  expect_error(create_tdm(data.frame(x = 1:3)), "Terms column not found")

  expect_error(
    create_tdm(preprocessed, min_df = 4, max_df = 0.5),
    "No terms remain after filtering"
  )
})
