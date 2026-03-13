test_that("create_comat, type utilities, and abc_model run on toy data", {
  entity_data <- data.frame(
    doc_id = c("d1", "d1", "d2", "d2", "d3", "d3", "d4", "d4"),
    entity = c("migraine", "serotonin", "migraine", "cgrp", "sumatriptan", "migraine", "cgrp", "inflammation"),
    entity_type = c("disease", "chemical", "disease", "protein", "drug", "disease", "protein", "biological_process"),
    stringsAsFactors = FALSE
  )

  co_matrix <- create_comat(
    entity_data = entity_data,
    doc_id_col = "doc_id",
    entity_col = "entity",
    type_col = "entity_type",
    normalize = TRUE,
    normalization_method = "cosine"
  )

  expect_true(is.matrix(as.matrix(co_matrix)))
  expect_false(is.null(attr(co_matrix, "entity_types")))

  type_dist <- get_type_dist(co_matrix)
  expect_s3_class(type_dist, "data.frame")
  expect_true(all(c("entity_type", "count", "percentage") %in% names(type_dist)))

  filtered <- filter_by_type(co_matrix, types = c("disease", "drug"))
  expect_true(all(attr(filtered, "entity_types") %in% c("disease", "drug")))
})

test_that("entity validation helpers return logical outputs", {
  expect_true(is_valid_biomedical_entity("migraine", "disease"))
  expect_true(is_valid_biomedical_entity("receptor", "protein"))
  expect_false(is_valid_biomedical_entity("optimization", "chemical"))
  expect_false(is_valid_biomedical_entity("europe", "disease"))

  expect_false(LBDiscoverAnalysis:::validate_biomedical_entity("analysis", "disease"))
  expect_true(LBDiscoverAnalysis:::query_external_api("sumatriptan", "unknown_type"))

  expect_true(
    LBDiscoverAnalysis:::validate_entity_comprehensive(
      term = "migraine",
      claimed_type = "disease",
      use_nlp = FALSE,
      use_external_api = FALSE
    )
  )
  expect_false(
    LBDiscoverAnalysis:::validate_entity_comprehensive(
      term = "analysis",
      claimed_type = "disease",
      use_nlp = FALSE,
      use_external_api = FALSE
    )
  )
})

test_that("create_report and enhance_abc_kb handle lightweight inputs", {
  results <- list(
    abc = data.frame(
      a_term = "migraine",
      b_term = "serotonin",
      c_term = "cgrp",
      abc_score = 0.42,
      stringsAsFactors = FALSE
    )
  )

  out <- tempfile(fileext = ".html")
  create_report(results, output_file = out)

  expect_true(file.exists(out))
  expect_true(any(grepl("Literature-Based Discovery Report", readLines(out, warn = FALSE))))

  empty_abc <- data.frame(
    a_term = character(),
    b_terms = character(),
    c_term = character(),
    stringsAsFactors = FALSE
  )
  enhanced <- enhance_abc_kb(empty_abc, knowledge_base = "umls")
  expect_s3_class(enhanced, "data.frame")
  expect_equal(nrow(enhanced), 0)
})
