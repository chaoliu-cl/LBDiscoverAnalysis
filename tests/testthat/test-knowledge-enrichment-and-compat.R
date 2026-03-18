test_that(".lbdiscover_call dispatches through the LBDiscover namespace", {
  entity_data <- data.frame(
    doc_id = c("d1", "d1", "d2", "d2"),
    entity = c("migraine", "serotonin", "migraine", "cgrp"),
    entity_type = c("disease", "chemical", "disease", "protein"),
    stringsAsFactors = FALSE
  )

  co_matrix <- LBDiscoverAnalysis:::.lbdiscover_call(
    "create_comat",
    entity_data = entity_data,
    doc_id_col = "doc_id",
    entity_col = "entity",
    type_col = "entity_type",
    normalize = FALSE
  )

  expect_true(is.matrix(as.matrix(co_matrix)))
  expect_true(all(c("migraine", "serotonin", "cgrp") %in% rownames(as.matrix(co_matrix))))

  expect_error(
    LBDiscoverAnalysis:::.lbdiscover_call("definitely_not_a_real_lbdiscover_function"),
    "not found in LBDiscover namespace"
  )
})

test_that("enhance_abc_kb enriches non-empty ABC results with UMLS fields", {
  abc_results <- data.frame(
    a_term = "migraine",
    b_terms = "serotonin, cgrp",
    c_term = "sumatriptan",
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    .lbdiscover_call = function(fun_name, term, api_key = NULL) {
      expect_equal(fun_name, "query_umls")
      expect_equal(api_key, "test-key")

      list(
        cui = paste0("CUI_", term),
        semantic_type = paste0("TYPE_", term)
      )
    },
    .package = "LBDiscoverAnalysis"
  )

  enriched <- suppressMessages(
    enhance_abc_kb(abc_results, knowledge_base = "umls", api_key = "test-key")
  )

  expect_equal(enriched$a_cui, "CUI_migraine")
  expect_equal(enriched$a_semantic_type, "TYPE_migraine")
  expect_equal(enriched$c_cui, "CUI_sumatriptan")
  expect_equal(enriched$c_semantic_type, "TYPE_sumatriptan")
})

test_that("enhance_abc_kb enriches non-empty ABC results with MeSH fields", {
  abc_results <- data.frame(
    a_term = "migraine",
    b_terms = "serotonin, cgrp",
    c_term = "sumatriptan",
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    .lbdiscover_call = function(fun_name, term, api_key = NULL) {
      expect_equal(fun_name, "query_mesh")
      expect_null(api_key)

      list(
        mesh_id = paste0("MESH_", term),
        tree_number = paste0("TREE_", term)
      )
    },
    .package = "LBDiscoverAnalysis"
  )

  enriched <- suppressMessages(
    enhance_abc_kb(abc_results, knowledge_base = "mesh")
  )

  expect_equal(enriched$a_mesh_id, "MESH_migraine")
  expect_equal(enriched$a_tree_number, "TREE_migraine")
  expect_equal(enriched$c_mesh_id, "MESH_sumatriptan")
  expect_equal(enriched$c_tree_number, "TREE_sumatriptan")
})
