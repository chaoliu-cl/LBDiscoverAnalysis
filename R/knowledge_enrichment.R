#' Enhance ABC results with external knowledge
#'
#' This function enhances ABC results with information from external knowledge bases.
#'
#' @param abc_results A data frame containing ABC results.
#' @param knowledge_base Character string, the knowledge base to use ("umls" or "mesh").
#' @param api_key Character string. API key for the knowledge base (if needed).
#'
#' @return A data frame with enhanced ABC results.
#' @export
enhance_abc_kb <- function(abc_results, knowledge_base = c("umls", "mesh"),
                           api_key = NULL) {

  knowledge_base <- match.arg(knowledge_base)

  if (nrow(abc_results) == 0) {
    message("ABC results are empty")
    return(abc_results)
  }

  enhanced_results <- abc_results

  unique_terms <- unique(c(
    abc_results$a_term,
    unlist(strsplit(abc_results$b_terms, ", ")),
    abc_results$c_term
  ))

  message("Enhancing ", length(unique_terms), " unique terms with ",
          knowledge_base, " information...")
  pb <- utils::txtProgressBar(min = 0, max = length(unique_terms), style = 3)

  term_info <- list()
  for (i in seq_along(unique_terms)) {
    term <- unique_terms[i]

    if (knowledge_base == "umls") {
      term_info[[term]] <- LBDiscover::query_umls(term, api_key = api_key)
    } else if (knowledge_base == "mesh") {
      term_info[[term]] <- LBDiscover::query_mesh(term)
    }

    utils::setTxtProgressBar(pb, i)
  }

  close(pb)

  if (knowledge_base == "umls") {
    enhanced_results$a_cui <- sapply(enhanced_results$a_term, function(term) term_info[[term]]$cui)
    enhanced_results$a_semantic_type <- sapply(enhanced_results$a_term, function(term) term_info[[term]]$semantic_type)
    enhanced_results$c_cui <- sapply(enhanced_results$c_term, function(term) term_info[[term]]$cui)
    enhanced_results$c_semantic_type <- sapply(enhanced_results$c_term, function(term) term_info[[term]]$semantic_type)
  } else if (knowledge_base == "mesh") {
    enhanced_results$a_mesh_id <- sapply(enhanced_results$a_term, function(term) term_info[[term]]$mesh_id)
    enhanced_results$a_tree_number <- sapply(enhanced_results$a_term, function(term) term_info[[term]]$tree_number)
    enhanced_results$c_mesh_id <- sapply(enhanced_results$c_term, function(term) term_info[[term]]$mesh_id)
    enhanced_results$c_tree_number <- sapply(enhanced_results$c_term, function(term) term_info[[term]]$tree_number)
  }

  enhanced_results
}
