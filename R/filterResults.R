#' @title Filter results on specific classification level
#'
#' @description
#'
#' `filterResults` performs filtering of results obtained from other functions
#'
#' @param results `data.frame` Result data.frame from SwissLipidsR
#' @param classification_level `character` Level of classification that shall be returned
#'
#' @return `data.frame` A filtered data.frame is returned
#'
#' @author Michael Witting
#'
#' @export
#'
#' @examples
#'
#' results <- swissLipidsAdvancedSearch(formula = "C39H76NO8P")
#' filterResults(results, classification_level = "Species")
filterResults <- function(results, classification_level = c("Category",
                                                            "Main class",
                                                            "Sub class",
                                                            "Species",
                                                            "Molecular subspecies",
                                                            "Structural subspecies",
                                                            "Isomeric subspecies")) {

  # sanity check on classification level
  match.arg(classification_level)

  if(!"classification_level" %in% colnames(results)) {

    stop("results does not contain 'classification_level' column")

  }

  results[which(results$classification_level == classification_level),]

}
