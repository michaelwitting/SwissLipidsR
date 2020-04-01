#' @title SwissLipids API Mapping
#'
#' @description
#'
#' `swissLipidsMapping` uses the SwissLipids API to perform mapping between
#'     different database identifier
#'
#' @param from `character` Abbrevation of data from which the ids shall be mapped
#' @param to `character` Abbrevation of database to which the ids shall be mapped
#' @param ids `character` A vector of IDs that shall be mapped.
#'
#' @return `list` A named list of the same length as ids is returned.
#'
#' @author Michael Witting
#'
#'
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
#' @examples
#'
#' ids <- c("SLM:000048885", "SLM:000000651")
#' from <- "SwissLipids"
#' to <- "LipidMaps"
#'
#' swissLipidsMapping(from, to, ids)
swissLipidsMapping <- function(from = c("SwissLipids", "LipidMaps", "ChEBI", "HMDB"),
                               to = c("SwissLipids", "LipidMaps", "ChEBI", "HMDB", "UniProtKB", "Rhea"),
                               ids) {

  # check input
  match.arg(from)
  match.arg(to)

  query_url <- paste0(BASE_URL, "mapping?from=", from, "&to=", to, "&ids=", paste(ids, collapse = ","))

  fromJSON(URLencode(query_url))

}
