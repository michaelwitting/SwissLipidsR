#' @title SwissLipids API basic search function
#'
#' @description
#'
#' `swissLipidsSearch` Performs a basic search using the SwissLipids API
#'
#' @param term `character` Search term which shall be searched in SwissLipids
#' @param type `character` Type of search term, either metabolite or protein
#'
#' @return `data.frame` A data.frame with the search results
#'
#' @author Michael Witting#'
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#'
#' @export
#'
#' @examples
#'
#' swissLipidsSearch("Phosphatidate (36:2)")
swissLipidsSearch <- function(term, type = c("metabolite", "protein")) {
    # check input
    match.arg(type)

    # create query url
    query_url <- paste0(BASE_URL, "search?term=", term, "&type=", type)

    jsonlite::fromJSON(URLencode(query_url))

}

#' @title SwissLipids API advanced search function
#'
#' @description
#'
#' `swissLipidsAdvancedSearch` Performs an advanced search using the SwissLipids API
#'
#' @param name `character`
#' @param smiles `character`
#' @param inchikey `character`
#' @param formula `character`
#' @param mz `numeric`
#' @param adduct `character`
#' @param massErrorRate `numeric`
#'
#' @return `data.frame` A named list with the search results
#'
#' @author Michael Witting#'
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#'
#' @export
#'
#' @examples
#'
#'swissLipidsAdvancedSearch(name = "PC(34:2)")
#'swissLipidsAdvancedSearch(formula = "C39H76NO8P")
#'
#'swissLipidsAdvancedSearch(mz = 410.243, adduct = "MassExact", massErrorRate = 0.001)
swissLipidsAdvancedSearch <- function(name = NA_character_,
                                      smiles = NA_character_,
                                      inchikey = NA_character_,
                                      formula = NA_character_,
                                      mz = NA_real_,
                                      adduct = NA_character_,
                                      massErrorRate = NA_real_) {
  # create query
  query <- "advancedSearch?"

  if (!is.na(name)) {
    query <- paste0(query, "Name=", name, "&")
  }

  if (!is.na(smiles)) {
    query <- paste0(query, "SMILES=", smiles, "&")
  }

  if (!is.na(inchikey)) {
    query <- paste0(query, "InChIKey=", inchikey, "&")
  }

  if (!is.na(formula)) {
    query <- paste0(query, "Formula=", formula, "&")
  }

  if (!is.na(mz)) {
    query <- paste0(query, "mz=", mz, "&")
  }

  if (!is.na(adduct)) {
    if (!any(adduct %in% c("MassExact", "MassM", "MassMH", "MassMK", "MassMNa",
                       "MassMLi", "MassMNH4", "MassMmH", "MassMCl", "MassMOAc")))
      {
      stop(adduct, " is not correct, please select from MassExact, MassM, MassMH, MassMK, MassMNa, MassMLi, MassMNH4, MassMmH, MassMCl, MassMOAc")
    }

    query <- paste0(query, "adduct=", adduct, "&")
  }

  if (!is.na(massErrorRate)) {
    query <- paste0(query, "massErrorRate=", massErrorRate, "&")
  }

  # remove last "&"
  query <- gsub("&$", "", query)

  # create query url
  query_url <- paste0(BASE_URL, query)

  jsonlite::fromJSON(URLencode(query_url))

}




swissLipidsGetEntity <- function(entity_id) {
  # create query url
  query_url <- paste0(BASE_URL, "entity/entity_id=", entity_id)

  fromJSON(URLencode(query_url))

}


swissLipidsGetChildren <- function(x) {

}
