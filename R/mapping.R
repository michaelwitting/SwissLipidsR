swissLipidsMapping <- function(from = c("SwissLipids", "LipidMaps", "ChEBI", "HMDB"),
                               to = c("SwissLipids", "LipidMaps", "ChEBI", "HMDB", "UniProtKB", "Rhea"),
                               ids) {

  # check input
  match.arg(from)
  match.arg(to)

  query_url <- paste0(BASE_URL, "mapping?from=", from, "&to=", to, "&ids=", paste(ids, collapse = ","))

  fromJSON(URLencode(query_url))

}
