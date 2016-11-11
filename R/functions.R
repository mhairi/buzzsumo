
#' Use Buzzsumo API to get the most shared content for a domain
#'
#' @param url Domain
#' @param api_key Buzzsumo API key
#' @return A nested list of results
#' @export
#'
#' @examples \dontrun{
#' get_most_shared('slate.com', '123')
#' }
get_most_shared <- function(url, api_key){

  get_url <- 'http://api.buzzsumo.com/search/articles.json'

  parameters <- list(
    'q'       = url,
    'api_key' = api_key
  )

  response <- httr::GET(get_url, query = parameters)

  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  httr::stop_for_status(response)

  content <- httr::content(response, type = 'text', encoding = 'utf-8')
  parsed  <- jsonlite::fromJSON(content, simplifyVector = FALSE)

  return(parsed)
}

#' Create a dataframe containing the most shared article
#'
#' @param parsed A nested list; the result of running get_most_shared
#'
#' @return A dataframe with one row
#' @export
#'
#' @examples \dontrun{
#' parsed <- get_most_shared('slate.com', '123')
#' parse_top_result(parsed)
#' }
parse_top_result <- function(parsed){

  result <- parsed$results[[1]]

  return(as.data.frame(result))
}


