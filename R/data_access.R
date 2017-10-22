#' get_api_response
#'
#' @param url
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_api_response <- function(
      url = "https://api.github.com/repos/ropensci/ozunconf17/issues",
                    ...) {
  #todo: what happens when more than one page
   httr::GET(url,
       query = list(state = "all", per_page = 100, page = 1))
}



#' get_json_data
#'
#' @param response
#'
#' @return
#' @export
#'
#' @examples
get_json_data <- function(response = get_api_response()) {
  httr::content(response, type = "text")

}


#' parse_json
#'
#' @param jsondata
#'
#' @return
#' @export
#'
#' @examples
parse_json <- function(jsondata = get_json_data()) {
  jsonlite::fromJSON(jsondata)
}






