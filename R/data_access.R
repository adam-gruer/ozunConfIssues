#' get_api_response
#'
#' @param url the full path to a github api v3 endpoint
#' @param ... other agruments passed on to methods
#'
#' @return aresponse from the github v3 API
#' @export
#'
#' @examples get_api_response()
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
#' @return github api response content converted to a character vector
#' @export
#'
#' @examples get_json_data()
get_json_data <- function(response = get_api_response()) {
  httr::content(response, type = "text")

}


#' parse_json
#'
#' @param jsondata
#'
#' @return a list of parsed json data
#' @export
#'
#' @examples parse_json()
parse_json <- function(jsondata = get_json_data()) {
  jsonlite::fromJSON(jsondata)
}






#' github_pat
#'
#' @return github personal access token from an environment variable labelled GITHUB_PAT
#' @export
#'
#' @examples github_pat()
github_pat <- function() {
  pat <- Sys.getenv('GITHUB_PAT')
  if (identical(pat, "")) {
    stop("Please set env var GITHUB_PAT to your github personal access token",
         call. = FALSE)
  }

  pat
}

#' github_api
#'
#' @param path the path to a github API v3 endpoint , excluding the root https://api.github.com
#' @param token a github personal access token
#'
#' @return a list of class github_api containing 3 elements:
#'         content - parsed json data,
#'         path - the url of the api endpoint the data was requested from,
#'         response - the complete response from the request
#'
#' @export
#'
#' @examples github_api("/repos/ropensci/ozunconf17/issues")
github_api <- function(path,
                       token = github_pat(),
                       user_agent = httr::user_agent("https://github.com/adam-gruer/ozunConfIssues")) {


  url <- httr::modify_url("https://api.github.com", path = path)
  resp <- httr::GET(url,httr::add_headers(Authorization = paste("token",token)), user_agent)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }


  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "github_api"
  )



}



#' print.github_api
#'
#' @param x a list of class github_api
#' @param ... other arguments passed on to methods
#'
#' @return invisbly returns x
#' @export
#'
#' @examples print( github_api("repos/ropensci/ozunconf17/issues"))
print.github_api <- function(x, ...) {
  cat("<GitHub ", x$path, ">\n", sep = "")
  utils::str(x$content)
  invisible(x)
}


#' github_current_user
#'
#' @return a list of class github_api retrieving the github details for the current authenticated user
#' @export
#'
#' @examples github_current_user()
github_current_user <- function(){
  github_api("user")
}



#' next_page
#'
#' @param response
#'
#' @return page number, a character vector of length 1 if the api response has a link to a next page
#' @export
#'
#' @examples next_page(github_api("repos/ropensci/ozunconf17/issues"))
next_page <- function(response){
  link <- response$response$headers$link

    stringr::str_extract(string = link,
                         pattern = "(?<=\\?page=)(\\d+)(?=>;\\srel=\\\"next\\\")")

}


#' last_page
#'
#' @param response
#'
#' @return page number, a character vector of length 1 if the api response has a link to a last page
#' @export
#'
#' @examples last_page(github_api("repos/ropensci/ozunconf17/issues"))
last_page <- function(response){
  link <- response$response$headers$link

  stringr::str_extract(string = link,
                       pattern = "(?<=\\?page=)(\\d+)(?=>;\\srel=\\\"last\\\")")

}


#' get_all
#'
#' @return a list of lists of class github_api from paginated calls to a github api
#' @export
#'
#' @examples get_all()
get_all <- function(){

  responses <- github_api("repos/ropensci/ozunconf17/issues")

  if(length(responses$response$headers$link) != 0 ) {

    Reduce(function(a, x){
    Sys.sleep(5)
    list(a,github_api(paste0(responses$path,"?page=",x)))

  }, seq.int(2,as.integer(last_page(responses))), responses)

  } else  responses


}

#' all_content
#'
#' @return a list of the content  from paginated github_api reponses
#' @export
#'
#' @examples all_content()
all_content <- function(){

Reduce(function(a,x){
  c(a,x$content)

}, get_all(), list())

}


#' issues_df
#'
#' @return a dat.frame of repo issues
#' @export
#'
#' @examples issues_df()
issues_df <- function() {

issues <-   Reduce(function(a,x) {
       list(
           url = c(a$url, x$url),
           number = c(a$number, x$number),
           title = c(a$title, x$title),
           user = c(a$user, x$user$login),
           body = c(a$body, x$body),
           comments = c(a$comments, x$comments),
           comments_url = c(a$comments_url, x$comments_url)
          )
      },
      all_content(),
      init =  list(url = character(), number = integer(), title = character(),user = character(),
                            body = character(), comments = integer(), comments_url = character())
)

as.data.frame(issues, stringsAsFactors = FALSE)

}


#' pages_df
#'
#' @param pages
#'
#' @return a tibble of of reposnes from github API
#' @export
#'
#' @examples pages_df()
pages_df <- function(pages = get_all()){
  dplyr::tibble(
    content = purrr::map(pages,"content"),
    path =  purrr::map_chr(pages,"path"),
    response = purrr::map(pages,"response")
    )
}

#' issues_df2
#'
#' @param content
#'
#' @return a tibble of gitgub issues for the specified repo
#' @export
#'
#' @examples issues_df2()
#' @importFrom dplyr %>%
issues_df2 <- function(content = pages_df()$content){
   content <- Reduce(function(a,x){c(a,x)},content,list())
    dplyr::tibble(
    url = purrr::map_chr(content ,"url"),
    repository_url = purrr::map_chr(content, "repository_url"),
    labels_url = purrr::map_chr(content, "labels_url"),
    comments_url = purrr::map_chr(content, "comments_url"),
    events_url = purrr::map_chr(content, "events_url"),
    html_url = purrr::map_chr(content, "html_url"),
    id = purrr::map_int(content, "id"),
    number = purrr::map_int(content, "number"),
    title = purrr::map_chr(content, "title"),
    user = purrr::map(content, "user"),
    labels = purrr::map(content, "labels"),
    state = purrr::map_chr(content, "state"),
    locked = purrr::map_lgl(content, "locked"),
    assignee = purrr::map(content,"assignee"),
    assignees = purrr::map(content,"assignees"),
    milestone = purrr::map(content,"milestone"),
    comments = purrr::map_int(content, "comments"),
    created_at = purrr::map_chr(content, "created_at",.null = NA_character_) %>%
                           as.POSIXct(tz = "GMT",format = "%Y-%m-%dT%T"),
    updated_at = purrr::map_chr(content, "updated_at",.null = NA_character_) %>%
                           as.POSIXct(tz = "GMT",format = "%Y-%m-%dT%T"),
    closed_at = purrr::map_chr(content, "closed_at",.null = NA_character_) %>%
                           as.POSIXct(tz = "GMT",format = "%Y-%m-%dT%T"),
    author_association = purrr::map_chr(content, "author_association"),
    body = purrr::map_chr(content, "body")
    )
}







