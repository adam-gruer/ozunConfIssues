
# load libraries ----
library("ghql")
library("jsonlite")
library("httr")
library(dplyr)
library(purrr)

# create graphql client ----
token <-  Sys.getenv('GITHUB_PAT') #github_pat()

cli <- GraphqlClient$new(
  url = "https://api.github.com/graphql",
  headers = add_headers(Authorization = paste0("Bearer ",token ))
)


# load schema -------------------------------------------------------------
cli$load_schema()
cli$schema$data$`__schema`$types$name


# basic query -------------------------------------------------------------

qry <- Query$new()
#qry$query('myquery', 'query { }')
#qry$query('myquery2', 'query { }')
qry

#qry$queries$getmyrepodata
#cli$exec(qry$queries$myquery)
qry <- Query$new()


qry$query('ozunconfissues',
          '{
            rateLimit {
              nodeCount
              limit
              cost
              remaining
              resetAt
            }
            repository(owner: "ropenSci", name: "ozunconf17") {
              nameWithOwner
              createdAt
              description
              id
              url
              watchers(first: 1) {
                totalCount
              }
              stargazers(first: 1) {
                totalCount
              }
              issues(first: 50) {
                nodes {
                  id
                  title
                  url
                  state
                  createdAt
                  number
                  bodyHTML
                  bodyText
                  closed
                  author {
                    login
                    url
                    resourcePath
                    avatarUrl
                  }
                  labels(first: 10) {
                    totalCount
                    nodes {
                      id
                      name
                    }
                  }
                  reactionGroups {
                    content
                    createdAt
                    users(first: 20) {
                      totalCount
                      nodes {
                        id
                        name
                      }
                    }
                  }
                  comments(first: 30) {
                    totalCount
                    nodes {
                      id
                      bodyHTML
                      bodyText
                      createdAt
                      author {
                        login
                        url
                        resourcePath
                        avatarUrl
                      }
                      reactionGroups {
                        content
                        createdAt
                        users(first: 10) {
                          totalCount
                          nodes {
                            id
                            name
                          }
                        }
                      }
                    }
                  }
                }
                pageInfo {
                  endCursor
                  hasNextPage
                  hasPreviousPage
                  startCursor
                }
                totalCount
              }
            }
          }')

ozunconf_issues_raw <- cli$exec(qry$queries$ozunconfissues)

issues <- ozunconf_issues_raw$data$repository$issues$nodes
names(issues)
map(issues, class)
x <- map_df(ozunconf_issues_raw,list("repository","issues","nodes"))

issues <- ozunconf_issues_raw$data$repository$issues$edges$issue
issues$comments$edges[[2]]$comment %>% names()



str(ozunconf_issues_raw)


# make request ----
p <- httr::POST("https://api.github.com/graphql",
           httr::add_headers(Authorization = paste("bearer",github_pat())),
           content_type_json(),
            body =  "{ \"query\": \"query { viewer { login }}\" } ")

# parse response to JSON ----
p$headers$`content-type`
json <- httr::content(p,
              as = "parsed",
              type = p$headers$`content-type`,
              flatten = TRUE)
names(json)

