
# load libraries ----
library("ghql")
library("jsonlite")
library("httr")

# create graphql client ----
token <-  github_pat()
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

ozunconf_issues_raw$data$repository$issues$edges$issue[3,]
qry$query('ozunconfissues', '{
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
          edges {
          cursor
          issue: node {
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
          edges {
          cursor
          node {
          id
          name
          }
          }
          }
          reactionGroups {
          content
          createdAt
          users(first: 20) {
          totalCount
          edges {
          user: node {
          id
          name
          }
          }
          }
          }
          comments(first: 30) {
          totalCount
          edges {
          comment: node {
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
          edges {
          user: node {
          id
          name
          }
          }
          }
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
          }
          ')





ozunconf_issues_raw <- cli$exec(qry$queries$ozunconfissues)

qry$queries$ozunconf_issues


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

