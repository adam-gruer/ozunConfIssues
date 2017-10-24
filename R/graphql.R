
p <- httr::POST("https://api.github.com/graphql",
           httr::add_headers(Authorization = paste("bearer",github_pat())),
           body =  "{\"query\": \"query {\
  repository(owner:\"octocat\", name:\"Hello-World\") {\
issues(last:20, states:CLOSED) {\
edges {\
node {\
title\
url\
labels(first:5) {\
edges {\
node {\
name\
}\
}\
}\
}\
}\
}\
}\
}\"}")

