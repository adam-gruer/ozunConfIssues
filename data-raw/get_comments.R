comments <- github_api("/repos/ropensci/ozunconf17/issues/17/comments")


issues17 <-  issues[issues$number == 17,]


issues17$comments_list <- I(comments$content)

