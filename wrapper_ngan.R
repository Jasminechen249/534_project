library(httr)
library(jsonlite)

# https://api.github.com
# /repos/nganlyle/534_project/events

# set a user agent to identify the client
ua <- user_agent("http://github.com/nganlyle")

# create a function to interact with the github events api
github_api <- function(owner, repo) {
  path = sprintf("repos/%s/%s/events", owner, repo)
  url <- modify_url("https://api.github.com/", path = path)
  resp <- GET(url)

# raise error if the reponse is not in json
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

# parse response with jsonlite
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE)

# raise error if the request fails    
  if (http_error(resp)) {
    stop(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>", 
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

# structure the response
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "github_api"
  )
}

github_api("nganlyle", "534_project")
github_api("hadley", "httr")

rate_limit <- function() {
  github_api("/rate_limit")
}
rate_limit()

print.github_api <- function(x, ...) {
  cat("<GitHub ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}

