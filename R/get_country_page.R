#' Function to query the API by keyword country and results page.
#'
#' This function allows you to query the adzuna API, specifying a keyword, a country code and the number of results that you want. The API limit is 50 but if you specify more than that this function will continue to run your query, request succesive pages of the results and return the aggregate data object as a data.frame.
#' @param keyword A search string (required)
#' @param country A two letter country code. Any one of "gb", "au", "br", "ca", "de", "fr", "in", "nl", "pl", "ru", "za". Defaults to "gb".
#' @param id Your app id provided by Adzuna
#' @param key Your app key provided by Adzuna
#' @param n_results The number of results requested.
#' @keywords cats
#' @export
#' @examples
#' # (not run)
#' # id <- [Your app id]
#' # key <- [Your app key]
#' # get_country_page("data science", "gb", id, key)

get_country_page <- function(keyword, country = "gb", id, key, n_results = 50) {

  total_runs <- 1
  if(n_results > 51) total_runs <- ceiling(n_results / 50)

  makeURL <- function(page = 1, ...) {
    this_url <<- paste0("http://api.adzuna.com:80/v1/api/jobs/",
                        country,
                        "/search/",
                        page, "?",
                        "app_id=", id,
                        "&app_key=", key,
                        "&results_per_page=50",
                        "&what=", sub(" ", "%20", keyword))
  }

  cat("\ndowloading...")
  makeURL()
  results <- list(fromJSON(this_url)$results)

  if(total_runs > 1) {
    for(i in 2:total_runs) {
      cat("\n  page ", i)
      makeURL(page = i)
      results[[i]] <- fromJSON(this_url)$results
    }
  }

  return(rbind.pages(results))

}
