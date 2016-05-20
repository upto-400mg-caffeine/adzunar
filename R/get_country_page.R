#' Function to query the API by keyword country and results page.
#'
#' This function allows you to query the adzuna API, specifying a keyword, a country code and the number of results that you want. The API limit is 50 per page but if you specify more than that this function will continue to run your query, request succesive pages of the results and return the aggregate data object as a `data.frame`. You can request results that exceed the maximum returned by the API.
#' @param keyword A search string (required)
#' @param country A two letter country code. Any one of "gb", "au", "br", "ca", "de", "fr", "in", "nl", "pl", "ru", "za". Defaults to "gb".
#' @param app_id Your app id provided by Adzuna (required)
#' @param app_key Your app key provided by Adzuna (required)
#' @param n_results The number of results requested. Defaults to 50.
#' @keywords adzuna, API, data download, job adverts
#' @export
#' @examples
#' # (not run)
#' # id <- [Your app id]
#' # key <- [Your app key]
#' # get_country_page("data science", "gb", id, key)

get_country_page <- function(
  keyword,
  country = "gb",
  app_id, app_key,
  n_results = 50
) {

  total_runs <- 1
  if(n_results > 51) total_runs <- ceiling(n_results / 50)

  makeURL <- function(page = 1, ...) {
    this_url <<- paste0("http://api.adzuna.com:80/v1/api/jobs/",
                        country,
                        "/search/",
                        page, "?",
                        "app_id=", app_id,
                        "&app_key=", app_key,
                        "&results_per_page=50",
                        "&what=", sub(" ", "%20", keyword))
  }

  cat("\ndowloading...")
  makeURL()
  dat <- jsonlite::fromJSON(this_url)

  n <- dat$count
  if(n < n_results) total_runs <- ceiling(n / 50)

  results <- list(dat$results)
  if(total_runs > 1) {
    for(i in 2:total_runs) {
      cat("\n  page ", i)
      makeURL(page = i)
      results[[i]] <- jsonlite::fromJSON(this_url)$results
    }
  }

  if(n < n_results){ cat("\n    your search returned ", n, " results") }else{ cat("\n    your search returned", n_results, "results") }

  return(jsonlite::rbind.pages(results))

}
