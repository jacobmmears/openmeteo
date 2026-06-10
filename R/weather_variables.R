#' Retrieve valid hourly and daily weather variables
#'
#' @description
#'
#' `weather_variables()` retrieves an incomplete list of _hourly_ and _daily_
#' variables accepted by [weather_forecast()] and [weather_history()], such as
#' temperature or precipitation.
#'
#' Refer to the following documentation for the forecast and history API
#' endpoints for detailed descriptions, units, and caveats:
#'
#' Forecast API <https://open-meteo.com/en/docs>
#'
#' Historical API <https://open-meteo.com/en/docs/historical-weather-api>
#'
#' @return A list of valid hourly and daily weather variables
#'
#' @export
#'
#' @examples
#' \donttest{
#' weather_variables()
#' }
weather_variables <- function() {
  # Open-Meteo moved their openapi specs into an `openapi/` subdir and renamed
  # them in late 2024; the prior URLs now 404.
  base_url <- "https://raw.githubusercontent.com/open-meteo/open-meteo/main/openapi/"

  forecast_url <- paste0(base_url, "forecast.yml")
  forecast_params <-
    .retrieve_om_schema(forecast_url)$paths$`/v1/forecast`$get$parameters

  history_url <- paste0(base_url, "historical-weather.yml")
  history_params <-
    .retrieve_om_schema(history_url)$paths$`/v1/archive`$get$parameters

  list(
    hourly_forecast_vars = .vars_from_schema(forecast_params, "hourly"),
    daily_forecast_vars  = .vars_from_schema(forecast_params, "daily"),
    hourly_history_vars  = .vars_from_schema(history_params, "hourly"),
    daily_history_vars   = .vars_from_schema(history_params, "daily")
  )
}

# retrieves openai yaml from URL as an R object
.retrieve_om_schema <- function(url) {
  pl <- httr::GET(url)
  .response_OK(pl)

  yaml::yaml.load(httr::content(pl))
}

.vars_from_schema <- function(params, name) {
  for (i in params) {
    if (i$name == name) {
      return(i$schema$items$enum)
    }
  }
  stop("Unable to find variables from parsed YAML params")
}
