#' format date
#'
#' Format dates returned by API, see details.
#'
#' @param date date column, see examples.
#'
#' @details Webhose's servers are in Israel, this is used as timezone.
#'
#' @return vector of class \code{POSIXct}.
#'
#' @examples
#' \dontrun{
#' token <- wh_token("xXX-x0X0xX0X-00X")
#'
#' wef <- wh_news(token, q = '"R programming language"') %>%  # use quote marks!
#'   wh_collect() %>% # collect results
#'   dplyr::mutate(published = wh_date(published))
#' }
#'
#' @export
whe_date <- function(date){
  lubridate::as_datetime(date)
}
