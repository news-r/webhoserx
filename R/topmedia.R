globalVariables(c(".", "name"))

#' Top 100 media.
#'
#' A dataset containing top 100 media sources.
#'
#' @format A data frame with 100 rows and 2 variables:
#' \describe{
#'   \item{name}{name of media}
#'   \item{thread.site}{site base url}
#' }
"topmedia"

#' Identify Media
#'
#' Identify mentions from top media.
#'
#' @inheritParams e_highlights
#'
#' @examples
#' \dontrun{
#' library(webhose)
#' token <- wh_token("xXX-x0X0xX0X-00X")
#'
#' token %>%
#'   wh_news(q = '"R programming"') %>%
#'   wh_collect() -> rstats # collect results
#'
#' rstats %>%
#'   whe_topmedia() -> topmedia
#' }
#'
#' @return if \code{data.frame} is passed will append a boolean column named \code{topmedia}.
#'
#' @rdname whe_topmedia
#' @export
whe_topmedia <- function(wh) UseMethod("whe_topmedia")

#' @rdname whe_topmedia
#' @method whe_topmedia data.frame
#' @export
whe_topmedia.data.frame <- function(wh){
  tp <- topmedia

  wh %>%
    dplyr::left_join(tp, by = "thread.site") %>%
    dplyr::mutate(topmedia = ifelse(!is.na(name), TRUE, FALSE)) %>%
    dplyr::select(-name) %>%
    as.data.frame(.)
}
