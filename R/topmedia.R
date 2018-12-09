#' Top 100 media.
#'
#' A dataset containing top 100 media sources.
#'
#' @format A data frame with 100 rows and 2 variables:
#' \describe{
#'   \item{name}{name of media}
#'   \item{thread.site}{site base url}
#' }
#' @source World Economic Forum
"topmedia"

#' Identify Media
#'
#' Identify mentions from top media.
#'
#' @param wh object returned by \code{wh_collect}, see examples.
#'
#' @examples
#' \dontrun{
#' library(webhose)
#' token <- wh_token("xXX-x0X0xX0X-00X")
#'
#' token %>%
#'   wh_news(q = '"World Economic Forum"') %>%
#'   wh_collect() -> wef # collect results
#'
#' wef %>%
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
