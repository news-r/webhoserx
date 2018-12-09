#' Get number of paragraphs
#'
#' Scan for terms in the body of articles.
#'
#' @param wh \emph{highlighted} object returned by \code{wh_collect}, see examples.
#' @param search regex to search, passed to \code{\link{grepl}}.
#' @param output name of output column.
#'
#' @examples
#' \dontrun{
#' library(webhose)
#' token <- wh_token("xXX-x0X0xX0X-00X")
#'
#' token %>%
#'   wh_news(q = '"World Economic Forum"') %>%  # use highlight!
#'   wh_collect() -> wef # collect results
#'
#' wef %>%
#'   whe_search("risks", "GRR") -> wef
#' }
#'
#' @rdname whe_search
#' @export
whe_search <- function(wh, search, output) UseMethod("whe_search")

#' @rdname whe_search
#' @method whe_search data.frame
#' @export
whe_search.data.frame <- function(wh, search, output){
  wh[[output]] <- grepl(search, wh$text)

  wh
}
