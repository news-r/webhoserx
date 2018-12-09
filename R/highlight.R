#' Get highlighted terms
#'
#' Extract highlighted terms
#'
#' @param wh \emph{highlighted} object returned by \link[webhoser]{wh_collect}, see examples.
#' @param flatten whether to flatten ngrams.
#'
#' @examples
#' \dontrun{
#' library(webhose)
#' token <- wh_token("xXX-x0X0xX0X-00X")
#'
#' token %>%
#'   wh_news(q = '"R programming language"') %>%  # use highlight!
#'   wh_collect() -> rstats # collect results
#'
#' rstats %>%
#'   whe_highlights() -> highlights
#'
#' highlights <- rstats %>%
#'   dplyr::mutate(
#'     nparagraphs = whe_highlights(text)
#'   )
#' }
#'
#' @name e_highlights
#' @export
whe_highlights <- function(wh, flatten = FALSE) UseMethod("whe_highlights")

#' @rdname e_highlights
#' @method whe_highlights data.frame
#' @export
whe_highlights.data.frame <- function(wh, flatten = FALSE){
  x <- lapply(stringr::str_extract_all(wh$highlightText, "<em>(.*?)</em>"), function(x){
    gsub("<em>|</em>", "", x)
  })

  if(isTRUE(flatten)){
    x <- sapply(x, paste0, collapse = ", ")
  }
  wh$highlights <- x
  wh
}

#' @rdname e_highlights
#' @method whe_highlights character
#' @export
whe_highlights.character <- function(wh, flatten = FALSE){
  x <- lapply(stringr::str_extract_all(wh$highlightText, "<em>(.*?)</em>"), function(x){
    gsub("<em>|</em>", "", x)
  })

  if(isTRUE(flatten)){
    x <- sapply(x, paste0, collapse = ", ")
  }
  x
}
