#' Lexical diversity
#'
#' Calculate the lexical diversity or complexity of text.
#'
#' @inheritParams e_highlights
#' @param measure lexical diversity type to compute, passed to \link[quanteda]{textstat_lexdiv}.
#'
#' @examples
#' \dontrun{
#' library(webhose)
#' token <- wh_token("xXX-x0X0xX0X-00X")
#'
#' token %>%
#'   wh_news(q = '"R programming"') %>%
#'   wh_collect() -> wef # collect results
#'
#' rstats %>%
#'   whe_lexdiv() -> lexdiv
#'
#' library(dplyr)
#'
#' rstats %>%
#'   mutate(lexdiv = whe_lexdiv(text)) # pass text column
#' }
#'
#' @return if \code{data.frame} is passed will append a column named \code{lexdiv},
#' boolean.
#' If \code{character} vector is passed the function returns a \code{boolean} vector.
#'
#' @rdname whe_lexdiv
#' @export
whe_lexdiv <- function(wh, measure = "R") UseMethod("whe_lexdiv")

#' @rdname whe_lexdiv
#' @method whe_lexdiv data.frame
#' @export
whe_lexdiv.data.frame <- function(wh, measure = "R"){

  dfm <- quanteda::dfm(wh$text)
  lexdiv <- quanteda::textstat_lexdiv(dfm, measure = measure)

  wh$lexdiv <- unname(lexdiv)
  wh
}

#' @rdname whe_lexdiv
#' @method whe_lexdiv character
#' @export
whe_lexdiv.character <- function(wh, measure = "R"){
  dfm <- quanteda::dfm(wh)
  lexdiv <- quanteda::textstat_lexdiv(dfm, measure = measure)
  unname(lexdiv)
}
