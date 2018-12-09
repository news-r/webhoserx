#' Search
#'
#' Search for terms.
#'
#' @inheritParams e_highlights
#' @param search regex to search, passed to \code{\link{grepl}}.
#' @param output name of output column.
#' @param where column where to search.
#'
#' @examples
#' \dontrun{
#' library(webhose)
#' token <- wh_token("xXX-x0X0xX0X-00X")
#'
#' token %>%
#'   wh_news(q = '"R programming"') %>%  # use highlight!
#'   wh_collect() -> rstats # collect results
#'
#' rstats %>%
#'   whe_search("Hadley|Wickham", "HadleyWickham") -> hadley
#' }
#'
#' @return Returns the number of times the \code{search} was found.
#'
#' @rdname whe_search
#' @export
whe_search <- function(wh, search, output) UseMethod("whe_search")

#' @rdname whe_search
#' @method whe_search data.frame
#' @export
whe_search.data.frame <- function(wh, search, output, where = "text"){

  if(missing(search))
    stop("missing search", call. = FALSE)

  if(missing(output))
    stop("missing output", call. = FALSE)

  out <- lapply(wh[[where]], function(x, y){
    grep(y, x)
  }, y = search) %>%
    lapply(., function(x){
      ifelse(length(x), x,  0)
    }) %>%
    unlist()

  wh[[output]] <- out

  wh
}


#' Search in the first paragraph
#'
#' Search the number of times something is present in the first parargraph.
#'
#' @param wh \emph{text} object returned by \code{wh_collect}, see examples.
#'
#' @examples
#' \dontrun{
#' library(webhose)
#' token <- wh_token("xXX-x0X0xX0X-00X")
#'
#' token %>%
#'   wh_news(q = '"World Economic Forum"', highlight = TRUE) %>%  # use highlight!
#'   wh_collect() -> wef # collect results
#'
#' wef %>%
#'   whe_mentioned_wef_1p() -> mentioned
#'
#' mentioned <- wef %>%
#'   dplyr::mutate(nmentioned_wef = whe_mentioned_wef_1p(text))
#' }
#'
#' @return if \code{data.frame} is passed will append a boolean column named \code{mentioned.wef.1p}.
#' If \code{character} vector is passed the function returns a \code{logical} vector.
#'
#' @seealso \code{\link{whe_mentioned}}
#'
#' @rdname whe_mentioned_wef_1p
#' @export
whe_search_1p <- function(wh) UseMethod("whe_mentioned_wef_1p")

#' @rdname whe_mentioned_wef_1p
#' @method whe_mentioned_wef_1p data.frame
#' @export
whe_search_1p.data.frame <- function(wh){
  para <- lapply(wh$text, function(x){
    tokenizers::tokenize_paragraphs(x, paragraph_break = "\n")[[1]][1]
  })

  mentions <- sapply(para, function(x){
    stringr::str_extract_all(x, "World Economic Forum|WEF")
  })

  wh$mentioned.wef.1p <- sapply(mentions, function(x){
    ifelse(length(x), TRUE, FALSE)
  })
  wh
}

#' @rdname whe_mentioned_wef_1p
#' @method whe_mentioned_wef_1p character
#' @export
whe_search_1p.character <- function(wh){
  para <- lapply(wh, function(x){
    tokenizers::tokenize_paragraphs(x, paragraph_break = "\n")[[1]][1]
  })

  mentions <- sapply(para, function(x){
    stringr::str_extract_all(x, "World Economic Forum|WEF")
  })

  sapply(mentions, function(x){
    ifelse(length(x), TRUE, FALSE)
  })
}
