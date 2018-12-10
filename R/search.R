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
#'   wh_news(q = '"R programming"') %>%
#'   wh_collect() -> rstats # collect results
#'
#' rstats %>%
#'   whe_search("Hadley|Wickham", "Hadley_Wickham") -> hadley
#' }
#'
#' @return Returns the number of times the \code{search} was found.
#'
#' @rdname whe_search
#' @export
whe_search <- function(wh, search, output, where) UseMethod("whe_search")

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
#' Search the number of times a regex is present in the first parargraph.
#'
#' @inheritParams whe_search
#'
#' @examples
#' \dontrun{
#' library(webhose)
#' token <- wh_token("xXX-x0X0xX0X-00X")
#'
#' token %>%
#'   wh_news(q = '"R programming" OR rstats', highlight = TRUE) %>%
#'   wh_collect() -> rstats # collect results
#'
#' rstats %>%
#'   whe_search_1p("Hadley")
#' }
#'
#' @return if \code{data.frame} is passed will append a boolean column named \code{output}.
#' If \code{character} vector is passed the function returns a \code{logical} vector.
#'
#' @seealso \code{\link{whe_mentioned}}
#'
#' @rdname whe_search_1p
#' @export
whe_search_1p <- function(wh, search, output) UseMethod("whe_search_1p")

#' @rdname whe_search_1p
#' @method whe_search_1p data.frame
#' @export
whe_search_1p.data.frame <- function(wh, search, output){

  if(missing(search))
    stop("missing search", call. = FALSE)

  para <- lapply(wh$text, function(x){
    tokenizers::tokenize_paragraphs(x, paragraph_break = "\n")[[1]][1]
  })

  mentions <- sapply(para, function(x, y){
    grep(x, y)
  }, y = search) %>%
    lapply(., function(x){
      ifelse(length(x), x,  0)
    }) %>%
    unlist()

  wh[[output]] <- mentions
  wh
}
