#' Get precending bigrams
#'
#' @inheritParams e_highlights
#' @param n number of terms, defaults to 2.
#' @param flatten whether to flatten ngrams.
#'
#' @examples
#' \dontrun{
#' library(webhose)
#' token <- wh_token("xXX-x0X0xX0X-00X")
#'
#' token %>%
#'   wh_news(q = 'golang') %>%  # use highlight!
#'   wh_collect() -> rstats # collect results
#'
#' rstats %>%
#'   whe_pre_gram() -> pre_bigrams
#'
#' library(dplyr)
#'
#' rstats %>%
#'   mutate(pre.gram = whe_pre_gram(highlightText, n = 3, flatten = TRUE)) -> pre_trigrams
#' }
#'
#' @return if \code{data.frame} is passed will append a column named \code{pre.gram.n} where \code{n} is
#' the number of terms, parameter \code{n}. If \code{character} vector is passed will return list
#' (or vector dependeing on \code{flatten}) of n-grams.
#'
#' @seealso \code{\link{whe_post_gram}}
#'
#' @rdname whe_pre_gram
#' @export
whe_pre_gram <- function(wh, n = 2, flatten = FALSE) UseMethod("whe_pre_gram")

#' @rdname whe_pre_gram
#' @method whe_pre_gram data.frame
#' @export
whe_pre_gram.data.frame <- function(wh, n = 2, flatten = FALSE){
  name <- paste0("pre.gram.", n)
  x <- sapply(stringr::str_extract_all(wh$highlightText, paste0("(\\w+\\s+){", n,"}(?=\\<em\\>)")), trimws)

  if(isTRUE(flatten)){
    x <- sapply(x, paste0, collapse = ", ")
  }
  wh[[name]] <- x
  wh
}

#' @rdname whe_pre_gram
#' @method whe_pre_gram character
#' @export
whe_pre_gram.character <- function(wh, n = 2, flatten = FALSE){
  x <- sapply(stringr::str_extract_all(wh, paste0("(\\w+\\s+){", n,"}(?=\\<em\\>)")), trimws)
  if(isTRUE(flatten)){
    x <- sapply(x, paste0, collapse = ", ")
  }
  x
}

#' Get following bigrams
#'
#' @inheritParams whe_pre_gram
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
#'   whe_post_gram(3) -> post_trigrams
#'
#' library(dplyr)
#' rstats %>%
#'   mutate(post.bigram = whe_post_gram(highlightText, flatten = TRUE)) -> post_bigrams
#' }
#'
#' @return if \code{data.frame} is passed will append a column named \code{pre.gram.n} where \code{n} is
#' the number of terms, parameter \code{n}. If \code{character} vector is passed will return list
#' (or vector dependeing on \code{flatten}) of n-grams.
#'
#' @seealso \code{\link{whe_pre_gram}}
#' @rdname whe_post_gram
#' @export
whe_post_gram <- function(wh, n = 2, flatten = FALSE) UseMethod("whe_post_gram")

#' @rdname whe_post_gram
#' @method whe_post_gram data.frame
#' @export
whe_post_gram.data.frame <- function(wh, n = 2, flatten = FALSE){
  name <- paste0("post.gram.", n)
  x <- sapply(stringr::str_extract_all(wh$highlightText,
                                                  paste0("([/]em>)(\\s+\\w+){", n, "}")), trimws)

  x <- sapply(x, function(x){gsub("/em>", "", x)})
  if(isTRUE(flatten)){
    x <- sapply(x, paste0, collapse = ", ")
  }
  wh[[name]] <- x
  wh
}

#' @rdname whe_post_gram
#' @method whe_post_gram character
#' @export
whe_post_gram.character <- function(wh, n = 2, flatten = FALSE){
  x <- sapply(stringr::str_extract_all(wh, paste0("([/]em>)(\\s+\\w+){", n, "}")), trimws)
  x <- sapply(x, function(x){gsub("/em>", "", x)})
  if(isTRUE(flatten)){
    x <- sapply(x, paste0, collapse = ", ")
  }
  x
}
