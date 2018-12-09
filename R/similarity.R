#' Compare articles with set of documents
#'
#' Compare articles with set of documents, i.e.: press releases.
#'
#' @inheritParams e_highlights
#' @param docs documents to compare the articles with.
#' @param progress whether to show progress bar.
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
#' library(rvest)
#'
#' html <- read_html('http://reports.weforum.org/global-gender-gap-report-2017/press-release/')
#'
#' # scrape Gender Gap Report press release
#' html %>%
#'   html_nodes(".content") %>%
#'   html_children() %>%
#'   html_text() %>%
#'   .[5:40] %>%
#'   paste0(., collapse = "\n") -> pr
#'
#' wef %>%
#'   whe_similarity(pr) -> similarity
#'
#' library(dplyr)
#' wef %>%
#'   mutate(nmentions = whe_mentions(text)) -> similarity
#' }
#'
#' @details This function uses the \href{Jacquard's index}{https://en.wikipedia.org/wiki/Jaccard_index}
#'
#' @return if a \code{data.frame} is passed will append a column named \code{similarity.*} where \code{*} is the input document number.
#' If a \code{character} vector is passed the function returns a \code{character} vector.
#'
#' @rdname whe_similarity
#' @export
whe_similarity <- function(wh, docs, progress = interactive()) UseMethod("whe_similarity")

#' @rdname whe_similarity
#' @method whe_similarity data.frame
#' @export
whe_similarity.data.frame <- function(wh, docs, progress = interactive()){

  content <- c(docs, wh$text)
  doc_names <- sapply(1:length(docs), function(x){
    paste0("similarity.", x)
  })
  names(content) <- c(doc_names, wh$uuid)

  corpus <- textreuse::TextReuseCorpus(text = content, progress = progress) # build corpus

  comparisons <- textreuse::pairwise_compare(corpus, textreuse::jaccard_similarity, progress = progress) # compare documents

  # filter documents to only take `docs`
  len <- 1 + length(docs)
  cp <- comparisons[1:length(docs),len:ncol(comparisons)]

  # build as data.frame
  cp <- as.data.frame(cp)
  if(length(docs) > 1) cp <- as.data.frame(t(cp))
  names(cp) <- doc_names
  cp$uuid <- as.character(rownames(cp))
  rownames(cp) <- NULL

  wh <- dplyr::left_join(wh, cp, by = "uuid")

  wh
}

#' @rdname whe_similarity
#' @method whe_similarity character
#' @export
whe_similarity.character <- function(wh, docs, progress = interactive()){

  content <- c(docs, wh)
  doc_names <- sapply(1:length(docs), function(x){
    paste0("similarity.", x)
  })
  names(content) <- c(doc_names, wh$uuid)

  corpus <- textreuse::TextReuseCorpus(text = content, progress = progress) # build corpus

  comparisons <- textreuse::pairwise_compare(corpus, textreuse::jaccard_similarity, progress = progress) # compare documents

  # filter documents to only take `docs`
  len <- 1 + length(docs)
  cp <- comparisons[1:length(docs),len:ncol(comparisons)]

  # build as data.frame
  cp <- as.data.frame(cp)
  if(length(docs) > 1) cp <- as.data.frame(t(cp))
  names(cp) <- doc_names
  cp$uuid <- as.character(rownames(cp))
  rownames(cp) <- NULL

  cp
}
