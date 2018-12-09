#' Get number of paragraphs with mentions
#'
#' Counts number of paragraphs with mentions.
#'
#' @param wh \emph{highlighted} object returned by \code{wh_collect}, see examples.
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
#'   whe_mentioned() -> mentioned
#'
#' mentioned <- wef %>%
#'   dplyr::mutate(nmentioned = whe_mentioned(highlightText))
#' }
#'
#' @return if \code{data.frame} is passed will append a column named \code{mentioned}.
#' If \code{character} vector is passed the function returns a \code{character} vector.
#'
#' @seealso \code{\link{whe_mentioned_wef}}
#'
#' @rdname whe_mentioned
#' @export
whe_mentioned <- function(wh) UseMethod("whe_mentioned")

#' @rdname whe_mentioned
#' @method whe_mentioned data.frame
#' @export
whe_mentioned.data.frame <- function(wh){
  para <- lapply(wh$highlightText, function(x){
    tokenizers::tokenize_paragraphs(x, paragraph_break = "\n")
  })

  mentions <- lapply(para, function(x){
    lapply(x, function(y){
      stringr::str_extract_all(y, "<em>(.*?)</em>")
    })
  })

  x <- lapply(mentions, function(x){
    lapply(x, function(y){
      sapply(y, length)
    })
  })

  wh$mentioned <- sapply(x, function(z){
    length(z[[1]][z[[1]] > 0])
  })
  wh
}

#' @rdname whe_mentioned
#' @method whe_mentioned character
#' @export
whe_mentioned.character <- function(wh){
  para <- lapply(wh, function(x){
    tokenizers::tokenize_paragraphs(x, paragraph_break = "\n")
  })

  mentions <- lapply(para, function(x){
    lapply(x, function(y){
      stringr::str_extract_all(y, "<em>(.*?)</em>")
    })
  })

  x <- lapply(mentions, function(x){
    lapply(x, function(y){
      sapply(y, length)
    })
  })

  sapply(x, function(z){
    length(z[[1]][z[[1]] > 0])
  })
}

#' Get number of paragraphs with mentions of the WEF
#'
#' Counts number of paragraphs with mentions of the \code{World Economic Forum} or \code{WEF}.
#'
#' @param wh \emph{highlighted} object returned by \code{wh_collect}, see examples.
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
#'   whe_mentioned_wef() -> mentioned
#'
#' mentioned <- wef %>%
#'   dplyr::mutate(nmentioned_wef = whe_mentioned_wef(highlightText))
#' }
#'
#' @return if \code{data.frame} is passed will append a column named \code{mentioned.wef}.
#' If \code{character} vector is passed the function returns a \code{character} vector.
#'
#' @seealso \code{\link{whe_mentioned}}
#'
#' @rdname whe_mentioned_wef
#' @export
whe_mentioned_wef <- function(wh) UseMethod("whe_mentioned_wef")

#' @rdname whe_mentioned_wef
#' @method whe_mentioned_wef data.frame
#' @export
whe_mentioned_wef.data.frame <- function(wh){
  para <- lapply(wh$highlightText, function(x){
    tokenizers::tokenize_paragraphs(x, paragraph_break = "\n")
  })

  mentions <- lapply(para, function(x){
    lapply(x, function(y){
      stringr::str_extract_all(y, "<em>World Economic Forum</em>|<em>WEF</em>")
    })
  })

  x <- lapply(mentions, function(x){
    lapply(x, function(y){
      sapply(y, length)
    })
  })

  wh$mentioned.wef <- sapply(x, function(z){
    length(z[[1]][z[[1]] > 0])
  })
  wh
}

#' @rdname whe_mentioned_wef
#' @method whe_mentioned_wef character
#' @export
whe_mentioned_wef.character <- function(wh){
  para <- lapply(wh, function(x){
    tokenizers::tokenize_paragraphs(x, paragraph_break = "\n")
  })

  mentions <- lapply(para, function(x){
    lapply(x, function(y){
      stringr::str_extract_all(y, "<em>World Economic Forum</em>|<em>WEF</em>")
    })
  })

  x <- lapply(mentions, function(x){
    lapply(x, function(y){
      sapply(y, length)
    })
  })

  sapply(x, function(z){
    length(z[[1]][z[[1]] > 0])
  })
}

#' Identify articles with WEF mentioned in 1st paragraph
#'
#' Identifies if \code{World Economic Forum} or \code{WEF} is mentioned in 1st paragraph of article.
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
whe_mentioned_wef_1p <- function(wh) UseMethod("whe_mentioned_wef_1p")

#' @rdname whe_mentioned_wef_1p
#' @method whe_mentioned_wef_1p data.frame
#' @export
whe_mentioned_wef_1p.data.frame <- function(wh){
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
whe_mentioned_wef_1p.character <- function(wh){
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
