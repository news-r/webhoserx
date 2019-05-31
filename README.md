# webhoserx

Extension for the [webhoser](https://webhoser.john-coene.com) package, mainly for feature extraction from text.

## Install

```r
# install.packages("remotes")
remotes::install_github("news-r/webhoserx")
```

## Functions

Functions from the `webhoser` package start with `wh` while functions of the `webhoserx` package start with `whe`.

* `whe_pre_gram` and `whe_post_gram` to extract n-grams preceding and following mentions (highlighted terms).
* `whe_paragraphs` count paragraphs.
* `whe_sentences` count sentences.
* `whe_words` count words.
* `whe_topmedia` identify mentions from top media.
* `whe_mentioned` count number of paragraphs with mentions of any term from search.
* `whe_search` counts the number of times a regular expression is found in a particular column.
* `whe_search_1p` get number of times a regular expression in found in the first paragraph.
* `whe_mentions` counts number of mentions (highlighted terms).
* `whe_highlight` extract mentions (highlighted terms).
* `whe_entities` split flattened entities: `persons`, `organizations` or `locations`.
* `whe_quotes` identify articles with quotations.
* `whe_date` parses date.
* `whe_lexdiv` get lexical diversity. 
* `whe_sentiment` get sentiment of text.
* `whe_similarity` **beta** function to compare similarity between articles and a given document (i.e. : press release).
* `whe_search` Scan for terms in the body of articles.

**Important**: many functions rely on the `highlightText` field returned only if `highlight = TRUE` in `wh_news`, see functions' examples.

