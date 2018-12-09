# webhoser.extension

Extension for the [webhoser](https://webhoser.john-coene.com) package, mainly for feature extraction from text.

## Install

```r
# install.packages("remotes")
remotes::install_github("JohnCoene/webhoser.extension")
```

## Functions

Functions from the `webhoser` package start with `wh` while functions of the `webhoser.extension` package start with `whe`.

* `whe_pre_gram` and `whe_post_gram` to extract n-grams preceding and following mentions (highlighted terms).
* `whe_paragraphs` count paragraphs.
* `whe_sentences` count sentences.
* `whe_words` count words.
* `whe_topmedia` identify mentions from top media.
* `whe_mentioned` count number of paragraphs with mentions of any term from search.
* `whe_mentioned_wef` count number of paragraphs with mentions of the WEF.
* `whe_mentioned_wef_1p` identify if WEF is mentioned in 1st paragraph.
* `whe_mentions` counts number of mentions (highlighted terms).
* `whe_highlight` extract mentions (highlighted terms).
* `whe_entities` split flattened entities: `persons`, `organizations` or `locations`.
* `whe_quotes` identify articles with quotations.
* `whe_date` parses date.
* `whe_lexdiv` get lexical diversity. 
* `whe_similarity` **beta** function to compare similarity between articles and a given document (i.e. : press release).
* `whe_search` Scan for terms in the body of articles.

**Important**: most functions rely on the `highlightText` field returned only if `highlight = TRUE` in `wh_news` or `wh_broadcasts` call, see examples.

