
<!-- README.md is generated from README.Rmd. Please edit that file -->

# litllm

<!-- badges: start -->
<!-- badges: end -->

The goal of `litllm` is to help you mine pdfs for scholarly analysis.

## Installation

You can install the development version of litllm from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("avahoffman/litllm")
```

## Getting started

``` r
library(litllm)
```

`litllm` uses the OpenAI API. You’ll need an API key and internet
connection to use it. Check that you have an API key saved in your
environment:

``` r
ll_check_api_key()
#> Looks like you have a key stored!🎉
#> If you are having issues later, the key could be entered incorrectly or may no longer be active on OpenAI's API interface.
```

*Note: The OpenAI API costs money. It is very cheap (around \$0.80 for
analyzing 100 pdfs), but you’ll need to add credits through their
interface.*

Create a subdirectory in your working directory. We typically call ours
“pdf/”. Add the pdf files for any publications you’d like to analyze to
this directory.

*Note: We recommend starting with ~2 files so you can make sure
everything is working as expected.*

## Getting authors and titles

The first thing you might want to do is iterate through all your pdf
files, finding the article’s title and author information:

``` r
# look at files in the "pdf/" directory
author_df <- ll_extract_authors("pdf/")
```

`ll_extract_authors()` produces a tibble with a column “paper_id” (the
file name), “title” (the article title), and “auth” (individual author
information). Each author will be on a separate row.
