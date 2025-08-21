
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

`litllm` uses the OpenAI API. You’ll need an API key to use it. Check
that you have an API key saved in your environment:

``` r
ll_check_api_key()
#> Looks like you have a key stored!🎉
#> If you are having issues later, the key could be entered incorrectly or may no longer be active on OpenAI's API interface.
```
