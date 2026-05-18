# Perform logical checks on a vector of institutions using ChatGPT.

For example, we might be interested in checking whether author
institutions are a nonprofit, private company, or university.

## Usage

``` r
ll_find_institution_type(
  text,
  model = "gpt-5.4-mini",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  logical_check = "in the United States"
)
```

## Arguments

- text:

  A vector of strings containing cleaned institutions. These might be
  the output of \`ll_pull_institutions\`.

- model:

  string, which OpenAI ChatGPT model to use.

- api_key:

  string, the API key if you want to supply it manually. There is no
  free one, but used with a minimal model is very cheap.
  https://openai.com/api/pricing/.

- logical_check:

  string, a phrase that can be answered "Yes" or "no" when combined with
  "Is this institution...". For example, "in the United States" or "a
  Minority Serving Institution".

## Value

A data frame containing T/F data for each item in \`text\`.

## Examples

``` r
ll_find_institution_type("Johns Hopkins University", logical_check = "an institution with >5000 students")
#> Johns Hopkins University --- duration: 6
#> # A tibble: 0 × 2
#> # ℹ 2 variables: name <int>, an_institution_with_5000_students <lgl>
```
