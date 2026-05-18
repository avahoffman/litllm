# Extract "Institutions" from a messy vector using ChatGPT.

Extract "Institutions" from a messy vector using ChatGPT.

## Usage

``` r
ll_pull_institutions(
  text,
  model = "gpt-5.4-mini",
  api_key = Sys.getenv("OPENAI_API_KEY")
)
```

## Arguments

- text:

  A vector of strings containing messy data.

- model:

  string, which OpenAI ChatGPT model to use.

- api_key:

  string, the API key if you want to supply it manually. There is no
  free one, but used with a minimal model is very cheap.
  https://openai.com/api/pricing/.

## Value

A vector containing clean data.
