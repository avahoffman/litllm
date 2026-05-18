# Mine ChatGPT for pdf metadata: Title, Authors, and Institutions.

Mine ChatGPT for pdf metadata: Title, Authors, and Institutions.

## Usage

``` r
ll_extract_authors(
  file_path,
  model = "gpt-5.4-mini",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  clean_authors = TRUE
)
```

## Arguments

- file_path:

  string, A single pdf file or directory containing pdfs.

- model:

  string, which OpenAI ChatGPT model to use.

- api_key:

  string, the API key if you want to supply it manually. There is no
  free one, but used with a minimal model is very cheap.
  https://openai.com/api/pricing/.

- clean_authors:

  logical, whether to output a cleaned dataframe of authors. If FALSE,
  returns raw ChatGPT output. This can be useful in case ChatGPT goes
  off script and hallucinates.

## Value

a dataframe.
