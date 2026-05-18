# Mine ChatGPT for pdf metadata: works cited information.

Mine ChatGPT for pdf metadata: works cited information.

## Usage

``` r
ll_extract_references(
  file_path,
  model = "gpt-5.4-mini",
  api_key = Sys.getenv("OPENAI_API_KEY"),
  clean_references = TRUE
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

- clean_references:

  logical, whether to output a cleaned list of dataframes. If FALSE,
  returns raw ChatGPT output. This can be useful in case ChatGPT goes
  off script and hallucinates.

## Value

a list of dataframe(s).
