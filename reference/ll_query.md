# Perform a query against OpenAI's API.

Perform a query against OpenAI's API.

## Usage

``` r
ll_query(
  source,
  prompt,
  model = "gpt-5.4-mini",
  api_key = Sys.getenv("OPENAI_API_KEY")
)
```

## Arguments

- source:

  string, either a file path to a PDF or a string of text that data will
  be mined from. Examples: "pdf/journal_article.pdf" or "Here is a very
  long string of text I would like to query directly"

- prompt:

  string, a prompt for ChatGPT.

- model:

  string, which OpenAI ChatGPT model to use. Default is "gpt-5.4-mini".
  See: <https://platform.openai.com/docs/models>

- api_key:

  string, the OpenAI API key. Defaults to the OPENAI_API_KEY environment
  variable. There is no free tier, but minimal model usage is very
  affordable. See: <https://openai.com/api/pricing/>

## Value

string, the raw text response content from the OpenAI API

## Details

This function sends text content (either extracted from a PDF file or
direct text input) to OpenAI's ChatGPT API along with a user-specified
prompt. It handles file reading, API communication, and rate limiting.

See <https://openai.com/api/pricing/> for information on different
models.

## Note

This function depends on functions
[`ll_check_connection()`](http://www.avahoffman.com/litllm/reference/ll_check_connection.md)
and
[`ll_journal_corrections()`](http://www.avahoffman.com/litllm/reference/ll_journal_corrections.md).

## See also

<https://openai.com/api/pricing/> for model pricing information

<https://platform.openai.com/docs/models> for model selection

## Examples

``` r
if (FALSE) { # \dontrun{
# Query a PDF file
result1 <- ll_query("research_paper.pdf", "Summarize the main findings")

# Query direct text
result2 <- ll_query(
   "The explosion in genomic data has revolutionized biomedical science.",
   "What is the topic?"
)

# Use a different model
result3 <- ll_query("research_paper.pdf", "Extract key findings", model = "gpt-5-mini")
} # }
```
