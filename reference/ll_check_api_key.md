# Check for the OpenAI API Key.

Check for the OpenAI API Key.

## Usage

``` r
ll_check_api_key()
```

## Value

A message.

## Examples

``` r
ll_check_api_key() # Lets you know if an API key is correctly stored for next steps
#> Error in ll_check_api_key(): No OpenAI API key detected. You'll need to generate one and add it to your R Environment. You can do this by entering `usethis::edit_r_environ()` and adding OPENAI_API_KEY=XXX to a new line.
```
