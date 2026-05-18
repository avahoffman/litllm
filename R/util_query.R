#' Perform a query against OpenAI's API.
#'
#' @details
#' This function sends text content (either extracted from a PDF file or direct
#' text input) to OpenAI's ChatGPT API along with a user-specified prompt. It
#' handles file reading, API communication, and rate limiting.
#'
#' See \url{https://openai.com/api/pricing/} for information on different models.
#'
#' @param source string, either a file path to a PDF or a string of text that
#'   data will be mined from. Examples: "pdf/journal_article.pdf" or
#'   "Here is a very long string of text I would like to query directly"
#' @param prompt string, a prompt for ChatGPT.
#' @param model string, which OpenAI ChatGPT model to use. Default is
#'   "gpt-5.4-mini". See: \url{https://platform.openai.com/docs/models}
#' @param api_key string, the OpenAI API key. Defaults to the OPENAI_API_KEY
#'   environment variable. There is no free tier, but minimal model usage is
#'   very affordable. See: \url{https://openai.com/api/pricing/}
#'
#' @return string, the raw text response content from the OpenAI API
#' @export
#'
#' @examples
#' \dontrun{
#' # Query a PDF file
#' result1 <- ll_query("research_paper.pdf", "Summarize the main findings")
#'
#' # Query direct text
#' result2 <- ll_query(
#'    "The explosion in genomic data has revolutionized biomedical science.",
#'    "What is the topic?"
#' )
#'
#' # Use a different model
#' result3 <- ll_query("research_paper.pdf", "Extract key findings", model = "gpt-5-mini")
#' }
#'
#' @note This function depends on functions \code{\link{ll_check_connection()}}
#'    and \code{\link{ll_journal_corrections()}}.
#'
#' @seealso
#'
#' \url{https://openai.com/api/pricing/} for model pricing information
#'
#' \url{https://platform.openai.com/docs/models} for model selection
ll_query <- function(source,
                     prompt,
                     model = "gpt-5.4-mini",
                     api_key = Sys.getenv("OPENAI_API_KEY")) {
  # Record start time for rate limiting and performance tracking
  start <- Sys.time()

  # Check internet connection
  ll_check_connection()

  # ===== TEXT SOURCE =====
  # Collect text from a pdf source. Otherwise, treat the string as a literal
  # source as-is
  if (file.exists(source)) {
    message(c("Querying: ", source))
    text <- suppressMessages(pdftools::pdf_text(source))
  } else {
    text <- source
  }

  # Prep text for query -- combine all text pages/elements and apply
  # journal-specific corrections
  text_processed <- ll_journal_corrections(stringr::str_c(text, collapse = "\\n"))

  # ===== API REQUEST =====
  # Construct the request body according to OpenAI Chat Completions API format
  # See: https://platform.openai.com/docs/api-reference/chat/create
  body <-
    list(model = model, messages = list(list(
      role = "user",
      content = stringr::str_c(prompt, text_processed)
    )))

  # Send POST request to OpenAI Chat Completions endpoint
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    body = body,
    encode = "json",
    httr::add_headers(
      `Authorization` = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    )
  )

  # Parse the JSON response from the API
  content <- httr::content(response, "parsed")
  content_out <- content$choices[[1]]$message$content

  # ===== RATE LIMITING =====
  end <- Sys.time()
  total <- end - start
  # Sleep a bit to meet rate limitations
  # TODO: make n an argument to this function (sometimes 6 is overkill)
  n <- 6 # Minimum seconds between requests
  if (total < n) {
    Sys.sleep(n - total)
    total <- n
  }
  message(c(source, " --- duration: ", total))

  # Return the AI-generated response text
  return(content_out)
}

