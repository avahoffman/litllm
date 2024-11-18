#' Perform a query against OpenAI's API.
#'
#' See https://openai.com/api/pricing/ for information on different models.
#'
#' @param source string, either a file or string of text that data will be mined
#' from. "pdf/journal_article.pdf" or "Here is a very long string of text I
#' would like to query directly"
#' @param prompt string, a prompt for ChatGPT.
#' @param model string, which OpenAI ChatGPT model to use.
#' @param api_key string, the API key if you want to supply it manually. There is no free one, but used with a minimal model is very cheap. https://openai.com/api/pricing/.
#'
#' @return Raw content from API
#' @export
#'
#' @examples
ll_query <- function(source,
                     prompt,
                     model = "gpt-4o-mini",
                     api_key = Sys.getenv("OPENAI_API_KEY")) {
  start <- Sys.time()
  ll_check_connection()

  # Collect text from a pdf source. Otherwise, treat the string as a literal source itself
  if (file.exists(source)) {
    message(c("Querying: ", source))
    text <- suppressMessages(pdftools::pdf_text(source))
  } else {
    text <- source
  }
  text_processed <- ll_journal_corrections(stringr::str_c(text, collapse = "\\n"))

  # Create the full query
  body <-
    list(model = model, messages = list(list(
      role = "user",
      content = stringr::str_c(prompt, text_processed)
    )))

  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    body = body,
    encode = "json",
    httr::add_headers(
      `Authorization` = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    )
  )

  content <- httr::content(response, "parsed")
  content_out <- content$choices[[1]]$message$content

  end <- Sys.time()
  total <- end - start
  message(c(source, " duration: ", total))

  return(content_out)
}


#' Remove some journal specific stuff.
#'
#' @param x Collapsed output from pdftools that needs to be cleaned up
#'
#' @return Cleaned string.
#' @export
#'
#' @examples
ll_journal_corrections <- function(x) {
  x <- stringr::str_remove(x, "Downloaded from www.annualreviews.org. .*")
  return(x)
}


#' Clean up authors and title from ll_query-type output.
#'
#' @param x string, the whole response from ChatGPT
#'
#' @return a dataframe.
#'
#' @examples
ll_clean_authors <- function(x) {
  if(is.null(x)){
    stop("Check that each query is going through. Is the rate exceeding the limit?")
  }

  # Break up unstructured response
  content_split_line <- stringr::str_split(x, "\n")[[1]]

  # Pull out author and title chunks based on "Title" and "Author(s)" headings
  authors_chunk <-
    stringr::str_trim(content_split_line[-c(1:which(stringr::str_detect(content_split_line, "Author\\(s\\):")))])

  title_chunk <-
    stringr::str_trim(stringr::str_match(content_split_line, "(?<=Title:).*"))
  if ("**" %in% title_chunk) {
    # Sometimes ChatGPT refuses to behave and will bold the Title and break line anyway
    title_chunk <-
      str_trim(content_split_line[which(str_detect(content_split_line, "(?<=Title:).*")) + 1])
  }
  # Remove any NA lines and give the same title for each author in the item
  title_chunk <- title_chunk[!is.na(title_chunk)]
  title_chunk <- rep(title_chunk, length(authors_chunk))

  # Combine everything in a nice DF
  return(
    data.frame(title = title_chunk, auth = authors_chunk)
  )
}


#' Mine ChatGPT for pdf metadata: Title, Authors, and Institutions.
#'
#' @param file_path string, A single pdf file or directory containing pdfs.
#' @param model string, which OpenAI ChatGPT model to use.
#' @param api_key string, the API key if you want to supply it manually. There is no free one, but used with a minimal model is very cheap. https://openai.com/api/pricing/.
#' @param clean_authors logical, whether to output a cleaned dataframe of authors. If FALSE, returns raw ChatGPT output. This can be useful in case ChatGPT goes off script and hallucinates.
#'
#' @return a dataframe.
#' @export
#'
#' @examples
ll_extract_authors <- function(file_path,
                               model = "gpt-4o-mini",
                               api_key = Sys.getenv("OPENAI_API_KEY"),
                               clean_authors = TRUE) {
  # I looked into parallelizing this, but ran into rate limitation. So probably just best to let this run and be patient :)

  # Check first, is file_path a directory?
  toggle_dir <- dir.exists(file_path)

  if (!toggle_dir & !file.exists(file_path)) {
    stop(
      "Please supply a file (e.g., 'paper.pdf') or existing directory (e.g., 'pdf/') for the `file_path` argument."
    )
  }
  if (toggle_dir & length(list.files(file_path)) == 0) {
    stop("Please check that the directory specified contains pdfs to process.")
  }
  if (toggle_dir) {
    files <- paste0(file_path, list.files(file_path))
    if (!file.exists(files[1])) {
      stop("Please format your directory like 'pdf/' with a trailing `/`.")
    }
  } else {
    files <- file_path
  }

  ll_check_connection()

  # Write file to save time and $$
  if (!file.exists("author_list.rds")) {
    author_list <-
      mapply(
        ll_query,
        files,
        SIMPLIFY = FALSE,
        model = model,
        api_key = api_key,
        prompt = "Can you extract the article title and author information (names and institutions)? Please print 'Title:' followed by the title. Then, print 'Author(s):' followed by each author, institution combination as a separate line. Take any text that is all caps and use appropriate capitalization. Do not bold 'Title:' or 'Author(s):'."
      )
    save(author_list, file = "author_list.rds")
    message("File written to: ", here::here(), "/author_list.rds")
  } else {
    message(
      "Using cached version of author_list previously generated at: ",
      here::here(),
      "/author_list.rds"
    )
    load("author_list.rds")[[1]]
    if(author_list == "author_list"){
      stop("There was a problem saving this file previously. Please delete the cached file and try running `ll_extract_authors` again.")
    }
  }

  # Optional cleaning of output
  if(clean_authors){
    authors_output <-
      dplyr::bind_rows(lapply(author_list, ll_clean_authors), .id = "paper_id")
  } else {
    authors_output <- author_list
  }

  return(authors_output)
}


#' Extract "Institutions" from a messy vector using ChatGPT.
#'
#' @param text A vector of strings containing messy data.
#' @param model string, which OpenAI ChatGPT model to use.
#' @param api_key string, the API key if you want to supply it manually. There is no free one, but used with a minimal model is very cheap. https://openai.com/api/pricing/.
#'
#' @return A vector containing clean data.
#' @export
#'
#' @examples
ll_pull_institutions <- function(text,
                                 model = "gpt-4o-mini",
                                 api_key = Sys.getenv("OPENAI_API_KEY")) {
  ll_check_connection()

  if (!is.character(text)) {
    stop("Please supply a character type vector or variable.")
  }

  institutions <-
    mapply(
      ll_query,
      text,
      SIMPLIFY = FALSE,
      model = model,
      api_key = api_key,
      prompt = "Please extract just the institution, such as a university or organization. Response can be NA if the institution is unclear. Please do not include any other text in the response."
    )

  return(unlist(institutions))
}


#' Perform logical checks on a vector of institutions using ChatGPT.
#'
#' For example, we might be interested in checking whether author institutions are a nonprofit, private company, or university.
#'
#' @param text A vector of strings containing cleaned institutions. These might be the output of `ll_pull_institutions`.
#' @param model string, which OpenAI ChatGPT model to use.
#' @param api_key string, the API key if you want to supply it manually. There is no free one, but used with a minimal model is very cheap. https://openai.com/api/pricing/.
#' @param logical_check string, a phrase that can be answered "Yes" or "no" when combined with "Is this institution...". For example, "in the United States" or "a Minority Serving Institution".
#'
#' @return A data frame containing T/F data for each item in `text`.
#' @export
#'
#' @examples ll_check_institution_type("Johns Hopkins University", logical_check = "an institution with >5000 students")
ll_check_institution_type <- function(text,
                                      model = "gpt-4o-mini",
                                      api_key = Sys.getenv("OPENAI_API_KEY"),
                                      logical_check = "in the United States") {
  ll_check_connection()

  if (!is.character(text)) {
    stop("Please supply a character type vector or variable.")
  }

  prompt_ <- paste0(
    "Please print TRUE if this institution is ",
    logical_check,
    ". Please print FALSE if this institution is not ",
    logical_check,
    ". Please only print TRUE or FALSE. Do not include any other text in the response."
  )

  institutions <-
    mapply(
      ll_query,
      text,
      SIMPLIFY = FALSE,
      model = model,
      api_key = api_key,
      prompt = prompt_
    )

  out_df <- tibble::enframe(unlist(institutions))
  colnames(out_df)[2] <- logical_check
  out_df <- janitor::clean_names(out_df)

  return(out_df)
}
