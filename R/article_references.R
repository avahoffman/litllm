#' Mine ChatGPT for pdf metadata: works cited information.
#'
#' @param file_path string, A single pdf file or directory containing pdfs.
#' @param model string, which OpenAI ChatGPT model to use.
#' @param api_key string, the API key if you want to supply it manually. There is no free one, but used with a minimal model is very cheap. https://openai.com/api/pricing/.
#' @param clean_references logical, whether to output a cleaned list of dataframes. If FALSE, returns raw ChatGPT output. This can be useful in case ChatGPT goes off script and hallucinates.
#'
#' @return a list of dataframe(s).
#' @export
#'
#' @examples
ll_extract_references <- function(file_path,
                                  model = "gpt-5.4-mini",
                                  api_key = Sys.getenv("OPENAI_API_KEY"),
                                  clean_references = TRUE) {
  files <- ll_check_file_structure(file_path)
  ll_check_connection()
  cache_file <- "reference_list.rds"

  # Write file to save time and $$
  if (!file.exists(cache_file)) {
    reference_list <-
      mapply(
        ll_query,
        files,
        SIMPLIFY = FALSE,
        model = model,
        api_key = api_key,
        prompt = "Mine every reference made at the end of this document. Please return information in tabular format: each cited work should include Title, Author, Year, Journal, URL (if present), and DOI (if present). Use `\t` and `\n` notation, not markdown. Take any text that is all caps and use appropriate capitalization. Do not bold column headers. Return the full list of works/literature cited. Do not return any conversational chatter such as 'Let me know if you need anything else!'."
      )
    saveRDS(reference_list, file = cache_file)
    message("File written to: ", here::here(), "/", cache_file)
  } else {
    message(
      "Using cached version of reference_list previously generated at: ",
      here::here(),
      "/",
      cache_file
    )
    reference_list <- readRDS(cache_file)
    if (reference_list[1] == "reference_list") {
      stop(
        "There was a problem saving this file previously. Please delete the cached file and try running `ll_extract_references` again."
      )
    }
  }

  # Optional cleaning of output
  if (clean_references) {
    reference_output <-
      dplyr::bind_rows(lapply(reference_list, ll_clean_references), .id = "paper_id")
  } else {
    reference_output <- reference_list
  }

  return(reference_output)
}


#' Clean up title, year, and journal from ll_query-type output.
#'
#' @param x string, the whole response from ChatGPT
#'
#' @return a dataframe.
#'
#' @examples
ll_clean_references <- function(x) {
  if (is.null(x)) {
    stop("Check that each query is going / went through. Is the rate exceeding the limit?")
  }

  # Break up unstructured response into rows
  content_split_line <- stringr::str_split(x, "\n")[[1]]

  # Save column names and split the rest
  col_names_row <- stringr::str_split(content_split_line, "\t")[[1]]

  # Separate out the info by column
  temp_df <- data.frame(content = content_split_line[2:length(content_split_line)])
  temp_df <- tidyr::separate(temp_df, content, sep = "\t", into = tolower(col_names_row))

  return(temp_df)
}


ll_validate_reference_pubmed <- function(x){
  the_year <- as.numeric(x$year)
  the_author <- stringr::str_remove(x$author, "; et al.") # "et al" will break pubmed search
  pm_hit <- FALSE
  while(!pm_hit){
    # Check title, journal, year
    pm_query <- paste0(x$title)
    pm_res <- rentrez::entrez_search(db = "pubmed", term = pm_query, api_key = Sys.getenv("PUBMED_API_KEY"))$count
    if (pm_res > 0) break

    # Check title, journal, author
    pm_query <- paste0(x$title, " AND ", x$journal, "[Journal] AND ", the_author, "[Author]")
    pm_res <- rentrez::entrez_search(db = "pubmed", term = pm_query, api_key = Sys.getenv("PUBMED_API_KEY"))$count
    if (pm_res > 0) break

    #Check title, author, year
    pm_query <- paste0(x$title, " AND ", the_author, "[Author] AND ", the_year - 1, ":", the_year + 1, "[DP]")
    pm_res <- rentrez::entrez_search(db = "pubmed", term = pm_query, api_key = Sys.getenv("PUBMED_API_KEY"))$count
    if (pm_res > 0) break

    # Check journal, author, year
    pm_query <- paste0(the_author, "[Author] AND ", x$journal, "[Journal] AND ", the_year - 1, ":", the_year + 1, "[DP]")
    pm_res <- rentrez::entrez_search(db = "pubmed", term = pm_query, api_key = Sys.getenv("PUBMED_API_KEY"))$count
    break
  }

  if (pm_res == 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


ll_validate_reference_url <- function(x){
  # Check that the URL is valid
  tryCatch({
    get_res <- httr::GET(x$url)
    return(get_res$status_code)
  }, error = function(msg) {
    return(NA)
  })
}


ll_validate_references <- function(reference_df){
  reference_list <-
    split(reference_df, seq(nrow(reference_df)))

  pubmed_hit <-
    mapply(ll_validate_reference_pubmed, reference_list)

  url_response <-
    mapply(ll_validate_reference_url, reference_list)

  reference_df$pubmed_hit <- pubmed_hit
  reference_df$url_response <- url_response

  return(reference_df)
}


testfunc <- function(){
  # SCRATCH
  my_dat <- ll_extract_references(file = "Taylor_etal.pdf", clean_references = TRUE)
  my_dat_subset <- my_dat[7:12,]
  my_dat_validated <- ll_validate_references(my_dat)
  my_dat_validated

  my_dat_subset2 <- my_dat_validated[!(my_dat_validated$pubmed_hit),]
  my_dat_validated2 <- ll_validate_references(my_dat_subset2)
  my_dat_validated2

}
