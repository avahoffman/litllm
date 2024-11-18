# These functions are meant to be shortcuts or are used by other functions.

#' Check for the OpenAI API Key.
#'
#' @return A message.
#' @export
#'
#' @examples ll_check_api_key() # Lets you know if an API key is correctly stored for next steps
ll_check_api_key <- function(){
  # Set with usethis::edit_r_environ()
  # api_key <- Sys.getenv("OPENAI_API_KEY") can be used to set manually if you really want to
  if(Sys.getenv("OPENAI_API_KEY") == ""){
    stop("No OpenAI API key detected. You'll need to generate one and add it to your R Environment. You can do this by entering `usethis::edit_r_environ()` and adding OPENAI_API_KEY=XXX to a new line.")
  } else {
    message("Looks like you have a key stored!🎉\nIf you are having issues later, the key could be entered incorrectly or may no longer be active on OpenAI's API interface.")
  }
}


#' Check for internet connection.
#'
#' @return Stop if no connection detected
#'
#' @examples check_connection()
ll_check_connection <- function(){
  # It's gonna be pretty hard to query an API without internet, but I feel like the error message is extremely opaque.
  if (!curl::has_internet()) {
    stop(
      "Please check your connection. Internet is required to make a query."
    )
  }
}
