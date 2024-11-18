ll_setup <- function(load=FALSE) {
  if(load){
    # Run functions to test the package
    devtools::document()
    devtools::load_all()
    testthat::test_local()

    # Add a package dependency
    # usethis::use_package("janitor", "Imports")
  }
}



