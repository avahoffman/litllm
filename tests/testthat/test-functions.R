test_that("ll_find_institution_type", {
  expect_equal(
    ll_find_institution_type("Johns Hopkins University", logical_check = "an institution with >5000 students"),
    tibble::tibble(
      name = "Johns Hopkins University",
      an_institution_with_5000_students = "TRUE"
    )
  )
})
