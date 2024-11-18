test_that("ll_check_institution_type", {
  expect_equal(
    ll_check_institution_type("Johns Hopkins University", logical_check = "an institution with >5000 students"),
    tibble(
      name = "Johns Hopkins University",
      an_institution_with_5000_students = "TRUE"
    )
  )
})
