test_that("get_value_from_list", {
  lst <- list("name" = "value")

  expect_equal(
    get_value_from_list(lst, "name"),
    "value"
  )

  expect_true(is.na(
    get_value_from_list(NULL, "name")
  ))

  expect_equal(
    get_value_from_list(NULL, "name", "default"),
    "default"
  )
})
