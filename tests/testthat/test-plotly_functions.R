config1 <- list(
  "attribute1" = "a",
  "attribute2" = "b"
)

test_that("validate_plotly_config", {

  expect_null(validate_plotly_config(
    config = config1,
    required_attrbutes = "attribute1",
    optional_attributes = "attribute2"
  ))

  expect_null(validate_plotly_config(
    config = config1,
    required_attrbutes = c("attribute1",  "attribute2")
  ))

  expect_error(
    validate_plotly_config(config = c()),
    "Config malformed: is NULL"
  )

  expect_error(
    validate_plotly_config(config = NULL),
    "Config malformed: is NULL"
  )

  expect_error(
    validate_plotly_config(config = list()),
    "Config malformed: is empty"
  )

  expect_error(
    validate_plotly_config(config = NA),
    "Config malformed: is NA"
  )

  expect_error(
    validate_plotly_config(c("x")),
    "Config malformed: is not named"
  )

  expect_error(
    validate_plotly_config(
      config = config1,
      required_attrbutes = "attribute3"
    ),
    "Config malformed: missing required attributes;"
  )

  expect_error(
    validate_plotly_config(
      config = config1
    ),
    "Config malformed: extraneous attributes;"
  )

})


test_that("validate_plotly_data", {

  expect_null(validate_plotly_data(
    dplyr::tibble("x" = 1)
  ))

  expect_error(
    validate_plotly_data(NULL),
    "Data malformed: is NULL"
  )

  expect_error(
    validate_plotly_data(NA),
    "Data malformed: is not a dataframe"
  )

  expect_error(
    validate_plotly_data(list("x" = 1)),
    "Data malformed: is not a dataframe"
  )

  expect_error(
    validate_plotly_data(dplyr::tibble()),
    "Data malformed: has no columns"
  )

  expect_error(
    validate_plotly_data(
      dplyr::slice(dplyr::tibble("x" = 1), 0)
    ),
    "Data malformed: has no rows"
  )

})
