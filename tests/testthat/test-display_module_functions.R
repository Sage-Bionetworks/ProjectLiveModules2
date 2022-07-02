test_that("validate_plot_config", {
  expect_null(
    validate_plot_config(
      config = list("name" = "name", "entity" = "entity", "barchart" = {})
    )
  )
  expect_error(
    validate_plot_config(
      config = list("name" = "name", "entity" = "entity")
    ),
    "Config malformed: missing an allowed plot type"
  )
  expect_error(
    validate_plot_config(
      config = list("name" = "name", "entity" = "entity", "plot" = {})
    ),
    "Config malformed: missing an allowed plot type"
  )
  expect_error(
    validate_plot_config(
      config = list("entity" = "entity", "barchart" = {})
    ),
    "Config malformed: has no name"
  )
  expect_error(
    validate_plot_config(
      config = list("name" = "name", "barchart" = {})
    ),
    "Config malformed: has no entity"
  )
})
