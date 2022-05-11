data <-
  testthat::test_path("RDS", "data.rds") %>%
  readRDS() %>%
  purrr::pluck("Files")

test_that("create_barchart_config", {
  expect_equal(
    create_barchart_config("Files", "assay", "none", "none"),
    list(
      "type" = "barchart",
      "entity" = "Files",
      "x_attribute" = "assay"
    )
  )

  expect_equal(
    create_barchart_config("Files", "assay", "file", "none"),
    list(
      "type" = "barchart",
      "entity" = "Files",
      "x_attribute" = "assay",
      "group_attribute" = "file"
    )
  )

  expect_equal(
    create_barchart_config("Files", "assay", "none", "file"),
    list(
      "type" = "barchart",
      "entity" = "Files",
      "x_attribute" = "assay",
      "color_attribute" = "file"
    )
  )

})


test_that("create_standard_barchart", {
  fig <- create_standard_barchart(
    data,
    list("x_attribute" = "assay")
  )
  expect_type(fig, "list")
  print(fig)
})

test_that("create_stacked_barchart", {
  fig <- create_stacked_barchart(
    data,
    list(
      "x_attribute" = "assay",
      "color_attribute" = "file_format"
    )
  )
  expect_type(fig, "list")
  print(fig)
})

test_that("create_grouped_barchart", {
  fig <- create_grouped_barchart(
    data,
    list(
      "x_attribute" = "assay",
      "group_attribute" = "file_format"
    )
  )
  expect_type(fig, "list")
  print(fig)
})

test_that("create_stacked_grouped_barchart", {
  fig <- create_stacked_grouped_barchart(
    data,
    list(
      "x_attribute" = "assay",
      "group_attribute" = "file_format",
      "color_attribute" = "access_type"
    )
  )
  expect_type(fig, "list")
  print(fig)
})


test_that("create_plotly_barchart", {
  fig <- create_plotly_barchart(
    dplyr::tibble(
      "assay" = c("A1", "A2", "A1", "A2"),
      "file_format" = c("FF1", "FF1", "FF2", "FF2"),
      "Count" = c(10, 20, 30, 40)
    ),
    x_col = "assay",
    color_col = "file_format",
    y_col = "Count"
  )
  expect_type(fig, "list")
  print(fig)
})