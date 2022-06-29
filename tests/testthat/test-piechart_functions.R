data <-
  testthat::test_path("RDS", "data.rds") %>%
  readRDS() %>%
  purrr::pluck("Files")

test_that("create_piechart", {
  fig <- create_piechart(
    data,
    list("label_attribute" = "assay", "palette" = "viridis")
  )
  expect_type(fig, "list")
  print(fig)
})


test_that("create_plotly_piechart", {
  fig <- create_plotly_piechart(
    dplyr::tibble(
      "assay" = c("A1", "A2", "A1", "A2"),
      "Count" = c(10, 20, 30, 40)
    ),
    labels_col = "assay",
    values_col = "Count"
  )
  expect_type(fig, "list")
  print(fig)
})
