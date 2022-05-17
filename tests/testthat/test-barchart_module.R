test_that("barchart_module_ui", {
  expect_type(barchart_module_ui("id"), "list")
})

data <-
  testthat::test_path("RDS", "data.rds") %>%
  readRDS() %>%
  purrr::pluck("Files")

config <- list(
  "name" = "test_box_title",
  "x_attribute" = "assay"
)

test_that("barchart_module_server", {
  shiny::testServer(
    barchart_module_server,
    args = list(
      "data" = shiny::reactive(data),
      "config" = shiny::reactive(config)
    ),
    {
      expect_equal(box_title(), "test_box_title")
      expect_type(output$plot, "character")
    }
  )
})
