test_that("datatable_module_ui", {
  expect_type(datatable_module_ui("id"), "list")
})

data <-
  testthat::test_path("RDS", "data.rds") %>%
  readRDS() %>%
  purrr::pluck("Files")

config <- list(
  "name" = "test_box_title"
)

test_that("datatable_module_server", {
  shiny::testServer(
    datatable_module_server,
    args = list(
      "data" = shiny::reactive(data),
      "config" = shiny::reactive(config)
    ),
    {
      expect_equal(box_title(), "test_box_title")
      expect_type(output$datatable, "character")
    }
  )
})
