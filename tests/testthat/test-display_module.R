test_that("display_module_ui", {
  expect_type(display_module_ui("id"), "list")
})

data <-
  testthat::test_path("RDS", "data.rds") %>%
  readRDS()

config <-
  testthat::test_path("JSON", "test.json") %>%
  jsonlite::read_json() %>%
  purrr::pluck("plots", 1)



test_that("display_module_server", {
  shiny::testServer(
    display_module_server,
    args = list(
      "data" = shiny::reactive(data),
      "config" = shiny::reactive(config)
    ),
    {
      expect_true(tibble::is_tibble(selected_data()))
    }
  )
})

