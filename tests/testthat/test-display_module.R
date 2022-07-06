test_that("display_module_ui", {
  expect_type(display_module_ui("id"), "list")
})

data <-
  testthat::test_path("RDS", "data.rds") %>%
  readRDS() %>%
  purrr::pluck("Files")

configs <-
  testthat::test_path("JSON", "test.json") %>%
  jsonlite::read_json() %>%
  purrr::pluck("plots")

config1 <- configs[[1]]
config2 <- configs[[2]]
config3 <- configs[[3]]

test_that("display_module_server1", {
  shiny::testServer(
    display_module_server,
    args = list(
      "data" = shiny::reactive(data),
      "config" = shiny::reactive(config1)
    ),
    {
      expect_type(validated_config(), "list")
      expect_type(validated_data(), "list")

      expect_equal(box_title(), "Barchart 1")
      expect_equal(plot_type(), "barchart")
      expect_equal(plot_config(), list("x_attribute" = "initiative"))
      expect_true(tibble::is_tibble(plot_function_row()))
      expect_equal(plot_function_row()$plot_type, "barchart")
      expect_equal(nrow(plot_function_row()), 1)
      expect_type(ui_module(), "closure")
    }
  )
})

test_that("display_module_server2", {
  shiny::testServer(
    display_module_server,
    args = list(
      "data" = shiny::reactive(data),
      "config" = shiny::reactive(config2)
    ),
    {
      expect_equal(box_title(), "Piechart 1")
      expect_equal(plot_type(), "piechart")
      expect_equal(plot_config(), list("label_attribute" = "initiative"))
      expect_true(tibble::is_tibble(plot_function_row()))
      expect_equal(plot_function_row()$plot_type, "piechart")
      expect_equal(nrow(plot_function_row()), 1)
      expect_type(ui_module(), "closure")
    }
  )
})

test_that("display_module_server3", {
  shiny::testServer(
    display_module_server,
    args = list(
      "data" = shiny::reactive(data),
      "config" = shiny::reactive(config3)
    ),
    {
      expect_equal(box_title(), "Data Table 1")
      expect_equal(plot_type(), "datatable")
      expect_length(plot_config(), 0)
      expect_true(tibble::is_tibble(plot_function_row()))
      expect_equal(plot_function_row()$plot_type, "datatable")
      expect_equal(nrow(plot_function_row()), 1)
      expect_type(ui_module(), "closure")
    }
  )
})
