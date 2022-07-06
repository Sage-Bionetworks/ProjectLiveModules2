test_that("plotly_module_ui", {
  expect_type(plotly_module_ui("id"), "list")
})

data <-
  testthat::test_path("RDS", "data.rds") %>%
  readRDS() %>%
  purrr::pluck("Files")

config <- list("label_attribute" = "assay")

test_that("plotly_module_server", {
  shiny::testServer(
    plotly_module_server,
    args = list(
      "data" = shiny::reactive(data),
      "config" = shiny::reactive(config),
      "plot_type" = shiny::reactive("piechart")
    ),
    {
      session$setInputs("palette_choice" = "magma")
      expect_type(validated_config(), "list")
      expect_true(tibble::is_tibble(validated_data()))
      expect_type(output$palette_ui, "list")
      expect_type(plot_function(), "closure")
      expect_type(plot(), "list")
      expect_type(output$plot, "character")
    }
  )
})
