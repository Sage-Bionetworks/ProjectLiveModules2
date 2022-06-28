test_that("barchart_module_ui", {
  expect_type(barchart_module_ui("id"), "list")
})

data <-
  testthat::test_path("RDS", "data.rds") %>%
  readRDS() %>%
  purrr::pluck("Files")

config <- list(
  "x_attribute" = "assay",
  "pallete" = "viridis"
)

test_that("barchart_module_server", {
  shiny::testServer(
    barchart_module_server,
    args = list(
      "data" = shiny::reactive(data),
      "config" = shiny::reactive(config)
    ),
    {
      session$setInputs("palette_choice" = "magma")
      expect_type(validated_config(), "list")
      expect_true(tibble::is_tibble(validated_data()))
      expect_type(output$plot, "character")
    }
  )
})
