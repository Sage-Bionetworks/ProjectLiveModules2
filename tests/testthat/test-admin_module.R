test_that("admin_module_ui", {
  expect_type(admin_module_ui("id"), "list")
})

data <-
  testthat::test_path("RDS", "data.rds") %>%
  readRDS() %>%
  purrr::pluck("Files")

expected_config <- list(
  "type" = "datatable",
  "entity" = "Files"
)

test_that("admin_module_server", {
  shiny::testServer(
    admin_module_server,
    args = list(
      "data" = shiny::reactive(data)
    ),
    {
      session$setInputs("display_choice" = "Data Table")
      session$setInputs("entity_choice" = "Files")

      expect_equal(
        session$getReturned()(),
        expected_config
      )
    }
  )
})
