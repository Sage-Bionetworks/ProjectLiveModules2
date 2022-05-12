test_that("admin_datatable_module_ui", {
  expect_type(admin_datatable_module_ui("id"), "list")
})

data <-
  testthat::test_path("RDS", "data.rds") %>%
  readRDS() %>%
  purrr::pluck("Files")

expected_config <- list(
  "type" = "datatable",
  "entity" = "Files"
)

test_that("admin_datatable_module_server", {
  shiny::testServer(
    admin_datatable_module_server,
    args = list(
      "data" = shiny::reactive(data),
      "entity" = shiny::reactive("Files"),
      "config" = shiny::reactive(NULL)
    ),
    {
      expect_equal(
        session$getReturned()(),
        expected_config
      )
    }
  )
})
