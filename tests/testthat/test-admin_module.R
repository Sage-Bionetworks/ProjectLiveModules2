test_that("admin_module_ui", {
  expect_type(admin_module_ui("id"), "list")
})

data <-
  testthat::test_path("RDS", "data.rds") %>%
  readRDS() %>%
  purrr::pluck("Files")



test_that("admin_module_server_datatable", {
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
        list(
          "type" = "datatable",
          "entity" = "Files"
        )
      )
    }
  )
})

test_that("admin_module_server_barchart", {
  shiny::testServer(
    admin_module_server,
    args = list(
      "data" = shiny::reactive(data)
    ),
    {
      session$setInputs("display_choice" = "Barchart")
      session$setInputs("entity_choice" = "Files")
      session$setInputs("barchart-x_attribute" = "assay")
      session$setInputs("barchart-color_attribute" = "file_format")
      session$setInputs("barchart-group_attribute" = "year")

      expect_equal(
        session$getReturned()(),
        list(
          "type" = "barchart",
          "entity" = "Files",
          "x_attribute" = "assay",
          "group_attribute" = "year",
          "color_attribute" = "file_format"
        )
      )
    }
  )
})
