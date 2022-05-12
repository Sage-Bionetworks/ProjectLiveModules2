test_that("admin_module_ui", {
  expect_type(admin_module_ui("id"), "list")
})

data <-
  testthat::test_path("RDS", "data.rds") %>%
  readRDS()

mock_config_list <-
  testthat::test_path("JSON", "test.json") %>%
  jsonlite::read_json() %>%
  purrr::pluck("plots")

expected_barchart_config <-  list(
  "type" = "barchart",
  "entity" = "Files",
  "x_attribute" = "assay",
  "group_attribute" = "year",
  "color_attribute" = "file_format"
)

expected_datatable_config <- list(
  "type" = "datatable",
  "entity" = "Files"
)

test_that("admin_module_server_barchart_no_json_input", {
  shiny::testServer(
    admin_module_server,
    args = list(
      "data" = shiny::reactive(data)
    ),
    {
      session$setInputs("config_method_choice" = "none")
      session$setInputs("config_choice" = "Plot 1")
      session$setInputs("display_choice" = "barchart")
      session$setInputs("entity_choice" = "Files")
      session$setInputs("barchart-x_attribute" = "assay")
      session$setInputs("barchart-color_attribute" = "file_format")
      session$setInputs("barchart-group_attribute" = "year")

      # other input
      expect_true(is.na(display_selection_default()))
      expect_type(output$display_selection_ui, "list")
      expect_true(is.na(entity_selection_default()))
      expect_type(output$entity_selection_ui, "list")

      # rest
      expect_true(tibble::is_tibble(selected_data()))
      expect_equal(barchart_config(), expected_barchart_config)
      expect_equal(config(), expected_barchart_config)
      expect_type(json_config(), "character")
      expect_equal(session$getReturned()(), expected_barchart_config)
    }
  )
})

test_that("admin_module_server_barchart_with_json_input", {
  shiny::testServer(
    admin_module_server,
    args = list(
      "data" = shiny::reactive(data)
    ),
    {
      session$setInputs("config_method_choice" = "upload")
      session$setInputs("json_upload"= list(datapath = "JSON/test.json"))
      session$setInputs("config_choice" = "Plot 1")
      session$setInputs("display_choice" = "barchart")
      session$setInputs("entity_choice" = "Files")
      session$setInputs("barchart-x_attribute" = "assay")
      session$setInputs("barchart-color_attribute" = "file_format")
      session$setInputs("barchart-group_attribute" = "year")

      # JSON input
      expect_equal(input_config_names(), c("Plot 1", "Plot 2", "Data Table 1"))
      expect_type(output$config_selection_ui, "list")
      expect_equal(selected_input_config()$name, "Plot 1")
      expect_equal(selected_input_config()$type, "barchart")
      expect_equal(selected_input_config()$entity, "Files")

      # other input
      expect_equal(display_selection_default(), "barchart")
      expect_type(output$display_selection_ui, "list")
      expect_equal(entity_selection_default(), "Files")
      expect_type(output$entity_selection_ui, "list")

      # rest
      expect_true(tibble::is_tibble(selected_data()))
      expect_equal(barchart_config(), expected_barchart_config)
      expect_equal(config(), expected_barchart_config)
      expect_type(json_config(), "character")
      expect_equal(session$getReturned()(), expected_barchart_config)
    }
  )
})

test_that("admin_module_server_datatable_with_json_input", {
  shiny::testServer(
    admin_module_server,
    args = list(
      "data" = shiny::reactive(data)
    ),
    {
      session$setInputs("config_method_choice" = "upload")
      session$setInputs("json_upload"= list(datapath = "JSON/test.json"))
      session$setInputs("config_choice" = "Data Table 1")
      session$setInputs("display_choice" = "datatable")
      session$setInputs("entity_choice" = "Files")

      # JSON input
      expect_equal(input_config_names(), c("Plot 1", "Plot 2", "Data Table 1"))
      expect_type(output$config_selection_ui, "list")
      expect_equal(selected_input_config()$name, "Data Table 1")
      expect_equal(selected_input_config()$type, "datatable")
      expect_equal(selected_input_config()$entity, "Files")

      # other input
      expect_equal(display_selection_default(), "datatable")
      expect_type(output$display_selection_ui, "list")
      expect_equal(entity_selection_default(), "Files")
      expect_type(output$entity_selection_ui, "list")

      # rest
      expect_true(tibble::is_tibble(selected_data()))
      expect_equal(datatable_config(), expected_datatable_config)
      expect_equal(config(), expected_datatable_config)
      expect_type(json_config(), "character")
      expect_equal(session$getReturned()(), expected_datatable_config)
    }
  )
})




