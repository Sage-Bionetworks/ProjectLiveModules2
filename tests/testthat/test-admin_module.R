test_that("admin_module_ui", {
  expect_type(admin_module_ui("id"), "list")
})

data <-
  testthat::test_path("RDS", "data.rds") %>%
  readRDS()

expected_barchart_config <-  list(
  "type" = "barchart",
  "entity" = "Files",
  "name" = "Plot 1",
  "x_attribute" = "assay",
  "group_attribute" = "year",
  "color_attribute" = "file_format"
)

expected_datatable_config <- list(
  "type" = "datatable",
  "entity" = "Files",
  "name" = "Data Table 1"
)

test_that("admin_module_server_barchart_no_json_input", {
  shiny::testServer(
    admin_module_server,
    args = list(
      "data" = shiny::reactive(data)
    ),
    {
      session$setInputs("json-config_method_choice" = "none")
      session$setInputs("json-config_choice" = "Plot 1")
      session$setInputs("display_choice" = "barchart")
      session$setInputs("entity_choice" = "Files")
      session$setInputs("name_choice" = "Plot 1")
      session$setInputs("barchart-x_attribute-attribute_choice" = "assay")
      session$setInputs("barchart-color_attribute-attribute_choice" = "file_format")
      session$setInputs("barchart-group_attribute-attribute_choice" = "year")

      # other input
      expect_equal(name_selection_default(), "")
      expect_type(output$name_selection_ui, "list")
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
      session$setInputs("json-config_method_choice" = "upload")
      session$setInputs("json-json_upload"= list(datapath = "JSON/test.json"))
      session$setInputs("json-config_choice" = "Plot 1")
      session$setInputs("display_choice" = "barchart")
      session$setInputs("entity_choice" = "Files")
      session$setInputs("name_choice" = "Plot 1")
      session$setInputs("barchart-x_attribute-attribute_choice" = "assay")
      session$setInputs("barchart-color_attribute-attribute_choice" = "file_format")
      session$setInputs("barchart-group_attribute-attribute_choice" = "year")

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
      session$setInputs("json-config_method_choice" = "upload")
      session$setInputs("json-json_upload"= list(datapath = "JSON/test.json"))
      session$setInputs("json-config_choice" = "Data Table 1")
      session$setInputs("display_choice" = "datatable")
      session$setInputs("entity_choice" = "Files")
      session$setInputs("name_choice" = "Data Table 1")

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

