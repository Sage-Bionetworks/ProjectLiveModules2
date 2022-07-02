test_that("admin_module_ui", {
  expect_type(admin_module_ui("id"), "list")
})

data <-
  testthat::test_path("RDS", "data.rds") %>%
  readRDS()



expected_barchart_config1 <-  list(
  "x_attribute" = "assay",
  "group_attribute" = "year",
  "color_attribute" = "file_format"
)

expected_barchart_config2 <-  list(
  "entity" = "Files",
  "name" = "Barchart 1",
  "barchart" = expected_barchart_config1
)

test_that("admin_module_server_barchart_no_json_input", {
  shiny::testServer(
    admin_module_server,
    args = list(
      "data" = shiny::reactive(data)
    ),
    {
      session$setInputs("json-config_method_choice" = "none")
      session$setInputs("json-config_choice" = "Barchart 1")
      session$setInputs("display_choice-attribute_choice" = "barchart")
      session$setInputs("entity_choice-attribute_choice" = "Files")
      session$setInputs("name_choice" = "Barchart 1")
      session$setInputs("barchart-x_attribute-attribute_choice" = "assay")
      session$setInputs(
        "barchart-color_attribute-attribute_choice" = "file_format"
      )
      session$setInputs("barchart-group_attribute-attribute_choice" = "year")

      # other input
      expect_equal(name_selection_default(), "")
      expect_type(output$name_selection_ui, "list")

      expect_equal(display_choice(), "barchart")
      expect_equal(entity_choice(), "Files")

      # rest
      expect_true(tibble::is_tibble(selected_data()))
      expect_equal(plot_config(), expected_barchart_config1)
      expect_equal(output_config(), expected_barchart_config2)
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
      session$setInputs("json-json_upload" = list(datapath = "JSON/test.json"))
      session$setInputs("json-config_choice" = "Barchart 1")
      session$setInputs("display_choice-attribute_choice" = "barchart")
      session$setInputs("entity_choice-attribute_choice" = "Files")
      session$setInputs("name_choice" = "Barchart 1")
      session$setInputs("barchart-x_attribute-attribute_choice" = "assay")
      session$setInputs(
        "barchart-color_attribute-attribute_choice" = "file_format"
      )
      session$setInputs("barchart-group_attribute-attribute_choice" = "year")

      # other input
      expect_equal(name_selection_default(), "Barchart 1")
      expect_type(output$name_selection_ui, "list")

      expect_equal(entity_choice(), "Files")
      expect_equal(display_choice(), "barchart")
      expect_true(tibble::is_tibble(plot_function_row()))
      expect_equal(plot_function_row()$plot_type, "barchart")
      expect_equal(nrow(plot_function_row()), 1)
      expect_type(ui_module(), "closure")


      # rest
      expect_true(tibble::is_tibble(selected_data()))
      expect_equal(plot_config(), expected_barchart_config1)
      expect_equal(output_config(), expected_barchart_config2)
    }
  )
})

expected_piechart_config1 <-  list(
  "label_attribute" = "assay"
)

expected_piechart_config2 <-  list(
  "entity" = "Files",
  "name" = "Piechart 1",
  "piechart" = expected_piechart_config1
)

test_that("admin_module_server_piechart_with_json_input", {
  shiny::testServer(
    admin_module_server,
    args = list(
      "data" = shiny::reactive(data)
    ),
    {
      session$setInputs("json-config_method_choice" = "upload")
      session$setInputs("json-json_upload" = list(datapath = "JSON/test.json"))
      session$setInputs("json-config_choice" = "Piechart 1")
      session$setInputs("display_choice-attribute_choice" = "piechart")
      session$setInputs("entity_choice-attribute_choice" = "Files")
      session$setInputs("name_choice" = "Piechart 1")
      session$setInputs("piechart-label_attribute-attribute_choice" = "assay")

      # other input
      expect_equal(name_selection_default(), "Piechart 1")
      expect_type(output$name_selection_ui, "list")

      expect_equal(display_choice(), "piechart")
      expect_equal(entity_choice(), "Files")
      expect_true(tibble::is_tibble(plot_function_row()))
      expect_equal(plot_function_row()$plot_type, "piechart")
      expect_equal(nrow(plot_function_row()), 1)
      expect_type(ui_module(), "closure")

      # rest
      expect_true(tibble::is_tibble(selected_data()))
      expect_equal(plot_config(), expected_piechart_config1)
      expect_equal(output_config(), expected_piechart_config2)
    }
  )
})

expected_datatable_config1 <- list()

expected_datatable_config2 <- list(
  "entity" = "Files",
  "name" = "Data Table 1",
  "datatable" = expected_datatable_config1
)

test_that("admin_module_server_datatable_with_json_input", {
  shiny::testServer(
    admin_module_server,
    args = list(
      "data" = shiny::reactive(data)
    ),
    {
      session$setInputs("json-config_method_choice" = "upload")
      session$setInputs("json-json_upload" = list(datapath = "JSON/test.json"))
      session$setInputs("json-config_choice" = "Data Table 1")
      session$setInputs("display_choice-attribute_choice" = "datatable")
      session$setInputs("entity_choice-attribute_choice" = "Files")
      session$setInputs("name_choice" = "Data Table 1")

      # other input
      expect_equal(display_choice(), "datatable")
      expect_equal(entity_choice(), "Files")
      expect_true(tibble::is_tibble(plot_function_row()))
      expect_equal(plot_function_row()$plot_type, "datatable")
      expect_equal(nrow(plot_function_row()), 1)
      expect_type(ui_module(), "closure")

      # rest
      expect_true(tibble::is_tibble(selected_data()))
      expect_equal(plot_config(), expected_datatable_config1)
      expect_equal(output_config(), expected_datatable_config2)
    }
  )
})
