test_that("json_module_ui", {
  expect_type(json_module_ui("id"), "list")
})

test_that("json_module_no_json", {
  shiny::testServer(
    json_module_server,
    args = list(),
    {
      session$setInputs("config_method_choice" = "none")
      expect_error(config_list())
      expect_error(config_names())
      expect_error(output$config_selection_ui)
      expect_null(selected_config())
      expect_null(session$getReturned()())
    }
  )
})

test_that("json_module_with_json1", {
  shiny::testServer(
    json_module_server,
    args = list(),
    {
      session$setInputs("config_method_choice" = "upload")
      session$setInputs("json_upload"= list(datapath = "JSON/test.json"))
      session$setInputs("config_choice" = "Plot 1")
      expect_type(config_list(), "list")
      expect_length(config_list(), 3)
      expect_equal(config_names(), c("Plot 1", "Plot 2", "Data Table 1"))
      expect_type(output$config_selection_ui, "list")
      expect_equal(selected_config()$name, "Plot 1")
      expect_equal(selected_config()$entity, "Files")
      expect_true("barchart" %in% names(selected_config()))
      expect_type(session$getReturned()(), "list")
    }
  )
})

test_that("json_module_with_json2", {
  shiny::testServer(
    json_module_server,
    args = list(),
    {
      session$setInputs("config_method_choice" = "upload")
      session$setInputs("json_upload"= list(datapath = "JSON/test.json"))
      session$setInputs("config_choice" = "Data Table 1")
      expect_type(config_list(), "list")
      expect_length(config_list(), 3)
      expect_equal(config_names(), c("Plot 1", "Plot 2", "Data Table 1"))
      expect_type(output$config_selection_ui, "list")
      expect_equal(selected_config()$name, "Data Table 1")
      expect_equal(selected_config()$entity, "Files")
      expect_true("datatable" %in% names(selected_config()))
      expect_type(session$getReturned()(), "list")
    }
  )
})
