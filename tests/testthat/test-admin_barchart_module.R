test_that("admin_barchart_module_ui", {
  expect_type(admin_barchart_module_ui("id"), "list")
})

data <-
  testthat::test_path("RDS", "data.rds") %>%
  readRDS() %>%
  purrr::pluck("Files")


test_that("admin_barchart_module_server_no_group_or_color", {
  shiny::testServer(
    admin_barchart_module_server,
    args = list(
      "data" = shiny::reactive(data),
      "input_config" = shiny::reactive(NULL),
      "entity" = shiny::reactive("Files"),
      "name" = shiny::reactive("Plot 1")
    ),
    {

      session$setInputs("x_attribute" = "assay")
      session$setInputs("color_attribute" = "none")
      session$setInputs("group_attribute" = "none")

      expect_type(column_choices(), "character")

      expect_true(is.na(x_attribute_default()))
      expect_type(output$x_attribute_ui, "list")
      expect_true(is.na(color_attribute_default()))
      expect_type(output$color_attribute_ui, "list")
      expect_true(is.na(group_attribute_default()))
      expect_type(output$group_attribute_ui, "list")


      expect_type(output$x_attribute_ui, "list")
      expect_type(output$color_attribute_ui, "list")
      expect_type(output$group_attribute_ui, "list")

      expect_equal(
        session$getReturned()(),
        list(
          "type" = "barchart",
          "entity" = "Files",
          "name" = "Plot 1",
          "x_attribute" = "assay"
        )
      )
    }
  )
})

barchart_config1 <-  list(
  "name" = "Plot 1",
  "type" = "barchart",
  "entity" = "Files",
  "x_attribute" = "assay"
)

test_that("admin_barchart_module_server_no_group_or_color_with_config", {
  shiny::testServer(
    admin_barchart_module_server,
    args = list(
      "data" = shiny::reactive(data),
      "input_config" = shiny::reactive(barchart_config1),
      "entity" = shiny::reactive("Files"),
      "name" = shiny::reactive("Plot 1")
    ),
    {

      session$setInputs("x_attribute" = "assay")
      session$setInputs("color_attribute" = "none")
      session$setInputs("group_attribute" = "none")

      expect_type(column_choices(), "character")

      expect_equal(x_attribute_default(), "assay")
      expect_type(output$x_attribute_ui, "list")
      expect_true(is.na(color_attribute_default()))
      expect_type(output$color_attribute_ui, "list")
      expect_true(is.na(group_attribute_default()))
      expect_type(output$group_attribute_ui, "list")


      expect_type(output$x_attribute_ui, "list")
      expect_type(output$color_attribute_ui, "list")
      expect_type(output$group_attribute_ui, "list")

      expect_equal(
        session$getReturned()(),
        list(
          "type" = "barchart",
          "entity" = "Files",
          "name" = "Plot 1",
          "x_attribute" = "assay"
        )
      )
    }
  )
})

test_that("admin_barchart_module_server_with_group", {
  shiny::testServer(
    admin_barchart_module_server,
    args = list(
      "data" = shiny::reactive(data),
      "input_config" = shiny::reactive(NULL),
      "entity" = shiny::reactive("Files"),
      "name" = shiny::reactive("Plot 1")
    ),
    {

      session$setInputs("x_attribute" = "assay")
      session$setInputs("color_attribute" = "none")
      session$setInputs("group_attribute" = "file_format")

      expect_type(column_choices(), "character")

      expect_true(is.na(x_attribute_default()))
      expect_type(output$x_attribute_ui, "list")
      expect_true(is.na(color_attribute_default()))
      expect_type(output$color_attribute_ui, "list")
      expect_true(is.na(group_attribute_default()))
      expect_type(output$group_attribute_ui, "list")


      expect_equal(
        session$getReturned()(),
        list(
          "type" = "barchart",
          "entity" = "Files",
          "name" = "Plot 1",
          "x_attribute" = "assay",
          "group_attribute" = "file_format"
        )
      )
    }
  )
})

test_that("admin_barchart_module_server_with_group_and_color", {
  shiny::testServer(
    admin_barchart_module_server,
    args = list(
      "data" = shiny::reactive(data),
      "input_config" = shiny::reactive(NULL),
      "entity" = shiny::reactive("Files"),
      "name" = shiny::reactive("Plot 1")
    ),
    {

      session$setInputs("x_attribute" = "assay")
      session$setInputs("color_attribute" = "year")
      session$setInputs("group_attribute" = "file_format")

      expect_type(column_choices(), "character")

      expect_true(is.na(x_attribute_default()))
      expect_type(output$x_attribute_ui, "list")
      expect_true(is.na(color_attribute_default()))
      expect_type(output$color_attribute_ui, "list")
      expect_true(is.na(group_attribute_default()))
      expect_type(output$group_attribute_ui, "list")

      expect_equal(
        session$getReturned()(),
        list(
          "type" = "barchart",
          "entity" = "Files",
          "name" = "Plot 1",
          "x_attribute" = "assay",
          "group_attribute" = "file_format",
          "color_attribute" = "year"
        )
      )
    }
  )
})
