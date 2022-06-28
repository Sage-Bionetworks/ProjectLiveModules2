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
      "input_config" = shiny::reactive(NULL)
    ),
    {

      session$setInputs("x_attribute-attribute_choice" = "assay")
      session$setInputs("color_attribute-attribute_choice" = "none")
      session$setInputs("group_attribute-attribute_choice" = "none")

      expect_type(column_choices(), "character")
      expect_equal(x_attribute(), "assay")
      expect_type(color_column_choices(), "character")
      expect_equal(color_attribute(), "none")
      expect_type(group_column_choices(), "character")
      expect_equal(group_attribute(), "none")

      expect_equal(
        session$getReturned()(),
        list("x_attribute" = "assay")
      )
    }
  )
})

test_that("admin_barchart_module_server_no_group_or_color_with_config", {
  shiny::testServer(
    admin_barchart_module_server,
    args = list(
      "data" = shiny::reactive(data),
      "input_config" = shiny::reactive(list("x_attribute" = "assay"))
    ),
    {
      session$setInputs("x_attribute-attribute_choice" = "assay")
      session$setInputs("color_attribute-attribute_choice" = "none")
      session$setInputs("group_attribute-attribute_choice" = "none")

      expect_type(column_choices(), "character")
      expect_equal(x_attribute(), "assay")
      expect_type(color_column_choices(), "character")
      expect_equal(color_attribute(), "none")
      expect_type(group_column_choices(), "character")
      expect_equal(group_attribute(), "none")

      expect_equal(
        session$getReturned()(),
        list("x_attribute" = "assay")
      )
    }
  )
})

test_that("admin_barchart_module_server_with_group", {
  shiny::testServer(
    admin_barchart_module_server,
    args = list(
      "data" = shiny::reactive(data),
      "input_config" = shiny::reactive(NULL)
    ),
    {
      session$setInputs("x_attribute-attribute_choice" = "assay")
      session$setInputs("color_attribute-attribute_choice" = "none")
      session$setInputs("group_attribute-attribute_choice" = "file_format")


      expect_type(column_choices(), "character")
      expect_equal(x_attribute(), "assay")
      expect_type(color_column_choices(), "character")
      expect_equal(color_attribute(), "none")
      expect_type(group_column_choices(), "character")
      expect_equal(group_attribute(), "file_format")


      expect_equal(
        session$getReturned()(),
        list(
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
      "input_config" = shiny::reactive(NULL)
    ),
    {
      session$setInputs("x_attribute-attribute_choice" = "assay")
      session$setInputs("color_attribute-attribute_choice" = "year")
      session$setInputs("group_attribute-attribute_choice" = "file_format")

      expect_type(column_choices(), "character")
      expect_equal(x_attribute(), "assay")
      expect_type(color_column_choices(), "character")
      expect_equal(color_attribute(), "year")
      expect_type(group_column_choices(), "character")
      expect_equal(group_attribute(), "file_format")

      expect_equal(
        session$getReturned()(),
        list(
          "x_attribute" = "assay",
          "group_attribute" = "file_format",
          "color_attribute" = "year"
        )
      )
    }
  )
})
