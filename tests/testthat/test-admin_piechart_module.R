test_that("admin_piechart_module_ui", {
  expect_type(admin_piechart_module_ui("id"), "list")
})

data <-
  testthat::test_path("RDS", "data.rds") %>%
  readRDS() %>%
  purrr::pluck("Files")


test_that("admin_piechart_module_server_no_config", {
  shiny::testServer(
    admin_piechart_module_server,
    args = list(
      "data" = shiny::reactive(data),
      "input_config" = shiny::reactive(NULL)
    ),
    {
      session$setInputs("label_attribute-attribute_choice" = "assay")

      expect_type(column_choices(), "character")
      expect_equal(label_attribute(), "assay")

      expect_equal(
        session$getReturned()(),
        list("label_attribute" = "assay")
      )
    }
  )
})

test_that("admin_piechart_module_server_no_group_or_color_with_config", {
  shiny::testServer(
    admin_piechart_module_server,
    args = list(
      "data" = shiny::reactive(data),
      "input_config" = shiny::reactive(list("label_attribute" = "file_format"))
    ),
    {
      session$setInputs("label_attribute-attribute_choice" = "assay")

      expect_type(column_choices(), "character")
      expect_equal(label_attribute(), "assay")

      expect_equal(
        session$getReturned()(),
        list("label_attribute" = "assay")
      )
    }
  )
})
