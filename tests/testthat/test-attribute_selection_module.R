test_that("attribute_selection_module_ui", {
  expect_type(attribute_selection_module_ui("id"), "list")
})

test_that("attribute_selection_module_server_no_config", {
  shiny::testServer(
    attribute_selection_module_server,
    args = list(
      "config" = shiny::reactive(NULL),
      "attribute_name" = shiny::reactive("x_attribute"),
      "attribute_choices" = shiny::reactive(c("A1", "A2", "A3"))
    ),
    {
      expect_true(is.na(attribute_default()))
      session$setInputs("attribute_choice" = "A1")
      expect_equal(session$getReturned()(), "A1")
    }
  )
})

test_that("attribute_selection_module_server_with_config1", {
  shiny::testServer(
    attribute_selection_module_server,
    args = list(
      "config" = shiny::reactive(list("x_attribute" = "A2")),
      "attribute_name" = shiny::reactive("x_attribute"),
      "attribute_choices" = shiny::reactive(c("A1", "A2", "A3"))
    ),
    {
      expect_equal(attribute_default(), "A2")
      session$setInputs("attribute_choice" = "A1")
      expect_equal(session$getReturned()(), "A1")
    }
  )
})

test_that("attribute_selection_module_server_with_config1", {
  shiny::testServer(
    attribute_selection_module_server,
    args = list(
      "config" = shiny::reactive(list("x_attribute" = "A2")),
      "attribute_name" = shiny::reactive("x_attribute"),
      "attribute_choices" = shiny::reactive(c("A1", "A2", "A3"))
    ),
    {
      expect_equal(attribute_default(), "A2")
      session$setInputs("attribute_choice" = "A2")
      expect_equal(session$getReturned()(), "A2")
    }
  )
})
