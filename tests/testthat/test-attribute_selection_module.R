test_that("attribute_sel_module_ui", {
  expect_type(attribute_sel_module_ui("id"), "list")
})

attribute_choices_input <- c(
  "A1" = "A1", "A2" = "A2", "A3" = "A3", "None" = "none"
)

test_that("attribute_sel_module_server_no_config", {
  shiny::testServer(
    attribute_sel_module_server,
    args = list(
      "config" = shiny::reactive(NULL),
      "attribute_name" = shiny::reactive("x_attribute"),
      "attribute_choices" = shiny::reactive(attribute_choices_input)
    ),
    {
      session$setInputs("attribute_choice" = "A1")

      expect_null(validated_config())
      expect_equal(validated_name(), "x_attribute")
      expect_equal(validated_choices(), attribute_choices_input)
      expect_true(is.na(attribute_default()))
      expect_equal(session$getReturned()(), "A1")
    }
  )
})

test_that("attribute_sel_module_server_with_config1", {
  shiny::testServer(
    attribute_sel_module_server,
    args = list(
      "config" = shiny::reactive(list("x_attribute" = "A2")),
      "attribute_name" = shiny::reactive("x_attribute"),
      "attribute_choices" = shiny::reactive(attribute_choices_input)
    ),
    {
      expect_equal(attribute_default(), "A2")
      session$setInputs("attribute_choice" = "A1")
      expect_equal(session$getReturned()(), "A1")
    }
  )
})

test_that("attribute_sel_module_server_with_config2", {
  shiny::testServer(
    attribute_sel_module_server,
    args = list(
      "config" = shiny::reactive(list("x_attribute" = "A2")),
      "attribute_name" = shiny::reactive("x_attribute"),
      "attribute_choices" = shiny::reactive(attribute_choices_input)
    ),
    {
      expect_equal(attribute_default(), "A2")
      session$setInputs("attribute_choice" = "A2")
      expect_equal(session$getReturned()(), "A2")
    }
  )
})

test_that("attribute_sel_module_server_with_config_none_selection", {
  shiny::testServer(
    attribute_sel_module_server,
    args = list(
      "config" = shiny::reactive(list("x_attribute" = "A2")),
      "attribute_name" = shiny::reactive("color_attribute"),
      "attribute_choices" = shiny::reactive(attribute_choices_input)
    ),
    {
      session$setInputs("attribute_choice" = "none")
      expect_equal(validated_config(), list("x_attribute" = "A2"))
      expect_equal(validated_name(), "color_attribute")
      expect_equal(validated_choices(), attribute_choices_input)

      expect_true(is.na(attribute_default()))
      expect_equal(session$getReturned()(), "none")
    }
  )
})
