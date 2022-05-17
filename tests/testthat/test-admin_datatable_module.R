test_that("admin_datatable_module_ui", {
  expect_type(admin_datatable_module_ui("id"), "list")
})


test_that("admin_datatable_module_server", {
  shiny::testServer(
    admin_datatable_module_server,
    args = list(
      "input_config" = shiny::reactive(list())
    ),
    {
      expect_equal(
        session$getReturned()(),
        list()
      )
    }
  )
})
