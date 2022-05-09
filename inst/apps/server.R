server <- function(input, output) {

  library(magrittr)

  data <- readRDS("../../tests/testthat/RDS/data.rds")

  output$data_selection_ui <- shiny::renderUI({
    shiny::selectInput(
      "entity_choice",
      label = "Select Items to plot.",
      choices = names(data)
    )
  })

  selected_data <- shiny::reactive({
    shiny::req(input$entity_choice)
    purrr::pluck(data, input$entity_choice)
  })


  admin_datatable_module_server(
    "datatable",
    selected_data,
    shiny::reactive(list())
  )

  admin_barchart_module_server(
    "barchart",
    selected_data,
    shiny::reactive(input$entity_choice)
  )

}
