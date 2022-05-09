server <- function(input, output) {

  library(magrittr)

  data <- readRDS("../../inst/RDS/data.rds")

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

  output$data_table <- DT::renderDataTable({
    shiny::req(
      selected_data(),
      input$display_choice == "Data Table"
    )

    selected_data()
  })

  admin_barchart_module_server(
    "barchart",
    selected_data,
    shiny::reactive(input$entity_choice)
  )



  # column_choices <- shiny::reactive({
  #   shiny::req(input$entity_choice)
  #
  #   entity_columns <- list(
  #     "Files" = c(
  #       "Assay" = "assay",
  #       "Access Type" = "access_type",
  #       "Year" = "year",
  #       "Study ID" = "study_id"
  #     ),
  #     "Publications" = c(
  #       "Assay" = "assay",
  #       "Year" = "year",
  #       "Study ID" = "study_id"
  #     ),
  #     "Tools" = c(
  #       "Study ID" = "study_id"
  #     ),
  #     "Studies" = c(
  #       "Initiative" = "initiative"
  #     )
  #   )
  #
  #   entity_columns %>%
  #     purrr::pluck(input$entity_choice)
  # })
  #
  # output$barchart_x_attribute_ui <- shiny::renderUI({
  #   shiny::req(column_choices)
  #   shiny::selectInput(
  #     inputId  = "barchart_x_attribute",
  #     label    = "Select attribute to use as x column.",
  #     choices  = column_choices()
  #   )
  # })
  #
  # output$barchart_color_attribute_ui <- shiny::renderUI({
  #   shiny::req(column_choices, input$barchart_x_attribute)
  #   choices <- c("None" = "none", column_choices())
  #   choices <- choices[input$barchart_x_attribute != choices]
  #
  #   shiny::selectInput(
  #     inputId  = "barchart_color_attribute",
  #     label    = "Select attribute to use as barchart color.",
  #     choices  = choices
  #   )
  # })
  #
  # output$barchart_group_attribute_ui <- shiny::renderUI({
  #   shiny::req(column_choices, input$barchart_x_attribute)
  #   choices <- c("None" = "none", column_choices())
  #   choices <- choices[input$barchart_x_attribute != choices]
  #
  #   shiny::selectInput(
  #     inputId  = "barchart_group_attribute",
  #     label    = "Select attribute to use as barchart grouping.",
  #     choices  = choices
  #   )
  # })
  #
  #
  #
  # barchart_config <- shiny::reactive({
  #   shiny::req(
  #     input$display_choice == "Barchart",
  #     input$barchart_x_attribute,
  #     input$barchart_group_attribute,
  #     input$barchart_color_attribute
  #   )
  #
  #   list(
  #     "x_attribute"     = input$barchart_x_attribute,
  #     "color_attribute" = input$barchart_color_attribute,
  #     "group_attribute" = input$barchart_group_attribute
  #   )
  # })
  #
  # barchart_module_server(
  #   "barchart",
  #   selected_data,
  #   barchart_config
  # )

}
