#' Admin Barchart Module UI
#'
#' @param id A shiny id
#'
#' @export
admin_barchart_module_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        solidHeader = TRUE,
        status = "warning",
        title = "Barchart Controls",
        shiny::uiOutput(ns("x_attribute_ui")),
        shiny::uiOutput(ns("color_attribute_ui")),
        shiny::uiOutput(ns("group_attribute_ui"))
      )
    ),
    shiny::fluidRow(barchart_module_ui(ns("barchart")))
  )

}


#' Admin Barchart Module Server
#'
#' @param id A shiny id
#' @param data A shiny::reactive that returns a data frame
#' @param input_config A shiny::reactive that returns a named list or Null.
#' @param entity A shiny::reactive that returns a string
#'
#' @export
admin_barchart_module_server <- function(id, data, input_config, entity){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      #TODO: replace this with schematic API call
      column_choices <- shiny::reactive({
        shiny::req(data())
        choices <- colnames(data())
        names <- stringr::str_to_title(colnames(data()))
        purrr::set_names(choices, names)
      })

      x_attribute_default <- shiny::reactive({
        get_value_from_list(input_config(), "x_attribute")
      })

      output$x_attribute_ui <- shiny::renderUI({
        shiny::req(
          !is.null(x_attribute_default()),
          column_choices()
        )
        shiny::selectInput(
          inputId  = ns("x_attribute"),
          label    = "Select attribute to use as x column.",
          choices  = column_choices(),
          selected = x_attribute_default()
        )
      })

      color_attribute_default <- shiny::reactive({
        get_value_from_list(input_config(), "color_attribute")
      })

      output$color_attribute_ui <- shiny::renderUI({
        shiny::req(
          !is.null(color_attribute_default()),
          column_choices(),
          input$x_attribute
        )
        choices <- c("None" = "none", column_choices())
        choices <- choices[input$x_attribute != choices]

        shiny::selectInput(
          inputId  = ns("color_attribute"),
          label    = "Select attribute to use as barchart color.",
          choices  = choices,
          selected = color_attribute_default()
        )
      })

      group_attribute_default <- shiny::reactive({
        get_value_from_list(input_config(), "group_attribute")
      })

      output$group_attribute_ui <- shiny::renderUI({
        shiny::req(
          !is.null(group_attribute_default()),
          column_choices(),
          input$x_attribute
        )
        choices <- c("None" = "none", column_choices())
        choices <- choices[input$x_attribute != choices]

        shiny::selectInput(
          inputId  = ns("group_attribute"),
          label    = "Select attribute to use as barchart grouping.",
          choices  = choices,
          selected = group_attribute_default()
        )
      })


      output_config <- shiny::reactive({
        shiny::req(
          entity(),
          input$x_attribute,
          input$group_attribute,
          input$color_attribute
        )

        create_barchart_config(
          entity(),
          input$x_attribute,
          input$group_attribute,
          input$color_attribute
        )
      })

      barchart_module_server(
        "barchart",
        data,
        output_config
      )

      return(output_config)

    }
  )
}
