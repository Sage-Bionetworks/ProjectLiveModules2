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
#' @param data A data frame
#' @param entity A string
#'
#' @export
admin_barchart_module_server <- function(id, data, entity){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      #TODO: replace this with schematic API call
      column_choices <- shiny::reactive({
        shiny::req(entity())

        entity_columns <- list(
          "Files" = c(
            "Assay" = "assay",
            "Access Type" = "access_type",
            "Year" = "year",
            "Study ID" = "study_id"
          ),
          "Publications" = c(
            "Assay" = "assay",
            "Year" = "year",
            "Study ID" = "study_id"
          ),
          "Tools" = c(
            "Study ID" = "study_id"
          ),
          "Studies" = c(
            "Initiative" = "initiative"
          )
        )

        entity_columns %>%
          purrr::pluck(entity())
      })


      output$x_attribute_ui <- shiny::renderUI({
        shiny::req(column_choices)
        shiny::selectInput(
          inputId  = ns("x_attribute"),
          label    = "Select attribute to use as x column.",
          choices  = column_choices()
        )
      })

      output$color_attribute_ui <- shiny::renderUI({
        shiny::req(column_choices, input$x_attribute)
        choices <- c("None" = "none", column_choices())
        choices <- choices[input$x_attribute != choices]

        shiny::selectInput(
          inputId  = ns("color_attribute"),
          label    = "Select attribute to use as barchart color.",
          choices  = choices
        )
      })

      output$group_attribute_ui <- shiny::renderUI({
        shiny::req(column_choices, input$x_attribute)
        choices <- c("None" = "none", column_choices())
        choices <- choices[input$x_attribute != choices]

        shiny::selectInput(
          inputId  = ns("group_attribute"),
          label    = "Select attribute to use as barchart grouping.",
          choices  = choices
        )
      })


      config <- shiny::reactive({
        shiny::req(
          input$x_attribute,
          input$group_attribute,
          input$color_attribute
        )

        list(
          "x_attribute"     = input$x_attribute,
          "color_attribute" = input$color_attribute,
          "group_attribute" = input$group_attribute
        )
      })

      barchart_module_server(
        "barchart",
        data,
        config
      )

      return(config)

    }
  )
}
