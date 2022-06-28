#' Admin Barchart Module UI
#'
#' @param id A shiny id
admin_barchart_module_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      solidHeader = TRUE,
      status = "warning",
      title = "Barchart Controls",
      attribute_sel_module_ui(ns("x_attribute")),
      attribute_sel_module_ui(ns("color_attribute")),
      attribute_sel_module_ui(ns("group_attribute"))
    )
  )

}


#' Admin Barchart Module Server
#'
#' @param id A shiny id
#' @param data A shiny::reactive that returns a data frame
#' @param input_config A shiny::reactive that returns a named list or Null.
admin_barchart_module_server <- function(id, data, input_config) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      #TODO: replace this with schematic API call
      column_choices <- shiny::reactive({
        shiny::req(data())
        choices <- colnames(data())
        names <- stringr::str_to_title(colnames(data()))
        purrr::set_names(choices, names)
      })

      x_attribute <- attribute_sel_module_server(
        "x_attribute",
        config            = input_config,
        attribute_name    = shiny::reactive("x_attribute"),
        attribute_choices = column_choices,
        ui_label          = shiny::reactive("Select x attribute")
      )

      # adds none and removes x_attribute from list
      color_column_choices <- shiny::reactive({
        shiny::req(column_choices(), x_attribute())
        choices <- c("None" = "none", column_choices())
        choices <- choices[x_attribute() != choices]
        return(choices)
      })

      color_attribute <- attribute_sel_module_server(
        "color_attribute",
        config            = input_config,
        attribute_name    = shiny::reactive("color_attribute"),
        attribute_choices = color_column_choices,
        ui_label          = shiny::reactive("Select color attribute")
      )

      # removes color choice from list if not "none"
      group_column_choices <- shiny::reactive({
        shiny::req(color_column_choices(), color_attribute())
        choices <- color_column_choices()
        if (color_attribute() != "none") {
          choices <- choices[color_attribute() != choices]
        }
        return(choices)
      })

      group_attribute <- attribute_sel_module_server(
        "group_attribute",
        config            = input_config,
        attribute_name    = shiny::reactive("group_attribute"),
        attribute_choices = group_column_choices,
        ui_label          = shiny::reactive("Select group attribute")
      )

      output_config <- shiny::reactive({
        shiny::req(
          x_attribute(),
          group_attribute(),
          color_attribute()
        )

        create_barchart_config(
          x_attribute(),
          group_attribute(),
          color_attribute()
        )
      })

      return(output_config)

    }
  )
}
