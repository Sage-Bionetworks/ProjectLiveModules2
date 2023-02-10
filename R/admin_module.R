#' Admin Module UI
#'
#' @param id A shiny id
#'
#' @export
admin_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        solidHeader = TRUE,
        status = "warning",
        title = "Controls",
        json_module_ui(ns("json")),
        shiny::uiOutput(ns("name_selection_ui")),
        attribute_sel_module_ui(ns("entity_choice")),
        attribute_sel_module_ui(ns("display_choice"))
      )
    ),
    shiny::uiOutput(ns("admin_plot_module_ui")),
    display_module_ui(ns("display")),
    shiny::downloadButton(ns("download_json"), "Download JSON"),
  )

}


#' Admin Module Server
#'
#' @param id A shiny id
#' @param data a shiny::reactive that returns a named list of data frames
#'
#' @export
admin_module_server <- function(id, data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # json upload ----
      selected_input_config <- json_module_server("json")

      # name selection ----
      name_selection_default <- shiny::reactive({
        if (is.null(selected_input_config())) {
          default <- ""
        } else {
          default <- selected_input_config()$name
        }
        return(default)
      })

      output$name_selection_ui <- shiny::renderUI({
        shiny::req(!is.null(name_selection_default()))
        shiny::textInput(
          ns("name_choice"),
          label = "Pick a unique name for your plot.",
          value = name_selection_default()
        )
      })

      # entity (component) selection ----
      entity_choice <- attribute_sel_module_server(
        "entity_choice",
        config            = selected_input_config,
        ui_label          = shiny::reactive("Select Items to plot."),
        attribute_name    = shiny::reactive("entity"),
        attribute_choices = shiny::reactive(
          purrr::set_names(names(data()), names(data()))
        )
      )

      # display (plot type) selection ----
      display_choice <- attribute_sel_module_server(
        "display_choice",
        config            = selected_input_config,
        ui_label          = shiny::reactive("Select How to display items."),
        attribute_name    = shiny::reactive("type"),
        attribute_choices = shiny::reactive(
          get_plot_table() %>%
            dplyr::select("display_name", "plot_type") %>%
            tibble::deframe(.)
        )
      )

      # select ui module ----

      plot_function_row <- shiny::reactive({
        shiny::req(display_choice())
        dplyr::filter(
          get_plot_table(),
          .data$plot_type == display_choice()
        )
      })

      ui_module <- shiny::reactive({
        shiny::req(plot_function_row())
        column <- dplyr::pull(plot_function_row(), "admin_ui_module")
        return(column[[1]])
      })

      output$admin_plot_module_ui <- shiny::renderUI({
        shiny::req(ui_module())
        ui_module()(id = ns(display_choice()))
      })

      # rest ----

      selected_data <- shiny::reactive({
        shiny::req(entity_choice())
        purrr::pluck(data(), entity_choice())
      })

      plot_config_list <- {
        ids       <- get_plot_table()$plot_type
        functions <- get_plot_table()$admin_server_module
        config_list <- purrr::map2(
            functions,
            ids,
            ~do.call(
              what = .x,
              args = list(
                "id" = .y,
                data = selected_data,
                input_config = selected_input_config
              )
            )
          )
        purrr::set_names(config_list, ids)
      }

      plot_config <- shiny::reactive({
        shiny::req(display_choice())
        plot_config_list[[display_choice()]]()
      })

      output_config <- shiny::reactive({
        shiny::req(
          display_choice(),
          !is.null(entity_choice()),
          !is.null(input$name_choice),
          !is.null(plot_config())
        )

        config <-
          list(
            entity_choice(),
            input$name_choice,
            plot_config()
          ) %>%
          purrr::set_names(
            "entity",
            "name",
            display_choice()
          )
        return(config)
      })

      display_module_server(
        id = "display",
        config = output_config,
        data = selected_data
      )

      output$download_json <- shiny::downloadHandler(
        filename = function() "test.json",
        content = function(con) {
          writeLines(
            jsonlite::toJSON(output_config()),
            con
          )
        }
      )

    }
  )
}
