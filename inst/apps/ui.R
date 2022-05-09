shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Basic dashboard"),

  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(
        "Main", tabName = "main", icon = shiny::icon("bar-chart-o")
      )
    )
  ),

  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "main",
        shiny::fluidRow(
          shinydashboard::box(
            width = 12,
            solidHeader = TRUE,
            status = "warning",
            title = "Controls",
            shiny::uiOutput("data_selection_ui"),
            shiny::selectInput(
              "display_choice",
              label = "Select How to display items.",
              choices = list("Barchart", "Data Table")
            )
          )
        ),
        conditionalPanel(
          condition = "input.display_choice == 'Data Table'",
          shiny::fluidRow(
            shinydashboard::box(
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              title = "Data Table",
              DT::dataTableOutput("data_table")
            )
          )
        ),
        conditionalPanel(
          condition = "input.display_choice == 'Barchart'",
          admin_barchart_module_ui("barchart")
        )

        # conditionalPanel(
        #   condition = "input.display_choice == 'Barchart'",
        #   shiny::fluidRow(
        #     shinydashboard::box(
        #       width = 12,
        #       solidHeader = TRUE,
        #       status = "warning",
        #       title = "Barchart Controls",
        #       shiny::uiOutput("barchart_x_attribute_ui"),
        #       shiny::uiOutput("barchart_color_attribute_ui"),
        #       shiny::uiOutput("barchart_group_attribute_ui")
        #     )
        #   )
        # ),
        # conditionalPanel(
        #   condition = "input.display_choice == 'Barchart'",
          # shiny::fluidRow(barchart_module_ui("barchart"))
        # )
      )
    )
  )
)
