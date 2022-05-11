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
        admin_module_ui("admin")
      )
    )
  )
)
