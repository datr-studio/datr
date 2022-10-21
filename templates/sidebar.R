sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Main", tabName = "main", icon = icon("chart-line"))
  )
)

sidebarServer <- function(input, output, session) {

}
