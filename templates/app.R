suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(shinyalert))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinyr))
suppressPackageStartupMessages(library(glue))


app <- function() {
  saucer::source_folder("app", excl = "app.R")
  addResourcePath("www", "app/www")
  ui <- tagList(
    addStyleSheet(),
    addFavicon(),
    addReturnKeyListener(),
    dashboardPage(
      header = header,
      sidebar = sidebar,
      body = body,
      title = "Title goes here"
    )
  )

  server <- function(input, output, session) {
    urlHandler(input, output, session)
    servers(input, output, session)
    sidebarServer(input, output, session)
  }

  shinyApp(ui, server, options = list(launch.browser = T))
}

app()
