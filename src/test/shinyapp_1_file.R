library(shiny)

sidebar <- dashboardSidebar(
  # Create a select list
  "mijn_label"
  )
body <- dashboardBody(
  textOutput("name")
)

ui <- dashboardPage(header = dashboardHeader(),
                    sidebar = sidebar,
                    body = body
)

server <- function(input, output) {
  output$name <- renderText({
    "mijn_tekstje"
  })
}

shinyApp(ui, server)