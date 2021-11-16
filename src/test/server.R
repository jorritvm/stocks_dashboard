
server <- function(input, output) {
  output$name <- renderText({
    "mijn_tekstje"
  })
}