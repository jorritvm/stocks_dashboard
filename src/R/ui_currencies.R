tab_currencies = tabItem(
  tabName = "currencies",
  
  navbarPage(
    title = "",
    
    tabPanel(
      "Chart",
      box_full_dark(
        selectInput("fx_symbol", "Select exchange rate", ""),
        radio_window_plot("fx_window"),
        title = "Input"
      ),
      box_full_light(
        plotlyOutput("fx_plot"),
        DTOutput("fx_table"),
        title = "FOREX rate"
      )
    ),
    
    tabPanel("Table",
             DTOutput('fx_list')),
    
    tabPanel(
      "Edit",
      box_half_dark(
        textInput("add_fx_symbol", "Add a currency rate", ""),
        actionButton("add_fx_btn", "Add", icon("plus")),
        title = "Add"
      ),
      box_half_dark(
        selectInput("remove_fx_symbol", "Remove a currency rate", ""),
        actionButton("remove_fx_btn", "Remove", icon("minus")),
        title = "Remove"
      ),
      box_full_dark(
        actionButton("update_fx_btn", label = "Update FX data", icon("sync")),
        title = "Action"
      )
    )
  )
)