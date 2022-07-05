tab_stocks = tabItem(
  tabName = "stocks",
  navbarPage(
    title = "",
    
    tabPanel("List",
             DTOutput('stock_list')),
    
    tabPanel(
      "Profile",
      box_full_dark(
        selectInput("profile_stock_key", "View stock profile", ""),
        title = "Input"
      ),
      box_full_light(tableOutput("profile_stock_table"),
                     title = "Profile")
    ),
    tabPanel(
      "Candlestick",
      box_full_dark(
        selectInput("cs_key", "Stock", ""),
        checkboxInput("cs_in_euro", label = "Force in EURO", value = FALSE),
        radio_window_plot("cs_window"),
        title = "Input"
      ),
      box_full_light(plotlyOutput("cs_plot"),
                     title = "Graph")
    ),
    tabPanel(
      "Benchmark",
      box_full_dark(
        selectInput("bench_key", "Stock", ""),
        selectInput("bench_base", "Benchmark", ""),
        checkboxInput("bench_in_euro", label = "Force compare in EURO", value = FALSE),
        radio_window_plot("bench_window"),
        title = "Input"
      ),
      box_full_light(plotlyOutput("bench_plot"),
                     title = "Graph")
    ),
    
    tabPanel(
      "Edit",
      box_half_dark(
        textInput("add_stock_symbol", "Enter RIC (Reuters instrument code) to add a stock", ""),
        selectInput("add_stock_region", "Stock region", choices = c("US", "EU")),
        actionButton("add_stock_symbol_btn", "Add", icon("plus")),
        title = "Input"
      ),
      box_half_dark(
        selectInput("remove_stock_symbol", "Remove a stock from profiles & OHLC, not from transactions!", ""),
        actionButton("remove_stock_symbol_btn", "Remove", icon("minus")),
        title = "Input"
      ),
      box_full_dark(
        actionButton("update_ohlc_btn", "Update OHLC data", icon("sync")),
        title = "Action"
      ),
      box_full_dark(
        actionButton("reset_ohlc_btn", "Reset all OHLC data - CAREFULL!!!", icon("trash")),
        title = "Action"
      )
    )
  )
)