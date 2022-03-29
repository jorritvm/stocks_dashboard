header = dashboardHeader(title = "Stocks dashboard",
                         dropdownMenuOutput("notif_menu"))

sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("Portfolio", tabName = "portfolio", icon = icon("pie-chart")),
    menuItem("Currencies", tabName = "currencies", icon = icon("coins")),
    menuItem("Stocks", tabName = "stocks", icon = icon("bar-chart-o")),
    menuItem("About", tabName = "about", icon = icon("info")),
    actionButton("debug_btn", label = "debug") # TODO: remove this at the end
  )
)

body = dashboardBody(tabItems(

    tabItem(
    tabName = "portfolio",
    navbarPage(
      title = "",
      tabPanel(
        "Position",
        box_full_light(
          plotlyOutput("position_per_broker"),
          title = "Position per account [EUR]"
        ),
        box_full_light(
          plotlyOutput("position_per_stock"),
          title = "Position per stock [EUR]"
        )
      ),
      tabPanel(
        "Evolution",
        box_full_dark(
          selectInput("pf_broker", "Select broker", ""),
          radio_window_plot("pf_window"),
          title = "Input"
        ),
        box_full_light(
          plotlyOutput("portfolio_position"),
          # plotlyOutput("pf_plot"),
          title = "Portfolio evolution"
        )
      ),
      tabPanel(
        "Performance",
        box_full_light(
          plotlyOutput("total_performance"),
          title = "Total performance"
        )
      ),
      tabPanel(
        "Timing",
        box_full_dark(
          selectInput("timing_key", "Stock", ""),
          radio_window_plot("timing_window"),
          title = "Input"
        ),
        box_full_light(
          plotlyOutput("market_timing_p"),
          plotlyOutput("market_timing_q"),
          plotlyOutput("market_timing_pq"),
          title = "Market timing"
        )
      ),
      tabPanel("Transactions",
               DTOutput("transactions_table")),
      tabPanel(
        "Batch upload",
        box_explain_excel_upload(),
        box_full_dark(
          fileInput(
            "batch_portfolio_file",
            "Choose XLSX File",
            multiple = FALSE,
            accept = c(
              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
              ".xlsx"
            )
          ),
          title = "Upload"
        )
      )
    )
  ),
  
  
  tabItem(
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
  ),
  
  
  tabItem(
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
          textInput("add_stock_symbol", "Enter Yahoo symbol to add a stock", ""),
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
  ),
  

  tabItem(tabName = "about",
          DTOutput('table_debug'),
          img(src = "wsb.jpg"))
))


ui = dashboardPage(header, 
                   sidebar, 
                   body, 
                   skin = "green")

