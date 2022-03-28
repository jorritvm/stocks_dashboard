header = dashboardHeader(title = "Stocks dashboard")

sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("Portfolio", tabName = "portfolio"),
    menuItem("Currencies", tabName = "currencies"),
    menuItem("Stocks", tabName = "stocks"),
    menuItem("About", tabName = "about")
    # actionButton("debug_btn", label = "debug") # TODO: remove this at the end
  )
)

body = dashboardBody(tabItems(

    tabItem(
    tabName = "portfolio",
    navbarPage(
      title = "",
      tabPanel(
        "Position",
        box(
          plotlyOutput("position_per_broker"),
          title = "Position per account",
          status = "info",
          solidHeader = TRUE,
          width = 12
        ),
        box(
          plotlyOutput("position_per_stock"),
          title = "Position per stock",
          status = "info",
          solidHeader = TRUE,
          width = 12
        )
      ),
      tabPanel(
        "Plot",
        box(
          selectInput("pf_broker", "Select broker", ""),
          radio_window_plot("pf_window"),
          title = "Input",
          status = "primary",
          solidHeader = TRUE,
          width = 12
        ),
        box(
          plotlyOutput("portfolio_position"),
          # plotlyOutput("pf_plot"),
          title = "Portfolio evolution",
          status = "info",
          solidHeader = TRUE,
          width = 12
        )
      ),
      tabPanel(
        "Performance",
        box(
          plotlyOutput("total_performance"),
          title = "Total performance",
          status = "info",
          solidHeader = TRUE,
          width = 12
        )
      ),
      tabPanel(
        "Timing",
        box(
          selectInput("timing_key", "Stock", ""),
          radio_window_plot("timing_window"),
          title = "Input",
          status = "primary",
          solidHeader = TRUE,
          width = 12
        ),
        box(
          plotOutput("market_timing_p"),
          plotOutput("market_timing_q"),
          plotOutput("market_timing_v"),
          title = "Market timing",
          status = "info",
          solidHeader = TRUE,
          width = 12
        )
      ),
      tabPanel("Transactions",
               DTOutput("transactions_table")),
      tabPanel(
        "Batch upload",
        box_explain_excel_upload(),
        box(
          fileInput(
            "batch_portfolio_file",
            "Choose XLSX File",
            multiple = FALSE,
            accept = c(
              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
              ".xlsx"
            )
          ),
          title = "Upload",
          status = "primary",
          solidHeader = TRUE,
          width = 12
        ),
        box(
          textOutput("update_transaction_text"),
          title = "Info",
          status = "info",
          solidHeader = TRUE,
          width = 12
        )
      )
    )
  ),
  
  
  tabItem(
    tabName = "currencies",
    
    navbarPage(
      title = "",
      
      tabPanel("List",
               DTOutput('fx_list')),
      
      tabPanel(
        "Plot",
        box(
          selectInput("fx_symbol", "Select exchange rate", ""),
          radio_window_plot("fx_window"),
          title = "Input",
          status = "primary",
          solidHeader = TRUE,
          width = 12
        ),
        box(
          plotlyOutput("fx_plot"),
          DTOutput("fx_table"),
          title = "FOREX rate",
          status = "info",
          solidHeader = TRUE,
          width = 12
        )
      ),
      
      tabPanel(
        "Add",
        box(
          textInput("add_fx_symbol", "Add a currency rate", ""),
          actionButton("add_fx_btn", "Add", icon("plus")),
          title = "Input",
          status = "primary",
          solidHeader = TRUE,
          width = 6
        ),
        box(
          textOutput("add_fx_output"),
          title = "Result",
          status = "info",
          solidHeader = TRUE,
          width = 6
        )
      ),
      
      tabPanel(
        "Remove",
        box(
          selectInput("remove_fx_symbol", "Remove a currency rate", ""),
          actionButton("remove_fx_btn", "Remove", icon("minus")),
          title = "Input",
          status = "primary",
          solidHeader = TRUE,
          width = 6
        ),
        box(
          textOutput("remove_fx_output"),
          title = "Result",
          status = "info",
          solidHeader = TRUE,
          width = 6
        )
      ),
      tabPanel(
        "Update",
        box(
          actionButton("update_fx_btn", label = "Update FX data", icon("sync")),
          title = "Action",
          status = "primary",
          solidHeader = TRUE,
          width = 6
        ),
        box(
          textOutput("update_fx_text"),
          title = "Result",
          status = "info",
          solidHeader = TRUE,
          width = 6
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
        box(
          selectInput("profile_stock_key", "View stock profile", ""),
          title = "Input",
          status = "primary",
          solidHeader = TRUE,
          width = 12
        ),
        box(
          tableOutput("profile_stock_table"),
          title = "Profile",
          status = "info",
          solidHeader = TRUE,
          width = 12
        )
      ),
      tabPanel(
        "Candlestick",
        box(
          selectInput("cs_key", "", ""),
          radio_window_plot("cs_window"),
          title = "Input",
          status = "primary",
          solidHeader = TRUE,
          width = 12
        ),
        box(
        plotlyOutput("cs_plot"),
        title = "Graph",
        status = "info",
        solidHeader = TRUE,
        width = 12
      )
    ),
    tabPanel(
      "Benchmark",
      box(
        selectInput("bench_key", "Stock", ""),
        selectInput("bench_base", "Benchmark", ""),
        radio_window_plot("bench_window"),
        title = "Input",
        status = "primary",
        solidHeader = TRUE,
        width = 12
      ),
      box(
        plotlyOutput("bench_plot"),
        title = "Graph",
        status = "info",
        solidHeader = TRUE,
        width = 12
      )
    ),
      
      tabPanel(
        "Add",
        box(
          textInput("add_stock_symbol", "Enter Yahoo symbol to add a stock", ""),
          selectInput("add_stock_region", "Stock region", choices = c("US", "EU")),
          actionButton("add_stock_symbol_btn", "Add", icon("plus")),
          title = "Input",
          status = "primary",
          solidHeader = TRUE,
          width = 6
        ),
        box(
          textOutput("add_stock_output"),
          title = "Result",
          status = "info",
          solidHeader = TRUE,
          width = 6
        )
      ),
      
      tabPanel(
        "Remove",
        box(
          selectInput("remove_stock_symbol", "Remove a stock from profiles & OHLC, not from transactions!", ""),
          actionButton("remove_stock_symbol_btn", "Remove", icon("minus")),
          title = "Input",
          status = "primary",
          solidHeader = TRUE,
          width = 6
        ),
        box(
          textOutput("remove_stock_output"),
          title = "Result",
          status = "info",
          solidHeader = TRUE,
          width = 6
        )
      ),
      tabPanel(
        "Update",
        box(
          actionButton("update_ohlc_btn", "Update OHLC data", icon("sync")),
          title = "Action",
          status = "primary",
          solidHeader = TRUE,
          width = 6
        ),
        box(
          textOutput("update_ohlc_text"),
          title = "Action",
          status = "info",
          solidHeader = TRUE,
          width = 6
        )
      )
    )
  ),
  

  tabItem(tabName = "about",
          img(src = "wsb.jpg"))
))


ui = dashboardPage(header, 
                   sidebar, 
                   body, 
                   skin = "green")

