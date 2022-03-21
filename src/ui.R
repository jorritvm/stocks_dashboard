source("load_libraries.R")

header = dashboardHeader(title = "Stocks dashboard")

sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("Portfolio", tabName = "portfolio"),
    menuItem("Currencies", tabName = "currencies"),
    menuItem("Stocks", tabName = "stocks"),
    menuItem("About", tabName = "about"),
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
          radioButtons(
            "pf_window",
            "",
            choices = list(
              "All" = "all",
              "5Y" = "5Y",
              "3Y" = "3Y",
              "2Y" = "2Y",
              "1Y" = "1Y",
              "YTD" = "YTD",
              "6M" = "6M",
              "3M" = "3M",
              "1M" = "1M",
              "2W" = "2W",
              "1W" = "1W"
            ),
            selected = "YTD",
            inline = TRUE
          ),
          title = "Input",
          status = "primary",
          solidHeader = TRUE,
          width = 12
        ),
        box(
          plotlyOutput("pf_plot"),
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
          radioButtons(
            "timing_window",
            "",
            choices = list(
              "All" = "all",
              "5Y" = "5Y",
              "3Y" = "3Y",
              "2Y" = "2Y",
              "1Y" = "1Y",
              "YTD" = "YTD",
              "6M" = "6M",
              "3M" = "3M",
              "1M" = "1M",
              "2W" = "2W",
              "1W" = "1W"
            ),
            selected = "YTD",
            inline = TRUE
          ),
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
        box(
          tags$p("Upload an excel file with at these named columns:"),
          tags$ol(
            tags$li(HTML("<b>symbol:</b> yahoo ticker")),
            tags$li(
              HTML("<b>date:</b> date of transaction using ISO8601 date format")
            ),
            tags$li(HTML(
              "<b>type:</b> should be 'buy', 'sell', 'div', 'cash_in', 'cash_out', 'transfer_in', 'transfer_out'"
            )),
            tags$li(HTML("<b>amount:</b> amount of shares")),
            tags$li(HTML(
              "<b>money:</b> total revenue/cost of the transaction"
            )),
            tags$li(
              HTML(
                "<b>currency:</b> 3 letter abbreviation for the currency of the money column"
              )
            )
          ),
          tags$p(
            "For amount & money, the sign will not matter, type is used to determine sign."
          ),
          tags$p(
            "If money is not given for a buy or sell order, closing price will be used."
          ),
          tags$p("If currency is not given, original stock currency will be used."),
          title = "Readme",
          status = "info",
          solidHeader = TRUE,
          width = 12
        ),
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
          radioButtons(
            "fx_window",
            "",
            choices = list(
              "All" = "all",
              "5Y" = "5Y",
              "3Y" = "3Y",
              "2Y" = "2Y",
              "1Y" = "1Y",
              "YTD" = "YTD",
              "6M" = "6M",
              "3M" = "3M",
              "1M" = "1M",
              "2W" = "2W",
              "1W" = "1W"
            ),
            selected = "YTD",
            inline = TRUE
          ),
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
          radioButtons(
            "cs_window",
            "",
            choices = list(
              "All" = "all",
              "5Y" = "5Y",
              "3Y" = "3Y",
              "2Y" = "2Y",
              "1Y" = "1Y",
              "YTD" = "YTD",
              "6M" = "6M",
              "3M" = "3M",
              "1M" = "1M",
              "2W" = "2W",
              "1W" = "1W"
            ),
            selected = "YTD",
            inline = TRUE
          ),
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
        radioButtons(
          "bench_window",
          "",
          choices = list(
            "All" = "all",
            "5Y" = "5Y",
            "3Y" = "3Y",
            "2Y" = "2Y",
            "1Y" = "1Y",
            "YTD" = "YTD",
            "6M" = "6M",
            "3M" = "3M",
            "1M" = "1M",
            "2W" = "2W",
            "1W" = "1W"
          ),
          selected = "YTD",
          inline = TRUE
        ),
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

