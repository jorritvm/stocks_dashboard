tab_portfolio = tabItem(
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
      ),
      box_full_light(
        DTOutput("positions_per_stock_broker_tbl"),
        title = "Overview of positions per stock & broker"
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
)