tab_dashboard = tabItem(
  tabName = "dashboard",
  valueBoxOutput("vb_total_stocks", width = 6),
  valueBoxOutput("vb_total_cash", width = 6),
  valueBoxOutput("overall_performance", width = 6),
  valueBoxOutput("ytd_performance", width = 6),
  valueBoxOutput("best_stock_overall", width = 6),
  valueBoxOutput("worst_stock_overall", width = 6),
  valueBoxOutput("best_stock_ytd", width = 6),
  valueBoxOutput("worst_stock_ytd", width = 6),
  valueBoxOutput("latest_ohlc_update", width = 6),
  valueBoxOutput("latest_fx_update", width = 6)
)