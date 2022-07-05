################################################################
# DASHBOARD TAB
################################################################
output$vb_total_stocks = renderValueBox(valueBox(
  round(sum(pos_sb()$position_euro), 2),
  "Total stocks",
  icon = icon("euro-sign"),
  color = "green",
  width = 6
))
output$vb_total_cash = renderValueBox(valueBox(
  round(cash_b_evol()[order(-date)][, .SD[1], by = account][, sum(cash_position)],2),
  "Total cash",
  icon = icon("euro-sign"),
  color = "green",
  width = 6
))
output$overall_performance = renderValueBox(valueBox(
  "todo",
  "Overall performance",
  icon = icon("sort"),
  color = "aqua",
  width = 6
))
output$ytd_performance = renderValueBox(valueBox(
  "todo",
  "YTD performance",
  icon = icon("sort"),
  color = "aqua",
  width = 6
))
output$best_stock_overall = renderValueBox({
  q = pos_sb()[order(performance_rel)][nrow(pos_sb())]
  s = q$symbol
  p = round(q$performance_rel, 2)
  valueBox(
    p,
    paste0("Best stock overall: ", s),
    icon = icon("sort"),
    color = "aqua",
    width = 6
  )
})
output$worst_stock_overall = renderValueBox({
  q = pos_sb()[order(performance_rel)][1]
  s = q$symbol
  p = round(q$performance_rel, 2)
  valueBox(
    p,
    paste0("Worst stock overall: ", s),
    icon = icon("sort"),
    color = "orange",
    width = 6
  )
})
output$best_stock_ytd = renderValueBox(valueBox(
  "todo",
  "Best stock YTD",
  icon = icon("sort"),
  color = "aqua",
  width = 6
))
output$worst_stock_ytd = renderValueBox(valueBox(
  "todo",
  "Worst stock YTD",
  icon = icon("sort"),
  color = "orange",
  width = 6
))
output$latest_ohlc_update = renderValueBox(valueBox(
  max(ohlc()$date),
  "Latest OHLC update",
  icon = icon("fas fa-sync"),
  color = "navy",
  width = 6
))
output$latest_fx_update = renderValueBox(valueBox(
  max(fx()$date),
  "Latest FX update",
  icon = icon("sync"),
  color = "navy",
  width = 6
))