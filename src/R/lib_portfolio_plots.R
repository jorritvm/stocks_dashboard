#' creates a plot for actual portfolio position per broker
#'
#' @param pos_b 
#'
#' @return
#' @export
plot_position_per_broker = function(pos_b, pos_c) {
  pos = merge(pos_c, pos_b, by = "account")
  pos = melt(pos, id.vars = "account")
  pos = pos[order(account)]
  
  fig = ggplotly(
    ggplot(pos, aes(x = account, y = value)) +
    geom_col(aes(fill = variable), width = 0.7) + 
    scale_y_continuous(labels = scales::comma, expand = c(0,0,0,5000)) +
    labs(x = "", y = "") +
    scale_fill_brewer(palette = "Paired") +
    coord_flip() 
  )
         
  return(fig)
}


#' creates a plot for actual portfolio position per stock (irrespective of broker)
#'
#' @param pos_s 
#'
#' @return
#' @export
plot_position_per_stock = function(pos_s) {
  pos_s = pos_s[order(symbol)]
  fig = plot_ly(x = round(pos_s$position_euro,0), 
                y = pos_s$key, 
                type = 'bar', 
                orientation = 'h') %>% 
    layout(yaxis = list(autorange="reversed"))
  
  return(fig)
}


#' creates a plot that shows the evolution of the portfolio in absolute value (euro)
#'
#' @param broker 
#' @param pf_window 
#' @param pos_sb_evol 
#'
#' @return
plot_portfolio_evolution = function(broker, 
                                    pf_window, 
                                    pos_sb_evol,
                                    cash_b_evol) {
  
  # filter by broker
  ppos = copy(pos_sb_evol)
  cpos = copy(cash_b_evol)
  if (tolower(broker) != "all") {
    ppos = ppos[account == broker]
    cpos = cpos[account == broker]
  } 
  
  # filter by time window
  w = window_to_start_end_dates(pf_window)
  if (!is.null(w$start)) {
    ppos = ppos[date >= w$start & date <= w$end]
    cpos = cpos[date >= w$start & date <= w$end]
  }
  
  # # aggregate
  # ppos_plotdata = ppos[, .(portfolio = sum(position_euro)), by = .(date)][order(date)]
  # 
  # # plot
  # p = ggplotly(
  #      ggplot(ppos_plotdata, 
  #              aes(x=date, y=portfolio)) + 
  #      geom_line() + 
  #      labs(y = "Portfolio [EUR]") +     
  #      scale_x_date(limits = c(w$start, w$end), 
  #                   expand = c(0,0),
  #                   date_breaks  = "1 month") +
  #      theme(axis.text.x = element_text(angle = 90))
  # )
  
  
  # aggregate over all stocks per broker and date
  ppos = ppos[, .(portfolio = sum(position_euro)), by = .(date, account)][order(date)]
  pos = merge(ppos[, .(account, date, stocks = portfolio)], 
              cpos[, .(account, date, cash = cash_position)], 
              by = c("account", "date"))
  posl = melt(pos, id.vars = c("account", "date"))
  
  # plot
  p = ggplotly(
    ggplot(posl[], 
           aes(x=date, y=value, fill = account)) + 
      geom_area(position = "stack", alpha = 0.7) +
      facet_grid(rows = vars(variable), scales = "free") + 
      labs(y = "[EUR]") +     
      scale_x_date(
        # limits = c(w$start, w$end), # broken in last plotly release
        # expand = c(0,0), # broken in last plotly release
        date_breaks  = "1 month") +
      theme(axis.text.x = element_text(angle = 90))
  )
  
  return(p)
}


#' create a plot that shows price evolution of a stock.
#' colors the region where you are holding that stock.
#'
#' @param pos_sb_evol_subset 
#'
#' @return
#' @export
plot_market_timing_p = function(pos_sb_evol_subset) {
  s = pos_sb_evol_subset[1, symbol]

  start = min(pos_sb_evol_subset$date)
  end   = max(pos_sb_evol_subset$date)
  # plot 
  p = 
    ggplotly(
      ggplot(data = pos_sb_evol_subset,
           aes(x = date)) + 
      geom_col(aes(y = price,  fill = amount_holding, color =amount_holding), width = 1) +
      scale_fill_gradient(low = "white", high = "darkgreen", limits = c(0,NA)) +
      scale_color_gradient(low = "white", high = "darkgreen", limits = c(0,NA)) +
      geom_line(aes(y = price), size = 1) +
      scale_x_date(
        limits = c(start, end),
        expand = c(0,0),
        date_breaks  = "1 month") +
      labs(y = paste0("Stock value [EUR]")) + 
      theme(legend.position = "none") + 
      theme(axis.text.x = element_text(angle = 90))
    )
  return(p)
}


#' create a plot that show how many units of a stock you are holding over time
#' colors the region where you are holding that stock.
#'
#' @param pos_sb_evol_subset 
#'
#' @return
#' @export
plot_market_timing_q = function(pos_sb_evol_subset) {
  start = min(pos_sb_evol_subset$date)
  end = max(pos_sb_evol_subset$date)
  
  # plot 
  p = 
    ggplotly(
      ggplot(data = pos_sb_evol_subset,
             aes(x = date)) + 
      geom_col(aes(y = amount_holding,  fill = amount_holding, color = amount_holding), width = 1) +
      scale_fill_gradient(low = "white", high = "darkgreen", limits = c(0,NA)) +
      scale_color_gradient(low = "white", high = "darkgreen", limits = c(0,NA)) +
      geom_line(aes(y = amount_holding), size = 1) +
      scale_x_date(
        limits = c(start, end),
        expand = c(0,0),
        date_breaks  = "1 month") +
      labs(y = "Position holding [#]") + 
      theme(legend.position = "none") + 
      theme(axis.text.x = element_text(angle = 90))
    )
  
  return(p)
}


#' create a plot that shows how much value of a stock you are holding over time
#' colors the region where you are holding that stock.
#'
#' @param pos_sb_evol_subset 
#'
#' @return
#' @export
plot_market_timing_pq = function(pos_sb_evol_subset) {
  s = pos_sb_evol_subset[1, symbol]

  start = min(pos_sb_evol_subset$date)
  end = max(pos_sb_evol_subset$date)
  # plot 
  p = 
    ggplotly(
      ggplot(data = pos_sb_evol_subset,
             aes(x = date)) + 
      geom_col(aes(y = position_euro,  fill = amount_holding, color = amount_holding), width = 1) +
      scale_fill_gradient(low = "white", high = "darkgreen", limits = c(0,NA)) +
      scale_color_gradient(low = "white", high = "darkgreen", limits = c(0,NA)) +
      geom_line(aes(y = position_euro), size = 1) +
      scale_x_date(
        limits = c(start, end),
        expand = c(0,0),
        date_breaks  = "1 month") +
      labs(y =  paste0("Value holding [EUR]")) +
      theme(legend.position = "none") + 
      theme(axis.text.x = element_text(angle = 90))
    )
  return(p)
}