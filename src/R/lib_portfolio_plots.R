#' creates a plot for actual portfolio position per broker
#'
#' @param pos_b 
#'
#' @return
#' @export
plot_position_per_broker = function(pos_b) {
  pos_b = pos_b[order(account)]
  fig = plot_ly(x = round(pos_b$position_euro,0), 
                y = pos_b$account, 
                type = 'bar', 
                orientation = 'h') %>% 
    layout(yaxis = list(autorange="reversed"))
  
  
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
                                    pos_sb_evol) {
  
  # filter by broker
  ppos = copy(pos_sb_evol)
  if (tolower(broker) != "all") {
    ppos = ppos[account == broker]
  } 
  
  # filter by time window
  w = window_to_start_end_dates(pf_window)
  ppos = ppos[date >= w$start & date <= w$end]
  
  # aggregate
  ppos_plotdata = ppos[, .(portfolio = sum(position_euro)), by = .(date)][order(date)]
  
  # plot
  p = ggplotly(
       ggplot(ppos_plotdata, 
               aes(x=date, y=portfolio)) + 
       geom_line() + 
       labs(y = "Portfolio [EUR]") +     
       scale_x_date(limits = c(w$start, w$end), 
                    expand = c(0,0),
                    date_breaks  = "1 month") +
       theme(axis.text.x = element_text(angle = 90))
  )
  return(p)
}


#' create a plot that shows price evolution of a stock and colors the region where you are holding that stock
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