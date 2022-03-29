#' creates a plot for actual portfolio position per broker
#'
#' @param pos_b 
#'
#' @return
#' @export
plot_position_per_broker = function(pos_b) {
  pos_b = pos_b[order(account)]
  fig = plot_ly(x = round(pos_b$portfolio,0), 
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
  fig = plot_ly(x = round(pos_s$portfolio,0), 
                y = pos_s$key, 
                type = 'bar', 
                orientation = 'h') %>% 
    layout(yaxis = list(autorange="reversed"))
  
  return(fig)
}


plot_portfolio_evolution = function(broker, pf_window, portfolio_positions) {
  w = window_to_start_end_dates(pf_window)
  
  if (broker == "All") {
    ppos = rbindlist(portfolio_positions)
  } else {
    ppos = portfolio_positions[[broker]]  
  }
  
  ppos = ppos[date >= w$start & date <= w$end]
  ppos_plotdata = ppos[, .(portfolio = sum(euro_position)), by = .(date)][order(date)]
  
  p = ggplotly(ggplot(ppos_plotdata, 
                      aes(x=date, y=portfolio)) + 
                 geom_line() + 
                 labs(y = "Portfolio [EUR]") +     
                 scale_x_date(limits = c(w$start, w$end), expand = c(0,0)) 
  )
  return(p)
}
