#' create a candlestick plot for a stock
#'
#' @param window time window to plot
#' @param k key string "symbol | company" for stock to plot in thick black
#' @param profiles 
#'
#' @return
#' @export
plot_candlestick = function(profiles,
                            ohlc,
                            ohlc_euro,
                            k,
                            in_euro,
                            window) {
  
  # get profile info
  profile = profiles[key == k]
  
  # get symbol
  sym = key_to_symbol(k)
  
  # translate window to start & end days
  w = window_to_start_end_dates(window)
  
  # get ohlc data
  ohlc_choice = if(in_euro) { ohlc_euro } else { ohlc }
  data = get_ohlc(ohlc_choice, c(sym), w$start, w$end)
  
  # currency
  currency = ifelse(in_euro, "EUR", profile$currency)
  
  # plotly
  fig <- data %>% plot_ly(x = ~date, type="candlestick",
                          open = ~open, close = ~close,
                          high = ~high, low = ~low) 
  fig <- fig %>% layout(xaxis = list(rangeslider = list(visible = F)),  yaxis = list(title = paste0("Close [",currency, "]")))
  fig <- fig %>% add_lines(x = ~date, y = ~close, line = list(color = 'black', width = 0.75), inherit = FALSE)
  fig <- fig %>% layout(showlegend = FALSE)
  
  return(fig)
}


#' create a plot that shows a stock versus a benchmark
#'
#' @param key key string "symbol | company" for stock to plot in thick black
#' @param bench key string "symbol | company" for stock to use as baseline
#' @param window time window to plot
#'
#' @return
#' @export
plot_benchmark = function(key,
                          bench,
                          window,
                          in_euro,
                          ohlc,
                          ohlc_euro) {
  
  # get symbols
  sym = key_to_symbol(key)
  bench = key_to_symbol(bench)
  
  # translate window to start & end days
  w = window_to_start_end_dates(window)
  
  # get ohlc data
  ohlc_choice = if(in_euro) { ohlc_euro } else { ohlc }
  dt_s = as.data.table(get_ohlc(ohlc_choice, sym,   w$start, w$end))
  dt_b = as.data.table(get_ohlc(ohlc_choice, bench, w$start, w$end))
  
  # keep only overlapping window
  start = min(dt_s$date, dt_b$date)
  dt_s = dt_s[date >= start]
  dt_b = dt_b[date >= start]
  
  # add relative positions
  s_ref = dt_s[order(date)][1, close]
  b_ref = dt_b[order(date)][1, close]
  
  dt_s[, relative := round(((close / s_ref)-1)*100, 3)]
  dt_b[, relative := round(((close / b_ref)-1)*100, 3)]
  
  # ggplot
  fig = ggplot(data = dt_b, 
               aes(x = date, y = relative)) +
    geom_line(size = 0.75, 
              color = "red", 
              linetype = "dashed") +
    geom_line(data = dt_s,
              size = 1.5) +
    scale_x_date(date_breaks = "1 month",
                 limits = c(start, w$end),
                 expand = c(0,0)) +
    labs(x = "Date", y = "Relative gain [%]")
  
  return(fig)
}