#' convert a window string to start & end dates
#'
#' @param window 
#'
#' @return
#' @export
window_to_start_end_dates = function(window) {
  end = today()
  if (window == "all") start = NULL
  if (window == "5Y") start = today() - years(5)
  if (window == "3Y") start = today() - years(3)
  if (window == "2Y") start = today() - years(2)
  if (window == "1Y") start = today() - years(1)
  if (window == "6M") start = today() - months(6)
  if (window == "3M") start = today() - months(3)
  if (window == "1M") start = today() - months(1)
  if (window == "2W") start = today() - weeks(2)
  if (window == "1W") start = today() - weeks(1)
  if (window == "YTD") start = ymd(paste0(year(today()), "-01-01"))
  
  result = named_list(start, end)
  return(result)
}


#' create a candlestick plot for a stock
#'
#' @param sym stock to plot
#' @param window time window to plot
#'
#' @return
#' @export
plot_candlestick = function(sym,
                            window) {
  
  # translate window to start & end days
  w = window_to_start_end_dates(window)
  
  # get ohlc data
  data = get_ohlc(c(sym), w$start, w$end)
  
  # plotly
  fig <- data %>% plot_ly(x = ~date, type="candlestick",
                          open = ~open, close = ~close,
                          high = ~high, low = ~low) 
  fig <- fig %>% layout(xaxis = list(rangeslider = list(visible = F)))
  fig <- fig %>% add_lines(x = ~date, y = ~close, line = list(color = 'black', width = 0.75), inherit = FALSE)
  fig <- fig %>% layout(showlegend = FALSE)
  
  return(fig)
}


#' create a plot that shows a stock versus a benchmark
#'
#' @param sym stock to plot
#' @param bench stock to use as baseline
#' @param window time window to plot
#'
#' @return
#' @export
plot_benchmark = function(sym,
                          bench,
                          window) {
  
  # translate window to start & end days
  w = window_to_start_end_dates(window)
  
  # get ohlc data
  dt_s = as.data.table(get_ohlc(sym,   w$start, w$end))
  dt_b = as.data.table(get_ohlc(bench, w$start, w$end))
  
  # keep only overlapping window
  start = min(dt_s$date, dt_b$date)
  dt_s = dt_s[date >= start]
  dt_b = dt_b[date >= start]
  
  # add relative positions
  s_ref = dt_s[order(date)][1, adjusted]
  b_ref = dt_b[order(date)][1, adjusted]
  
  dt_s[, relative := round(((adjusted / s_ref)-1)*100, 3)]
  dt_b[, relative := round(((adjusted / b_ref)-1)*100, 3)]
  
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
    labs(x = "Date", y = "Relative gain")
  
  return(fig)
}

plot_fx = function(fx, window) {
  # translate window to start & end days
  w = window_to_start_end_dates(window)
  
  # get ohlc data
  data = get_fx(fx, w$start, w$end)
  
  # plotly
  fig <- data %>% plot_ly(x = ~date,  line = list(color = 'black', width = 0.75), y = ~rate) 

  return(fig)
}
