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
#' @param key key string "symbol | company
#' @param window time window to plot
#'
#' @return
#' @export
plot_candlestick = function(k,
                            window,
                            profiles) {
  
  # get profile info
  profile = profiles[key == k]
  
  # get symbol
  sym = key_to_symbol(k)
  
  # translate window to start & end days
  w = window_to_start_end_dates(window)
  
  # get ohlc data
  data = get_ohlc(c(sym), w$start, w$end)
  
  # plotly
  fig <- data %>% plot_ly(x = ~date, type="candlestick",
                          open = ~open, close = ~close,
                          high = ~high, low = ~low) 
  fig <- fig %>% layout(xaxis = list(rangeslider = list(visible = F)),  yaxis = list(title = paste0("Close [",profile$currency, "]")))
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
                          window) {
  
  # get symbols
  sym = key_to_symbol(key)
  bench = key_to_symbol(bench)
  
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
    labs(x = "Date", y = "Relative gain [%]")
  
  return(fig)
}


#' create a plot for an FX
#'
#' @param fx string identifier for FX
#' @param window string representation of time window
#'
#' @return
#' @export
plot_fx = function(fx, window) {
  # translate window to start & end days
  w = window_to_start_end_dates(window)
  
  # get ohlc data
  data = get_fx(fx, w$start, w$end)
  
  # plotly
  fig <- data %>% plot_ly(x = ~date,  line = list(color = 'black', width = 0.75), y = ~rate) 

  return(fig)
}


#' creates a plot for actual portfolio position per broker
#'
#' @param trpb 
#'
#' @return
#' @export
plot_position_per_broker = function(trpb) {
  trpb = trpb[order(account)]
  fig = plot_ly(x = round(trpb$portfolio,0), 
                y = trpb$account, 
                type = 'bar', 
                orientation = 'h') %>% 
        layout(yaxis = list(autorange="reversed"))
  
  
  return(fig)
}


#' creates a plot for actual portfolio position per stock (irrespective of broker)
#'
#' @param trps 
#'
#' @return
#' @export
plot_position_per_stock = function(trps) {
  trps = trps[order(symbol)]
  fig = plot_ly(x = round(trps$portfolio,0), 
                y = trps$key, 
                type = 'bar', 
                orientation = 'h') %>% 
        layout(yaxis = list(autorange="reversed"))
  
  return(fig)
}

#' calculate data linked to market timing for a certain stock
#'
#' @param timing_key key for stock "symbol | company"
#' @param timing_window window for plot
#' @param tr transactions table
#'
#' @return a data.table with structure:
#'         - date: Date
#'         - position: numeric
#'         - close: numeric
#'         - value: numeric
#'         - holding: numeric
#' @export
calculate_market_timing = function(timing_key, timing_window, tr) {
  timing_symbol = key_to_symbol(timing_key)
  w = window_to_start_end_dates(timing_window)
  
  # prepare & filter tr
  trs = copy(tr)
  trs = trs[type == "sell", amount := amount * -1]
  trs = trs[type == "transfer_out", amount := amount * -1]
  trs = trs[type != "div" & symbol == timing_symbol]
  
  # get one amount per day
  trs = trs[,
            .(amount = sum(amount)),
            by = date][amount != 0]
  
  # get the position from the transactions
  trs[, position := cumsum(amount)]
  trs[, amount := NULL]
  
  # filter window
  trs = rbind(trs, 
              data.table(date = c(w$start, w$end), position = c(NA,NA)))
  trs = trs[order(date)]
  trs[, position := na.locf.0b(position)]
  trsub = trs[date >= w$start & date <= w$end]
  
  # pad and add close value
  trsub = pad(trsub, interval = "day")
  trsub[, position := na.locf.0b(position)]
  ohlc = as.data.table(get_ohlc(timing_symbol, w$start, w$end))
  trsub = merge(trsub,
                ohlc[, .(date, close)],
                by = "date",
                all.x = TRUE)
  
  # missing weekend ohlc -> LOCF and also fix leading NA
  trsub[, close := na.locf.cb(close)]

  #valuate
  trsub[, value := close * position]
  trsub[, holding := (position != 0) * close]
  
  # add symbol
  trsub[, symbol := timing_symbol]

  return(trsub)
}
 
#' create a plot that shows price evolution of a stock and colors the region where you are holding that stock
#'
#' @param trsub 
#' @param profiles 
#'
#' @return
#' @export
plot_market_timing_p = function(trsub, profiles) {
  s = trsub[1, symbol]
  profile = profiles[symbol == s]

  start = min(trsub$date)
  end = max(trsub$date)
  # plot 
  p = 
    ggplot(data = trsub,
           aes(x = date)) + 
      geom_area(aes(y = holding,  fill = as.factor(position))) +
      geom_line(aes(y = close)) +
      scale_fill_brewer(palette="Greens") + 
      scale_x_date(
                 limits = c(start, end),
                 expand = c(0,0)) +
      labs(y = paste0("Stock value [", profile$currency, "]")) + 
      theme(legend.position = "none")
           
  return(p)
}

#' create a plot that show how many units of a stock you are holding over time
#'
#' @param trsub 
#'
#' @return
#' @export
plot_market_timing_q = function(trsub) {
  start = min(trsub$date)
  end = max(trsub$date)
  # plot 
  p = 
    ggplot(data = trsub,
           aes(x = date)) + 
    geom_line(aes(y = position)) +
    scale_x_date(
      limits = c(start, end),
      expand = c(0,0)) +
    labs(y = "Position holding [#]") + 
    theme(legend.position = "none")
  
  return(p)
}


#' create a plot that shows how much value of a stock you are holding over time
#'
#' @param trsub 
#' @param profiles 
#'
#' @return
#' @export
plot_market_timing_v = function(trsub, profiles) {
  s = trsub[1, symbol]
  profile = profiles[symbol == s]
  
  start = min(trsub$date)
  end = max(trsub$date)
  # plot 
  p = 
    ggplot(data = trsub,
           aes(x = date)) + 
    geom_line(aes(y = value)) +
    scale_x_date(
      limits = c(start, end),
      expand = c(0,0)) +
    labs(y =  paste0("Value holding [", profile$currency, "]")) +
    theme(legend.position = "none")
  
  return(p)
}

na.locf.cb = function(x) {
  y = na.locf(x, na.rm = FALSE)
  z = na.locf(y, fromLast = TRUE)
  return(z)
}

na.locf.0b = function(x) {
  y = na.locf(x, na.rm = FALSE)
  y[is.na(y)] = 0
  return(y)
}

standardize = function(x) {
  return((x-min(x))/(max(x)-min(x)))
}


