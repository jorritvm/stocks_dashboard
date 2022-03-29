#' returns amount of records in stock_profiles table 
#' 
#' cheap function used to check if the datatable was changed and hook up to reactivePoll
#'
#' @return
#' @export
get_count_profiles = function() {
  n = get_count_table("stock_profiles")
  return(n)
}


#' read the stock profiles table in its entirety
#'
#' @return a data.table where each line documents a stock with structure:
#'         - symbol: character
#'         - currency: character
#'         - exchange_symbol: character
#'         - exchange_name: character
#'         - company_name: character
#'         - business_summary: character
#'         - industry: character
#'         - address1: character
#'         - zip: character
#'         - city: character
#'         - country: character
#'         - website: character
#'         - key: character
#' @export
get_stock_profiles = function() {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # query
  result = tbl(con, "stock_profiles") %>%
    arrange(symbol) %>%
    mutate(key = paste(symbol, company_name, sep = " | ")) %>%
    as.data.table()
  
  # close
  dbDisconnect(con)  
  
  return(result)
}

#' returns a key-value (2 column) table with profile information on a stock
#'
#' @param keyvalue string: "symbol | company"
#'
#' @return a data.table with structure:
#'         - parameter: character
#'         - value: character
#' @export
get_stock_profile_table = function(keyvalue) {
  p_all = get_stock_profiles()
  p_one = p_all %>% 
    filter(key == keyvalue)
  p_one_transpose = cbind(nms = names(p_one), t(p_one)) %>% as.data.table()
  setnames(p_one_transpose, c("parameter", "value"))
  return(p_one_transpose)
}


#' returns a data.frame of with limited key info on all stocks in our DB
#'
#' @param profiles 
#'
#' @return a data.table with structure:
#'         - exchange: character
#'         - symbol: character
#'         - company: character
#'         - currency: character
#'         - price_adj: numeric
#'         - latest_close: Date
#' @export 
get_stock_key_info = function(profiles) {
  # filter some columns
  result = profiles %>% 
    select(exchange_name, symbol, company_name, currency) %>%
    rename(exchange = exchange_name, company = company_name)
  
  # get latest close data
  close_data = get_latest_close()
  
  # join
  result = result %>% 
    inner_join(close_data, by = "symbol")
  
  return(result)
}


#' returns amount of records in stock_ohlc table 
#' 
#' cheap function used to check if the datatable was changed and hook up to reactivePoll
#'
#' @return
#' @export
get_count_ohlc = function() {
  n = get_count_table("stock_ohlc")
  return(n)
}


#' read entire OHLC table and return data.table
#'
#' @return a data.table containing OHLC data for all symbols with structure:
#'         - symbol: character
#'         - date: Date
#'         - open: numeric
#'         - high: numeric
#'         - low: numeric
#'         - close: numeric
#'         - volume: numeric
#'         - adjusted: numeric
#' @export
get_all_ohlc = function() {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # use some dplyr for the select query
  result = tbl(con, "stock_ohlc") %>%
    as.data.table() %>%
    mutate(date = ymd(date))
  
  # close
  dbDisconnect(con)  
  
  return(result)
}


#' returns OHLC data for the requested symbol(s) from the DB
#'
#' @param sym single symbol string or vector of multiple symbols
#' @param start_date 
#' @param end_date 
#'
#' @return a data.table containing OHLC data for one or multiple symbols with structure:
#'         - symbol: character
#'         - date: Date
#'         - open: numeric
#'         - high: numeric
#'         - low: numeric
#'         - close: numeric
#'         - volume: numeric
#'         - adjusted: numeric
#' @export
get_ohlc = function(sym, start_date = NULL, end_date = NULL) {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # use some dplyr for the select query
  result = tbl(con, "stock_ohlc") %>%
    filter(symbol %in% sym) %>%
    as.data.table()
  result = result %>%
    mutate(date = ymd(date))
  
  if (!is.null(start_date)) {
    result = result %>% 
      filter(date >= start_date)
  }
  if (!is.null(end_date)) {
    result = result %>% 
      filter(date <= end_date)
  } 
  
  # close
  dbDisconnect(con)  
  
  return(result)
}








#' add a stock to the DB by adding profile & ohlc data
#'
#' @param add_symbol yahoo symbol
#' @param region region US or EU, default NULL
#'
#' @return
#' @export
add_stock = function(add_symbol, region = NULL) {
  # get profile
  profile = get_profile_info_from_api(add_symbol, region, api_read_key())
  safe_write_stock_profile(profile)
  
  # get OHLC
  start_date = today() - 10000
  end_date = today()- 0
  ohlc = get_ohlc_from_api(add_symbol, start_date, end_date)
  safe_write_ohlc_data(ohlc) 
}


#' safely write a new stock profile to the DB, if a record with the same symbol
#' already exists it first deletes the old info, then adds the new data, in
#' order to avoid duplicates
#'
#' @param profile a list containing stock profile info
#'
#' @return
#' @export
safe_write_stock_profile = function(profile) {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # select the symbols already in the data table
  db_symbols = tbl(con, "stock_profiles") %>% 
    select(symbol) %>%
    collect() %>% 
    unlist() %>% 
    unname()
  
  # remove old record if the new one has the same symbol
  if (profile$symbol %in% db_symbols) {
    query = "DELETE FROM stock_profiles WHERE symbol = ?"
    dbExecute(con, query, params = profile$symbol)
  }
  
  # write the record  
  df = as.data.frame(profile)
  dbAppendTable(con, "stock_profiles", df)
  
  # close
  dbDisconnect(con)  
}



#' safely write a stock OHLC data to the DB
#' 
#' if part of the OHLC already exists this first deletes the old info, 
#' then adds the new data, in order to avoid duplicates
#'
#' @param ohlc data.table
#'
#' @return
#' @export
safe_write_ohlc_data = function(ohlc) {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # drop database queries that are overlapping
  sym = ohlc[1, symbol]
  from_date = unlist(ohlc[, .(format_ISO8601(min(date)))])
  to_date   = unlist(ohlc[, .(format_ISO8601(max(date)))])
  query = 'SELECT 
              * 
           FROM 
              stock_ohlc 
           WHERE 
              symbol = ? 
           AND 
              date BETWEEN ? AND ?'
  t = dbGetQuery(con, query, params = list(sym, from_date, to_date))
  if (nrow(t) > 0) {
    query = 'DELETE
             FROM 
                stock_ohlc 
             WHERE 
                symbol = ? 
             AND 
                date BETWEEN ? AND ?'
    dbExecute(con, query, params = list(sym, from_date, to_date))
  }
  
  # write new records
  ohlc = ohlc[, date := format_ISO8601(date)]
  dbAppendTable(con, "stock_ohlc", ohlc)
  
  # close
  dbDisconnect(con)  
}


#' remove a stock from our DB tables (OHLC & profiles)
#'
#' @param key_to_remove string "symbol | company"
#'
#' @return
#' @export
remove_stock_from_db = function(key_to_remove) {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  symbol = key_to_symbol(key)
  
  # delete OHLC
  query = 'DELETE
             FROM 
                stock_ohlc 
             WHERE 
                symbol = ?'
  dbExecute(con, query, params = list(symbol))
  
  # delete profile
  query = 'DELETE
             FROM 
                stock_profiles 
             WHERE 
                symbol = ?'
  dbExecute(con, query, params = list(symbol))
  
  # close
  dbDisconnect(con)  
}


#' returns a small table with for each stock the latest close date & value
#'
#' @return a data.table with structure:
#'         - symbol: character
#'         - price_adj: numeric
#'         - latest_close: Date
#' @export
get_latest_close = function() {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  result = dbGetQuery(con, 
                      'SELECT symbol, round(adjusted, 2) as price_adj, max(date) as latest_close
                      FROM stock_ohlc 
                      GROUP BY symbol') %>% as.data.table()
  result[, latest_close := ymd(latest_close)][]
  
  # close
  dbDisconnect(con)  
  
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