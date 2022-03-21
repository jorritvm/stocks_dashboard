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
      as.data.table()
  result = result %>%
    mutate(date = ymd(date))
  
  # close
  dbDisconnect(con)  
  
  return(result)
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
