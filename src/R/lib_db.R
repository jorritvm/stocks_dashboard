#' returns the location of the sqlite database as a file path string
#'
#' @return
#' @export
get_db_location = function() {
  db_loc = file.path(dirname(here()), 
                     "db",
                     "data.db")
  return(db_loc)
}


#' read the stock profiles table in its entirety
#'
#' @return tibble
#' @export
get_stock_profiles = function() {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # query
  result = tbl(con, "stock_profiles") %>%
    arrange(symbol) %>%
    as_tibble()
  
  # close
  dbDisconnect(con)  
  
  return(result)
}


#' returns a key-value (2 column) table with profile information on a stock
#'
#' @param sym string stock symbol
#'
#' @return
#' @export
get_stock_profile_table = function(sym) {
  p_all = get_stock_profiles()
  p_one = p_all %>% 
    filter(symbol == sym)
  p_one_transpose = as_tibble(cbind(nms = names(p_one), t(p_one)))
  names(p_one_transpose) = c("parameter", "value")
  return(p_one_transpose)
}


#' returns a data.frame of with limited key info on all stocks in our DB
#'
#' @param profiles 
#'
#' @return
#' @export 
get_stock_key_info = function(profiles) {
  # filter some columns
  result = profiles %>% 
    select(exchange_name, symbol, company_name, currency) %>%
    rename(exchange = exchange_name, company = company_name)
  
  # get latest close data
  close_data = get_latest_close()
  
  # join
  result = result %>% inner_join(close_data, by = "symbol")
  
  return(result)
  
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
  
  dbDisconnect(con)  
}


#' returns OHLC data for the requested symbol(s)
#'
#' @param sym single symbol string or vector of multiple symbols
#' @param start date
#' @param end date
#'
#' @return tibble
#' @export
get_ohlc = function(sym, start = NULL, end = NULL) {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # use some dplyr for the select query
  result = tbl(con, "stock_ohlc") %>%
            filter(symbol %in% sym) %>%
            as_tibble()
  result = result %>%
           mutate(date = ymd(date))
  result
  if (!is.null(start)) {
    result = result %>% 
              filter(date >= start)
  }
  if (!is.null(end)) {
    result = result %>% 
      filter(date <= end)
  } 
  
  # select the symbols already in the data table
  dbDisconnect(con)  
  
  return(result)
}

#' safely write a stock OHLC data to the DB, if part of the OHLC already exists
#' it first deletes the old info, then adds the new data, in order to avoid duplicates
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
  
  # select the symbols already in the data table
  dbDisconnect(con)  
}


#' remove a stock from our DB tables
#'
#' @param symbol 
#'
#' @return
#' @export
remove_stock_from_db = function(symbol) {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
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
  
  # select the symbols already in the data table
  dbDisconnect(con)  
}


#' returns a small table with for each stock the latest close date & value
#'
#' @return
#' @export
#'
#' @examples
get_latest_close = function() {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  result = dbGetQuery(con, 
                      'SELECT symbol, round(adjusted, 2) as price_adj, max(date) as latest_close
                      FROM stock_ohlc 
                      GROUP BY symbol') %>% as_tibble()
  
  # select the symbols already in the data table
  dbDisconnect(con)  
  
  return(result)
  
}



