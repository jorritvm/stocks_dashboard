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


get_ohlc = function(sym, start = NULL, end = NULL) {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # use some dplyr for the select query
  result = tbl(con, "stock_ohlc") %>%
            filter(symbol == sym) %>%
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


#' for all stocks, add the OHLC data between latest update & today to the DB
#'
#' @return
#' @export
update_all_ohlc = function() {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # loop all stocks
  latest_close = get_latest_close()
  for (i in 1:nrow(latest_close)) {
    
    symbol = unlist(latest_close[i, "symbol"])
    start_date = ymd(latest_close[i, "latest_close"])
    end_date = today()
    
    if ((end_date - start_date) > 1) {
      new_ohlc_data = get_ohlc_from_api(symbol, start_date + days(1), end_date)     
      new_ohlc_data = new_ohlc_data %>% mutate(date = format_ISO8601(date))
      
      # write the record  
      dbAppendTable(con, "stock_ohlc", new_ohlc_data)
    }
  }
  
  # close
  dbDisconnect(con)  
}
