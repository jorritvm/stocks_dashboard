#' collect the OHLC data of a given stock yahoo api via quantmod
#' 
#' Note: All days in the window [start_date, end_date] that are closing days on the exchange are omitted from the result.
#' If the exchange was closed on start_date the first row of output will correspond to the first day after the start_date where the exchange was open again. 
#' If the exchange was closed on end_date the last row of output will correspond to the last day before the end_date where the exchange was still open.
#' 
#' @param symbol symbol known by yahoo finance to identify the stock
#' @param start_date date object, 
#' @param end_date date object 
#'
#' @return a data.table containing the OHLC information of a stock with structure:
#'         - symbol: character
#'         - date: Date
#'         - open: numeric
#'         - high: numeric
#'         - low: numeric
#'         - close: numeric
#'         - volume: numeric
#'         - adjusted: numeric
#' @export
get_ohlc_from_api = function(symbol, start_date, end_date) {
  # symbol always upper case
  symbol = toupper(symbol)
  
  # query the yahoo api for OHlC data
  query_result = getSymbols(Symbols = symbol, 
                            auto.assign = FALSE)
  names(query_result) = str_replace(names(query_result), paste0(symbol,"."), "")
  
  # subset the result
  subset_string = paste0(as.character(start_date), "/", as.character(end_date))
  query_result = query_result[subset_string]
  
  # convert to data table
  dt_ohlc = as.data.table(query_result)
  setnames(dt_ohlc, "index", "date")
  
  # if all rows are just NA make table empty
  if (sum(complete.cases(dt_ohlc)) == 0) {
    dt_ohlc = dt_ohlc[0]  
  }
  
  if (nrow(dt_ohlc) > 0) {
    # remove tail NA
    dt_ohlc = dt_ohlc[ seq( max(which(!is.na(dt_ohlc$Close))) ) ]
    
    # fill NA with last observation
    dt_ohlc = copy(na.locf(dt_ohlc))
  }
  
  # add symbol info
  dt_ohlc[, symbol := symbol]
  setcolorder(dt_ohlc, "symbol")
  setnames(dt_ohlc, tolower(names(dt_ohlc)))
  
  return(dt_ohlc)
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


#' extend OHLC table with closing price in euros
#'
#' @param ohlc 
#' @param profiles 
#' @param fx 
#'
#' @return a data.table with structure:
#'         - symbol: character
#'         - date: Date
#'         - open: numeric
#'         - high: numeric
#'         - low: numeric
#'         - close: numeric
#'         - volume: numeric
#'         - adjusted: numeric
#'         - close_in_euro: numeric
convert_ohlc_to_euro = function(ohlc, profiles, fx) {
  # get the currency and put equivalent closing value in euro
  ohlc_euro = ohlc %>% 
    left_join(select(profiles, symbol, currency), by = "symbol") %>%
    mutate(fx = paste0(currency,"/EUR")) %>%
    left_join(fx, by = c("fx", "date"))
  
  ohlc_euro[fx == "EUR/EUR", rate := 1]
  cols = c("open", "high", "low", "close", "adjusted")
  ohlc_euro[, (cols) := lapply(.SD, function(x) {x * rate} ), .SDcols = cols]
  
  # drop excess tables
  ohlc_euro[, currency := NULL]
  ohlc_euro[, fx := NULL]
  ohlc_euro[, rate := NULL]
  
  # drop data with NA because we are missing data (like fx) sometimes
  ohlc_euro = ohlc_euro[complete.cases(ohlc_euro)]
  
  return(ohlc_euro)
}


#' returns a small table with for each stock the latest close date & value
#'
#' @return a data.table with structure:
#'         - symbol: character
#'         - date: Date
#'         - open: numeric
#'         - high: numeric
#'         - low: numeric
#'         - close: numeric
#'         - volume: numeric
#'         - adjusted: numeric
#' @export
get_latest_close = function(ohlc) {
  result = ohlc[order(-date)][, .SD[1], by = symbol]
  return(result)
}


#' returns OHLC data for the requested symbol(s) from the DB
#'
#' @param sym single symbol string or vector of multiple symbols
#' @param start_date date object
#' @param end_date date object
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
get_ohlc = function(ohlc, sym, start_date = NULL, end_date = NULL) {
  
  result = ohlc %>%
    filter(symbol %in% sym) 
  
  if (!is.null(start_date)) {
    result = result %>% 
      filter(date >= start_date)
  }
  if (!is.null(end_date)) {
    result = result %>% 
      filter(date <= end_date)
  } 
  result = result %>% 
    arrange(date, symbol)
  
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
  # make sure it does not exist in our DB
  remove_stock_from_db_by_symbol(add_symbol)
  
  # get profile
  profile = get_profile_info_from_api(add_symbol, region, api_read_key())
  safe_write_stock_profile(profile)
  
  # get OHLC
  start_date = today() - 10000
  end_date = today()- 0
  ohlc = get_ohlc_from_api(add_symbol, start_date, end_date)
  safe_write_ohlc_data(ohlc) 
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
  symbol = key_to_symbol(key_to_remove)
  remove_stock_from_db_by_symbol(symbol)
}

remove_stock_from_db_by_symbol = function(symbol) {
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
  
  # close
  dbDisconnect(con)  
}


#' for all stocks, add the OHLC data between latest update & today to the DB
#'
#' @return
#' @export
update_all_ohlc = function(ohlc) {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # loop all stocks
  latest_close = get_latest_close(ohlc)
  for (i in 1:nrow(latest_close)) {
    
    symbol = unlist(latest_close[i, "symbol"])
    start_date = latest_close[i, date]
    end_date = today()
    
    if ((end_date - start_date) > 1) {i
      new_ohlc_data = get_ohlc_from_api(symbol, start_date + days(1), end_date)     
      if (nrow(new_ohlc_data) > 0) {
        new_ohlc_data = new_ohlc_data %>% mutate(date = format_ISO8601(date))
        
        # sometimes the API fucks up and gives us duplicate symbol-date entries -> fix that here:
        new_ohlc_data = new_ohlc_data[, .SD[1], by = .(symbol, date)]
        
        # write the record  
        #dbAppendTable(con, "stock_ohlc", new_ohlc_data)
      }
    }
  }
  
  # close
  dbDisconnect(con)  
}


#' clear the stocks_ohlc table
#'
#' @return
#' @export
wipe_ohlc = function() {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # delete OHLC data
  query = 'DELETE
             FROM 
                stock_ohlc'
  dbExecute(con, query)
  
  # close
  dbDisconnect(con) 
  
  print("OHLC wiped")
}


#' clear and repopulate the stocks_ohlc table with data for all stocks in the profiles table
#'
#' @return
#' @export
reset_ohlc = function() {
  
  wipe_ohlc()
  
  # fetch anew for every symbol in the profiles table
  symbols = rv$profiles$symbol
  for (symbol in symbols) { 
    # get OHLC
    start_date = today() - 10000
    end_date = today()
    ohlc = get_ohlc_from_api(symbol, start_date, end_date)
    # sometimes the API fucks up and gives us duplicate symbol-date entries -> fix that here:
    ohlc = ohlc[, .SD[1], by = .(symbol, date)]
    safe_write_ohlc_data(ohlc)
  }
  
  print("OHLC data reset")
}