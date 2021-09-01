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