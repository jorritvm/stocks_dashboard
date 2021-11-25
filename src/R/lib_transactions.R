#############################################
# transactions
#############################################

#' read complete transactions table
#'
#' @return
#' @export
get_transactions = function() {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  tr = dbReadTable(con, "transactions")
  tr = tr %>% mutate(date = ymd(date)) %>% arrange(date)
  
  # close
  dbDisconnect(con)  
  
  return(tr)
}

#' safely write a transaction data to the DB, if part of the transactions already exist
#' it first deletes the old info, then adds the new data, in order to avoid duplicates
#'
#' @param tr data.table containing symbol, date, type, amount, money columns
#'
#' @return
#' @export
safe_write_transaction_data = function(tr) {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # get all pre-existing transactions in the DB
  all_tr = get_transactions()
  
  # drop new entries that are already in the DB - use dplyr
  safe_tr = anti_join(tr, 
                      all_tr, 
                      by = c("symbol", "date", "type", "amount", "money"))
  
  # write new records
  safe_tr = safe_tr[, date := format_ISO8601(date)]
  dbAppendTable(con, "transactions", safe_tr)
  
  # select the symbols already in the data table
  dbDisconnect(con)  
}


#' will return a string value to identify the type of input file that was provided
#'
#' @param dt data.table of input file
#'
#' @return
#' @export
check_for_transaction_file_type = function(dt) {
  return("saxo") 
}