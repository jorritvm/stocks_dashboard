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
  tr = tr %>%
        mutate(date = ymd(date)) %>%
        mutate(amount = abs(amount)) %>%    
        arrange(date) %>% 
        as.data.table()
  tr[type == "div", amount := NA]
  
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
  if (names(dt)[1] == "Transactiedatum") {
    file_type = "saxo"
  } else if (names(dt)[1] == "Datum") {
    file_type = "bolero"
  }
  return(file_type)
}


get_current_position_per_stock_and_broker = function(tr) {
  trp = copy(tr)
  
  # correct signs
  trp = trp[type == "sell", amount := amount * -1]
  trp = trp[type == "transfer_out", amount := amount * -1]
  
  # aggregate transactions to current position
  trp = trp[type != "div", 
            .(amount = sum(amount)), 
            by = .(symbol, account)][amount != 0]
  
  # add latest evaluation
  trp = trp %>% 
    left_join(get_latest_close(), by = "symbol")
  trp[, latest_value := amount * price_adj]
  trp[, latest_close := NULL]
  
  return(trp)
}


get_current_position_per_stock = function(trp) {
  result = trp[, .(portfolio = sum(latest_value)), by = .(symbol)]
  return(result)
}


get_current_position_per_broker = function(trp) {
  result = trp[, .(portfolio = sum(latest_value)), by = .(account)]
  return(result)
}
