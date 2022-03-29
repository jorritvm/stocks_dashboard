#' returns amount of records in transactions table 
#' 
#' cheap function used to check if the datatable was changed and hook up to reactivePoll
#'
#' @return
#' @export
get_count_transactions = function() {
  n = get_count_table("transactions")
  return(n)
}



#' read complete transactions table
#'
#' @return a data.table with structure:
#'         - symbol: character
#'         - date: Date
#'         - type: character
#'         - amount: numeric
#'         - money: numeric
#'         - account: character
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
                      by = c("symbol", "date", "type", "amount", "money", "account"
                             ))
  
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
#' @return a string stating the 'source of the input file' e.g. 'bolero' or 'saxo'
#' @export
check_for_transaction_file_type = function(dt) {
  if (names(dt)[1] == "Transactiedatum") {
    file_type = "saxo"
  } else if (names(dt)[1] == "Datum") {
    file_type = "bolero"
  }
  return(file_type)
}


#' returns the current position (using latest close) per stock and per broker
#'
#' @param tr transactions data table
#'
#' @return a data.table with structure:
#'         - symbol: character
#'         - account: character
#'         - amount: numeric
#'         - price_adj: numeric
#'         - latest_value: numeric
#' @export
get_current_position_per_stock_and_broker = function(tr) {
  trp = copy(tr)
  
  # correct signs
  trp = trp[type == "sell", amount := amount * -1]
  trp = trp[type == "transfer_out", amount := amount * -1]
  
  # aggregate transactions to current position
  trp = trp[type != "div" & !type %like% "cash", 
            .(amount = sum(amount)), 
            by = .(symbol, account)][amount != 0]
  
  # add latest evaluation
  trp = trp %>% 
    left_join(get_latest_close(), by = "symbol") %>% 
    copy()
  trp[, latest_value := amount * price_adj]
  trp[, latest_close := NULL][]
  
  return(trp)
}


#' returns an aggregate actual position per stock
#'
#' @param trp  output from get_current_position_per_stock_and_broker
#'
#' @return a data.table with structure:
#'         - symbol: character
#'         - portfolio: numeric
#'         - company_name: character
#'         - key: character
#' @export
get_current_position_per_stock = function(trp, profiles) {
  trps = trp %>% 
    group_by(symbol) %>% 
    summarise(portfolio = sum(latest_value)) %>%
    left_join(profiles[, c("symbol", "company_name", "key")], by = "symbol") %>% 
    as.data.table()
                                      
  return(trps)
}


#' returns an aggregate actual position per broker
#'
#' @param trp output from get_current_position_per_stock_and_broker
#'
#' @return a data.table with structure:
#'         - account: character
#'         - portfolio: numeric
#' @export
get_current_position_per_broker = function(trp) {
  result = trp[, .(portfolio = sum(latest_value)), by = .(account)]
  return(result)
}
