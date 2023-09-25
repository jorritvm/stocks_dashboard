# dt = as.data.table(read.xlsx(file.choose()))

#' imports saxo transaction log into database
#'
#' @param dt 
#'
#' @return
#' @export
#'
#' @examples
import_saxo_transaction_log = function(dt, account_id) {
  
  # fix types
  if (class(dt$Transactiedatum) == "numeric") {
    dt[, date := ymd(numeric_date_to_iso8601(Transactiedatum))] 
  } else if (class(dt$Transactiedatum) == "character") {
    dt[, date := dmy(Transactiedatum)] 
  }
  
  # add RIC symbol to the records of this dataset
  if (!"symbol" %in% names(dt)) dt[, symbol := character()]
  
  new_saxo_yahoo_map = unique(dt[!is.na(symbol), .(saxo = `Instrumentsymbool`, yahoo = symbol)])
  full_saxo_yahoo_map = safe_write_saxo_map(new_saxo_yahoo_map)
  dt = dt %>% left_join(full_saxo_yahoo_map, by = c("Instrumentsymbool" = "saxo"))
  
  # map 'Acties' to relevant transaction types
  # currently ignored
  # - corporate actions - bronbelastirng
  # - aandelensplitsing
  # - debetrente
  # - fusie
  # - Roerende voorheffing (Rente)
  # - rente
  # - waardevermindering
  
  dt[, type := ""]
  dt[tolower(Acties) %like% 'ividend', type := "div"] # cashdividend / herbeleggingsdividend
  dt[tolower(Acties) %like% "verkoop.*", type := "sell"]  
  dt[tolower(Acties) %like% "openbaar overnamebod", type := "sell"]  
  dt[tolower(Acties) %like% "ankoop.*", type := "buy"]
  dt[tolower(Acties) %like% "storting*" , type := "cash_in"]  
  dt[tolower(Acties) %like% "opname" , type := "cash_out"]  
  dt[tolower(Acties) %like% "transfer in.*" , type := "transfer_in"]
  
  dt = dt[type != ""]

  # extract amount
  dt[type %in% c("transfer_in", "buy", "sell"),
      amount := as.numeric(str_extract(Acties, "\\d+"))]
  # hack for very unusual openbaar overnamebod
  dt[Acties %like% "overnameb" & yahoo == "TNET.BR", amount := 293]
  
  # filter columns & set proper names
  dt = dt[, .(symbol = yahoo, 
              date = date,
              type,
              amount = abs(amount),
              money =  abs(Boekingsbedrag))]
  
  dt[, account := account_id]
  dt[type %like% "transfer", money := NA]
  
  # make sure that + - + on the same date are aggregated
  dt = dt[, .(amount = sum(amount), money = sum(money)), by = .(date, symbol, type, account)]
  setcolorder(dt, c("symbol", "date", "type", "amount", "money", "account"))

  # push it to the db
  safe_write_transaction_data(dt)
}


#' add new entries for the SAXO-Yahoo stock map to the DB if they don't exist yet
#'
#' @param upload_saxo_yahoo_map 
#'
#' @return
#' @export
#'
#' @examples
safe_write_saxo_map = function(upload_saxo_yahoo_map) {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # get the saxo map we already have
  saxo_yahoo_map_db = dbReadTable(con, "saxo_map")
  
  # keep values that are not yet in the DB
  new_saxo_yahoo_items = 
    anti_join(upload_saxo_yahoo_map, 
              saxo_yahoo_map_db, 
              by = c("saxo", "yahoo"))
  
  if (nrow(new_saxo_yahoo_items) > 0) {
    # upload new entries to the db
    dbAppendTable(con, "saxo_map", new_saxo_yahoo_items)
    
    # make sure to also add the new stocks to the DB
    for (i in 1:nrow(new_saxo_yahoo_items)) {
      add_stock(new_saxo_yahoo_items[i, yahoo])
    }
  }
  # now that we have full collection in the DB we extract it again
  full_saxo_yahoo_map =  dbReadTable(con, "saxo_map")
  
  # close the DB connection
  dbDisconnect(con)  
  
  # return the full set
  return(full_saxo_yahoo_map)
}
