#' imports saxo transaction log into datbase
#'
#' @param dt 
#'
#' @return
#' @export
#'
#' @examples
import_saxo_transaction_log = function(dt) {
  
  # fix types
  dt[, date := ymd(numeric_date_to_iso8601(Transactiedatum))]
  dt[, `Instrument-Id` := as.character(`Instrument-Id`)]
  
  # fix symbol
  if (!"symbol" %in% names(dt)) dt[, symbol := NA]
  upload_saxo_yahoo_map = unique(dt[!is.na(symbol), .(saxo = `Instrument-Id`, yahoo = symbol)])
  full_saxo_yahoo_map = safe_write_saxo_map(upload_saxo_yahoo_map)
  dt = dt %>% left_join(full_saxo_yahoo_map, by = c("Instrument-Id" = "saxo"))
  
  # filter rows
  keep_these_types = c("Verkoop","Aankoop","Cashdividend","Herbeleggingsdividend")
  dt = dt[Event %in% keep_these_types]
  
  # filter columns & set proper names
  dt = dt[, .(symbol = yahoo, 
               date = date,
               type = Event,
               amount = Aantal,
               money = `Geboekt.bedrag.rekeningvaluta`)]
  dt[tolower(type) %like% 'verkoop', type := "sell"]  
  dt[tolower(type) %like% 'aankoop', type := "buy"]  
  dt[tolower(type) %like% 'ividend', type := "div"]  
  dt[amount == "-", amount := ""]
  dt[, amount := as.numeric(amount)]
  dt[, account = "saxo"]
  
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
  
  # upload new entries to the db
  dbAppendTable(con, "saxo_map", new_saxo_yahoo_items)
  
  # now that we have full collection in the DB we extract it again
  full_saxo_yahoo_map =  dbReadTable(con, "saxo_map")
  
  # close the DB connection
  dbDisconnect(con)  
  
  # make sure to also add the new stocks to the DB
  for (i in 1:nrow(new_saxo_yahoo_items)) {
    add_stock(new_bolero_yahoo_items[i, saxo])
  }
  
  # return the full set
  return(full_saxo_yahoo_map)
}
