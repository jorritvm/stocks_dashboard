# dt = as.data.table(read.xlsx(file.choose()))

#' import bolero upload file into transaction table
#'
#' @param dt content of a bolero upload file as a data.table with structure
#'         - Datum: numeric
#'         - Transactie: character
#'         - Type.effect: character
#'         - Details: character
#'         - Waarde: numeric
#'         - Munt: character
#'         - symbol: character
#'
#' @return nothing
#' @export
import_bolero_transaction_log = function(dt, ohlc) { 
  # fix types
  dt[, date := ymd(numeric_date_to_iso8601(Datum))]
  dt[, str_date := as.character(date)]

  # extract bolero description details
  dt[, Event := ""]
  dt[Details %like% "Aankoop", Event := "Aankoop"]
  dt[Details %like% "Verkoop", Event := "Verkoop"]
  dt[Details %like% "Provisionering|Terugstorting", Event := "cash_in"]
  dt[Details %like% "Overschrijving naar klant", Event := "cash_out"]
  
  if (any(is.na(dt$Event))) { print(dt) ; stop("Your bolero upload contains an unknown event") }
  
  dt[, bolero_description := str_trim(gsub("Aankoop|Verkoop|Online", "", Details))]
  
  # fix symbol
  if (!"symbol" %in% names(dt)) dt[, symbol := NA]
  upload_bolero_yahoo_map = unique(dt[!is.na(symbol), .(bolero = bolero_description, yahoo = symbol)])
  full_bolero_yahoo_map = safe_write_bolero_map(upload_bolero_yahoo_map)
  dt = dt %>% left_join(full_bolero_yahoo_map, by = c("bolero_description" = "bolero"))
  
  # subset rows for stock operations
  keep_these_types = c("Verkoop","Aankoop","Cashdividend","Herbeleggingsdividend")
  dt_s = dt[Event %in% keep_these_types & Transactie %like% "effecten"]
  
  if (any(is.na(dt_s$yahoo))) { print(dt_s) ; stop("Your bolero upload contains new stocks that need a RIC mapping - complete the symbol column in the upload") }
  
  # bolero does not provide 'amount' so we have to guess it from the ohlc data
  # we use the days' middle price of the day
  mean_ohlc = ohlc %>% 
            mutate(price = (high + low) /2) %>%
            select(symbol, date, price)
  # if you made a transaction today, value date is in the future, so you wont have OHLC data, 
  # so we use the days' median price as price estimate
  mean_ohlc = pad(mean_ohlc, 
          end_val = max(dt_s$date), 
          group = "symbol")
  mean_ohlc[, price := na.locf.cb(price), by = symbol]
  
  dt_s = dt_s %>% 
    left_join(mean_ohlc, by = c("yahoo" = "symbol", "date")) %>%
    mutate(Aantal = round(abs(Waarde) / price))
  
  dt_s = dt_s[, .(symbol = yahoo,
                  date = date,
                  type = Event,
                  amount = Aantal,
                  money = Waarde)]
                  
  # subset rows for cash operations
  dt_c = dt[Event %in% c("cash_in", "cash_out")]
  dt_c = dt_c[, .(symbol, 
                  date, 
                  type = Event, 
                  amount = NA, 
                  money = Waarde)]

  # add cash transactions again
  dt_all = rbind(dt_s,
                 dt_c)
  dt_all[tolower(type) %like% 'verkoop', type := "sell"]
  dt_all[tolower(type) %like% 'aankoop', type := "buy"]
  dt_all[tolower(type) %like% 'ividend', type := "div"] # todo: distinguish dist & acc dividend?
  dt_all[, amount := abs(as.numeric(amount))]
  dt_all[, money := abs(money)]
  dt_all[, account := "bolero"]
  
  safe_write_transaction_data(dt_all)
}


#' add new entries for the BOLERO-Yahoo stock map to the DB if they don't exist yet and returns the full map
#'
#' @param upload_bolero_yahoo_map 
#'
#' @return
#' @export
#'
#' @examples
safe_write_bolero_map = function(upload_bolero_yahoo_map) {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # get the bolero map we already have
  bolero_yahoo_map_db = dbReadTable(con, "bolero_map")
  
  if (nrow(upload_bolero_yahoo_map) > 0) {
    # keep values that are not yet in the DB
    new_bolero_yahoo_items = 
      anti_join(upload_bolero_yahoo_map, 
                bolero_yahoo_map_db, 
                by = c("bolero", "yahoo"))
    
    if (nrow(new_bolero_yahoo_items) > 0) {
      # upload new entries to the db
      dbAppendTable(con, "bolero_map", new_bolero_yahoo_items)
      
      # make sure to also add the new stocks to the DB
      for (i in 1:nrow(new_bolero_yahoo_items)) {
        add_stock(new_bolero_yahoo_items[i, yahoo])
      }
    }
  }
  
  # now that we have full collection in the DB we extract it again
  full_bolero_yahoo_map =  dbReadTable(con, "bolero_map")
  
  # close the DB connection
  dbDisconnect(con)  
  
  # return the full set
  return(full_bolero_yahoo_map)
}
