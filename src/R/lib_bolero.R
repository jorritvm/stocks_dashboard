# dt = as.data.table(read.xlsx(file.choose()))

import_bolero_transaction_log = function(dt) { 
  # fix types
  dt[, date := ymd(numeric_date_to_iso8601(Datum))]
  dt[, str_date := as.character(date)]

  # extract bolero description details
  dt[, Event := ""]
  dt[Details %like% "Aankoop", Event := "Aankoop"]
  dt[Details %like% "Verkoop", Event := "Verkoop"]
  dt[, bolero_description := str_trim(gsub("Aankoop|Verkoop|Online", "", Details))]
  
  # fix symbol
  if (!"symbol" %in% names(dt)) dt[, symbol := NA]
  upload_bolero_yahoo_map = unique(dt[!is.na(symbol), .(bolero = bolero_description, yahoo = symbol)])
  full_bolero_yahoo_map = safe_write_bolero_map(upload_bolero_yahoo_map)
  dt = dt %>% left_join(full_bolero_yahoo_map, by = c("bolero_description" = "bolero"))
  
  # filter rows
  keep_these_types = c("Verkoop","Aankoop","Cashdividend","Herbeleggingsdividend")
  dt = dt[Event %in% keep_these_types & Transactie %like% "effecten"]
  
  # bolero does not provide 'amount' so we have to guess it from the ohlc data!
  # if you made a transaction today, value date is in the future, so you wont have OHLC data, 
  # so we use todays close as future price estimate
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  df_ohlc = dbReadTable(con, "stock_ohlc")
  df_ohlc = df_ohlc %>% 
            mutate(price = (high + low) /2) %>%
            select(symbol, date, price)
  dbDisconnect(con)
  df_ohlc = df_ohlc %>% 
            full_join(dt[, .(date = str_date, symbol = yahoo)], by = c("symbol","date")) %>%
            arrange(symbol, date) %>%
            fill(price) %>%
            mutate(date = ymd(date))
  
  dt_p = dt %>% 
    left_join(df_ohlc, by = c("yahoo" = "symbol", "date")) %>%
    mutate(Aantal = round(abs(Waarde) / price))
  dt_p 
  
  # filter columns & set proper names
  dt_p = dt_p[, .(symbol = yahoo,
                  date = date,
                  type = Event,
                  amount = Aantal,
                  money = Waarde)]
  dt_p[tolower(type) %like% 'verkoop', type := "sell"]
  dt_p[tolower(type) %like% 'aankoop', type := "buy"]
  dt_p[tolower(type) %like% 'ividend', type := "div"]
  dt_p[, amount := as.numeric(amount)]
  dt_p[, account = "bolero"]
  
  safe_write_transaction_data(dt_p)
}


#' add new entries for the BOLERO-Yahoo stock map to the DB if they don't exist yet
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
  
  # keep values that are not yet in the DB
  new_bolero_yahoo_items = 
    anti_join(upload_bolero_yahoo_map, 
              bolero_yahoo_map_db, 
              by = c("bolero", "yahoo"))
  
  # upload new entries to the db
  dbAppendTable(con, "bolero_map", new_bolero_yahoo_items)
  
  # now that we have full collection in the DB we extract it again
  full_bolero_yahoo_map =  dbReadTable(con, "bolero_map")
  
  # close the DB connection
  dbDisconnect(con)  
  
  # make sure to also add the new stocks to the DB
  for (i in 1:nrow(new_bolero_yahoo_items)) {
    add_stock(new_bolero_yahoo_items[i, yahoo])
  }
  
  # return the full set
  return(full_bolero_yahoo_map)
}
