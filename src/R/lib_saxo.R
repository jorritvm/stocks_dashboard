# fpfn = file.choose()
# dt = as.data.table(read.xlsx(fpfn))

import_saxo_transaction_log = function(dt) {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # fix types
  dt[, date := ymd(numeric_date_to_iso8601(Transactiedatum))]
  dt[, `Instrument-Id` := as.character(`Instrument-Id`)]
  
  # fix symbol
  # 1. extract symbols provided by user
  if (!"symbol" %in% names(dt)) dt[, symbol := NA]
  upload_saxo_yahoo_map = unique(dt[!is.na(symbol), .(saxo = `Instrument-Id`, yahoo = symbol)])
  # 2. get the saxo map we already have
  saxo_yahoo_map_db = dbReadTable(con, "saxo_map")
  # 3. keep values that are not yet in the DB
  new_saxo_yahoo_items = 
    anti_join(upload_saxo_yahoo_map, 
              saxo_yahoo_map_db, 
              by = c("saxo", "yahoo"))
  # 4. upload to db
  dbAppendTable(con, "saxo_map", new_saxo_yahoo_items)
  # 5. now that we have full collection in the DB we extract it again
  full_saxo_yahoo_map =  dbReadTable(con, "saxo_map")
  # select the symbols already in the data table
  dbDisconnect(con)  
  
  # 6. merge symbol info to the dt
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
  
  safe_write_transaction_data(dt)
}