library(here)
library(RSQLite)
library(readr)

get_db_location = function() {
  db_folder = file.path(dirname(here()), "db")
  if (!dir.exists(db_folder)) dir.create(db_folder, 
                                         showWarnings = FALSE, 
                                         recursive = TRUE)
  db_file = "data.db"
  db_fpfn = file.path(db_folder, db_file)
  return(db_fpfn)
}

init_database = function() {
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # create an empty table for stock profiles
  init_stock_profiles_table(con)
  
  # create an empty table for ohlc data
  init_ohlc_table(con)
  
  # create an empty table for portfolio
  # todo
  
  dbDisconnect(con)
}

init_stock_profiles_table = function(con) {
  # s = as.data.table(
  #   list(
  #     symbol = character(0),
  #     currency = character(0),
  #     exchange_symbol = character(0),
  #     exchange_name = character(0),
  #     company_name = character(0),
  #     business_summary = character(0),
  #     industry = character(0),
  #     address1 = character(0),
  #     zip = character(0),
  #     city = character(0),
  #     country = character(0),
  #     website = character(0)
  #   )  
  # )
  # dbCreateTable(con, "stock_profiles", s)
  
  query = '
  CREATE TABLE stock_profiles (
  	symbol	TEXT,
  	currency	TEXT,
  	exchange_symbol	TEXT,
  	exchange_name	TEXT,
  	company_name	TEXT,
  	business_summary	TEXT,
  	industry	TEXT,
  	address1	TEXT,
  	zip	TEXT,
  	city	TEXT,
  	country	TEXT,
  	website	TEXT,
  	PRIMARY KEY(symbol)
  );'
  dbExecute(con, query)
}

init_ohlc_table = function(con) {
  # s = as.data.table(
  #   list(
  #     date = character(0),
  #     Open = numeric(0),
  #     High = numeric(0),
  #     Low = numeric(0),
  #     Close = numeric(0),
  #     Volume = numeric(0),
  #     Adjusted = numeric(0)
  #   )
  # )
  # dbCreateTable(con, "stock_ohlc", s)
  
  query = '
  CREATE TABLE "stock_ohlc" (
    "symbol"	TEXT,
    "date"	TEXT,
    "open"	REAL,
    "high"	REAL,
    "low"	REAL,
    "close"	REAL,
    "volume"	REAL,
    "adjusted"	REAL,
    PRIMARY KEY("symbol", "date")
  );'
  dbExecute(con, query)
  
  
}
