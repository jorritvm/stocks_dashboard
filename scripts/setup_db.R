#######################################
#
# This setup script will generate the 
# database file with the correct schemas
#
#######################################


#' initialises empty db file
#'
#' @return
#' @export
init_db_file = function() {
  db_loc = get_db_location()
  db_folder = dirname(db_loc)
  if (!dir.exists(db_folder)) dir.create(db_folder, 
                                         showWarnings = FALSE, 
                                         recursive = TRUE)
  if (file.exists(db_loc)) {
    file_backup(db_loc) 
    file.remove(db_loc)
  }
  file.create(db_loc)
}


#' will create an empty sqlite database with the required tables
#'
#' @return
#' @export
init_database = function() {
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # create an empty table for stock profiles
  init_stock_profiles_table(con)
  
  # create an empty table for OHLC data
  init_ohlc_table(con)
  
  # create an empty table for portfolio
  # todo
  
  dbDisconnect(con)
}


#' will create the stock_profiles_table
#'
#' @param con connection to db
#'
#' @return
#' @export
init_stock_profiles_table = function() {

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
  
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  dbExecute(con, query)
  dbDisconnect(con)
}


#' will create the ohlc table
#'
#' @param con connection to db 
#'
#' @return
#' @export
init_ohlc_table = function() {
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
  
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  dbExecute(con, query)
  dbDisconnect(con)
}


#' will create the fx_rates table
#'
#' @param con connection to db 
#'
#' @return
#' @export
init_fxrate_table = function() {

  query = '
    CREATE TABLE fx_rates (
      fx	TEXT,
      date	TEXT,
      rate	real,
      PRIMARY KEY("fx","date")
    );'
  
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  dbExecute(con, query)
  dbDisconnect(con)
}



#' will create the transactions table
#'
#' @param con connection to db 
#'
#' @return
#' @export
init_transactions_table = function() {

  query = '
    CREATE TABLE transactions (
  symbol	TEXT,
  date	TEXT,
  type	TEXT,
  amount	REAL,
  money REAL,
  account TEXT
);'
  
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  dbExecute(con, query)

  dbDisconnect(con)
}


#' will create the saxo map table
#'
#' @param con connection to db 
#'
#' @return
#' @export
init_saxomap_table = function() {

  query = '
    CREATE TABLE "saxo_map" (
      "saxo"	TEXT,
      "yahoo"	TEXT,
      PRIMARY KEY("saxo","yahoo")
    );
  '
  
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  dbExecute(con, query)
  
  dbDisconnect(con)
}


#' will create the bolero map table
#'
#' @param con connection to db 
#'
#' @return
#' @export
init_boleromap_table = function() {
  
  query = '
    CREATE TABLE "bolero_map" (
      "bolero"	TEXT,
      "yahoo"	TEXT,
      PRIMARY KEY("bolero","yahoo")
    );
  '
  
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  dbExecute(con, query)
  
  dbDisconnect(con)
}






##############################
# INIT ROUTINE
##############################

library(here)
library(RSQLite)
library(readr)

# init_db_file()
# init_stock_profiles_table()
# init_ohlc_table()
# init_fxrate_table()
# init_transactions_table()
# init_saxomap_table()
# init_boleromap_table()

