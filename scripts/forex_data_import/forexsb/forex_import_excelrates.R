################################################
# module to bulk import historical 
# fx rates from 
# https://forexsb.com/historical-forex-data
# download D1 in excel csv format
# ################################################

library(lubridate)
library(here)
library(padr)
source(here("R/lib_currency.R"))
source(here("R/lib_utils.R"))


# set wd to this current folder 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# setup files to parse
forex_list = list(
  c("EUR", "CHF", "C:/dev/R/stocks_dashboard/scripts/forex_data_import/forexsb/EURCHF_D1.csv"),
  c("EUR", "GBP", "C:/dev/R/stocks_dashboard/scripts/forex_data_import/forexsb/EURGBP_D1.csv"),
  c("EUR", "JPY", "C:/dev/R/stocks_dashboard/scripts/forex_data_import/forexsb/EURJPY_D1.csv"),
  c("EUR", "USD", "C:/dev/R/stocks_dashboard/scripts/forex_data_import/forexsb/EURUSD_D1.csv"),
  c("AG", "USD", "C:/dev/R/stocks_dashboard/scripts/forex_data_import/forexsb/XAGUSD_D1.csv"),
  c("AU", "USD", "C:/dev/R/stocks_dashboard/scripts/forex_data_import/forexsb/XAUUSD_D1.csv"),
  c("BRENT", "USD", "C:/dev/R/stocks_dashboard/scripts/forex_data_import/forexsb/BRENTCMDUSD_D1.csv")
)

# parse it
for (forex in forex_list) {

  from = forex[[1]]
  to   = forex[[2]]
  path = forex[[3]]
  
  # --- forward ---
  fxvalue = toupper(paste0(from,"/",to))
  print(paste("writing forex to db for:", fxvalue))
  
  # read and clean
  dt = fread(path)
  dt[, date := ymd(Time)]
  dt = dt[, .(date,  rate = Close)]
  
  # make sure we have an equidistant dataframe and NA are filled using LOCF
  dt = pad(dt, interval = "day")
  dt[, rate := na.locf(rate)]
  dt[, fx := fxvalue]
  setcolorder(dt, c("fx", "date", "rate"))

  # import to db
  safe_write_fx_data (dt)
  
  # --- backward ---
  fxvalue = toupper(paste0(to,"/",from))
  print(paste("writing forex to db for:", fxvalue))
  
  # transform
  dt[, fx := fxvalue]
  dt[, rate := round(1/rate, 6)]

  # import to db
  safe_write_fx_data (dt)
  
}
