rm(list = ls())

library(quantmod)
library(data.table)
library(lubridate)
library(stringr)
library(httr)
library(rjson)
library(dplyr)






#####
# get some profile data in the table
rapidapi_yahoo_key = read_rapidapi_yahoo_key()

profile = get_profile_info_from_api("TSLA", "US", rapidapi_yahoo_key)
safe_write_stock_profile(profile)

profile = get_profile_info_from_api("AAPL", "US", rapidapi_yahoo_key)
safe_write_stock_profile(profile)

profile = get_profile_info_from_api("MSFT", "US", rapidapi_yahoo_key)
safe_write_stock_profile(profile)

####

#####
# get some olhc data in the table
symbol = "IWDA.AS" 

start_date = today() - 20
end_date = today()- 10
ohlc = get_olhc(symbol, start_date, end_date )
safe_write_ohlc_data(ohlc)

start_date = today() - 10
end_date = today()- 0
ohlc = get_olhc(symbol, start_date, end_date )
safe_write_ohlc_data(ohlc)

start_date = today() - 10000
end_date = today()- 0
ohlc = get_olhc(symbol, start_date, end_date )
safe_write_ohlc_data(ohlc)


####