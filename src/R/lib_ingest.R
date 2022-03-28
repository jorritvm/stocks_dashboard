#' read the rapidapi yahoo key from <repo>\private\rapidapi_yahoo_key.txt
#'
#' @return api key as character
#' @export
api_read_key = function() {
  fpfn = file.path(dirname(here()), "private", "rapidapi_yahoo_key.txt")
  apikey = read_file(fpfn)
  return(apikey)
}


#' gather stock information (company profile) 
#'
#' @param symbol 
#' @param region 
#'
#' @return profile as a list, empty list if symbol is not found
#' @export
get_profile_info_from_api = function(symbol, 
                                     region = NULL, 
                                     rapidapi_yahoo_key = api_read_key()) {
  json_response = api_get_profile_as_json(symbol, region, rapidapi_yahoo_key) 
  profile = extract_usefull_company_profile_from_json(json_response)
  return(profile)
}


#' use the yahoo api to get company profile information as a json
#'
#' @param symbol 
#' @param region 
#'
#' @return company profile information as a json
#' @export
api_get_profile_as_json = function(symbol, region, rapidapi_yahoo_key) {
  # url = "https://apidojo-yahoo-finance-v1.p.rapidapi.com/stock/v2/get-profile"
  url = "https://yh-finance.p.rapidapi.com/stock/v2/get-profile"
  
  queryString = list(
    symbol = symbol,
    region = region
  )
  
  response = GET(url,
                 add_headers(`x-rapidapi-key` = rapidapi_yahoo_key),
                 query = queryString, 
                 content_type("application/json"))
  json_response = content(response, "text")
  
  return(json_response)
}


#' processes a json response from the yahoo api to extract the company profile info
#'
#' @param json_txt 
#'
#' @return list of company profile data that could be found in the json 
#'         (e.g. no business address for ETF's, ...)
#' @export
extract_usefull_company_profile_from_json = function(json_txt) {
  
  profile = list()
  if (json_txt != "") {
    json = fromJSON(json_txt)
    
    profile$symbol = json$symbol
    profile$currency = json$price$currency
    profile$exchange_symbol = json$price$exchange
    profile$exchange_name = json$price$exchangeName
    profile$company_name = json$price$longName
    profile$business_summary = json$assetProfile$longBusinessSummary
    profile$industry = json$assetProfile$industry
    profile$address1 = json$assetProfile$address1
    profile$zip = json$assetProfile$zip
    profile$city = json$assetProfile$city
    profile$country = json$assetProfile$country
    profile$website = json$assetProfile$website
  }
  
  return(profile)
}


#' collect the OHLC data of a given stock yahoo api via quantmod
#' 
#' Note: All days in the window [start_date, end_date] that are closing days on the exchange are omitted from the result.
#' If the exchange was closed on start_date the first row of output will correspond to the first day after the start_date where the exchange was open again. 
#' If the exchange was closed on end_date the last row of output will correspond to the last day before the end_date where the exchange was still open.
#' 
#' @param symbol symbol known by yahoo finance to identify the stock
#' @param start_date date object, 
#' @param end_date date object 
#'
#' @return a data.table containing the OHLC information of a stock with structure:
#'         - symbol: character
#'         - date: Date
#'         - open: numeric
#'         - high: numeric
#'         - low: numeric
#'         - close: numeric
#'         - volume: numeric
#'         - adjusted: numeric
#' @export
get_ohlc_from_api = function(symbol, start_date, end_date) {
  # symbol always upper case
  symbol = toupper(symbol)
  
  # query the yahoo api for OHlC data
  query_result = getSymbols(Symbols = symbol, 
                            auto.assign = FALSE)
  names(query_result) = str_replace(names(query_result), paste0(symbol,"."), "")
  
  # subset the result
  subset_string = paste0(as.character(start_date), "/", as.character(end_date))
  query_result = query_result[subset_string]
  
  # convert to data table
  dt_ohlc = as.data.table(query_result)
  setnames(dt_ohlc, "index", "date")
  
  # if all rows are just NA make table empty
  if (sum(complete.cases(dt_ohlc)) == 0) {
    dt_ohlc = dt_ohlc[0]  
  }
  
  if (nrow(dt_ohlc) > 0) {
    # remove tail NA
    dt_ohlc = dt_ohlc[ seq( max(which(!is.na(dt_ohlc$Close))) ) ]
    
    # fill NA with last observation
    dt_ohlc = na.locf(dt_ohlc) 
  }
  
  # add symbol info
  dt_ohlc[, symbol := symbol]
  setcolorder(dt_ohlc, "symbol")
  setnames(dt_ohlc, tolower(names(dt_ohlc)))
  
  return(dt_ohlc)
}


#' for all stocks, add the OHLC data between latest update & today to the DB
#'
#' @return
#' @export
update_all_ohlc = function() {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # loop all stocks
  latest_close = get_latest_close()
  for (i in 1:nrow(latest_close)) {
    
    symbol = unlist(latest_close[i, "symbol"])
    start_date = latest_close[i, latest_close]
    end_date = today()
    
    if ((end_date - start_date) > 1) {i
      new_ohlc_data = get_ohlc_from_api(symbol, start_date + days(1), end_date)     
      if (nrow(new_ohlc_data) > 0) {
        new_ohlc_data = new_ohlc_data %>% mutate(date = format_ISO8601(date))
        
        # sometimes the API fucks up and gives us duplicate symbol-date entries -> fix that here:
        new_ohlc_data = new_ohlc_data[, .SD[1], by = .(symbol, date)]
        
        # write the record  
        dbAppendTable(con, "stock_ohlc", new_ohlc_data)
      }
    }
  }
  
  # close
  dbDisconnect(con)  
}


#' clear the stocks_ohlc table
#'
#' @return
#' @export
wipe_ohlc = function() {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # delete OHLC data
  query = 'DELETE
             FROM 
                stock_ohlc'
  dbExecute(con, query)
  
  # close
  dbDisconnect(con) 
  
  print("OHLC wiped")
}


#' clear and repopulate the stocks_ohlc table with data for all stocks in the profiles table
#'
#' @return
#' @export
reset_ohlc = function() {
  
  wipe_ohlc()

  # fetch anew for every symbol in the profiles table
  symbols = rv$profiles$symbol
  for (symbol in symbols) { 
    # get OHLC
    start_date = today() - 10000
    end_date = today()
    ohlc = get_ohlc_from_api(symbol, start_date, end_date)
    # sometimes the API fucks up and gives us duplicate symbol-date entries -> fix that here:
    ohlc = ohlc[, .SD[1], by = .(symbol, date)]
    safe_write_ohlc_data(ohlc)
  }
  
  print("OHLC data reset")
}


#' collect the FX close data via quantmod oanda API
#'
#' @param fx string for FX separated by '/', e.g. USD/EUR
#' @param start_date date object 
#'
#' @return a data.table containing the OHLC information of a stock 
#' @export
get_fx_from_api = function(fx, start_date) {
  # fx
  fx = toupper(fx)
  fxsplit = str_split(fx, pattern = "/")[[1]]
  fx_num = fxsplit[1] # numerator
  fx_den = fxsplit[2] # denominator
  
  # query the oanda api for fx data
  query_result = getFX(fx, 
                       from = format_ISO8601(start_date),
                       auto.assign = FALSE)

  # fill NA with last observation
  fx_without_na = na.locf(query_result) 
  
  # convert to data table
  dt_fx = as.data.table(fx_without_na)
  setnames(dt_fx, c("date", "rate"))
  
  # add symbol info
  dt_fx[, fx := fx]
  setcolorder(dt_fx, c("fx", "date"))
  
  return(dt_fx)
}


#' for all fx, add the fx data between latest update & today to the DB
#'
#' @return
#' @export
update_all_fx = function() {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # loop all stocks
  latest_fx = get_latest_fx()
  for (i in 1:nrow(latest_fx)) {
    
    fx = unlist(latest_fx[i, "fx"])
    start_date = ymd(latest_fx[i, "latest_fx_date"])
    end_date = today()
    
    if ((end_date - start_date) > 1) {
      new_fx_data = get_fx_from_api(fx, start_date + days(1))     
      new_fx_data = new_fx_data %>% mutate(date = format_ISO8601(date))
      
      # write the record  
      dbAppendTable(con, "fx_rates", new_fx_data)
    }
  }
  
  # close
  dbDisconnect(con)  
}

