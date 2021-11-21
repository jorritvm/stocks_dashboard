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
get_profile_info_from_api = function(symbol, region, rapidapi_yahoo_key) {
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
#' @param symbol symbol known by yahoo finance to identify the stock
#' @param start_date date object 
#' @param end_date date object 
#'
#' @return a data.table containing the OHLC information of a stock 
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
  
  # fill NA with last observation
  ohlc_without_na = na.locf(query_result) 
  
  # convert to data table
  dt_ohlc = as.data.table(ohlc_without_na)
  setnames(dt_ohlc, "index", "date")
  
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
    start_date = ymd(latest_close[i, "latest_close"])
    end_date = today()
    
    if ((end_date - start_date) > 1) {
      new_ohlc_data = get_ohlc_from_api(symbol, start_date + days(1), end_date)     
      new_ohlc_data = new_ohlc_data %>% mutate(date = format_ISO8601(date))
      
      # write the record  
      dbAppendTable(con, "stock_ohlc", new_ohlc_data)
    }
  }
  
  # close
  dbDisconnect(con)  
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
    
    symbol = unlist(latest_fx[i, "fx"])
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

