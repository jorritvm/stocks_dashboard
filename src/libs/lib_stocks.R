#' read the rapidapi yahoo key from <repo>\private\rapidapi_yahoo_key.txt
#'
#' @return api key as character
#' @export
read_rapidapi_yahoo_key = function() {
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
  json_response = query_yahoo_api_profile(symbol, region, rapidapi_yahoo_key) 
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
query_yahoo_api_profile = function(symbol, region, rapidapi_yahoo_key) {
  url = "https://apidojo-yahoo-finance-v1.p.rapidapi.com/stock/v2/get-profile"
  
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


#' collect the OHLC data of a given stock
#'
#' @param symbol symbol known by yahoo finance to identify the stock
#' @param start_date date object 
#' @param end_date date object 
#' @param return_type 'xts' (default) or 'data.table')
#'
#' @return a data.table containing the OLHC information of a stock 
#' @export
get_olhc = function(symbol, start_date, end_date) {

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


#' #' removes NA's in the OHLC table using LOCF
#' #'
#' #' @param ohlc 
#' #'
#' #' @return the OLHC table without NA's
#' #' @export
#' cleanup_na = function(ohlc) {
#'   # last observation carried forward
#'   without_na = na.locf(ohlc)  
#'   return(without_na)
#' }