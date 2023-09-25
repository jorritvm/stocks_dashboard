#' read the rapidapi yahoo key from <repo>\private\rapidapi_yahoo_key.txt
#'
#' @return api key as character
#' @export
api_read_key = function() {
  fpfn_env = here("config.env")
  if (file.exists(fpfn_env)) {
    dotenv::load_dot_env(fpfn_env)
  }
  key = Sys.getenv("r_stock_dashboard_api_key")

  return(key)
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


#' returns amount of records in stock_profiles table 
#' 
#' cheap function used to check if the datatable was changed and hook up to reactivePoll
#'
#' @return
#' @export
get_count_profiles = function() {
  n = get_count_table("stock_profiles")
  return(n)
}


#' read the stock profiles table in its entirety
#'
#' @return a data.table where each line documents a stock with structure:
#'         - symbol: character
#'         - currency: character
#'         - exchange_symbol: character
#'         - exchange_name: character
#'         - company_name: character
#'         - business_summary: character
#'         - industry: character
#'         - address1: character
#'         - zip: character
#'         - city: character
#'         - country: character
#'         - website: character
#'         - key: character
#' @export
get_stock_profiles = function() {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # query
  result = tbl(con, "stock_profiles") %>%
    arrange(symbol) %>%
    mutate(key = paste(symbol, company_name, sep = " | ")) %>%
    as.data.table()
  
  # close
  dbDisconnect(con)  
  
  return(result)
}


#' returns a data.frame of with limited key info on all stocks in our DB
#'
#' @param profiles 
#'
#' @return a data.table with structure:
#'         - exchange: character
#'         - symbol: character
#'         - company: character
#'         - currency: character
#'         - price_adj: numeric
#'         - latest_close: Date
#' @export 
get_stock_key_info = function(profiles, ohlc) {
  # filter some columns
  result = profiles %>% 
    select(exchange_name, symbol, company_name, currency) %>%
    rename(exchange = exchange_name, company = company_name)
  
  # get latest close data
  close_data = get_latest_close(ohlc)
  
  # join
  result = result %>% 
    inner_join(close_data[, .(symbol, close)], 
               by = "symbol")
  
  return(result)
}


#' returns a key-value (2 column) table with profile information on a stock
#'
#' @param keyvalue string: "symbol | company"
#'
#' @return a data.table with structure:
#'         - parameter: character
#'         - value: character
#' @export
get_stock_profile_table = function(keyvalue, profiles) {
  profile_one = profiles %>% 
    filter(key == keyvalue)
  profile_one_transpose = cbind(nms = names(profile_one), t(profile_one)) %>% as.data.table()
  setnames(profile_one_transpose, c("parameter", "value"))
  return(profile_one_transpose)
}



#' safely write a new stock profile to the DB, if a record with the same symbol
#' already exists it first deletes the old info, then adds the new data, in
#' order to avoid duplicates
#'
#' @param profile a list containing stock profile info
#'
#' @return
#' @export
safe_write_stock_profile = function(profile) {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # select the symbols already in the data table
  db_symbols = tbl(con, "stock_profiles") %>% 
    select(symbol) %>%
    collect() %>% 
    unlist() %>% 
    unname()
  
  # remove old record if the new one has the same symbol
  if (profile$symbol %in% db_symbols) {
    query = "DELETE FROM stock_profiles WHERE symbol = ?"
    dbExecute(con, query, params = profile$symbol)
  }
  
  # write the record  
  df = as.data.frame(profile)
  dbAppendTable(con, "stock_profiles", df)
  
  # close
  dbDisconnect(con)  
}
