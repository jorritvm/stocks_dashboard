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


get_stock_key_info = function(profiles) {
  # filter some columns
  result = profiles %>% 
    select(exchange_name, symbol, company_name, currency) %>%
    rename(exchange = exchange_name, company = company_name)
  
  # get latest close data
  close_data = get_latest_close()
  print(close_data)
  
  # join
  result = result %>% inner_join(close_data, by = "symbol")

  print(result)
  
  return(result)
 
}


get_stock_profile_table = function(sym) {
  p_all = get_stock_profiles()
  p_one = p_all %>% 
            filter(symbol == sym)
  p_one_transpose = as_tibble(cbind(nms = names(p_one), t(p_one)))
  names(p_one_transpose) = c("parameter", "value")
  return(p_one_transpose)
}
  

plot_stock_evolution = function(sym, 
                                window) {
  
  print(sym)
  print(window)
  # translate window to start & end days
  end = today()
  if (window == "all") start = NULL
  if (window == "5Y") start = today() - years(5)
  if (window == "3Y") start = today() - years(3)
  if (window == "2Y") start = today() - years(2)
  if (window == "1Y") start = today() - years(1)
  if (window == "6M") start = today() - months(6)
  if (window == "3M") start = today() - months(3)
  if (window == "1M") start = today() - months(1)
  if (window == "2W") start = today() - weeks(2)
  if (window == "1W") start = today() - weeks(1)
  if (window == "YTD") start = ymd(paste0(year(today()), "-01-01"))
  
  # get ohlc data
  data = get_ohlc(sym, start, end)
  start = min(data$date)
  
  # ggplot
  # fig = ggplot(data, aes(x = date, y = close)) +
  #       geom_line(size = 1.5) + 
  #       geom_candlestick(aes(open = open, 
  #                            high = high, 
  #                            low = low, 
  #                            close = close),
  #                        alpha = 0.7) +  
  #       scale_x_date(date_breaks = "1 month", 
  #                   limits = c(start, end),
  #                   expand = c(0,0))
  
  # plotly
  fig <- data %>% plot_ly(x = ~date, type="candlestick",
                        open = ~open, close = ~close,
                        high = ~high, low = ~low) 
  fig <- fig %>% layout(xaxis = list(rangeslider = list(visible = F)))
  fig <- fig %>% add_lines(x = ~date, y = ~close, line = list(color = 'black', width = 0.75), inherit = FALSE)
  fig <- fig %>% layout(showlegend = FALSE)
  
    
  return(fig)
}
