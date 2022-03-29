#' returns the evolution over time of every stock per broker in euro
#'
#' @param tr transactions table
#' @param ohlc_euro ohlc table in euro
#'
#' @return a data.table with structure:
#'         - account: character
#'         - symbol: character
#'         - date: Date
#'         - amount_holding: numeric
#'         - price: numeric
#'         - position_euro: numeric
#' @export
get_stock_position_over_time_per_stock_and_broker = function(tr, 
                                                             ohlc_euro) {
  tr = tr[order(date)]
  
  start_date = min(tr$date)
  end_date = today()
  
  brokers = unique(tr$account)
  portfolio_list = list()
  for (broker in brokers) {
    trb = tr[account == broker]
    trb[type == 'sell',         amount := -1 * abs(amount)]
    trb[type == 'transfer_out', amount := -1 * abs(amount)]
    
    # get cumulative position from transactions
    pf = trb[type %in% c("buy", "sell", "transfer_in", "transfer_out")]
    pf = pf[, amount_holding := cumsum(amount), by = symbol]
    pf = pf[, .(symbol, date, amount_holding)]
    
    # if multiple transactions were done on the same day we need to keep only the last cumsum row
    pf = pf[, .SD[nrow(.SD)], by = .(symbol, date)]
    
    # go wide, one column per stock
    pf = dcast(pf, formula = date ~ symbol, value.var = "amount_holding")
    
    # expand timeframe
    dates = data.table(date = seq(start_date, end_date, by = "days"))
    pf = merge(
      x = dates,
      y = pf,
      by = "date",
      allow.cartesian = TRUE,
      all.x = TRUE
    )
    
    # fill out NA
    cols = names(pf)[-1]
    pf = pf[, (cols) := lapply(.SD, na.locf.0b), .SDcols = cols]
    
    # bring it back to long form
    pf = melt(pf,
              id.vars = 'date',
              variable.name = "symbol",
              value.name = "amount_holding")
    
    # add ohlc data
    pf = merge(pf,
               ohlc_euro[, .(symbol, date, price = close)],
               by = c("symbol", "date"),
               all.x = TRUE)
    
    # fill out the NA in the OHLC data (e.g. for the weekends)
    pf[, price := na.locf.cb(price), by = symbol]
    
    # calculate portfolio position
    pf[, position_euro := amount_holding * price]
    
    # sort output
    setorderv(pf, c("date", "symbol"))
    portfolio_list[[broker]] = pf
  }
  pf = rbindlist(portfolio_list, idcol = "account")
  
  return(pf)
}


#' returns the current nonzero positions (using latest close) per stock and per broker
#'
#' @param tr transactions data table
#'
#' @return a data.table with structure:
#'         - account: character
#'         - symbol: character
#'         - amount_holding: numeric
#'         - price: numeric
#'         - position_euro: numeric
#' @export
get_current_position_per_stock_and_broker = function(pos_sb_evol) {
  x = pos_sb_evol[order(-date)][, .SD[1], by = .(account, symbol)][amount_holding > 0][order(account, symbol)]
  x[, date := NULL]
  return(x)
}


#' returns an aggregate actual position per broker
#'
#' @param pos_sb output from get_current_position_per_stock_and_broker
#'
#' @return a data.table with structure:
#'         - account: character
#'         - portfolio: numeric
#' @export
get_current_position_per_broker = function(pos_sb) {
  result = pos_sb[, .(position_euro = sum(position_euro)), by = .(account)]
  return(result)
}


#' returns an aggregate actual position per stock
#'
#' @param trp  output from get_current_position_per_stock_and_broker
#'
#' @return a data.table with structure:
#'         - symbol: character
#'         - portfolio: numeric
#'         - company_name: character
#'         - key: character
#' @export
get_current_position_per_stock = function(pos_sb, profiles) {
  pos_s = pos_sb %>% 
    group_by(symbol) %>% 
    summarise(position_euro = sum(position_euro)) %>%
    left_join(profiles[, c("symbol", "company_name", "key")], by = "symbol") %>% 
    as.data.table()
  
  return(pos_s)
}




# TOD0
# expand_transactions_to_cash_positions = function(tr) {
#   profiles = get_stock_profiles()
#   
#   tr = tr[order(date)]
#   
#   start_date = min(tr$date)
#   # end_date = max(tr$date)
#   end_date = today()
#   
#   brokers = unique(tr$account)
#   portfolio_list = list()
#   for (broker in brokers) {
#     trb = tr[account == broker]  
#     trb[type == 'sell',         amount := -1 * abs(amount)]
#     trb[type == 'cash_out',     money  := -1 * abs(money)]
#     portfolio = trb[type %in% c("buy", "sell", "cash_in", "cash_out")]
#     
#     # set up currency & desired FX
#     portfolio = merge(portfolio, 
#                       profiles[, .(symbol, currency)], 
#                       by = "symbol")
#     portfolio[, fx := "EUR/EUR"]
#     portfolio[currency != "EUR", fx := paste0(currency, "/EUR")]
#     
#     # fetch fx rates
#     fx = get_all_fx()
#     portfolio = merge(portfolio,
#                       fx,
#                       by = c("date","fx"),
#                       all.x = TRUE)
#     portfolio[fx == "EUR/EUR", rate := 1]
#     portfolio[is.na(fx), rate := 1] # this is a red flag, an FX should be added to the db!
#     portfolio = portfolio[, rate := na.locf.0b(rate), by = .(symbol, fx)] # make sure we fill out missing FX until today
#     
#     # add ohlc data
#     ohlc = get_all_ohlc()
#     portfolio = merge(portfolio, 
#                       ohlc[, .(symbol, date, price = adjusted)], 
#                       by = c("symbol", "date"),
#                       all.x = TRUE)
#     
#     # fill out the NA in the OHLC data (e.g. for the weekends)
#     portfolio[, price := na.locf.cb(price), by = symbol]
#     
#     
#     
#     
#     
#     # if multiple transactions were done on the same day we need to keep only the last one
#     portfolio = portfolio[, .SD[nrow(.SD)], by = .(symbol, date)]
#     
#     # go wide, one column per stock
#     portfolio = dcast(portfolio, formula = date~symbol, value.var = "amount_holding")
#     
#     # expand timeframe
#     dates = data.table(date = seq(start_date,end_date, by = "days"))
#     portfolio = merge(x = dates,
#                       y = portfolio,
#                       by = "date",
#                       allow.cartesian = TRUE,
#                       all.x = TRUE)
#     
#     # fill out NA 
#     cols = names(portfolio)[-1]
#     portfolio = portfolio[, (cols) := lapply(.SD, na.locf.0b), .SDcols = cols]
#     
#     # bring it back to long form
#     portfolio = melt(portfolio,
#                      id.vars = 'date',
#                      variable.name = "symbol",
#                      value.name = "amount_holding")
#     
# 
#     
#     
#     
#     
#     # calculate portfolio position
#     portfolio[, euro_position := amount_holding * price * rate]
#     
#     # sort output
#     setorderv(portfolio, c("date","symbol"))
#     portfolio_list[[broker]] = portfolio
#   }
#   
#   return(portfolio_list)
# }






#' calculate data linked to market timing for a certain stock
#'
#' @param timing_key key for stock "symbol | company"
#' @param timing_window window for plot
#' @param tr transactions table
#'
#' @return a data.table with structure:
#'         - date: Date
#'         - position: numeric
#'         - close: numeric
#'         - value: numeric
#'         - holding: numeric
#' @export
calculate_market_timing = function(timing_key, timing_window, tr) {
  timing_symbol = key_to_symbol(timing_key)
  w = window_to_start_end_dates(timing_window)
  
  # prepare & filter tr
  trs = copy(tr)
  trs = trs[type == "sell", amount := amount * -1]
  trs = trs[type == "transfer_out", amount := amount * -1]
  trs = trs[type != "div" & symbol == timing_symbol]
  
  # get one amount per day
  trs = trs[,
            .(amount = sum(amount)),
            by = date][amount != 0]
  
  # get the position from the transactions
  trs[, position := cumsum(amount)]
  trs[, amount := NULL]
  
  # filter window
  trs = rbind(trs, 
              data.table(date = c(w$start, w$end), position = c(NA,NA)))
  trs = trs[order(date)]
  trs[, position := na.locf.0b(position)]
  trsub = trs[date >= w$start & date <= w$end]
  
  # pad and add close value
  trsub = pad(trsub, interval = "day")
  trsub[, position := na.locf.0b(position)]
  ohlc = as.data.table(get_ohlc(timing_symbol, w$start, w$end))
  trsub = merge(trsub,
                ohlc[, .(date, close)],
                by = "date",
                all.x = TRUE)
  
  # missing weekend ohlc -> LOCF and also fix leading NA
  trsub[, close := na.locf.cb(close)]
  
  #valuate
  trsub[, value := close * position]
  trsub[, holding := (position != 0) * close]
  
  # add symbol
  trsub[, symbol := timing_symbol]
  
  return(trsub)
}

#' create a plot that shows price evolution of a stock and colors the region where you are holding that stock
#'
#' @param trsub 
#' @param profiles 
#'
#' @return
#' @export
plot_market_timing_p = function(trsub, profiles) {
  s = trsub[1, symbol]
  profile = profiles[symbol == s]
  
  start = min(trsub$date)
  end = max(trsub$date)
  # plot 
  p = 
    ggplot(data = trsub,
           aes(x = date)) + 
    geom_area(aes(y = holding,  fill = as.factor(position))) +
    geom_line(aes(y = close)) +
    scale_fill_brewer(palette="Greens") + 
    scale_x_date(
      limits = c(start, end),
      expand = c(0,0)) +
    labs(y = paste0("Stock value [", profile$currency, "]")) + 
    theme(legend.position = "none")
  
  return(p)
}

#' create a plot that show how many units of a stock you are holding over time
#'
#' @param trsub 
#'
#' @return
#' @export
plot_market_timing_q = function(trsub) {
  start = min(trsub$date)
  end = max(trsub$date)
  # plot 
  p = 
    ggplot(data = trsub,
           aes(x = date)) + 
    geom_line(aes(y = position)) +
    scale_x_date(
      limits = c(start, end),
      expand = c(0,0)) +
    labs(y = "Position holding [#]") + 
    theme(legend.position = "none")
  
  return(p)
}


#' create a plot that shows how much value of a stock you are holding over time
#'
#' @param trsub 
#' @param profiles 
#'
#' @return
#' @export
plot_market_timing_v = function(trsub, profiles) {
  s = trsub[1, symbol]
  profile = profiles[symbol == s]
  
  start = min(trsub$date)
  end = max(trsub$date)
  # plot 
  p = 
    ggplot(data = trsub,
           aes(x = date)) + 
    geom_line(aes(y = value)) +
    scale_x_date(
      limits = c(start, end),
      expand = c(0,0)) +
    labs(y =  paste0("Value holding [", profile$currency, "]")) +
    theme(legend.position = "none")
  
  return(p)
}
