expand_transactions_to_portfolio_positions = function(tr) {
  profiles = get_stock_profiles()
  
  tr = tr[order(date)]
  
  start_date = min(tr$date)
  # end_date = max(tr$date)
  end_date = today()
  
  brokers = unique(tr$account)
  portfolio_list = list()
  for (broker in brokers) {
    trb = tr[account == broker]  
    trb[type == 'sell',         amount := -1 * abs(amount)]
    trb[type == 'transfer_out', amount := -1 * abs(amount)]
    
    # get cumulative position from transactions
    portfolio = trb[type %in% c("buy", "sell", "transfer_in", "transfer_out")]
    portfolio = portfolio[, amount_holding := cumsum(amount), by = symbol]
    portfolio = portfolio[, .(symbol, date, amount_holding)]
    
    # if multiple transactions were done on the same day we need to keep only the last one
    portfolio = portfolio[, .SD[nrow(.SD)], by = .(symbol, date)]
    
    # go wide, one column per stock
    portfolio = dcast(portfolio, formula = date~symbol, value.var = "amount_holding")
    
    # expand timeframe
    dates = data.table(date = seq(start_date,end_date, by = "days"))
    portfolio = merge(x = dates,
                      y = portfolio,
                      by = "date",
                      allow.cartesian = TRUE,
                      all.x = TRUE)
    
    # fill out NA 
    cols = names(portfolio)[-1]
    portfolio = portfolio[, (cols) := lapply(.SD, na.locf.0b), .SDcols = cols]
    
    # bring it back to long form
    portfolio = melt(portfolio,
                     id.vars = 'date',
                     variable.name = "symbol",
                     value.name = "amount_holding")
    
    # set up currency & desired FX
    portfolio = merge(portfolio, 
                      profiles[, .(symbol, currency)], 
                      by = "symbol")
    portfolio[, fx := "EUR/EUR"]
    portfolio[currency != "EUR", fx := paste0(currency, "/EUR")]
    
    # fetch fx rates
    fx = get_all_fx()
    portfolio = merge(portfolio,
                      fx,
                      by = c("date","fx"),
                      all.x = TRUE)
    portfolio[fx == "EUR/EUR", rate := 1]
    portfolio[is.na(fx), rate := 1] # this is a red flag, an FX should be added to the db!
    portfolio = portfolio[, rate := na.locf.0b(rate), by = .(symbol, fx)] # make sure we fill out missing FX until today
    
    # add ohlc data
    ohlc = get_all_ohlc()
    portfolio = merge(portfolio, 
                      ohlc[, .(symbol, date, price = adjusted)], 
                      by = c("symbol", "date"),
                      all.x = TRUE)
   
    # fill out the NA in the OHLC data (e.g. for the weekends)
    portfolio[, price := na.locf.cb(price), by = symbol]
    
    # calculate portfolio position
    portfolio[, euro_position := amount_holding * price * rate]
    
    # sort output
    setorderv(portfolio, c("date","symbol"))
    portfolio_list[[broker]] = portfolio
  }
  
  return(portfolio_list)
}


plot_portfolio_evolution = function(broker, pf_window, portfolio_positions) {
  w = window_to_start_end_dates(pf_window)
  
  if (broker == "All") {
    ppos = rbindlist(portfolio_positions)
  } else {
    ppos = portfolio_positions[[broker]]  
  }
  
  ppos = ppos[date >= w$start & date <= w$end]
  ppos_plotdata = ppos[, .(portfolio = sum(euro_position)), by = .(date)][order(date)]
  
  p = ggplotly(ggplot(ppos_plotdata, 
                      aes(x=date, y=portfolio)) + 
                 geom_line() + 
                 labs(y = "Portfolio [EUR]") +     
                 scale_x_date(limits = c(w$start, w$end), expand = c(0,0)) 
              )
  return(p)
}


#' creates a plot for actual portfolio position per broker
#'
#' @param trpb 
#'
#' @return
#' @export
plot_position_per_broker = function(trpb) {
  trpb = trpb[order(account)]
  fig = plot_ly(x = round(trpb$portfolio,0), 
                y = trpb$account, 
                type = 'bar', 
                orientation = 'h') %>% 
    layout(yaxis = list(autorange="reversed"))
  
  
  return(fig)
}


#' creates a plot for actual portfolio position per stock (irrespective of broker)
#'
#' @param trps 
#'
#' @return
#' @export
plot_position_per_stock = function(trps) {
  trps = trps[order(symbol)]
  fig = plot_ly(x = round(trps$portfolio,0), 
                y = trps$key, 
                type = 'bar', 
                orientation = 'h') %>% 
    layout(yaxis = list(autorange="reversed"))
  
  return(fig)
}

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
