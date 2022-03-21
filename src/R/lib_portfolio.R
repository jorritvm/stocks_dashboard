expand_transactions_to_portfolio_positions = function(tr) {
  profiles = get_stock_profiles()
  
  tr = tr[order(date)]
  
  start_date = min(tr$date)
  end_date = max(tr$date)
  
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
