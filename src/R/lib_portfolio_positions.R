#' extend transactions datatable with cumulative data on amount holding and cash position
#'
#' @param tr 
#'
#' @return a data.table with structure:
#'         - symbol: character
#'         - date: Date
#'         - type: character
#'         - amount: numeric
#'         - money: numeric
#'         - account: character
#'         - cash_delta: numeric
#'         - cash_position: numeric
#'         - amount_delta: numeric
#'         - amount_holding: numeric
#' @export
extend_transactions_with_cumulative_data = function(tr, ohlc) {
  
  brokers = unique(tr$account)
  tr_ext_list = list()
  
  for (broker in brokers) {
    trb = tr[account == broker][order(date)]
    
    # establish cash position evolution
    trb[, cash_delta := 0]
    trb[type %in% c('cash_in' , 'sell', 'div'), cash_delta :=  1 * abs(money)]
    trb[type %in% c('cash_out', 'buy')       ,  cash_delta := -1 * abs(money)]
    trb[, cash_position := cumsum(cash_delta)]
    
    # establish the amount holding evolution
    trb[, amount_delta := 0]
    trb[type %in% c('transfer_in',  'buy') , amount_delta :=  1 * abs(amount)]
    trb[type %in% c('transfer_out', 'sell'), amount_delta := -1 * abs(amount)]
    trb[, amount_holding := cumsum(amount_delta), by = symbol]
    
    # extend with single share transaction value 
    # for transactions the amount of shares and total price are known
    # for transfers the share price is not known so take the day's close price
    trb[, transaction_price := 0]
    trb[type %in% c("buy", "sell"), transaction_price := money / amount]
    trb = trb %>% left_join(ohlc[, .(symbol, date, close)], by = c("symbol", "date"))
    trb[type %in% c("transfer_in", "transfer_out"), transaction_price := close]
    trb[, close:=NULL]
        
    # extend with mean acquire price and mean acquire value
    trb[, mean_acquire_price := 0]
    trb[, mean_acquire_value := 0]
    trb_per_stock = list()
    for (stock in unique(trb$symbol)) {
      if (is.na(stock)) {
        trb_s = trb[is.na(symbol)]
      } else {
        trb_s = trb[symbol == stock]
      
        map = 0 # map = mean acquire price
        for (j in 1:nrow(trb_s)) {
          if (trb_s[j, type] %in% c("buy", "transfer_in")) {
            previously_holding = trb_s[j, amount_holding] - trb_s[j, amount_delta]
            newly_acquired = trb_s[j, amount_delta]
            currently_holding = trb_s[j, amount_holding]
            transaction_price = trb_s[j, transaction_price]
            map = (map *  previously_holding + transaction_price * newly_acquired) / currently_holding
            trb_s[j, mean_acquire_price := map]
            trb_s[j, mean_acquire_value := map * currently_holding]
          } else if (trb_s[j, type] %in% c("sell", "transfer_out")) {
            currently_holding = trb_s[j, amount_holding]
            trb_s[j, mean_acquire_price := map]
            trb_s[j, mean_acquire_value := map * currently_holding]
          }
        }
      }
      trb_per_stock[[stock]] = trb_s
    }
    trb = rbindlist(trb_per_stock) 
    trb = trb[order(date)]
    
    tr_ext_list[[broker]] = trb
  }
  tr_ext = rbindlist(tr_ext_list)
  return(tr_ext)
}


#' returns the evolution over time of the cash position per broker in euro
#'
#' @param tr_ext 
#'
#' @return
#' @export
get_cash_position_over_time_per_broker = function(tr_ext) {
  
  brokers = unique(tr_ext$account)
  cash_position_list = list()

  for (broker in brokers) {
    cash = tr_ext[account == broker][order(date)]
    
    # filter to positions that have impacted cash position
    cash = cash[abs(cash_delta) != 0]
    
    # if multiple transactions were done on the same day we need to keep only the last one
    cash = cash[, .SD[nrow(.SD)], by = .(date)]
    
    # expand timeframe
    if (nrow(cash) == 0) {
      cash =  data.table(date = today(), cash_position = 0)
    } 
    cp = pad(cash[, .(date, cash_position)], 
             interval = "day", 
             start_val = min(tr_ext$date), 
             end_val = today())
       
    # fill out NA
    cp[, cash_position := na.locf.0b(cash_position)]
    
    cash_position_list[[broker]] = cp
  }
  cash_position = rbindlist(cash_position_list, idcol = "account")
}
  
#' returns latest cash postion per broker
#'
#' @param cash_b_evol 
#'
#' @return
#' @export
get_current_cash_per_broker = function(cash_b_evol) {
    cpos = cash_b_evol[order(-date)][, .SD[1], by = account][, .(account, cash = cash_position)]  
    return(cpos)
}


#' returns the evolution over time of every stock per broker in euro
#'
#' @param ohlc_euro ohlc table in euro
#' @param tr_ext extended transactions table
#'
#' @return a data.table with structure:
#'         - account: character
#'         - symbol: character
#'         - date: Date
#'         - amount_holding: numeric
#'         - price: numeric
#'         - position_euro: numeric
#' @export
get_stock_position_over_time_per_stock_and_broker = function(tr_ext, 
                                                             ohlc_euro) {
  
  # extend tr with cumulative data
  start_date = min(tr_ext$date)
  end_date = today()
  
  brokers = unique(tr_ext$account)
  portfolio_list = list()
  for (broker in brokers) {
    pf = tr_ext[account == broker][order(date)]
    
    # filter to positions that have impacted stock positions
    pf = pf[abs(amount_delta) > 0]
    
    # if multiple transactions were done on the same day we need to keep only the last one
    pf = pf[, .SD[nrow(.SD)], by = .(date, symbol)]
    setnames(pf,
             c("amount_holding", 
               "mean_acquire_price", 
               "mean_acquire_value"),
             c("amo",
               "map",
               "mav"))
    
    # go wide, one column per stock
    pf = dcast(pf, formula = date ~ symbol, value.var = c("amo",
                                                          "map",
                                                          "mav"))
    
    # expand timeframe
    start_date = min(tr_ext$date)
    end_date = today()
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
              variable.name = "col",
              value.name = "value")
    pf[, c("what", "symbol") := tstrsplit(col, "_", fixed=TRUE)]
    pf = dcast(pf, date + symbol ~ what, value.var = "value")
    setnames(pf,
             c("amo",
               "map",
               "mav"),
            c("amount_holding", 
              "mean_acquire_price", 
              "mean_acquire_value"))

    # add ohlc data
    pf = merge(pf,
               ohlc_euro[, .(symbol, date, price = close)],
               by = c("symbol", "date"),
               all.x = TRUE)
    
    # fill out the NA in the OHLC data (e.g. for the weekends)
    pf[, price := na.locf.cb(price), by = symbol]
    
    # calculate portfolio position
    pf[, position_euro := amount_holding * price]
    
    # calculate performance in percent
    pf[, performance_rel := ((position_euro / mean_acquire_value) -1) * 100]
    pf[is.nan(performance_rel), performance_rel := 0]
    pf[is.infinite(performance_rel), performance_rel := 0]
    
    pf[, performance_abs := (position_euro - mean_acquire_value)]
    pf[is.nan(performance_abs), performance_abs := 0]
    pf[is.infinite(performance_abs), performance_abs := 0]
    
    # sort output
    setorderv(pf, c("date", "symbol"))
    portfolio_list[[broker]] = pf
  }
  pf = rbindlist(portfolio_list, idcol = "account")
  
  return(pf)
}


#' calculate data linked to market timing for a certain stock
#'
#' @param timing_key key for stock "symbol | company"
#' @param timing_window window for plot
#' @param tr transactions table
#'
#' @return a data.table with structure:
#'         - symbol: character
#'         - date: Date
#'         - price: numeric
#'         - amount_holding: numeric
#'         - position_euro: numeric
#' @export
get_one_stock_evolution = function(timing_key, 
                                   timing_window, 
                                   pos_sb_evol) {
  
  timing_symbol = key_to_symbol(timing_key)
  w = window_to_start_end_dates(timing_window)
  
  pos_sb_evol_subset = pos_sb_evol[symbol == timing_symbol & date >= w$start & date <= w$end,
                                   .(amount_holding = sum(amount_holding), position_euro = sum(position_euro)),
                                   by = .(symbol, date, price)]
  
  return(pos_sb_evol_subset)
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
  x = pos_sb_evol[order(-date)][
    , .SD[1], by = .(account, symbol)][
      amount_holding > 0][
        order(account, symbol)]
  setcolorder(
    x,
    c(
      "date",
      "account",
      "symbol",
      "amount_holding",
      "price",
      "position_euro",
      "mean_acquire_price",
      "mean_acquire_value",
      "performance_rel",
      "performance_abs"
    )
  )
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
  result = pos_sb[, .(stocks = sum(position_euro)), by = .(account)]
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


#' round all numerical columns in pos_sb to n digits
#'
#' @param pos_sb 
#' @param digits 
#'
#' @return a data.table with structure:
#'         - date: Date
#'         - account: character
#'         - symbol: character
#'         - amount_holding: numeric
#'         - price: numeric
#'         - position_euro: numeric
#'         - mean_acquire_price: numeric
#'         - mean_acquire_value: numeric
#'         - performance_rel: numeric
#'         - performance_abs: numeric
#' @export
pos_sb_round_numericals = function(pos_sb, digits = 2) {
  dt = copy(pos_sb)
  cols =  c("price", "position_euro", 
            "mean_acquire_price", "mean_acquire_value", "performance_rel", 
            "performance_abs") 
  dt[, (cols) := lapply(.SD, round, digits), .SDcols = cols]
  return(dt)
}






