#######################################
#
# This setup script will fill the workspace 
# with the global reactives used in the 
# shiny app to allow some interactive
# debugging
#
#######################################

source("R/libraries.R")

source("R/ui.R") # also sources the ui subcomponets
source("R/server.R")

source("R/lib_bolero.R")
source("R/lib_currency.R")
source("R/lib_portfolio_plots.R")
source("R/lib_portfolio_positions.R")
source("R/lib_portfolio_transactions.R")
source("R/lib_saxo.R")
source("R/lib_stock_ohlc.R")
source("R/lib_stock_plots.R")
source("R/lib_stock_profiles.R")
source("R/lib_utils.R")


### DEFINE REACTIVES
tr = get_transactions()
profiles = get_stock_profiles()
ohlc =  get_all_ohlc()
fx = get_all_fx()

ohlc_euro =  convert_ohlc_to_euro(ohlc, profiles, fx)

tr_ext = extend_transactions_with_cumulative_data(tr, ohlc)

cash_b_evol = get_cash_position_over_time_per_broker(tr_ext)
  
pos_sb_evol = get_stock_position_over_time_per_stock_and_broker(tr_ext, ohlc_euro)
pos_sb = get_current_position_per_stock_and_broker(pos_sb_evol)

pos_sb_evol_subset = get_one_stock_evolution("ABI.BR | Anheuser-Busch InBev SA/NV",
                                            "2Y",
                                            pos_sb_evol)


### having initialised the workspace, you can now pretty much run every function interactively
# ...


# ### analysis
# current cash position everywhere:
cash_b_evol[date == today()]

# current stock position per stock per broker
tr_ext[symbol != "" & !is.na(symbol)][order(date)][, .SD[nrow(.SD), .(amount_holding )], by = .(symbol, account)]
