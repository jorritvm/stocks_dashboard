lapply(list.files(path = "R", recursive = FALSE, pattern = "\\.R$", full.names = TRUE), source)
source("R/load_libraries.R")



### DEFINE REACTIVES
tr = get_transactions()
profiles = get_stock_profiles()
ohlc =  get_all_ohlc()
fx = get_all_fx()

ohlc_euro =  convert_ohlc_to_euro(ohlc, profiles, fx)


tr_ext = extend_transactions_with_cumulative_data(tr)

cash_b_evol = get_cash_position_over_time_per_broker(tr_ext)
  
pos_sb_evol = get_stock_position_over_time_per_stock_and_broker(tr_ext, ohlc_euro)
pos_sb = get_current_position_per_stock_and_broker(pos_sb_evol)

pos_sb_evol_subset = get_one_stock_evolution("ABI.BR | Anheuser-Busch InBev SA/NV",
                                            "2Y",
                                            pos_sb_evol)


