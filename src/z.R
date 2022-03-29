lapply(list.files(path = "R", recursive = FALSE, pattern = "\\.R$", full.names = TRUE), source)
source("R/load_libraries.R")



### DEFINE REACTIVES
tr = get_transactions()
profiles = get_stock_profiles()
ohlc =  get_all_ohlc()
fx = get_all_fx()

ohlc_euro =  convert_ohlc_to_euro(ohlc, profiles, fx)


pos_sb_evol = get_stock_position_over_time_per_stock_and_broker(tr, ohlc_euro)
pos_sb = get_current_position_per_stock_and_broker(pos_sb_evol)

pos_sb_evol_subset = get_one_stock_evolution("ABI.BR | Anheuser-Busch InBev SA/NV",
                                            "2Y",
                                            pos_sb_evol)


