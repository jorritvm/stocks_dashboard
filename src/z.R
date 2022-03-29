lapply(list.files(path = "R", recursive = FALSE, pattern = "\\.R$", full.names = TRUE), source)
source("R/load_libraries.R")



### DEFINE REACTIVES
tr = get_transactions()
profiles = get_stock_profiles()
ohlc =  get_all_ohlc()
fx = get_all_fx()
portfolio_positions = expand_transactions_to_portfolio_positions(tr)
