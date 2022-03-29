lapply(list.files(path = "R", recursive = FALSE, pattern = "\\.R$", full.names = TRUE), source)
source("R/load_libraries.R")

rv = list(  
  fx = get_latest_fx(),
            added_fx = "",
            removed_fx = "",
            updated_fx = FALSE,
            focus_stock = "",
            profiles = get_stock_profiles(),
            added_symbol = "",
            removed_symbol = "",
            updated_ohlc = FALSE,
            tr = get_transactions(),
            updated_transactions = FALSE
)


### DEFINE REACTIVES
tr = get_transactions()
profiles = get_stock_profiles()
ohlc =  get_all_ohlc()
fx = get_all_fx()
portfolio_positions = expand_transactions_to_portfolio_positions(tr)
