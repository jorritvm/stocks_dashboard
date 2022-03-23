lapply(list.files(path = "R", recursive = FALSE, pattern = "\\.R$", full.names = TRUE), source)
source("load_libraries.R")

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

