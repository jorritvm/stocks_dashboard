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

options(shiny.maxRequestSize = 30 * 1024 ^ 2) # 30 MB
reactlog_enable()

options = list(host = "0.0.0.0", port = 9999)
ui <- secure_app(ui, enable_admin = FALSE) # activate when running live
shinyApp(ui = ui, server = server, options = options)

