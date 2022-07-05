source("R/libraries.R")

source("R/ui.R") # also sources the ui subcomponets
source("R/server.R")

source("R/bolero.R")
source("R/currency.R")
source("R/portfolio_plots.R")
source("R/portfolio_positions.R")
source("R/portfolio_transactions.R")
source("R/saxo.R")
source("R/stock_ohlc.R")
source("R/stock_plots.R")
source("R/stock_profiles.R")
source("R/utils.R")

options(shiny.maxRequestSize = 30 * 1024 ^ 2) # 30 MB
reactlog_enable()

options = list(host = "0.0.0.0", port = 9999)
shinyApp(ui = ui, server = server, options = options)
