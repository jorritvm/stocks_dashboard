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

# load host and port from ENV variables 
fpfn_env = here("config.env")
if (file.exists(fpfn_env)) {
  dotenv::load_dot_env(fpfn_env)
}
h = Sys.getenv("r_stock_dashboard_host")
p = as.numeric(Sys.getenv("r_stock_dashboard_port"))
options = list(host = h, port = p)

# secure the server
# ui <- secure_app(ui, enable_admin = TRUE) # activate when running live

# run the server
options = c(options, list(shiny.autoreload = TRUE))
shinyApp(ui = ui, server = server, options = options)

