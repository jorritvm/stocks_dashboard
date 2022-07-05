source("R/ui_generic.R")

source("R/ui_about.R")
source("R/ui_currencies.R")
source("R/ui_dashboard.R")
source("R/ui_portfolio.R")
source("R/ui_stocks.R")

header = dashboardHeader(title = "Stocks dashboard",
                         dropdownMenuOutput("notif_menu"))

sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
    menuItem("Portfolio", tabName = "portfolio", icon = icon("chart-pie")),
    menuItem("Currencies", tabName = "currencies", icon = icon("coins")),
    menuItem("Stocks", tabName = "stocks", icon = icon("chart-bar")),
    menuItem("About", tabName = "about", icon = icon("info")) #,
    # actionButton("debug", "debug1")
  )
)

body = dashboardBody(
  tabItems(
    tab_dashboard,
    tab_portfolio,
    tab_currencies,
    tab_stocks,
    tab_about
  )
)


ui = dashboardPage(header, 
                   sidebar, 
                   body, 
                   skin = "green")

