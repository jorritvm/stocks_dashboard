source("R/ui_about.R")
source("R/ui_currencies.R")
source("R/ui_dashboard.R")
source("R/ui_generic.R")
source("R/ui_portfolio.R")
source("R/ui_stocks.R")

header = dashboardHeader(title = "Stocks dashboard",
                         dropdownMenuOutput("notif_menu"))

sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Portfolio", tabName = "portfolio", icon = icon("pie-chart")),
    menuItem("Currencies", tabName = "currencies", icon = icon("coins")),
    menuItem("Stocks", tabName = "stocks", icon = icon("bar-chart-o")),
    menuItem("About", tabName = "about", icon = icon("info"))
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
