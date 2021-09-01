library("shinydashboard")

header = dashboardHeader(title = "Stocks dashboard")

sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("Stocks list", tabName = "stocks_list"),
    menuItem("Stocks data", tabName = "stocks_data"),
    menuItem("Portfolio", tabName = "portfolio")
  )
)

body = dashboardBody(
  tabItems(
    tabItem(tabName = "stocks_list",
      tabBox(
        title = "",
        tabPanel("List", "Content for the first tab"),
        tabPanel("Add/remove", "Content for the second tab")
      )
    ),
    tabItem(tabName = "stocks_data",
            "stocks_data here"),
    tabItem(tabName = "portfolio",
            "Portfolio here")
  )
)

ui = dashboardPage(header, sidebar, body)