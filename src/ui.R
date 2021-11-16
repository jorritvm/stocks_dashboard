source("load_libraries.R")

header = dashboardHeader(title = "Stocks dashboard")

sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home"),
    menuItem("Stocks list", tabName = "stocks_list"),
    menuItem("Stocks data", tabName = "stocks_data"),
    menuItem("Portfolio", tabName = "portfolio")
  )
)

body = dashboardBody(
  
  tabItems(
    tabItem(tabName = "home",
            img(src="wsb.jpg")
            ),
    
    tabItem(tabName = "stocks_list",
            navbarPage(
              title = "",
              
              tabPanel("List",
                       DTOutput('stock_list')),
              
              tabPanel("Profile",
                       box(
                          selectInput("profile_stock_symbol", "View stock profile", ""),
                          title = "Input", status = "primary", solidHeader = TRUE, width = 12
                       ),
                       box(
                        tableOutput("profile_stock_table"),
                        title = "Profile", status = "info", solidHeader = TRUE, width = 12
                       )
                      ),
             
              tabPanel("Add",
                       box(textInput("add_stock_symbol", "Add a stock to this site", ""),
                           selectInput("add_stock_region", "Stock region", choices = c("US", "EU")),
                           actionButton("add_stock_symbol_btn", "Add", icon("plus")),
                           title = "Input", status = "primary", solidHeader = TRUE, width = 6
                       ), 
                       box(textOutput("add_stock_output"),
                           title = "Result", status ="info", solidHeader = TRUE, width = 6
                        )
                      ),
              
              tabPanel("Remove",
                       box(
                       selectInput("remove_stock_symbol", "Remove a stock from this site", ""),
                       actionButton("remove_stock_symbol_btn", "Remove", icon("minus")),
                       title = "Input", status = "primary", solidHeader = TRUE, width = 6
                       ), 
                       box(textOutput("remove_stock_output"),
                           title = "Result", status ="info", solidHeader = TRUE, width = 6
                       )),
              tabPanel("Update",
                       box(
                         actionButton("update_ohlc_btn", "Update OHLC data", icon("sync")),
                         title = "Action", status = "primary", solidHeader = TRUE, width = 6
                          ),
                       box(
                         textOutput("update_ohlc_text"),
                         title = "Action", status = "info", solidHeader = TRUE, width = 6
                          )
                       )
              )
    ),
    
    tabItem(tabName = "stocks_data",
            box(
              selectInput("plot_stock_symbol", "", ""),
              radioButtons("plot_stock_window",
                           "",
                           choices = list("All"= "all",
                                          "5Y" = "5Y",
                                          "3Y" = "3Y",
                                          "2Y" = "2Y",
                                          "1Y" = "1Y",
                                          "YTD" = "YTD",
                                          "6M" = "6M",
                                          "3M" = "3M",
                                          "1M" = "1M",
                                          "2W" = "2W",
                                          "1W" = "1W"),
                           inline = TRUE),
              title = "Input", status = "primary", solidHeader = TRUE, width = 12
            ),
            box(
              plotlyOutput("plot_stock_plot"),
              title = "Graph", status = "info", solidHeader = TRUE, width = 12
            )
          )
  )
)

ui = dashboardPage(header, 
                   sidebar, 
                   body, 
                   skin = "green")