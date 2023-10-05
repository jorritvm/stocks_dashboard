server = function(input, output, session) {

################################################################
# general
################################################################
  ### AUTHENTICATION
  fpfn_env = here("../scripts/credentials.env")
  if (file.exists(fpfn_env)) {
    dotenv::load_dot_env(fpfn_env)
  }
  cred_db_password <- Sys.getenv("cred_db_password")
  
  res_auth <- secure_server(
    check_credentials = check_credentials(
      normalizePath("../auth/credentials.sqlite"),
      passphrase = cred_db_password
    )
  )
  
  
  ### NOTIFICATIONS
  output$notif_menu <- renderMenu({
    dropdownMenu(type = "notifications", icon = icon("info"), .list = rv$msgs)
  })
  
  notify = function(txt, duration = NULL) {
    # fulltext = paste(tstamp(" ","-",":"), txt) 
    fulltext = paste0("[", strftime(Sys.time(), "%H:%M"), "] ", txt)
    
    # dropdown menu
    rv$msgs = c(list(notificationItem(
                        text = fulltext, # using tags$div() there is a possibility to add style here later for word wrap 
                        icon = icon("info"),
                        status = "info",
                        href = NULL)),
                rv$msgs)
    # corner popup
    id = showNotification(fulltext, duration = duration, closeButton = FALSE, type = "message")
    
    return(id)
  }
  
  ### REACTIVE values
  rv = reactiveValues(
    msgs = list(),
    focus_stock = ""
  )
  
  source("R/server_dashboard.R", local = TRUE)
  source("R/server_portfolio.R", local = TRUE)
  source("R/server_currency.R", local = TRUE)
  source("R/server_stocks.R", local = TRUE)
  
}
