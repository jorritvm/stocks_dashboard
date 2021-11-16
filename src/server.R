source("load_libraries.R")

server = function(input, output, session) {
  
  ### DEFINE REACTIVE DATASETS
  stock = reactiveValues(profiles = get_stock_profiles())

  ### LIST ALL STOCKS
  output$stock_list = renderDT(get_stock_key_info(stock$profiles))
  
  ### PROFILE A STOCK
  observe({ updateSelectInput(session, 
                              "profile_stock_symbol", 
                              label = NULL, 
                              choices = stock$profiles$symbol)
  })
  output$profile_stock_table = renderTable({get_stock_profile_table(input$profile_stock_symbol)})
  
  
  ### ADD A STOCK TO THE DB
  observeEvent(input$add_stock_symbol_btn, { 
    # get profile
    profile = get_profile_info_from_api(input$add_stock_symbol, input$add_stock_region, api_read_key())
    safe_write_stock_profile(profile)
    output$add_stock_output = renderText({
                                paste("Symbol", profile$symbol, "added to the DB.") 
                              }) 
    
    # get OHLC
    start_date = today() - 10000
    end_date = today()- 0
    ohlc = get_ohlc_from_api(input$add_stock_symbol, start_date, end_date)
    safe_write_ohlc_data(ohlc)
    
    # update stocks profile
    stock$profiles = get_stock_profiles()
                  
  })
  
  ### REMOVE A STOCK FROM THE DB
  observe({ updateSelectInput(session, 
                              "remove_stock_symbol", 
                              label = NULL, 
                              choices = stock$profiles$symbol)
  })
  
  observeEvent(input$remove_stock_symbol_btn, { 
    # remove it from db
    remove_stock_from_db(input$remove_stock_symbol)
    
    output$remove_stock_output = renderText({
      paste("Symbol", input$remove_stock_symbol, "deleted from the DB.") 
    }) 
  })
  
  ### update OHLC
  observeEvent(input$update_ohlc_btn, { 
    # remove it from db
    update_all_ohlc()
    output$update_ohlc_text = renderText({"OHLC data updated for all stocks in the DB."})
  })
  
  ### SHOW STOCK GRAPH
  observe({ updateSelectInput(session, 
                              "plot_stock_symbol", 
                              label = NULL, 
                              choices = stock$profiles$symbol)
  })
  output$plot_stock_plot = renderPlotly({plot_stock_evolution(input$plot_stock_symbol, 
                                                              input$plot_stock_window)
                                        })
  

}

