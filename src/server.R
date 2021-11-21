source("load_libraries.R")
options(shiny.maxRequestSize = 30*1024^2) # 30 MB 
reactlog_enable()

server = function(input, output, session) {
  
  # debug - remove this later
  eventReactive(input$debug_btn, { debug01() })
  
  ### DEFINE REACTIVE DATASETS
  rv = reactiveValues(profiles = get_stock_profiles(),
                         added_symbol = "",
                         removed_symbol = "",
                         updated_ohlc = FALSE)
  
  ################################
  ### PAGE: LIST ALL STOCKS
  # update table
  output$stock_list = renderDT(get_stock_key_info(rv$profiles))
  
  ################################
  ### PAGE: PROFILE A STOCK
  # update input list 
  observe({ updateSelectInput(session, 
                              "profile_stock_symbol", 
                              label = NULL, 
                              choices = rv$profiles$symbol)
  })
  
  # update table
  output$profile_stock_table = renderTable({get_stock_profile_table(input$profile_stock_symbol)})
  
  ################################
  ### PAGE: ADD A STOCK TO THE DB
  # add to DB
    observeEvent(input$add_stock_symbol_btn, { 
    add_symbol = input$add_stock_symbol
    
    # get profile
    profile = get_profile_info_from_api(add_symbol, input$add_stock_region, api_read_key())
    safe_write_stock_profile(profile)
   
    # get OHLC
    start_date = today() - 10000
    end_date = today()- 0
    ohlc = get_ohlc_from_api(add_symbol, start_date, end_date)
    safe_write_ohlc_data(ohlc)
    
    # update stocks profile
    rv$profiles = get_stock_profiles()
    
    # update status reactive value
    rv$added_symbol = add_symbol
  })
  
  # update status text
  output$add_stock_output = renderText({
    req(rv$added_symbol)
    paste("Symbol", rv$added_symbol, "added to the DB.") 
  }) 
  
  ################################
  ### PAGE: REMOVE A STOCK FROM THE DB
  # update input list 
  observe({ updateSelectInput(session, 
                              "remove_stock_symbol", 
                              label = NULL, 
                              choices = sort(rv$profiles$symbol))
  })
  
  # remove from db
  observeEvent(input$remove_stock_symbol_btn, { 
    symbol_to_remove = input$remove_stock_symbol
    
    # remove it from db
    remove_stock_from_db(symbol_to_remove)
    
    # update stocks profile
    rv$profiles = get_stock_profiles()
    
    # update status reactive value
    rv$removed_symbol = symbol_to_remove
  })
  
  # update status text
  output$remove_stock_output = renderText({
    req(rv$removed_symbol)
    paste("Symbol", rv$removed_symbol, "deleted from the DB.") 
  }) 

  ################################
  ### PAGE: update OHLC
  # update db
  observeEvent(input$update_ohlc_btn, { 
    update_all_ohlc()
    rv$updated_ohlc = rv$updated_ohlc + 1
  })
  # update status text
  output$update_ohlc_text = renderText({
    req(rv$updated_ohlc)
    "OHLC data updated for all stocks in the DB."
  })

  ################################  
  ### PAGE: CANDLESTICK
  # update input list 
  observe({ updateSelectInput(session, 
                              "cs_symbol", 
                              label = NULL, 
                              choices = rv$profiles$symbol)
  })
  # update plot
  output$cs_plot = renderPlotly({plot_candlestick(input$cs_symbol,
                                                              input$cs_window)
                                        })

  ################################  
  ### PAGE: BENCHMARK
  # update input list 
  observe({ updateSelectInput(session, 
                              "bench_symbol", 
                              label = NULL, 
                              choices = rv$profiles$symbol)
  })
  observe({   def = NULL
              if ("IWDA.AS" %in% rv$profiles$symbol) def = "IWDA.AS"
              updateSelectInput(session, 
                              "bench_base", 
                              label = NULL, 
                              choices = rv$profiles$symbol,
                              selected = def)
  })
  # update plot
  output$bench_plot = renderPlotly({plot_benchmark(input$bench_symbol,
                                                   input$bench_base,
                                                   input$bench_window)
  })
}

