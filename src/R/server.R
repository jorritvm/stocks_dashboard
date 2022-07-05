server = function(input, output, session) {

################################################################
# general
################################################################
  ### AUTHENTICATION
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  res_auth <- secure_server(
    check_credentials = check_credentials(
      normalizePath("../db/credentials.sqlite"),
      passphrase = key_get("R-shinymanager-key", NULL)
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

  
################################################################
# DASHBOARD TAB
################################################################
  output$vb_total_stocks = renderValueBox(valueBox(
    round(sum(pos_sb()$position_euro), 2),
    "Total stocks",
    icon = icon("euro-sign"),
    color = "green",
    width = 6
  ))
  output$vb_total_cash = renderValueBox(valueBox(
    round(cash_b_evol()[order(-date)][, .SD[1], by = account][, sum(cash_position)],2),
    "Total cash",
    icon = icon("euro-sign"),
    color = "green",
    width = 6
  ))
  output$overall_performance = renderValueBox(valueBox(
    "todo",
    "Overall performance",
    icon = icon("sort"),
    color = "aqua",
    width = 6
  ))
  output$ytd_performance = renderValueBox(valueBox(
    "todo",
    "YTD performance",
    icon = icon("sort"),
    color = "aqua",
    width = 6
  ))
  output$best_stock_overall = renderValueBox({
    q = pos_sb()[order(performance_rel)][nrow(pos_sb())]
    s = q$symbol
    p = round(q$performance_rel, 2)
    valueBox(
      p,
      paste0("Best stock overall: ", s),
      icon = icon("sort"),
      color = "aqua",
      width = 6
    )
  })
  output$worst_stock_overall = renderValueBox({
    q = pos_sb()[order(performance_rel)][1]
    s = q$symbol
    p = round(q$performance_rel, 2)
    valueBox(
      p,
      paste0("Worst stock overall: ", s),
      icon = icon("sort"),
      color = "orange",
      width = 6
    )
  })
  output$best_stock_ytd = renderValueBox(valueBox(
    "todo",
    "Best stock YTD",
    icon = icon("sort"),
    color = "aqua",
    width = 6
  ))
  output$worst_stock_ytd = renderValueBox(valueBox(
    "todo",
    "Worst stock YTD",
    icon = icon("sort"),
    color = "orange",
    width = 6
  ))
  output$latest_ohlc_update = renderValueBox(valueBox(
    max(ohlc()$date),
    "Latest OHLC update",
    icon = icon("fas fa-sync"),
    color = "navy",
    width = 6
  ))
  output$latest_fx_update = renderValueBox(valueBox(
    max(fx()$date),
    "Latest FX update",
    icon = icon("sync"),
    color = "navy",
    width = 6
  ))
  

  
################################################################
# PORTFOLIO TAB
################################################################
  tr = reactivePoll(1000,
                    session,
                    checkFunc = get_count_transactions,
                    valueFunc = get_transactions)
  
  tr_ext = reactive({
    extend_transactions_with_cumulative_data(tr())
  })
  
  cash_b_evol = reactive({
    get_cash_position_over_time_per_broker(tr_ext())
  })
  
  pos_sb_evol = reactive({
    get_stock_position_over_time_per_stock_and_broker(tr_ext(), 
                                                      ohlc_euro())
  })

  pos_sb = reactive({ 
    get_current_position_per_stock_and_broker(pos_sb_evol())
  })
  
  
  ################################
  ### PAGE: portfolio positions
  output$position_per_broker = renderPlotly({
    
    pos_b = get_current_position_per_broker(pos_sb())
    pos_c = get_current_cash_per_broker(cash_b_evol())
    plot_position_per_broker(pos_b, 
                             pos_c)
  })

  output$position_per_stock = renderPlotly({
    pos_s = get_current_position_per_stock(pos_sb(), profiles())
    plot_position_per_stock(pos_s)
  })
  
  
  output$positions_per_stock_broker_tbl =  renderDT(pos_sb(), 
                                                    options = list("pageLength" = 50))

  
  ##############################################################p
  ### PAGE: evolution
  observeEvent(pos_sb(), { 
    updateSelectInput(session, 
                      inputId = "pf_broker", 
                      choices = c("All", unique(pos_sb()$account))) 
  })

  output$portfolio_position = renderPlotly({
    plot_portfolio_evolution(input$pf_broker, 
                             input$pf_window, 
                             pos_sb_evol(),
                             cash_b_evol())
  })
  
  
  # ################################
  # ### PAGE: performance
  # output$total_performance = NULL
  
  
  ################################
  ### PAGE: market timing
  observeEvent(input$timing_key, { rv$focus_stock = input$timing_key })
  observeEvent(rv$focus_stock, { updateSelectInput(session = session, inputId = "timing_key", selected = rv$focus_stock) })

  observe({
    updateSelectInput(session,
                      "timing_key",
                      label = NULL,
                      choices = profiles()$key)
  })

  pos_sb_evol_subset = reactive({
    get_one_stock_evolution(input$timing_key,
                            input$timing_window,
                            pos_sb_evol())
  })

  output$market_timing_p  = renderPlotly(plot_market_timing_p(pos_sb_evol_subset()))
  output$market_timing_q  = renderPlotly(plot_market_timing_q(pos_sb_evol_subset()))
  output$market_timing_pq = renderPlotly(plot_market_timing_pq(pos_sb_evol_subset()))

  
  ################################
  ### PAGE: list all transactions
  output$transactions_table = renderDT(tr(), options = list("pageLength" = 50))


  ################################
  ### PAGE: batch upload transactions
  # handle file upload
  observeEvent(input$batch_portfolio_file, {
    req(input$batch_portfolio_file)

    # inform user that upload has started
    id = notify("Upload started.")
    on.exit(removeNotification(id), add = TRUE)

    # read and determine type of upload file
    dt = as.data.table(read.xlsx(input$batch_portfolio_file$datapath))
    file_source = check_for_transaction_file_type(dt)

    # parse upload file
    if (file_source == "saxo") {
      import_saxo_transaction_log(dt)
    }
    if (file_source == "bolero") {
      import_bolero_transaction_log(dt, ohlc())
    }
    
    # inform user that the upload has finished 
    notify("Upload finished.", 10)
  })


################################################################
# CURRENCIES TAB
################################################################
  fx = reactivePoll(1000,
                    session,
                    checkFunc = get_count_fx,
                    valueFunc = get_all_fx)
  
  unique_fx_symbols = reactive({ unique(fx()$fx) })
  
  
  ################################
  ### PAGE: CHART FX
  # update input list
  observeEvent(fx(), {
    updateSelectInput(session,
                      "fx_symbol",
                      label = NULL,
                      choices = unique_fx_symbols())
  })
  
  # update plot
  output$fx_plot = renderPlotly({
    plot_fx(fx(),
            input$fx_symbol,
            input$fx_window)
  })
  
  
  ################################
  ### PAGE: TABLE FX
  # update table
  output$fx_list = renderDT(fx(), options = list("pageLength" = 50))
  
  
  ################################
  ### PAGE: EDIT FX
  # add fx to DB
  observeEvent(input$add_fx_btn, {
    add_fx = input$add_fx_symbol
    
    # get FX (modifying the DB is a side-effect)
    start_date = today() - years(10) # attention - oanda only provides 180 days!!!
    dt_fx = get_fx_from_api(add_fx, start_date)
    safe_write_fx_data(dt_fx)
    
    # update status (setting an RV is a side-effect)
    txt = paste("FX", add_fx, "added to the DB.")
    notify(txt, 10)
  })
  
  # remove fx from DB
  observeEvent(unique_fx_symbols(), {
    updateSelectInput(session,
                      "remove_fx_symbol",
                      label = NULL,
                      choices = unique_fx_symbols())
  })
  
  observeEvent(input$remove_fx_btn, {
    remove_fx = input$remove_fx_symbol
    
    # remove it from db (side effect)
    remove_fx_from_db(remove_fx)
    
    # update status (setting an RV is a side-effect)
    txt = paste("FX", remove_fx, "deleted from the DB.")
    notify(txt, 10)
  })
  
  # update all FX
  observeEvent(input$update_fx_btn, {
    # inform user that we are starting batch update
    id = notify("Updating all FX")
    on.exit(removeNotification(id), add = TRUE)
    
    # batch update the fx data
    update_all_fx(fx())
    
    # inform user that we have finished the batch update
    notify("FX data updated for all currencies in the DB.", 10)
  })

   
################################################################
# STOCKS TAB
################################################################
  profiles = reactivePoll(1000,
                          session,
                          checkFunc = get_count_profiles,
                          valueFunc = get_stock_profiles)
  ohlc = reactivePoll(1000,
                      session,
                      checkFunc = get_count_ohlc,
                      valueFunc = get_all_ohlc)
  
  ohlc_euro = reactive({
    convert_ohlc_to_euro(ohlc(), profiles(), fx())
  })
  
  ################################
  ### PAGE: LIST ALL STOCKS
  # update table
  output$stock_list = renderDT(get_stock_key_info(profiles(), ohlc()), 
                               options = list("pageLength" = 50))

  
  ###############################
  ### PAGE: PROFILE A STOCK
  # update input list
  observeEvent(input$profile_stock_key, { rv$focus_stock = input$profile_stock_key })
  observeEvent(rv$focus_stock, { updateSelectInput(session = session, inputId = "profile_stock_key", selected = rv$focus_stock) })

  observeEvent(profiles(), {
    updateSelectInput(session,
                      "profile_stock_key",
                      label = NULL,
                      choices = profiles()$key)
  })

  # update table
  output$profile_stock_table = renderTable({
    get_stock_profile_table(input$profile_stock_key, isolate(profiles()))
  })
  

  ################################
  ### PAGE: CANDLESTICK
  # update input list
  observeEvent(input$cs_key, { rv$focus_stock = input$cs_key })
  observeEvent(rv$focus_stock, { updateSelectInput(session = session, inputId = "cs_key", selected = rv$focus_stock) })

  observe({
    updateSelectInput(session,
                      "cs_key",
                      label = NULL,
                      choices = profiles()$key)
  })
  # update plot
  output$cs_plot = renderPlotly({
    plot_candlestick(profiles(),
                     ohlc(),
                     ohlc_euro(),
                     input$cs_key,
                     input$cs_in_euro,
                     input$cs_window)
  })

  
  ################################
  ### PAGE: BENCHMARK
  # update input list
  observeEvent(input$bench_key, { rv$focus_stock = input$bench_key })
  observeEvent(rv$focus_stock, { updateSelectInput(session = session, inputId = "bench_key", selected = rv$focus_stock) })

  observeEvent(profiles(), {
    updateSelectInput(session,
                      "bench_key",
                      label = NULL,
                      choices = profiles()$key)
  })
  observeEvent(profiles(), {
    # set default benchmark symbol
    if ("IWDA.AS" %in% profiles()$symbol) {
      def = "IWDA.AS" 
    } else {
      def = profiles()[1, symbol]
    }
    
    updateSelectInput(
      session,
      "bench_base",
      label = NULL,
      choices = profiles()$key,
      selected = profiles()[symbol == def, "key"]
    )
  })
  # update plot
  output$bench_plot = renderPlotly({
    plot_benchmark(input$bench_key,
                   input$bench_base,
                   input$bench_window,
                   input$bench_in_euro,
                   ohlc(),
                   ohlc_euro())
  })

  
  ################################
  ### PAGE: EDIT STOCK
  # add to DB
  observeEvent(input$add_stock_symbol_btn, {
    add_symbol = input$add_stock_symbol
    
    # inform user that we are starting expensive update
    id = notify(paste("Started adding ", add_symbol, " to the DB."))
    on.exit(removeNotification(id), add = TRUE)
    
    add_stock(add_symbol, 
              input$add_stock_region)
    
    # inform user that we have finished the expensive update
    txt = paste("Symbol", input$add_stock_symbol, "added to the DB.")
    notify(txt, 10)
  })

  # remove stock from DB
  observeEvent(profiles(), {
    updateSelectInput(
      session,
      "remove_stock_symbol",
      label = NULL,
      choices = sort(profiles()$key)
    )
  })

  observeEvent(input$remove_stock_symbol_btn, {
    key_to_remove = input$remove_stock_symbol
    remove_stock_from_db(key_to_remove)

    # update status (setting an RV is a side-effect)
    txt =  paste("Symbol", key_to_symbol(key_to_remove), "deleted from the DB.")
    notify(txt, 10)
  })

  # update all stock OHLC
  observeEvent(input$update_ohlc_btn, {
    # inform user that we are starting batch update
    id = notify( "Updating all stock OHLC data")
    on.exit(removeNotification(id), add = TRUE)

    # batch update the fx data
    update_all_ohlc(ohlc())

    # inform user that we have finished the batch update
    notify("FX data updated for all currencies in the DB.", 10)

  })

  
}
