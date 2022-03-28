options(shiny.maxRequestSize = 30 * 1024 ^ 2) # 30 MB
reactlog_enable()

server = function(input, output, session) {
  # debug - remove this later
  eventReactive(input$debug_btn, {
    debug01()
  })
  
  tr = reactivePoll(1000, 
                    session,
                    checkFunc = get_count_transactions,
                    valueFunc = get_transactions
  )
  
  output$table_debug = renderDT(tr(), options = list("pageLength" = 50))
  
  
  
  ### DEFINE REACTIVE DATASETS
  rv = reactiveValues(
    fx = get_latest_fx(),
    added_fx = "",
    removed_fx = "",
    updated_fx = FALSE,
    focus_stock = "",
    profiles = get_stock_profiles(),
    added_symbol = "",
    removed_symbol = "",
    updated_ohlc = FALSE,
    tr = get_transactions(),
    updated_transactions = FALSE,
  )

  portfolio_positions = reactive({
    out = expand_transactions_to_portfolio_positions(rv$tr)
  })
  
  ################################################################
  # PORTFOLIO TAB
  ################################################################
  ################################
  ### PAGE: portfolio positions
  output$position_per_broker = renderPlotly({
    trp = get_current_position_per_stock_and_broker(rv$tr)
    trpb = get_current_position_per_broker(trp)
    plot_position_per_broker(trpb)
  })
  
  output$position_per_stock = renderPlotly({
    trp = get_current_position_per_stock_and_broker(rv$tr)
    trps = get_current_position_per_stock(trp, rv$profiles)
    plot_position_per_stock(trps)
  })
  
  ##############################################################p
  ### PAGE: plot
  observeEvent(rv$tr, { updateSelectInput(session = session, inputId = "pf_broker", choices = c("All", unique(rv$tr$account))) })
  
  output$portfolio_position = renderPlotly({
    plot_portfolio_evolution(input$pf_broker, input$pf_window, portfolio_positions() )
  })
  
  ################################
  ### PAGE: performance
  output$total_performance = NULL
  
  ################################
  ### PAGE: market timing
  observeEvent(input$timing_key, { rv$focus_stock = input$timing_key })
  observeEvent(rv$focus_stock, { updateSelectInput(session = session, inputId = "timing_key", selected = rv$focus_stock) })
  
  observe({
    updateSelectInput(session,
                      "timing_key",
                      label = NULL,
                      choices = rv$profiles$key)
  })
  
  trsub = reactive({
    calculate_market_timing(input$timing_key,
                            input$timing_window,
                            rv$tr)
  })
  
  output$market_timing_p = renderPlot(plot_market_timing_p(trsub(), rv$profiles))
  output$market_timing_q = renderPlot(plot_market_timing_q(trsub()))
  output$market_timing_v = renderPlot(plot_market_timing_v(trsub(), rv$profiles))
  
  ################################
  ### PAGE: list all transactions
  output$transactions_table = renderDT(rv$tr, options = list("pageLength" = 50))
  
  
  ################################
  ### PAGE: batch upload transactions
  # handle file upload
  observeEvent(input$batch_portfolio_file, {
    req(input$batch_portfolio_file)
    
    # read and detemrine upload file
    dt = as.data.table(read.xlsx(input$batch_portfolio_file$datapath))
    file_source = check_for_transaction_file_type(dt)
    
    # parse upload file
    if (file_source == "saxo") {
      import_saxo_transaction_log(dt)
    }
    if (file_source == "bolero") {
      import_bolero_transaction_log(dt)
    }
    
    rv$updated_transactions = rv$updated_transactions + 1
  })
  
  # update status text
  output$update_transaction_text = renderText({
    req(rv$updated_transactions)
    "New transactions added to the DB."
  })
  
  # update transactions reactive values dataset
  eventReactive(rv$updated_transactions, {
    rv$updated_transactions = get_transactions()
  })
  
  
  ################################################################
  # currencies TAB
  ################################################################
  ################################
  ### PAGE: LIST ALL FX
  # update table
  output$fx_list = renderDT(rv$fx, options = list("pageLength" = 50))
  
  # update input list
  observe({
    updateSelectInput(session,
                      "fx_symbol",
                      label = NULL,
                      choices = rv$fx$fx)
  })
  # update plot
  output$fx_plot = renderPlotly({
    plot_fx(input$fx_symbol,
            input$fx_window)
  })
  
  
  ################################
  ### PAGE: ADD AN FX TO THE DB
  # add to DB
  observeEvent(input$add_fx_btn, {
    add_fx = input$add_fx_symbol
    
    # get FX
    start_date = today() - years(10) # attention - oanda only provides 180 days!!!
    dt_fx = get_fx_from_api(add_fx, start_date)
    safe_write_fx_data(dt_fx)
    
    # update fx list
    rv$fx = get_latest_fx()
    
    # update status reactive value
    rv$added_fx = add_fx
  })
  
  # update status text
  output$add_fx_output = renderText({
    req(rv$added_fx)
    paste("FX", rv$added_fx, "added to the DB.")
  })
  
  ################################
  ### PAGE: REMOVE AN FX FROM THE DB
  # update input list
  observe({
    updateSelectInput(session,
                      "remove_fx_symbol",
                      label = NULL,
                      choices = sort(rv$fx$fx))
  })
  
  # remove from db
  observeEvent(input$remove_fx_btn, {
    fx_to_remove = input$remove_fx_symbol
    
    # remove it from db
    remove_fx_from_db(fx_to_remove)
    
    # update fx list
    rv$fx = get_latest_fx()
    
    # update status reactive value
    rv$removed_fx = fx_to_remove
  })
  
  # update status text
  output$remove_fx_output = renderText({
    req(rv$removed_fx)
    paste(tstamp(" ","-",":"), "FX", rv$removed_fx, "deleted from the DB.")
  })
  
  ################################
  ### PAGE: update FX
  # update status text
  # start_fx_text <- eventReactive(input$update_fx_btn, {
  #   "hallo" #paste(tstamp(" ","-",":"), "Started updating all FX")
  #   print("eventreactive!!!!!!")
  # })
  # 
  # update db
  observeEvent(input$update_fx_btn, {
    notif = paste(tstamp(" ","-",":"), "Updating all FX") # this does not work
    id <- showNotification(notif, duration = NULL, closeButton = FALSE, type = "message")
    on.exit(removeNotification(id), add = TRUE)
    
    update_all_fx()
    rv$updated_fx = paste(tstamp(" ","-",":"), "FX data updated for all currencies in the DB.")
  })
  

  # output$update_fx_text = renderText({
  #   start_fx_text()
  # })
  
  output$update_fx_text = renderText({
    req(rv$updated_fx)
    rv$updated_fx
  })
  
  ################################################################
  # STOCKS TAB
  ################################################################
  ################################
  ### PAGE: LIST ALL STOCKS
  # update table
  output$stock_list = renderDT(get_stock_key_info(rv$profiles), options = list("pageLength" = 50))
  
  ################################
  ### PAGE: PROFILE A STOCK
  # update input list
  observeEvent(input$profile_stock_key, { rv$focus_stock = input$profile_stock_key })
  observeEvent(rv$focus_stock, { updateSelectInput(session = session, inputId = "profile_stock_key", selected = rv$focus_stock) })
   
  observe({
    updateSelectInput(session,
                      "profile_stock_key",
                      label = NULL,
                      choices = rv$profiles$key)
  })
  
  # update table
  output$profile_stock_table = renderTable({
    # get_stock_profile_table(input$profile_stock_key)
    req(input$pf_window)
    mtcars
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
                      choices = rv$profiles$key)
  })
  # update plot
  output$cs_plot = renderPlotly({
    plot_candlestick(input$cs_key,
                     input$cs_window,
                     rv$profiles)
  })
  
  ################################
  ### PAGE: BENCHMARK
  # update input list
  observeEvent(input$bench_key, { rv$focus_stock = input$bench_key })
  observeEvent(rv$focus_stock, { updateSelectInput(session = session, inputId = "bench_key", selected = rv$focus_stock) })
  
  observe({
    updateSelectInput(session,
                      "bench_key",
                      label = NULL,
                      choices = rv$profiles$key)
  })
  observe({
    def = NULL
    if ("IWDA.AS" %in% rv$profiles$symbol)
      def = "IWDA.AS"
    updateSelectInput(
      session,
      "bench_base",
      label = NULL,
      choices = rv$profiles$key,
      selected = rv$profiles[symbol == def, "key"]
    )
  })
  # update plot
  output$bench_plot = renderPlotly({
    plot_benchmark(input$bench_key,
                   input$bench_base,
                   input$bench_window)
  })
  
  ################################
  ### PAGE: ADD A STOCK TO THE DB
  # add to DB
  observeEvent(input$add_stock_symbol_btn, {
    add_symbol = input$add_stock_symbol
    add_stock(add_symbol, input$add_stock_region)
    
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
  observe({
    updateSelectInput(
      session,
      "remove_stock_symbol",
      label = NULL,
      choices = sort(rv$profiles$key)
    )
  })
  
  # remove from db
  observeEvent(input$remove_stock_symbol_btn, {
    key_to_remove = input$remove_stock_symbol
    
    # remove it from db
    remove_stock_from_db(key_to_remove)
    
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
    notif = paste(tstamp(" ","-",":"), "Updating all OHLC data")
    id <- showNotification(notif, duration = NULL, closeButton = FALSE, type = "message")
    on.exit(removeNotification(id), add = TRUE)
    
    update_all_ohlc()
    rv$updated_ohlc =  paste(tstamp(" ","-",":"), "OHLC data updated for all stocks in the DB.")
  })
  # update status text
  output$update_ohlc_text = renderText({
    req(rv$updated_ohlc)
    rv$updated_ohlc
  })
  
 

  
}
