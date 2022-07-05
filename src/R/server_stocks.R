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
  notify("Stock OHLC data updated for all currencies in the DB.", 10)
  
})
