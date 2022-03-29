testcurrencies = function(input, output, session) {
  fx = reactivePoll(1000,
                    session,
                    checkFunc = get_count_fx,
                    valueFunc = get_all_fx)
  
  unique_fx_symbols = reactive({ unique(fx()[['fx']]) })
  
  
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
  
}