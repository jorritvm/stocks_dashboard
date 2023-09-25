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
  notify("Upload finished.", 10)
})

observeEvent(tr(), { 
  updateSelectInput(session, 
                    inputId = "select_account", 
                    choices = c(unique(tr()$account))) 
})

observeEvent(input$batch_portfolio_btn, {
  req(input$batch_portfolio_file)
  # we inform the user he has to update OHLC first!
  shinyalert(
    "Reminder!",
    text = "Don't forget to update OHLC data before uploading a dataset \n Continue?",
    type = "warning",
    showCancelButton = TRUE,
    inputId = "confirm_before_upload"
  )
})

observeEvent(input$confirm_before_upload, {
  if (input$confirm_before_upload) {
    # inform user that upload has started
    id = notify("Upload started.")
    on.exit(removeNotification(id), add = TRUE)
    
    # get account from input form
    if (input$radio_account == "new") {
      account = input$new_account
    } else {
      account = input$select_account
    }
    
    # read and determine type of upload file
    dt = as.data.table(read.xlsx(input$batch_portfolio_file$datapath))
    file_source = check_for_transaction_file_type(dt)
    
    # parse upload file
    if (file_source == "saxo") {
      import_saxo_transaction_log(dt, account)
    }
    if (file_source == "bolero") {
      import_bolero_transaction_log(dt, ohlc(), account)
    }
    
    # inform user that the upload has finished 
    notify("Batch upload file processed")
  }
})

