# commonly reused time window radio buttons
radio_window_plot = function(id) {
  radioButtons(
    id,
    "Window",
    choices = list(
      "All" = "all",
      "5Y" = "5Y",
      "3Y" = "3Y",
      "2Y" = "2Y",
      "1Y" = "1Y",
      "YTD" = "YTD",
      "6M" = "6M",
      "3M" = "3M",
      "1M" = "1M",
      "2W" = "2W",
      "1W" = "1W"
    ),
    selected = "YTD",
    inline = TRUE
  )  
}

# extracting the UI part that explains how to upload an excel file
box_explain_excel_upload = function() {
  box(
    tags$p("Upload an excel file with at these named columns:"),
    tags$ol(
      tags$li(HTML("<b>symbol:</b> yahoo ticker")),
      tags$li(
        HTML("<b>date:</b> date of transaction using ISO8601 date format")
      ),
      tags$li(HTML(
        "<b>type:</b> should be 'buy', 'sell', 'div', 'cash_in', 'cash_out', 'transfer_in', 'transfer_out'"
      )),
      tags$li(HTML("<b>amount:</b> amount of shares")),
      tags$li(HTML(
        "<b>money:</b> total revenue/cost of the transaction"
      )),
      tags$li(
        HTML(
          "<b>currency:</b> 3 letter abbreviation for the currency of the money column"
        )
      )
    ),
    tags$p(
      "For amount & money, the sign will not matter, type is used to determine sign."
    ),
    tags$p(
      "If money is not given for a buy or sell order, closing price will be used."
    ),
    tags$p("If currency is not given, original stock currency will be used."),
    title = "Readme",
    status = "info",
    solidHeader = TRUE,
    width = 12
  )  
}

# some predefined boxes
box_full_dark = function(...) {
  box(
    ...,
    status = "primary",
    solidHeader = TRUE,
    width = 12
  )
}

box_full_light = function(...) {
  box(
    ...,
    status = "info",
    solidHeader = TRUE,
    width = 12
  )
}

box_half_dark = function(...) {
  box(
    ...,
    status = "primary",
    solidHeader = TRUE,
    width = 6
  )
}

box_half_light = function(...) {
  box(
    ...,
    status = "info",
    solidHeader = TRUE,
    width = 6
  )
}