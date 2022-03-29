#' returns amount of records in fx_rates table 
#' 
#' cheap function used to check if the datatable was changed and hook up to reactivePoll
#'
#' @return
#' @export
get_count_fx = function() {
  n = get_count_table("fx_rates")
  return(n)
}


#' returns FX data for all symbol(s)
#'
#' @return data.table
#' @export
get_all_fx = function() {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # use some dplyr for the select query
  result = tbl(con, "fx_rates") %>%
    as.data.table() %>%
    mutate(date = ymd(date))
  
  # select the symbols already in the data table
  dbDisconnect(con)  
  
  return(result)
}


#' returns a small table with for each fx the latest date & value
#'
#' @return
#' @export
get_latest_fx = function() {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  result = dbGetQuery(con, 
                      'SELECT fx, rate, max(date) as latest_fx_date
                      FROM fx_rates 
                      GROUP BY fx') %>% as_tibble()
  
  # select the symbols already in the data table
  dbDisconnect(con)  
  
  return(result)
  
}

#' safely write a fxdata to the DB, if part of the fx history already exists
#' it first deletes the old info, then adds the new data, in order to avoid duplicates
#'
#' @param fx data.table containing fx, date, rate columns
#'
#' @return
#' @export
safe_write_fx_data = function(dt_fx) {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # drop database queries that are overlapping
  fx = dt_fx[1, fx]
  from_date = unlist(dt_fx[, .(format_ISO8601(min(date)))])
  to_date   = unlist(dt_fx[, .(format_ISO8601(max(date)))])
  query = 'SELECT 
              * 
           FROM 
              fx_rates 
           WHERE 
              fx = ? 
           AND 
              date BETWEEN ? AND ?'
  t = dbGetQuery(con, query, params = list(fx, from_date, to_date))
  if (nrow(t) > 0) {
    query = 'DELETE
             FROM 
                fx_rates 
             WHERE 
                fx = ? 
             AND 
                date BETWEEN ? AND ?'
    dbExecute(con, query, params = list(fx, from_date, to_date))
  }
  
  # write new records
  dt_fx = dt_fx[, date := format_ISO8601(date)]
  dbAppendTable(con, "fx_rates", dt_fx)
  
  # select the symbols already in the data table
  dbDisconnect(con)  
}


#' remove an FX from our DB tables
#'
#' @param fx 
#'
#' @return
#' @export
remove_fx_from_db = function(fx) {
  # open the db connection
  db_fpfn = get_db_location()
  con <- dbConnect(RSQLite::SQLite(), db_fpfn)
  
  # delete rate
  query = 'DELETE
             FROM 
                fx_rates 
             WHERE 
                fx = ?'
  dbExecute(con, query, params = list(fx))
  
  # select the symbols already in the data table
  dbDisconnect(con)  
}


#' create a plot for an FX
#'
#' @param fx string identifier for FX
#' @param window string representation of time window
#'
#' @return
#' @export
plot_fx = function(fx_table, fx_symbol, window) {
  # translate window to start & end days
  w = window_to_start_end_dates(window)
  
  # get fx data for this symbol and this window
  data =  fx_table %>% 
    filter(fx == fx_symbol, date >= w$start, date <= w$end) %>%
    arrange(date)
  
  # plotly
  fig <-
    data %>% 
    plot_ly(
      x = ~ date,
      y = ~ rate,
      line = list(
        color = 'black',
        width = 0.75
      ),
      type = "scatter",
      mode = "lines" # 'lines+markers'
    ) 
  
  return(fig)
}
