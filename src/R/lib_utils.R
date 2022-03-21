##########################
# NEW
##########################

#' LOCF where leading NA are filled out using FOCB
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
#' x = c(NA,NA, 1,2,3,NA,4,5,NA)
#' na.locf.cb(x)
na.locf.cb = function(x) {
  y = na.locf(x, na.rm = FALSE)
  z = na.locf(y, fromLast = TRUE)
  return(z)
}

#' LOCF where leading NA are replaced with 0
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
#' x = c(NA,NA, 1,2,3,NA,4,5,NA)
#' na.locf.0b(x)
na.locf.0b = function(x) {
  y = na.locf(x, na.rm = FALSE)
  y[is.na(y)] = 0
  return(y)
}


#' cuts the symbol from a key string "symbol | company"
#'
#' @param key string "symbol | company"
#'
#' @return string symbol
#' @export
key_to_symbol = function(key) {
  symbol = str_split_fixed(key, "\\ \\|\\ ", 2)[1]
  return(symbol)
}

#' returns the location of the sqlite database as a file path string
#'
#' @return
#' @export
get_db_location = function() {
  db_loc = file.path(dirname(here()), 
                     "db",
                     "data.db")
  return(db_loc)
}

#' return a string timestamp in ISO8601 using the provided separators
#'
#' @param date_hour_sep separator to be used between date and hour segment of the output
#' @param date_sep separator to be used between date components of the output
#' @param hour_sep separator to be used between hour components of the output
#'
#' @return string timestamp
#' @export
time_stamp = function(date_hour_sep = "-", date_sep = "", hour_sep = "", braces = FALSE) {
  date_pattern = paste(c("%Y","%m","%d"), collapse = date_sep)
  hour_pattern = paste(c("%H","%M","%S"), collapse = hour_sep)
  full_pattern = paste(c(date_pattern, hour_pattern), collapse = date_hour_sep)
  s = strftime(Sys.time(), full_pattern)
  
  if (braces) { 
    s = paste0("(", s, ")")
  }
  return(s)  
}

#' will create a backup of a file where the file is suffixed with a timestamp and '.backup'
#'
#' @param fpfn_from file path to file to be backed up
#'
#' @return
#' @export
file_backup = function(fpfn_from) {
  if (!file.exists(fpfn)) {
    warning(paste("File does not exist, no backup created for", fpfn))
  } else {
    fpfn_to = paste0(fpfn, ".", time_stamp("_","-","-"), ".backup")
    file.copy(from = fpfn_from, to = fpfn_to, copy.mode = TRUE, copy.date = TRUE) 
  }
}


#' converts a numeric date (e.g. when reading an excel file) to a iso8601 date string
#' works on windows only
#'
#' @param num numeric representation of date - for Excel on Windows, the origin date is December 30, 1899 for dates after 1900.
#'
#' @return
#' @export
numeric_date_to_iso8601 = function(num) {
  return(
    format(as.Date(num, origin = "1899-12-30"), '%Y-%m-%d')
  )  
}


##########################
# from JRUTILS 0.5.0
##########################

tstamp = function (date_hour_sep = "-", date_sep = "", hour_sep = "") 
{
  date_pattern = paste(c("%Y", "%m", "%d"), 
                       collapse = date_sep)
  hour_pattern = paste(c("%H", "%M", "%S"), 
                       collapse = hour_sep)
  full_pattern = paste(c(date_pattern, hour_pattern), collapse = date_hour_sep)
  return(strftime(Sys.time(), full_pattern))
}

#' create a list where for each element the name of the variable is the key and the content is the variable content
#'
#' @param ... comma seperated list of variables
#'
#' @return named list
#' @export
#'
#' @examples
#' a <- b <- c <- 1
#' named_list(a,b,c)
named_list <- function(...) {
  L <- list(...)
  snm <- sapply(substitute(list(...)),deparse)[-1]
  if (is.null(nm <- names(L))) nm <- snm
  if (any(nonames <- nm=="")) nm[nonames] <- snm[nonames]
  setNames(L,nm)
}