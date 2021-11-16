##########################
# NEW
##########################

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

##########################
# from JRUTILS 0.5.0
##########################