library(openxlsx)
library(lubridate)

forex_list = list(c("eur","usd"),
                  c("usd","eur"),
                  c("eur","jpy"),
                  c("jpy","eur"))

for (forex in forex_list) {

  num = forex[1]
  denom = forex[2]
  fx = toupper(paste0(num,"/",denom))
  
  print(paste("writing forex to db for:", fx))
  
  # read and clean
  fpfn = paste0("D:/dev/R/stocks_dashboard/private/",num,denom,".xlsx")

  
  dt = as.data.table(read.xlsx(fpfn))
  dt[, Date := numeric_date_to_iso8601(Date)]
  dt[, Date := ymd(Date)]
  dt[, fx := fx]
  setnames(dt, tolower(names(dt)))
  setnames(dt, tolower(denom), "rate")
  dt = dt[, .(fx, date, rate)]
  # str(dt)
  # dt
  
  # import to db
  safe_write_fx_data (dt)
  
}
