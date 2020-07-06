
#function to return year month xts variable (TRUE = year months, default is false)
xts_yearmon = function(data, var_name, start_year, 
                       start_month, end_year, end_month, time, yearmon=F, yearq=F){
  elapsed_months <- function(start_year, start_month, end_year, end_month) {
    start_date = paste0(start_year,'-',start_month,'-','01')
    end_date = paste0(end_year,'-',end_month,'-','01')
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
  }
  elapsed_quarters <- function(start_year, start_month, end_year, end_month){
    start_date = paste0(start_year,'-',start_month,'-','01')
    end_date = paste0(end_year,'-',end_month,'-','01')
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    4 * (ed$year - sd$year) + (ed$mon - sd$mon)
  }
  if(time == 'month'){
    freq = 12
    elapsedm = abs(elapsed_months(start_year, start_month, end_year, end_month))
    if(yearmon == T){
      data = xts(data[(nrow(data)-elapsedm):nrow(data), var_name], 
                 order.by = as.yearmon(index(ts(data[(nrow(data)-elapsedm):nrow(data), var_name], 
                                                start = c(start_year,start_month), end = c(end_year, end_month),
                                                frequency = freq))), frequency = freq)
      return(data)
    } else { 
      datat = xts(data[(nrow(data)-elapsedm):nrow(data), var_name], 
                  order.by = seq(as.Date(paste0(start_year,'-',start_month,'-','01')),
                                 length = elapsedm+1,
                                 by = "months"), frequency = freq)
      return(datat)
    }} else if(time == 'quarter'){
      freq = 4
      if(yearq==T){
        data = xts(data[(nrow(data)-elapsed_quarters):nrow(data), var_name], 
                   order.by = as.yearqtr(index(ts(data[(nrow(data)-elapsed_quarters):nrow(data), var_name], 
                                                  start = c(start_year,start_month), end = c(end_year, end_month),
                                                  frequency = freq))), frequency = freq)
        return(data)
      } else {
        datat = xts(data[(nrow(data)-elapsed_quarters):nrow(data), var_name], 
                    order.by = seq(as.Date(paste0(start_year,'-',start_month,'-','01')),
                                   length = elapsed_quarters+1, by = "quarters"), frequency = freq)
        return(datat)
      }
    } 
}