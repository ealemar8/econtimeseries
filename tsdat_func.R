
# function to generate year month xts variable (TRUE = year months, default is false)
# for Puerto Rico economic indicators using wide matrix published by Planing Board as input
# (want to reshape big year month matrix to long dataframe of two columns: 
# one for month-year and one for the observation)

xts_yearmon = function(dataa, var_name, yearmon=F){
  date = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov',
           'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun')
  dataa$Meses = date
  data = melt(dataa, id.vars = c('Meses'))
  names(data) = c('Meses', 'year', var_name)
  start_date = paste0(data[1, 'Meses'], ' ', data[1, 'year'])
  end_date = paste0(data[nrow(data), 'Meses'], ' ', data[nrow(data), 'year'])
  start_year = as.numeric(substr(start_date,
                                 nchar(start_date)-4, nchar(start_date)))
  st_month = as.character(substr(start_date, 1, 3))
  start_month = as.numeric(ifelse(match(st_month, month.abb)<10, 
                                  paste0('0',match(st_month, month.abb)), match(st_month, month.abb)))
  end_year = as.numeric(substr(end_date,
                               nchar(end_date)-4, nchar(end_date)))
  nd_month = as.character(substr(end_date, 1, 3))
  end_month = as.numeric(ifelse(match(nd_month, month.abb)<10, 
                                paste0('0',match(nd_month, month.abb)), match(nd_month, month.abb)))
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
  if(grepl(pattern = "Q", x = start_date)==F){
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
    }} else {
      freq = 4
      if(yearq==T){
        data = xts(data[(nrow(data)-elapsed_quarters):nrow(data), var_name], 
                   order.by = index(ts(data[(nrow(data)-elapsed_quarters):nrow(data), var_name], 
                                       start = c(start_year,start_month, 01), end = c(end_year, end_month, 01),
                                       frequency = freq)), frequency = freq)
        return(data)
      } else {
        datat = xts(data[(nrow(data)-elapsed_quarters):nrow(data), var_name], 
                    order.by = seq(as.Date(paste0(start_year,'-',start_month,'-','01')),
                                   length = elapsed_quarters+1, by = "quarters"), frequency = freq)
        return(datat)
      }
    } 
}
