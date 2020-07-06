

# function to generate level and difference plot of an xts series
graph_diflev = function(data, periods_back, var_name, country){
  par(mfrow = c(2,1))
  data$line = mean(data[(nrow(data)-periods_back):nrow(data), ])
  p= plot(data[(nrow(data)-periods_back):nrow(data), ], 
          main = paste(var_name,'en', country, '(Nivel)'))
  pd = plot(diff(log(data[(nrow(data)-periods_back):nrow(data), ])), 
            main = paste(var_name,'en', country, '(Cambio %)'))
  return(c(p, pd))
}

# function for graphing from particular period to last pbservation 
# (year and month provided in numbers)
plot_start = function(data, year, month, cap){
  par(mfrow=c(1,1))
  pldata = na.omit(data[which(index(data)==paste0(year,'-',month,'-','01')):nrow(data), ])
  p = plot.xts(pldata, main = cap)
  p
}


# event plot function
event = function(data, var_name, level = 'levels', year,
                 month, event_year, event_month, event_label){
  CapStr <- function(y) {
    c <- strsplit(y, " ")[[1]]
    paste(toupper(substring(c, 1,1)), substring(c, 2),
          sep="", collapse=" ")
  }
  if(level == 'levels'){
    plot_start(data[, var_name], year, month, paste0(CapStr(names(data)),' (Nivel)'))
    addEventLines(events = xts(c(event_label),  order.by = as.Date(c(paste0(event_year,'-',event_month,'-','01')))),
                  lty = 2, col = 'red', lwd = 3, on = 0, pos=3)
  } else if(level == 'first_diff'){
    plot_start(diff(data[, var_name]), year, month, paste0(CapStr(names(data)),' (Primera Diferencia)'))
    addEventLines(events = xts(c(event_label), order.by = as.Date(c(paste0(event_year,'-',event_month,'-','01')))),
                  lty = 2, col = 'red', lwd = 3, on = 0, pos=3)
  }
}


# acf / pacf
acfs = function(data, dif='levels', lagmax){
  if(dif=='levels'){
    par(mfrow=c(2,1))
    a=acf(na.omit(data), lag.max = lagmax, plot = F)
    p=pacf(na.omit(data), lag.max = lagmax, plot = F)
    pa=plot(a, main = paste0('ACF of ', CapStr(names(data)), ' (Levels)'))
    pp=plot(p, main = paste0('PACF of ', CapStr(names(data)), ' (Levels)'))
    return(c(pa,pp))
  } else if(dif=='first_diff'){
    par(mfrow=c(2,1))
    a=acf(na.omit(diff(data)), lag.max = lagmax, plot = F)
    p=pacf(na.omit(diff(data)), lag.max = lagmax, plot = F)
    pa=plot(a, main = paste0('ACF of ', CapStr(names(data)), ' (First Difference)'))
    pp=plot(p, main = paste0('PACF of ', CapStr(names(data)), ' (First Difference)'))  
    return(c(pa,pp))
  }
}

# fitted vs actual plot
fitted_actual = function(data, mod, start_date, 
                         end_date, cap = 'Fitted vs Actual'){
  st_year = as.numeric(substr(start_date,
                              nchar(start_date)-4, nchar(start_date)))
  st_month = as.character(substr(start_date, 1, 3))
  st_month = as.numeric(ifelse(match(st_month, month.abb)<10, 
                               paste0('0',match(st_month, month.abb)), match(st_month, month.abb)))
  nd_year = as.numeric(substr(end_date,
                              nchar(end_date)-4, nchar(end_date)))
  nd_month = as.character(substr(end_date, 1, 3))
  nd_month = as.numeric(ifelse(match(nd_month, month.abb)<10, 
                               paste0('0',match(nd_month, month.abb)), match(nd_month, month.abb)))
  fitt=ts(fitted(mod)[which(as.yearmon(index(data))==start_date):nrow(data)],
          start = c(st_year, st_month), end = c(nd_year, nd_month), frequency = 12)
  p=plot(fitt, type = 'l', ylab='', xaxt='n', main = cap, lwd=2, pch=10)
  tsp = attributes(fitt)$tsp
  dates = seq(as.Date(paste0(st_year,'-','0',st_month,'-','01'),
                      format='%Y-%m-%d'), by = "month", along = fitt)
  p
  axis(1, at = seq(tsp[1], tsp[2], along = fitt),
       labels = format(dates, "%b %Y"))
  lines(ts(data[which(as.yearmon(index(data))==start_date):nrow(data), ],
           start = c(st_year, st_month),
           end = c(nd_year, nd_month), frequency = 12),
        col = 'red', lwd=2, pch=10)
  legend("bottomleft", legend = c("Fitted", "Actual"),
         col = c('black', 'red'), lty=c(1,1), seg.len=1, lwd=4, pch=10)
}
