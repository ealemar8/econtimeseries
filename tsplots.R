

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