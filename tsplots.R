



# function to generate level and difference plot of an xts series
graph_diflev = function(data, start_date, var_name, country){
  start_year = as.numeric(substr(start_date,
                                 nchar(start_date)-4, nchar(start_date)))
  st_month = as.character(substr(start_date, 1, 3))
  start_month = as.numeric(ifelse(match(st_month, month.abb)<10, 
                                  paste0('0',match(st_month, month.abb)), match(st_month, month.abb)))
  par(mfrow = c(2,1))
  if(class(index(data))=='yearmon'){
    start_index = which(as.yearmon(index(data))
                        == as.yearmon(paste0(start_year,'-', start_month)))
    trend = 1:nrow(data)
    data$line = fitted(lm(data ~ trend))
    p= plot(data[start_index:nrow(data), ], 
            main = paste(var_name,'en', country, '(Nivel)'))
    pd = plot(diff(log(data[start_index:nrow(data), ])), 
              main = paste(var_name,'en', country, '(Cambio %)'))
  } else {
    start_index = which(index(data)
                        == paste0(start_year,'-', start_month, '-', '01'))
    trend = 1:nrow(data)
    data$line = fitted(lm(data ~ trend))
    p= plot(data[start_index:nrow(data), ], 
            main = paste(var_name,'en', country, '(Nivel)'))
    pd = plot(diff(log(data[start_index:nrow(data), ])), 
              main = paste(var_name,'en', country, '(Cambio %)'))
  }
  return(c(p, pd))
}

graph_diflev(data = empleo_xts, start_date = 'Jul 2017', country = 'Puerto Rico')

# function for graphing from particular period to last pbservation 
# (year and month provided in numbers)
plot_start = function(data, date, cap, linecol='black'){
  year = as.numeric(substr(date,nchar(date)-4, nchar(date)))
  st_month = as.character(substr(date, 1, 3))
  month = as.numeric(ifelse(match(st_month, month.abb)<10, 
      paste0('0',match(st_month, month.abb)), match(st_month, month.abb)))
  par(mfrow=c(1,1))
  pldata = na.omit(data[which(index(data)==paste0(year,'-',month,'-','01')):nrow(data), ])
  p = plot.xts(pldata, main = cap, col = linecol)
  p
}

# event plot function
event = function(data, var_name = names(data), 
                 level = 'levels', date, event_date, event_label,
                 ltyy=2, coll='red', lwdd = 3, onn = 0, poss=3){
  plot_start = function(data, year, month, cap){
    pldata = na.omit(data[which(index(data)==paste0(year,'-',month,'-','01')):nrow(data)])
    p = plot.xts(pldata, main = cap)
    p
  }
  year = as.numeric(substr(date,nchar(date)-4, nchar(date)))
  st_month = as.character(substr(date, 1, 3))
  month = as.numeric(ifelse(match(st_month, month.abb)<10, 
                            paste0('0',match(st_month, month.abb)), match(st_month, month.abb)))
  event_year = as.numeric(substr(event_date, nchar(event_date)-4, nchar(event_date)))
  ev_month = as.character(substr(event_date, 1, 3))
  event_month = as.numeric(ifelse(match(ev_month, month.abb)<10, 
                                  paste0('0',match(ev_month, month.abb)), match(ev_month, month.abb)))
  par(mfrow=c(1,1))
  if(level == 'levels'){
    plot_start(data, year, month, paste0(var_name,' (Nivel)'))
    addEventLines(events = xts(c(event_label),  
          order.by = as.Date(c(paste0(event_year,'-',event_month,'-','01')))),
                  lty = ltyy, col = coll, lwd = lwdd, on = onn, pos=poss)
  } else if(level == 'first_diff'){
    plot_start(diff(data), year, month, paste0(var_name,' (Primera Diferencia)'))
    addEventLines(events = xts(c(event_label), 
          order.by = as.Date(c(paste0(event_year,'-',event_month,'-','01')))),
                  lty = ltyy, col = coll, lwd = lwdd, on = onn, pos=poss)
  }
}


# acf / pacf of xts object
acfs = function(data, dif='levels', lagmax){
  if(dif=='levels'){
    par(mfrow=c(2,1))
    a=acf(na.omit(data), lag.max = lagmax, plot = F)
    p=pacf(na.omit(data), lag.max = lagmax, plot = F)
    pa=plot(a, main = paste0('ACF of ', names(data), ' (Levels)'), ylab='')
    pp=plot(p, main = paste0('PACF of ', names(data), ' (Levels)'), ylab='')
    return(c(pa,pp))
  } else if(dif=='first_diff'){
    par(mfrow=c(2,1))
    a=acf(na.omit(diff(data)), lag.max = lagmax, plot = F)
    p=pacf(na.omit(diff(data)), lag.max = lagmax, plot = F)
    pa=plot(a, main = paste0('ACF of ', names(data), ' (First Difference)'), ylab='')
    pp=plot(p, main = paste0('PACF of ', names(data), ' (First Difference)'), ylab='')  
    return(c(pa,pp))
  }
}

#distribution and acf
distacf = function(data, var_name, dif='levels', lagmax){
  if(dif=='levels'){
    par(mfrow=c(2,1))
    a=acf(na.omit(data), lag.max = lagmax, plot = F)
    plot(a, main = paste0('ACF of ', var_name, ' (Levels)'))
    hist(as.numeric(na.omit(data)), probability = T, 
         main = paste0('Histogram of ', var_name))
    lines(density(as.numeric(na.omit(data))), col='red')
  } else if(dif=='first_diff'){
    par(mfrow=c(2,1)) 
    difdat=na.omit(diff(data))
    pa=acf(difdat, lag.max = lagmax, plot = F)
    plot(pa, main = paste0('ACF of ',
          var_name,' (First Difference)'), xlab='')
    hist(as.numeric(difdat), probability = T,
         main = paste0('Histogram of ',
        var_name,' (First Difference)'), xlab='')
    lines(density(as.numeric(difdat)), col='red')
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
  par(mfrow=c(1,1))
  p=plot(fitt, type = 'l', ylab='', xaxt='n', main = cap, lwd=2, pch=15)
  tsp = attributes(fitt)$tsp
  dates = seq(as.Date(paste0(st_year,'-','0',st_month,'-','01'),
                      format='%Y-%m-%d'), by = "month", along = fitt)
  p
  axis(1, at = seq(tsp[1], tsp[2], along = fitt),
       labels = format(dates, "%b %Y"))
  lines(ts(data[which(as.yearmon(index(data))==start_date):nrow(data), ],
           start = c(st_year, st_month),
           end = c(nd_year, nd_month), frequency = 12),
        col = 'red', lwd=2, pch=18)
  legend("bottomleft", legend = c("Fitted", "Actual"),
         col = c('black', 'red'), lty=c(1,1), seg.len=1, lwd=4, pch=15)
}
