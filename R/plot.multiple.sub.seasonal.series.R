library(Tcomp)
#forecasting with multiple approach
library(forecast)
library(Mcomp)
library(M4comp2018)

#source('utils.R')

tile.name<-function(begin.position,num.of.seasons,ts.type){
  seasons=seq(begin.position,begin.position+num.of.seasons-1,1)
  #change to month format
  seasons<-format.combined.date(seasons,ts.type)
  #
  name=c()
  for (i in 1:length(seasons)) {
    name=c(name,paste(ts.type,seasons[i],sep = ''))
  }
  #'+'
  names.temp=''
  for (j in 1:length(name)) {
    names.temp=paste(names.temp,paste(name[j],'+',sep = ''),sep = '')
  }
  return(substr(names.temp, 1, nchar(names.temp)-1))
}
#tools for calculate matrics 
smape_cal <- function(outsample, forecasts) {
  #Used to estimate sMAPE
  outsample <- as.numeric(outsample) ; forecasts<-as.numeric(forecasts)
  smape <- (abs(outsample-forecasts)*200)/(abs(outsample)+abs(forecasts))
  return(smape)
}

mase_cal <- function(insample, outsample, forecasts) {
  stopifnot(stats::is.ts(insample))
  #Used to estimate MASE
  frq <- stats::frequency(insample)
  forecastsNaiveSD <- rep(NA,frq)
  for (j in (frq+1):length(insample)){
    forecastsNaiveSD <- c(forecastsNaiveSD, insample[j-frq])
  }
  masep<-mean(abs(insample-forecastsNaiveSD),na.rm = TRUE)
  
  outsample <- as.numeric(outsample) ; forecasts <- as.numeric(forecasts)
  mase <- (abs(outsample-forecasts))/masep
  return(mase)
}
#MSIS
#calculate msis of every time t
msis_cal<-function(insample, outsample,forecasts.lower,forecasts.upper,a=0.05){
  #
  frq <- stats::frequency(insample)
  m<-c()
  for (j in (frq+1):length(insample)){
    m <- c(m, abs(insample[j]-insample[j-frq]))
  }
  masep<-mean(m)
  #
  b<-c()
  for (i in 1:length(outsample)){
    U.subtract.L<-forecasts.upper[i]-forecasts.lower[i]
    if(outsample[i]<forecasts.lower[i]){
      r<-(2/a)*(forecasts.lower[i]-outsample[i])
    }else{
      r<-0
    }
    if(outsample[i]>forecasts.upper[i]){
      q<-(2/a)*(outsample[i]-forecasts.upper[i])
    }else{
      q<-0
    }
    b<-c(b, U.subtract.L+r+q)
  }
  return(b/masep)
}
#' calculate accuracy of specific horizen,such as Monthly data, accuracy of horizen 7-12
#
#' @param accuracy.of.every.horizen.m shape(num.of.ts*all.horizen)
#' @param begin.position  such as 7
#' @param end.position such as 12
#' @param level.value  interval prediction, such as 50, 85, 95%
#' @return forecasts which contain point and interval forecasts
#' @export
#'
#' @examples
#' # Not Run 
accuracy.of.specific.horizen.cal<-function(accuracy.of.every.horizen.m,begin.horizen,end.horizen){
  #get 
  return(mean(accuracy.of.every.horizen.m[,begin.horizen:end.horizen]))
}
#' get point,interval forecasts of one method
#
#' @param method.option method name, such as 'ets' or'arima'
#' @param x  historical data of ts, such as M3[[100]]$x
#' @param h  forecasting horizen
#' @param level.value  interval prediction, such as 50, 85, 95%
#' @return forecasts which contain point and interval forecasts
#' @export
#'
#' @examples
#' # Not Run 
forecast.method<-function(method.option,x,h,level.value){
  if(method.option=='ets'){
    model <- forecast::ets(x)
    forecasts<-forecast::forecast(model, level=c(level.value),h=h)
    point.forecasts<-forecasts$mean
    upper.forecasts<-forecasts$upper
    lower.forecasts<-forecasts$lower
  }else if(method.option=='arima'){
    #model <- forecast::auto.arima(x,stepwise=FALSE, approximation=FALSE)
    #model <- forecast::auto.arima(x,stepwise=TRUE, approximation=FALSE)
    model <- forecast::auto.arima(x)
    forecasts<-forecast::forecast(model, level=c(level.value),h=h)
    point.forecasts<-forecasts$mean
    upper.forecasts<-forecasts$upper
    lower.forecasts<-forecasts$lower
  }
  return(list(point.forecasts=point.forecasts,upper.forecasts=upper.forecasts,lower.forecasts=lower.forecasts))
}


#' get formatted date of the train data
#' eg. #type.of.ts=='Quarterly', change month to quarterly format such as 2 3 4 1 2 3 4 1
#
#' @param ts train data of time series eg. M4[[1]]
#' @param type.of.ts  type of ts eg. 'M' or'Q'
#'
#' @return formatted.date.of.test.data eg. #type.of.ts=='Quarterly', change month to quarterly format such as 2 3 4 1 2 3 4 1
#' @export
#'
#' @examples
#' # Not Run
get.the.formatted.date.of.the.train.data<-function(ts,type.of.ts){
  train.data<-ts$x
  library(zoo)
  all.date.of.the.train.data<-as.Date.ts(train.data)
  #extract month date from the original date like"2011-04-01"
  formatted.date.of.train.data<-c()
  for (i in 1:length(all.date.of.the.train.data)) {
    formatted.date.of.train.data<-c(formatted.date.of.train.data,as.numeric(strsplit(toString(all.date.of.the.train.data[i]), "-")[[1]][2]))
  }
  if(type.of.ts=='Q'){
    #
    #type.of.ts=='Quarterly'
    #change month to quarterly format such as 2 3 4 1 2 3 4 1
    formatted.date.of.train.data<-(formatted.date.of.train.data-1)%/%3+1
  }
  return(formatted.date.of.train.data)
}

train.data=M4
all.date.of.the.train.data<-as.Date.ts(train.data)

# #test
# ts=M4[[4642]]
# type.of.ts='M'
# 
# formatted.date.of.train.data<-get.the.formatted.date.of.the.train.data(ts,type.of.ts)
# 
# formatted.date.of.train.data



#' get formatted date of the test data
#' eg. #type.of.ts=='Quarterly', change month to quarterly format such as 2 3 4 1 2 3 4 1
#
#' @param ts test data of time series eg. M4[[1]]
#' @param type.of.ts  type of ts eg. 'M' or'Q'
#'
#' @return formatted.date.of.test.data eg. #type.of.ts=='Quarterly', change month to quarterly format such as 2 3 4 1 2 3 4 1
#' @export
#'
#' @examples
#' # Not Run
get.the.formatted.date.of.the.test.data<-function(ts,type.of.ts){
  test.data<-ts$xx
  all.date.of.the.test.data<-as.Date.ts(test.data)
  #extract month date from the original date like"2011-04-01"
  formatted.date.of.test.data<-c()
  for (i in 1:length(all.date.of.the.test.data)) {
    formatted.date.of.test.data<-c(formatted.date.of.test.data,as.numeric(strsplit(toString(all.date.of.the.test.data[i]), "-")[[1]][2]))
  }
  if(type.of.ts=='Q'){
    #
    #type.of.ts=='Quarterly'
    #change month to quarterly format such as 2 3 4 1 2 3 4 1
    formatted.date.of.test.data<-(formatted.date.of.test.data-1)%/%3+1
  }
  return(formatted.date.of.test.data)
}

# #test
# ts=M4[[52642]]
# type.of.ts='Q'
# 
# formatted.date.of.test.data<-get.the.formatted.date.of.the.test.data(ts,type.of.ts)
# 
# formatted.date.of.test.data

#forecast


#' get formatted month of the original month 
#  eg.c(11,12,13,14,15)---->c(11,12,1,2,3)
#' @param original.month  eg.c(11,12,13,14,15)
#'
#' @return eg.c(11,12,13,14,15)---->c(11,12,1,2,3)
#'
#' @examples
#' # Not Run
format.month<-function(original.month){
  formatted.month<-c()
  for (i in 1:length(original.month)) {
    if(original.month[i]>12){
      formatted.month<-c(formatted.month,original.month[i]-12)
    }else{
      formatted.month<-c(formatted.month,original.month[i])
    }
  }
  return(formatted.month)
}
# 
# #test
# m<-format.month(c(11,12,13,16,20))
# m
#eg.c(3,4,5,7)---->c(3,4,1,3)

#' get formatted quartely of the original quarterly 
#  eg.c(3,4,5,7)---->c(3,4,1,3)
#' @param original.quarter eg. c(3,4,5,7)
#'
#' @return eg.c(3,4,5,7)---->c(3,4,1,3)
#'
#' @examples
#' # Not Run
format.quarter<-function(original.quarter){
  formatted.quarter<-c()
  for (i in 1:length(original.quarter)) {
    if(original.quarter[i]>4){
      formatted.quarter<-c(formatted.quarter,original.quarter[i]-4)
    }else{
      formatted.quarter<-c(formatted.quarter,original.quarter[i])
    }
  }
  return(formatted.quarter)
}
#test
q<-format.quarter(c(3,4,5,7))
q
#format date
format.combined.date<-function(original.date,type.of.ts){
  if(type.of.ts=='M')
    return(format.month(original.date))
  else if (type.of.ts=='Q') {
    return(format.quarter(original.date))
  }
}
#get the corresponding ts value according to the combinded months
#' @param ts train data of the ts, eg. M4[[1]]
#'  @param combination.months  combined month, eg,c(1,2,3)
#'  @param formatted.date.of.the.ts  formatted date for the train data of ts
#'
#' @return new.ts : new ts constructed from the orginal ts
#'
#' @examples
#' # Not Run
get.corresponding.ts.data.according.to.the.combination.months<-function(ts,combination.months,formatted.date.of.the.ts){
  train.data<-ts$x
  new.ts<-c()
  for (i in 1:length(formatted.date.of.the.ts) ) {
    if(formatted.date.of.the.ts[i] %in% combination.months ){
      # print(train.data[i])
      new.ts<-c(new.ts,train.data[i])
    }
    
  }
  new.ts<-ts(new.ts,frequency=length(combination.months))
  return(new.ts)
}

# #test
# ts=M4[[4642]]
# type.of.ts='M'
# formatted.date<-get.the.formatted.date.of.the.train.data(ts,type.of.ts)
# 
# formatted.date.of.the.ts<-formatted.date
# formatted.date.of.the.ts
# combination.months<-c(1,2,3,4,5)
# formatted.date.of.the.ts[10] %in% combination.months 
# new_ts<-get.corresponding.ts.data.according.to.the.combination.months(ts,combination.months,formatted.date.of.the.ts)
# 
# frequency(new_ts)

#decide to forecast how many points need to be forecast in the horizen in every new ts forecasting
#' @param combination.months combined month, eg,c(1,2,3)
#'  @param formatted.date.of.the.test.data  formatted date for the test data of ts
#'
#' @return  result=list(new.horizen=counter,forecasts.position=forecasts.position)
#'          counter: the number of horizen for every new ts
#'          forecasts.position: the position of ervery forecasts
#'
#' @examples
#' # Not Run
get.new.horizen<-function(combination.months,formatted.date.of.the.test.data){
  counter<-0
  forecasts.position<-c()
  for (i in 1:length(formatted.date.of.the.test.data)) {
    if(formatted.date.of.the.test.data[i]%in% combination.months){
      counter<-counter+1
      forecasts.position<-c(forecasts.position,i)
    }
  }
  # if(counter%%length(combination.months)!=0){
  #   new.horizen<-counter%/%length(combination.months)+1
  # }else{
  #   new.horizen<-counter
  # }
  # result<-list(new.horizen=new.horizen,number.of.points<-counter)
  result<-list(new.horizen=counter,forecasts.position=forecasts.position)
  return(result)
}

#forecast every new ts 
#' @param i number of months (the number of sub-seasonal)
#'  @param j  begin position
#' @param formatted.date.of.the.train.data formatted date of train data of ts
#' @param formatted.date.of.the.test.data formatted date of train data of ts
#' @return  forecasts of the new ts
#' @examples
#' # Not Run
get.different.sub.seasonal.seqence.for.one.ts<-function(i,j,ts,formatted.date.of.the.train.data,formatted.date.of.the.test.data,type.of.ts){
  # formatted.date.of.the.train.data<-get.the.formatted.date.of.the.train.data(ts,type.of.ts)
  # formatted.date.of.the.test.data<-get.the.formatted.date.of.the.test.data(ts,type.of.ts)
  number.of.months<-i
  begin.position<-j
  end.position<-begin.position+number.of.months-1
  combination.dates<-seq(begin.position,end.position)
  #change to month format
  formatted.combination.months<-format.combined.date(combination.dates,type.of.ts)
  #begin to forecast
  #such as c(2,3,4)
  new.ts<-get.corresponding.ts.data.according.to.the.combination.months(ts,formatted.combination.months,formatted.date.of.the.train.data)

  return(new.ts)
}


plot.different.sub.seasonal.seqence.for.one.ts<-function(ts,type.of.ts){
  #get formatted date of the train data of the ts
  formatted.date.of.the.train.data<-get.the.formatted.date.of.the.train.data(ts,type.of.ts)
  formatted.date.of.the.test.data<-get.the.formatted.date.of.the.test.data(ts,type.of.ts)
  #
  #begin to plot
  if(type.of.ts=='M'){
    # number.of.months=1:11
    # begin.position=1:12
    image.index=paste(ts$st,'.svg',sep = '')
    image.path=paste('/Users/xushengxiang/Desktop/lixixi/forecasting-with-multi-steps/code/examples-M3/monthly/',image.index,sep = '')
    #begin to plot 
    # png(file=image.path,
    #     width=2000, height=2000)
    svg(image.path,width=30,height=30)
    par(mfrow = c(12, 12))  # Set up a 2 x 2 plotting space
    for (num.of.sub.seasons in 1:11) {
      for (begin.position in 1:12) {
        timm.series<-get.different.sub.seasonal.seqence.for.one.ts(num.of.sub.seasons,begin.position,ts,
                                                              formatted.date.of.the.train.data,
                                                              formatted.date.of.the.test.data,
                                                              type.of.ts)
        print('test1')
        print(timm.series)
        print('test2')
        #plot.ts(new.ts,xy.labels = '11')
        #plot(new.ts,main = 'Q1')
        title.name=tile.name(begin.position,num.of.sub.seasons,type.of.ts)
        plot(timm.series,xlab = 'Period',ylab = '',type = 'o',main = title.name,cex.main=0.9)
      }
      
      
    }
    title.name='M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11+M12'
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    dev.off()

  }else{
    #quarterly data
    # number.of.months=1:3
    # begin.position=1:4
    image.index=paste(ts$st,'.svg',sep = '')
    image.path=paste('/Users/xushengxiang/Desktop/lixixi/forecasting-with-multi-steps/code/examples-M3/quarterly/',image.index,sep = '')
    #begin to plot 
    # png(file=image.path,
    #     width=1300, height=1300)
    svg(image.path,width=10,height=10)
    par(mfrow = c(4, 4))  # Set up a 2 x 2 plotting space
    for (num.of.sub.seasons in 1:3) {
      for (begin.position in 1:4) {
        timm.series<-get.different.sub.seasonal.seqence.for.one.ts(num.of.sub.seasons,begin.position,ts,
                                                              formatted.date.of.the.train.data,
                                                              formatted.date.of.the.test.data,
                                                              type.of.ts)
        print(timm.series)
        #plot.ts(new.ts)
        title.name=tile.name(begin.position,num.of.sub.seasons,type.of.ts)
        plot(timm.series,main = title.name,xlab = 'Period',ylab = '',type = 'o')
    }

    
    }
    plot(ts$x,main='Q1+Q2+Q3+Q4',type = 'o')
    plot(ts$x,main='Q1+Q2+Q3+Q4',type = 'o')
    plot(ts$x,main='Q1+Q2+Q3+Q4',type = 'o')
    plot(ts$x,main='Q1+Q2+Q3+Q4',type = 'o')
    dev.off()
  
  }
}


plot.different.sub.seasonal.series<-function(ts,type.of.ts){
  #get formatted date of the train data of the ts
  formatted.date.of.the.train.data<-get.the.formatted.date.of.the.train.data(ts,type.of.ts)
  formatted.date.of.the.test.data<-get.the.formatted.date.of.the.test.data(ts,type.of.ts)
  #
  #begin to plot
  if(type.of.ts=='M'){
    # number.of.months=1:11
    # begin.position=1:12
    # image.index=paste(ts$st,'.svg',sep = '')
    # image.path=paste('/Users/xushengxiang/Desktop/lixixi/forecasting-with-multi-steps/code/examples-M3/monthly/',image.index,sep = '')
    # #begin to plot 
    # # png(file=image.path,
    # #     width=2000, height=2000)
    # svg(image.path,width=30,height=30)
    par(mfrow = c(12, 12))  # Set up a 2 x 2 plotting space
    for (num.of.sub.seasons in 1:11) {
      for (begin.position in 1:12) {
        timm.series<-get.different.sub.seasonal.seqence.for.one.ts(num.of.sub.seasons,begin.position,ts,
                                                                   formatted.date.of.the.train.data,
                                                                   formatted.date.of.the.test.data,
                                                                   type.of.ts)
        print('test1')
        print(timm.series)
        print('test2')
        #plot.ts(new.ts,xy.labels = '11')
        #plot(new.ts,main = 'Q1')
        title.name=tile.name(begin.position,num.of.sub.seasons,type.of.ts)
        plot(timm.series,xlab = 'Period',ylab = '',type = 'o',main = title.name,cex.main=0.9)
      }
      
      
    }
    title.name='M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11+M12'
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    #dev.off()
    
  }else{
    #quarterly data
    # number.of.months=1:3
    # begin.position=1:4
    # image.index=paste(ts$st,'.svg',sep = '')
    # image.path=paste('/Users/xushengxiang/Desktop/lixixi/forecasting-with-multi-steps/code/examples-M3/quarterly/',image.index,sep = '')
    # #begin to plot 
    # # png(file=image.path,
    # #     width=1300, height=1300)
    # svg(image.path,width=10,height=10)
    #png(file='multiple-series.png',width=1300, height=1300)
    #dev.new()
    par(mfrow = c(4, 4))  # Set up a 2 x 2 plotting space
    for (num.of.sub.seasons in 1:3) {
      for (begin.position in 1:4) {
        timm.series<-get.different.sub.seasonal.seqence.for.one.ts(num.of.sub.seasons,begin.position,ts,
                                                                   formatted.date.of.the.train.data,
                                                                   formatted.date.of.the.test.data,
                                                                   type.of.ts)
        #print(timm.series)
        #plot.ts(new.ts)
        title.name=tile.name(begin.position,num.of.sub.seasons,type.of.ts)
        plot(timm.series,main = title.name,xlab = 'Period',ylab = '',type = 'o')
      }
      
      
    }
    plot(ts$x,main='Q1+Q2+Q3+Q4',type = 'o')
    plot(ts$x,main='Q1+Q2+Q3+Q4',type = 'o')
    plot(ts$x,main='Q1+Q2+Q3+Q4',type = 'o')
    plot(ts$x,main='Q1+Q2+Q3+Q4',type = 'o')
    #dev.off()
    #par(grid)
    
  }
}
#projection from 'A''N''A'-------1,1.5,2
project.char.to.number.for.ets.method<-function(components){
  comps.number=rep(0,1,3)
  # Error term
  if (components[1]=="A"){
    comps.number[1] <- 1
  } else {
    comps.number[1] <- 2
  }
  #comps.char[AL,1] <- components[1]
  # Trend term
  if (components[2]=="A"){
    comps.number[2] <- 1
  } else if (components[2]=="Ad"){
    comps.number[2] <- 1.5
  } else if (components[2]=="M"){
    comps.number[2] <- 2
  } else if (components[2]=="Md"){
    comps.number[2] <- 2.5
  } else {
    comps.number[2] <- 0
  }
  #comps.char[AL,2] <- components[2]
  # Season term
  if (components[3]=="A"){
    comps.number[3] <- 1
  } else {if (components[3]=="M"){
    comps.number[3] <- 2
  } else
    comps.number[3] <- 0
  }
  #comps.char[AL,3] <- components[3]
  return(comps.number)
}
plot.ets.components.for.one.ts<-function(ts,type.of.ts){
  #get formatted date of the train data of the ts
  formatted.date.of.the.train.data<-get.the.formatted.date.of.the.train.data(ts,type.of.ts)
  formatted.date.of.the.test.data<-get.the.formatted.date.of.the.test.data(ts,type.of.ts)
  #
  #begin to plot
  if(type.of.ts=='M'){
    # number.of.months=1:11
    # begin.position=1:12
    image.index=paste(ts$st,'.svg',sep = '')
    image.path=paste('/Users/xushengxiang/Desktop/lixixi/forecasting-with-multi-steps/code/examples-M3/monthly-ets-components/',image.index,sep = '')
    #begin to plot 
    # png(file=image.path,
    #     width=2000, height=2000)
    svg(image.path,width=30,height=30)
    par(mfrow = c(12, 12))  # Set up a 2 x 2 plotting space
    #generate 
    for (num.of.sub.seasons in 1:11) {
      for (begin.position in 1:12) {
        timm.series<-get.different.sub.seasonal.seqence.for.one.ts(num.of.sub.seasons,begin.position,ts,
                                                                   formatted.date.of.the.train.data,
                                                                   formatted.date.of.the.test.data,
                                                                   type.of.ts)
        #begin to forecast
        
        print('test1')
        print(timm.series)
        print('test2')
        #plot.ts(new.ts,xy.labels = '11')
        #plot(new.ts,main = 'Q1')
        title.name=tile.name(begin.position,num.of.sub.seasons,type.of.ts)
        plot(timm.series,xlab = 'Period',ylab = '',type = 'o',main = title.name,cex.main=0.9)
      }
      
      
    }
    title.name='M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11+M12'
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name,cex.main=0.9)
    dev.off()
    
  }else{
    #quarterly data
    # number.of.months=1:3
    # begin.position=1:4
    image.index=paste(ts$st,'.svg',sep = '')
    image.path=paste('/Users/xushengxiang/Desktop/lixixi/forecasting-with-multi-steps/code/examples-M3/quarterly-ets-components/',image.index,sep = '')
    #begin to plot 
    # png(file=image.path,
    #     width=1300, height=1300)
    svg(image.path,width=10,height=10)
    par(mfrow = c(4, 4))  # Set up a 2 x 2 plotting space
    #generate a contatiner to container components
    components.number.array<-array(0,c(16,3))
    components.char.array<-array(0,c(16,3))
    #y
    y.label=c()
    count.temp=0
    for (num.of.sub.seasons in 1:3) {
      for (begin.position in 1:4) {
        timm.series<-get.different.sub.seasonal.seqence.for.one.ts(num.of.sub.seasons,begin.position,ts,
                                                                   formatted.date.of.the.train.data,
                                                                   formatted.date.of.the.test.data,
                                                                   type.of.ts)
        fit=ets(M3[[1000]]$x)
        est.method=fit$method
        mn <- nchar(est.method)
        model.name <- substring(est.method, seq(1,mn,1), seq(1,mn,1))
        comps.number=project.char.to.number.for.ets.method(model.name)
        count.temp=count.temp+1
        for (j in 1:3) {
          components.number.array[count.temp,j]=comps.number[j]
          components.char.array[count.temp,j]=model.name[j]
        }
        title.name=tile.name(begin.position,num.of.sub.seasons,type.of.ts)
        y.label=c(y.label,title.name)

      }
      
      
    }
    #begin to plot the ets components
    
    plot(ts$x,main='Q1+Q2+Q3+Q4',type = 'o')
    plot(ts$x,main='Q1+Q2+Q3+Q4',type = 'o')
    plot(ts$x,main='Q1+Q2+Q3+Q4',type = 'o')
    plot(ts$x,main='Q1+Q2+Q3+Q4',type = 'o')
    dev.off()
    
  }
}

