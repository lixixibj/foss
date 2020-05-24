library(Tcomp)
#forecasting with multiple approach
library(forecast)
library(Mcomp)
library(M4comp2018)
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
#' calcualte mean
accuracy.of.specific.horizen.cal<-function(accuracy.of.every.horizen.m,begin.horizen,end.horizen){
  #get 
  return(mean(accuracy.of.every.horizen.m[,begin.horizen:end.horizen]))
}
#calculate meadian
median.of.specific.horizen.cal<-function(accuracy.of.every.horizen.m,begin.horizen,end.horizen){
  tt= rowMeans(accuracy.of.every.horizen.m[,begin.horizen:end.horizen])
  return(median(tt))
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
  return(list(point.forecasts=point.forecasts,upper.forecasts=upper.forecasts,lower.forecasts=lower.forecasts,model=model))
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

# train.data=M4
# all.date.of.the.train.data<-as.Date.ts(train.data)

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
forecast.new.ts<-function(i,j,ts,formatted.date.of.the.train.data,formatted.date.of.the.test.data,method.option,level.value,type.of.ts){
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
  result<-get.new.horizen(formatted.combination.months,formatted.date.of.the.test.data)
  new.horizen<-result$new.horizen
  print('new_horizen')
  print(new.horizen)
  forecasts.position<-result$forecasts.position
  #point and interval forecasts for new ts
  forecasts<-forecast.method(method.option,new.ts,new.horizen,level.value)
  #forecasts<-auto_arima_forec(new.ts,new.horizen)
  #original horizen of the original ts
  point.forecasts.container<-rep(NA, times=ts$h)
  upper.forecasts.container<-rep(NA, times=ts$h)
  lower.forecasts.container<-rep(NA,times=ts$h)
  
  point.forecasts.container[forecasts.position]<-forecasts$point.forecasts
  upper.forecasts.container[forecasts.position]<-forecasts$upper.forecasts
  lower.forecasts.container[forecasts.position]<-forecasts$lower.forecasts
  #add model
  model=forecasts$model
  #forecasts.m<-matrix(NA,3,ts$h)
  #forecasts.m[1,]<-point.forecasts.container
  #forecasts.m[2,]<-upper.forecasts.container
  #forecasts.m[3,]<-lower.forecasts.container
  #return(list(point.forecasts=point.forecasts.container,upper.forecasts=upper.forecasts.container,lower.forecasts=lower.forecasts.container))
  #return(forecasts.m)
  return(list(point.forecasts=point.forecasts.container,upper.forecasts=upper.forecasts.container,
              lower.forecasts=lower.forecasts.container,model=model))
}


#forecast with multiple approches with parellel computing for one time series
#' @param ts M4[[1]]
#'  @param type.of.ts 'M' for monthly data and 'Q' for quarterly data
#' @return  list(smape,mase) : smape and mase for every step forecasting(h=1, h=1-2,...)
#' @examples
#' # Not Run
forecast.with.multiple.approaches<-function(ts,type.of.ts,method.option,level.value){
  #get formatted date of the train data of the ts
  formatted.date.of.the.train.data<-get.the.formatted.date.of.the.train.data(ts,type.of.ts)
  formatted.date.of.the.test.data<-get.the.formatted.date.of.the.test.data(ts,type.of.ts)
  #
  if(type.of.ts=='M'){
    # number.of.months=1:11
    # begin.position=1:12
    forecasts.results <- foreach(i =1:11, .combine = 'cbind',.export=c("format.month",
                                                                       "format.quarter",
                                                                       "format.combined.date",
                                                                       "forecast.new.ts",
                                                                       "get.corresponding.ts.data.according.to.the.combination.months",
                                                                       "get.new.horizen",
                                                                       "forecast.method")) %:%
      foreach(j = 1:12) %dopar% {
        forecast.new.ts(i,j,ts,formatted.date.of.the.train.data,formatted.date.of.the.test.data,method.option,level.value,type.of.ts)
        # i=i+1
        #f(i,j)
      }
    #process forecats.results 
    horizen<-ts$h
    point.forecasts.m<-matrix(NA,132,horizen)
    upper.forecasts.m<-matrix(NA,132,horizen)
    lower.forecasts.m<-matrix(NA,132,horizen)
    model.list=list()
    count_temp=1
    for (i in 1:length(forecasts.results)) {
      point.forecasts.m[i,]=forecasts.results[[i]]$point.forecasts
      upper.forecasts.m[i,]=forecasts.results[[i]]$upper.forecasts
      lower.forecasts.m[i,]=forecasts.results[[i]]$lower.forecasts
      model.list[[i]]=forecasts.results[[i]]$model
    }
    
    #use all ts as training data to forecast 
    forecasts<-forecast.method(method.option,ts$x,horizen,level.value)
    original.point.forecasts<-matrix(rep(forecasts$point.forecasts,each=12),nrow=12)
    original.upper.forecasts<-matrix(rep(forecasts$upper.forecasts,each=12),nrow=12)
    original.lower.forecasts<-matrix(rep(forecasts$lower.forecasts,each=12),nrow=12)
    original.model=forecasts$model
    model.list[[i+1]]=original.model
    #concentrate
    point.forecasts.final.m<-rbind(point.forecasts.m,original.point.forecasts)
    upper.forecasts.final.m<-rbind(upper.forecasts.m,original.upper.forecasts)
    lower.forecasts.final.m<-rbind(lower.forecasts.m,original.lower.forecasts)
  }else{
    #quarterly data
    # number.of.months=1:3
    # begin.position=1:4
    forecasts.results <- foreach(i =1:3, .combine = 'cbind',.export=c("format.month",
                                                                      "format.quarter",
                                                                      "format.combined.date",
                                                                      "forecast.new.ts",
                                                                      "get.corresponding.ts.data.according.to.the.combination.months",
                                                                      "get.new.horizen",
                                                                      "forecast.method")) %:%
      foreach(j =1:4) %dopar% {
        forecast.new.ts(i,j,ts,formatted.date.of.the.train.data,formatted.date.of.the.test.data,method.option,level.value,type.of.ts)
        # i=i+1
        #f(i,j)
      }
    #process forecats.results 
    horizen<-ts$h
    point.forecasts.m<-matrix(NA,12,horizen)
    upper.forecasts.m<-matrix(NA,12,horizen)
    lower.forecasts.m<-matrix(NA,12,horizen)
    model.list=list()
    count_temp=1
    for (i in 1:length(forecasts.results)) {
      point.forecasts.m[i,]=forecasts.results[[i]]$point.forecasts
      upper.forecasts.m[i,]=forecasts.results[[i]]$upper.forecasts
      lower.forecasts.m[i,]=forecasts.results[[i]]$lower.forecasts
      #model.array=c(model.array,forecasts.results[[i]]$model)
      model.list[[i]]=forecasts.results[[i]]$model
    }
    
    #use all ts as training data to forecast 
    forecasts<-forecast.method(method.option,ts$x,horizen,level.value)
    original.point.forecasts<-matrix(rep(forecasts$point.forecasts,each=4),nrow=4)
    original.upper.forecasts<-matrix(rep(forecasts$upper.forecasts,each=4),nrow=4)
    original.lower.forecasts<-matrix(rep(forecasts$lower.forecasts,each=4),nrow=4)
    original.model=forecasts$model
    #model.array=c(model.array,original.model)
    model.list[[i+1]]=original.model
    #concentrate
    point.forecasts.final.m<-rbind(point.forecasts.m,original.point.forecasts)
    upper.forecasts.final.m<-rbind(upper.forecasts.m,original.upper.forecasts)
    lower.forecasts.final.m<-rbind(lower.forecasts.m,original.lower.forecasts)
  }
  
  #model averaging
  final.point.forecasts<-colMeans(point.forecasts.final.m, na.rm=TRUE)
  final.upper.forecasts<-colMeans(upper.forecasts.final.m, na.rm=TRUE)
  final.lower.forecasts<-colMeans(lower.forecasts.final.m, na.rm=TRUE)
  #calculate accracy of every step
  #calculate accuracy for point forecasting
  smape<-smape_cal(ts$xx,final.point.forecasts)
  mase<-mase_cal(ts$x,ts$xx,final.point.forecasts)
  #calculate accuracy for interval forecasting
  msis<-msis_cal(ts$x,ts$xx,final.lower.forecasts,final.upper.forecasts)
  return(list(smape=smape,mase=mase,msis=msis,original.point.forecasts=point.forecasts.final.m,
              point.forecasts=final.point.forecasts,original.upper.forecasts=upper.forecasts.final.m,
              upper.forecasts=final.upper.forecasts,original.lower.forecasts=lower.forecasts.final.m,
              lower.forecasts=final.lower.forecasts,all.models=model.list))
}


#test

ts<-M3[[646]]
ts$h
type.of.ts='Q'



#forecast with parellel computing for batch data(many time series)
#' @param dataset eg. M4
#'  @param data.index  eg. 1:100
#'  @param type.of.ts 'M' for monthly data and 'Q' for quarterly data
#'  @param horizen 
#' @return  Null
#' @examples
#' # Not Run
forecast.for.batch.data<-function(dataset,method.option,level.value,saving.path){
  # 启用parallel作为foreach并行计算的后端
  library(doParallel)
  data.index=1:length(dataset)
  id=dataset[[1]]$st
  type.of.ts=strsplit(id,'')[[1]][1]
  # n.cores=parallel::detectCores()
  # cl <- makeCluster(n.cores)
  # registerDoParallel(cl)
  horizen.length=dataset[[1]]$h
  if(length(data.index)==1){
    x <- foreach(x=data.index,.combine='rbind') %do% forecast.with.multiple.approaches(dataset[[x]],type.of.ts,,method.option,level.value)
    #parelle computing needs to be adjusted
    #inint matrix, adjusted horizen
    smape.accuracy.matrix<-matrix(0,length(x)/10,horizen.length)
    mase.accuracy.matrix<-matrix(0,length(x)/10,horizen.length)
    msis.accuracy.matrix<-matrix(0,length(x)/10,horizen.length)
    for (row.temp in 1:(length(x)/2)) {
      smape.accuracy.matrix[row.temp,]<-x[[1]]
      mase.accuracy.matrix[row.temp,]<-x[[2]]
      msis.accuracy.matrix[row.temp,]<-x[[3]]
    }
    
  }else{
    # print('test')
    # 并行计算方式
    library(Mcomp)
    x <- foreach(x=data.index,.combine='rbind') %do% forecast.with.multiple.approaches(dataset[[x]],type.of.ts,method.option,level.value)
    # #别忘了结束并行
    # stopCluster(cl)
    
    #inint matrix, adjusted horizen
    horizen.length=horizen
    smape.accuracy.matrix<-matrix(0,(length(x)/10),horizen.length)
    mase.accuracy.matrix<-matrix(0,(length(x)/10),horizen.length)
    msis.accuracy.matrix<-matrix(0,(length(x)/10),horizen.length)
    intermediate.results<-list()
    for (row.temp in 1:(length(x)/10)) {
      smape.accuracy.matrix[row.temp,]<-x[[row.temp,1]]
      mase.accuracy.matrix[row.temp,]<-x[[row.temp,2]]
      msis.accuracy.matrix[row.temp,]<-x[[row.temp,3]]
      #20200330 save intermediate result
      intermediate.results.temp<-list(original.point.forecasts=x[[row.temp,4]],
                                      point.forecasts=x[[row.temp,5]],original.upper.forecasts=x[[row.temp,6]],
                                      upper.forecasts=x[[row.temp,7]],original.lower.forecasts=x[[row.temp,8]],
                                      lower.forecasts=x[[row.temp,9]],
                                      all.models=x[[row.temp,10]])
      intermediate.results[[row.temp]]=intermediate.results.temp
    }
    save.path=paste(saving.path,'intermediate.results.rda',sep = '')
    save(intermediate.results, file =save.path)
    # #calculate forecasting accuracy of every horizen
    # smape.accuracy.of.every.horizen<-c()
    # mase.accuracy.of.every.horizen<-c()
    # for (horizen.temp in 1:horizen.length) {
    #   smape.accuracy.of.every.horizen<-c(smape.accuracy.of.every.horizen,mean(smape.accuracy.matrix[,1:horizen.temp]))
    #   mase.accuracy.of.every.horizen<-c(mase.accuracy.of.every.horizen,mean(mase.accuracy.matrix[,1:horizen.temp]))
    # }
  }
  #calculate forecasting accuracy of specific horizen
  #M: 1, 1-6, 7-12,13-18,1-18
  #Q:1,1-3,4-6,7-8,1-8
  if(type.of.ts=='M'){
    #mean
    smape.accuracy.of.every.specific.horizen<-c(accuracy.of.specific.horizen.cal(smape.accuracy.matrix,1,1),
                                                accuracy.of.specific.horizen.cal(smape.accuracy.matrix,1,6),
                                                accuracy.of.specific.horizen.cal(smape.accuracy.matrix,7,12),
                                                accuracy.of.specific.horizen.cal(smape.accuracy.matrix,13,18),
                                                accuracy.of.specific.horizen.cal(smape.accuracy.matrix,1,18))
    mase.accuracy.of.every.specific.horizen<-c(accuracy.of.specific.horizen.cal(mase.accuracy.matrix,1,1),
                                               accuracy.of.specific.horizen.cal(mase.accuracy.matrix,1,6),
                                               accuracy.of.specific.horizen.cal(mase.accuracy.matrix,7,12),
                                               accuracy.of.specific.horizen.cal(mase.accuracy.matrix,13,18),
                                               accuracy.of.specific.horizen.cal(mase.accuracy.matrix,1,18))
    msis.accuracy.of.every.specific.horizen<-c(accuracy.of.specific.horizen.cal(msis.accuracy.matrix,1,1),
                                               accuracy.of.specific.horizen.cal(msis.accuracy.matrix,1,6),
                                               accuracy.of.specific.horizen.cal(msis.accuracy.matrix,7,12),
                                               accuracy.of.specific.horizen.cal(msis.accuracy.matrix,13,18),
                                               accuracy.of.specific.horizen.cal(msis.accuracy.matrix,1,18))
    names(smape.accuracy.of.every.specific.horizen)<-c('1.smape','1-6','7-12','13-18','1-18')
    names(mase.accuracy.of.every.specific.horizen)<-c('1.mase','1-6','7-12','13-18','1-18')
    names(msis.accuracy.of.every.specific.horizen)<-c('1.msis','1-6','7-12','13-18','1-18')
    
    #median
    
    smape.median.of.every.specific.horizen<-c(median(smape.accuracy.matrix[,1]),
                                              median.of.specific.horizen.cal(smape.accuracy.matrix,1,6),
                                              median.of.specific.horizen.cal(smape.accuracy.matrix,7,12),
                                              median.of.specific.horizen.cal(smape.accuracy.matrix,13,18),
                                              median.of.specific.horizen.cal(smape.accuracy.matrix,1,18))
    mase.median.of.every.specific.horizen<-c(median(mase.accuracy.matrix[,1]),
                                             median.of.specific.horizen.cal(mase.accuracy.matrix,1,6),
                                             median.of.specific.horizen.cal(mase.accuracy.matrix,7,12),
                                             median.of.specific.horizen.cal(mase.accuracy.matrix,13,18),
                                             median.of.specific.horizen.cal(mase.accuracy.matrix,1,18))
    msis.median.of.every.specific.horizen<-c(median(msis.accuracy.matrix[,1]),
                                             median.of.specific.horizen.cal(msis.accuracy.matrix,1,6),
                                             median.of.specific.horizen.cal(msis.accuracy.matrix,7,12),
                                             median.of.specific.horizen.cal(msis.accuracy.matrix,13,18),
                                             median.of.specific.horizen.cal(msis.accuracy.matrix,1,18))
    names(smape.median.of.every.specific.horizen)<-c('1.smape.median','1-6','7-12','13-18','1-18')
    names(mase.median.of.every.specific.horizen)<-c('1.mase.median','1-6','7-12','13-18','1-18')
    names(msis.median.of.every.specific.horizen)<-c('1.msis.median','1-6','7-12','13-18','1-18')
  }else if(type.of.ts=='Q'){
    #mean
    smape.accuracy.of.every.specific.horizen<-c(accuracy.of.specific.horizen.cal(smape.accuracy.matrix,1,1),
                                                accuracy.of.specific.horizen.cal(smape.accuracy.matrix,1,3),
                                                accuracy.of.specific.horizen.cal(smape.accuracy.matrix,4,6),
                                                accuracy.of.specific.horizen.cal(smape.accuracy.matrix,7,8),
                                                accuracy.of.specific.horizen.cal(smape.accuracy.matrix,1,8))
    mase.accuracy.of.every.specific.horizen<-c(accuracy.of.specific.horizen.cal(mase.accuracy.matrix,1,1),
                                               accuracy.of.specific.horizen.cal(mase.accuracy.matrix,1,3),
                                               accuracy.of.specific.horizen.cal(mase.accuracy.matrix,4,6),
                                               accuracy.of.specific.horizen.cal(mase.accuracy.matrix,7,8),
                                               accuracy.of.specific.horizen.cal(mase.accuracy.matrix,1,8))
    msis.accuracy.of.every.specific.horizen<-c(accuracy.of.specific.horizen.cal(msis.accuracy.matrix,1,1),
                                               accuracy.of.specific.horizen.cal(msis.accuracy.matrix,1,3),
                                               accuracy.of.specific.horizen.cal(msis.accuracy.matrix,4,6),
                                               accuracy.of.specific.horizen.cal(msis.accuracy.matrix,7,8),
                                               accuracy.of.specific.horizen.cal(msis.accuracy.matrix,1,8))
    names(smape.accuracy.of.every.specific.horizen)<-c('1.smape','1-3','4-6','7-8','1-8')
    names(mase.accuracy.of.every.specific.horizen)<-c('1.mase','1-3','4-6','7-8','1-8')
    names(msis.accuracy.of.every.specific.horizen)<-c('1.msis','1-3','4-6','7-8','1-8')
    #median
    
    smape.median.of.every.specific.horizen<-c(median(smape.accuracy.matrix[,1]),
                                              median.of.specific.horizen.cal(smape.accuracy.matrix,1,3),
                                              median.of.specific.horizen.cal(smape.accuracy.matrix,4,6),
                                              median.of.specific.horizen.cal(smape.accuracy.matrix,7,8),
                                              median.of.specific.horizen.cal(smape.accuracy.matrix,1,8))
    mase.median.of.every.specific.horizen<-c(median(mase.accuracy.matrix[,1]),
                                             median.of.specific.horizen.cal(mase.accuracy.matrix,1,3),
                                             median.of.specific.horizen.cal(mase.accuracy.matrix,4,6),
                                             median.of.specific.horizen.cal(mase.accuracy.matrix,7,8),
                                             median.of.specific.horizen.cal(mase.accuracy.matrix,1,8))
    msis.median.of.every.specific.horizen<-c(median(msis.accuracy.matrix[,1]),
                                             median.of.specific.horizen.cal(msis.accuracy.matrix,1,3),
                                             median.of.specific.horizen.cal(msis.accuracy.matrix,4,6),
                                             median.of.specific.horizen.cal(msis.accuracy.matrix,7,8),
                                             median.of.specific.horizen.cal(msis.accuracy.matrix,1,8))
    names(smape.median.of.every.specific.horizen)<-c('1.smape.median','1-3','4-6','7-8','1-8')
    names(mase.median.of.every.specific.horizen)<-c('1.mase.median','1-3','4-6','7-8','1-8')
    names(msis.median.of.every.specific.horizen)<-c('1.msis.median','1-3','4-6','7-8','1-8')
  }
  print(smape.accuracy.of.every.specific.horizen)
  print(mase.accuracy.of.every.specific.horizen)
  print(msis.accuracy.of.every.specific.horizen)
  print(smape.median.of.every.specific.horizen)
  print(mase.median.of.every.specific.horizen)
  print(msis.median.of.every.specific.horizen)
  return(msis.accuracy.matrix)
  
}

#forecast for one ts using benchmark method
#' @param ts: one time series eg. M4[[1]]
#' @param method.option: 'ets' or 'arima'
#' @param level.value: 95% for interval forecasting
#' @return  smape and mase
#' @examples
#' # Not Run
baseline.method<-function(ts,method.option,level.value){
  if(method.option=='ets'){
    model <- forecast::ets(ts$x)
    forecasts<-forecast::forecast(model, level=c(level.value),h=ts$h)
    point.forecasts<-forecasts$mean
    upper.forecasts<-forecasts$upper
    lower.forecasts<-forecasts$lower
  }else if(method.option=='arima'){
    #model <- forecast::auto.arima(ts$x,stepwise=FALSE, approximation=FALSE)
    model <- forecast::auto.arima(ts$x)
    forecasts<-forecast::forecast(model, level=c(level.value),h=ts$h)
    point.forecasts<-forecasts$mean
    upper.forecasts<-forecasts$upper
    lower.forecasts<-forecasts$lower
  }
  smape<-smape_cal(ts$xx,point.forecasts)
  mase<-mase_cal(ts$x,ts$xx,point.forecasts)
  msis<-msis_cal(ts$x,ts$xx,lower.forecasts,upper.forecasts)
  
  return(list(smape=smape,mase=mase,msis=msis))
  
}
baseline.method(M1[[182]],'arima',95)
ts=M1[[182]]
model <- forecast::ets(ts$x)
final.predict.value<-forecast::forecast(model, h=ts$h)$mean
ts$xx
final.predict.value

#forecast for many ts using benchmark method with parallel computing
#' @param dataset eg. M4
#' @param data.index eg. 1:100
#' @param horizen
#' @param n.cores: number of cores
#' @return  Null
#' @examples
#' # Not Run
forecast.for.batch.data.baseline.method<-function(dataset,data.index,ts.type,horizen,method.option,level.value){
  # print('test')
  # 并行计算方式
  library(Mcomp)
  x <- foreach(x=data.index,.combine='rbind') %do% baseline.method(dataset[[x]],method.option,level.value)
  #别忘了结束并行
  # print(x)
  
  #inint matrix, adjusted horizen
  horizen.length=horizen
  smape.accuracy.matrix<-matrix(0,(length(x)/3),horizen.length)
  mase.accuracy.matrix<-matrix(0,(length(x)/3),horizen.length)
  msis.accuracy.matrix<-matrix(0,(length(x)/3),horizen.length)
  for (row.temp in 1:(length(x)/3)) {
    smape.accuracy.matrix[row.temp,]<-x[[row.temp,1]]
    mase.accuracy.matrix[row.temp,]<-x[[row.temp,2]]
    msis.accuracy.matrix[row.temp,]<-x[[row.temp,3]]
  }
  #calculate forecasting accuracy of specific horizen
  #M: 1, 1-6, 7-12,13-18,1-18
  #Q:1,1-3,4-6,7-8,1-8
  if(type.of.ts=='M'){
    #mean
    smape.accuracy.of.every.specific.horizen<-c(accuracy.of.specific.horizen.cal(smape.accuracy.matrix,1,1),
                                                accuracy.of.specific.horizen.cal(smape.accuracy.matrix,1,6),
                                                accuracy.of.specific.horizen.cal(smape.accuracy.matrix,7,12),
                                                accuracy.of.specific.horizen.cal(smape.accuracy.matrix,13,18),
                                                accuracy.of.specific.horizen.cal(smape.accuracy.matrix,1,18))
    mase.accuracy.of.every.specific.horizen<-c(accuracy.of.specific.horizen.cal(mase.accuracy.matrix,1,1),
                                               accuracy.of.specific.horizen.cal(mase.accuracy.matrix,1,6),
                                               accuracy.of.specific.horizen.cal(mase.accuracy.matrix,7,12),
                                               accuracy.of.specific.horizen.cal(mase.accuracy.matrix,13,18),
                                               accuracy.of.specific.horizen.cal(mase.accuracy.matrix,1,18))
    msis.accuracy.of.every.specific.horizen<-c(accuracy.of.specific.horizen.cal(msis.accuracy.matrix,1,1),
                                               accuracy.of.specific.horizen.cal(msis.accuracy.matrix,1,6),
                                               accuracy.of.specific.horizen.cal(msis.accuracy.matrix,7,12),
                                               accuracy.of.specific.horizen.cal(msis.accuracy.matrix,13,18),
                                               accuracy.of.specific.horizen.cal(msis.accuracy.matrix,1,18))
    names(smape.accuracy.of.every.specific.horizen)<-c('1.smape','1-6','7-12','13-18','1-18')
    names(mase.accuracy.of.every.specific.horizen)<-c('1.mase','1-6','7-12','13-18','1-18')
    names(msis.accuracy.of.every.specific.horizen)<-c('1.msis','1-6','7-12','13-18','1-18')
    #median
    
    smape.median.of.every.specific.horizen<-c(median(smape.accuracy.matrix[,1]),
                                              median.of.specific.horizen.cal(smape.accuracy.matrix,1,6),
                                              median.of.specific.horizen.cal(smape.accuracy.matrix,7,12),
                                              median.of.specific.horizen.cal(smape.accuracy.matrix,13,18),
                                              median.of.specific.horizen.cal(smape.accuracy.matrix,1,18))
    mase.median.of.every.specific.horizen<-c(median(mase.accuracy.matrix[,1]),
                                             median.of.specific.horizen.cal(mase.accuracy.matrix,1,6),
                                             median.of.specific.horizen.cal(mase.accuracy.matrix,7,12),
                                             median.of.specific.horizen.cal(mase.accuracy.matrix,13,18),
                                             median.of.specific.horizen.cal(mase.accuracy.matrix,1,18))
    msis.median.of.every.specific.horizen<-c(median(msis.accuracy.matrix[,1]),
                                             median.of.specific.horizen.cal(msis.accuracy.matrix,1,6),
                                             median.of.specific.horizen.cal(msis.accuracy.matrix,7,12),
                                             median.of.specific.horizen.cal(msis.accuracy.matrix,13,18),
                                             median.of.specific.horizen.cal(msis.accuracy.matrix,1,18))
    names(smape.median.of.every.specific.horizen)<-c('1.smape.median','1-6','7-12','13-18','1-18')
    names(mase.median.of.every.specific.horizen)<-c('1.mase.median','1-6','7-12','13-18','1-18')
    names(msis.median.of.every.specific.horizen)<-c('1.msis.median','1-6','7-12','13-18','1-18')
  }else if(type.of.ts=='Q'){
    smape.accuracy.of.every.specific.horizen<-c(accuracy.of.specific.horizen.cal(smape.accuracy.matrix,1,1),
                                                accuracy.of.specific.horizen.cal(smape.accuracy.matrix,1,3),
                                                accuracy.of.specific.horizen.cal(smape.accuracy.matrix,4,6),
                                                accuracy.of.specific.horizen.cal(smape.accuracy.matrix,7,8),
                                                accuracy.of.specific.horizen.cal(smape.accuracy.matrix,1,8))
    mase.accuracy.of.every.specific.horizen<-c(accuracy.of.specific.horizen.cal(mase.accuracy.matrix,1,1),
                                               accuracy.of.specific.horizen.cal(mase.accuracy.matrix,1,3),
                                               accuracy.of.specific.horizen.cal(mase.accuracy.matrix,4,6),
                                               accuracy.of.specific.horizen.cal(mase.accuracy.matrix,7,8),
                                               accuracy.of.specific.horizen.cal(mase.accuracy.matrix,1,8))
    msis.accuracy.of.every.specific.horizen<-c(accuracy.of.specific.horizen.cal(msis.accuracy.matrix,1,1),
                                               accuracy.of.specific.horizen.cal(msis.accuracy.matrix,1,3),
                                               accuracy.of.specific.horizen.cal(msis.accuracy.matrix,4,6),
                                               accuracy.of.specific.horizen.cal(msis.accuracy.matrix,7,8),
                                               accuracy.of.specific.horizen.cal(msis.accuracy.matrix,1,8))
    names(smape.accuracy.of.every.specific.horizen)<-c('1.smape','1-3','4-6','7-8','1-8')
    names(mase.accuracy.of.every.specific.horizen)<-c('1.mase','1-3','4-6','7-8','1-8')
    names(msis.accuracy.of.every.specific.horizen)<-c('1.msis','1-3','4-6','7-8','1-8')
    #median
    
    smape.median.of.every.specific.horizen<-c(median(smape.accuracy.matrix[,1]),
                                              median.of.specific.horizen.cal(smape.accuracy.matrix,1,3),
                                              median.of.specific.horizen.cal(smape.accuracy.matrix,4,6),
                                              median.of.specific.horizen.cal(smape.accuracy.matrix,7,8),
                                              median.of.specific.horizen.cal(smape.accuracy.matrix,1,8))
    mase.median.of.every.specific.horizen<-c(median(mase.accuracy.matrix[,1]),
                                             median.of.specific.horizen.cal(mase.accuracy.matrix,1,3),
                                             median.of.specific.horizen.cal(mase.accuracy.matrix,4,6),
                                             median.of.specific.horizen.cal(mase.accuracy.matrix,7,8),
                                             median.of.specific.horizen.cal(mase.accuracy.matrix,1,8))
    msis.median.of.every.specific.horizen<-c(median(msis.accuracy.matrix[,1]),
                                             median.of.specific.horizen.cal(msis.accuracy.matrix,1,3),
                                             median.of.specific.horizen.cal(msis.accuracy.matrix,4,6),
                                             median.of.specific.horizen.cal(msis.accuracy.matrix,7,8),
                                             median.of.specific.horizen.cal(msis.accuracy.matrix,1,8))
    names(smape.median.of.every.specific.horizen)<-c('1.smape.median','1-3','4-6','7-8','1-8')
    names(mase.median.of.every.specific.horizen)<-c('1.mase.median','1-3','4-6','7-8','1-8')
    names(msis.median.of.every.specific.horizen)<-c('1.msis.median','1-3','4-6','7-8','1-8')
  }
  print(smape.accuracy.of.every.specific.horizen)
  print(mase.accuracy.of.every.specific.horizen)
  print(msis.accuracy.of.every.specific.horizen)
  print(smape.median.of.every.specific.horizen)
  print(mase.median.of.every.specific.horizen)
  print(msis.median.of.every.specific.horizen)
}
