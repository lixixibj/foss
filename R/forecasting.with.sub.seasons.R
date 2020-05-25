
#' caculate smape
#' test
#' @param outsample x
#' @param forecasts  forecasts
#' @return smape
#' @export
smape_cal <- function(outsample, forecasts) {
  #Used to estimate sMAPE
  outsample <- as.numeric(outsample) ; forecasts<-as.numeric(forecasts)
  smape <- (abs(outsample-forecasts)*200)/(abs(outsample)+abs(forecasts))
  return(smape)
}
#' caculate mase
#' test
#' @param insample x
#' @param outsample xx
#' @param forecasts  forecasts
#' @return smape
#' @importFrom stats is.ts frequency
#' @export
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
#' caculate msis
#' test
#' @param insample x
#' @param outsample xx
#' @param forecasts.lower  lower forecasts
#' @param forecasts.upper upper forecasts
#' @param a 0.05
#' @return msis value
#' @importFrom stats frequency
#' @export
msis_cal<-function(insample, outsample,forecasts.lower,forecasts.upper,a){
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
#' test
#' @param accuracy.of.every.horizen.m shape(num.of.ts,all.horizen)
#' @param begin.position  such as 7
#' @param end.position such as 12
#' 
#' @return forecasts which contain point and interval forecasts
#' @export
accuy_of_specific_horizen_cal<-function(accuracy.of.every.horizen.m,begin.horizen,end.horizen){
  #get 
  return(mean(accuracy.of.every.horizen.m[,begin.horizen:end.horizen]))
}
#' calculate median of specific horizen,such as Monthly data, accuracy of horizen 7-12
#' @param accuracy.of.every.horizen.m shape(num.of.ts,all.horizen)
#' @param begin.position  such as 7
#' @param end.position such as 12
#' @return forecasts which contain point and interval forecasts
#' @export
cal_median_of_specific_horizen<-function(accuracy.of.every.horizen.m,begin.horizen,end.horizen){
  tt= rowMeans(accuracy.of.every.horizen.m[,begin.horizen:end.horizen])
  return(median(tt))
}
#' get point and interva forecasts of one method
#' @param method.option 'ets' or 'arima'
#' @param x ts
#' @param h horizen
#' @param level.value 95, 90,85
#' @return A matrix of the infile
#' @importFrom forecast ets forecast auto.arima
#' @export 
method_forecast<-function(method.option,x,h,level.value){
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
#' @param ts train data of time series eg. M4[[1]]
#' @param type.of.ts  type of ts eg. 'M' or'Q'
#'
#' @return formatted.date.of.test.data eg. #type.of.ts=='Quarterly', change month to quarterly format such as 2 3 4 1 2 3 4 1
#' @importFrom zoo as.Date.ts
#' @export
get.the.formatted.date.of.the.train.data<-function(ts,type.of.ts){
  train.data<-ts$x
  # library(zoo)
  all.date.of.the.train.data<-zoo::as.Date.ts(train.data)
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

#' get formatted date of the test data
#' eg. #type.of.ts=='Quarterly', change month to quarterly format such as 2 3 4 1 2 3 4 1
#' @param ts test data of time series eg. M4[[1]]
#' @param type.of.ts  type of ts eg. 'M' or'Q'
#'
#' @return formatted.date.of.test.data eg. #type.of.ts=='Quarterly', change month to quarterly format such as 2 3 4 1 2 3 4 1
#' @export
get.the.formatted.date.of.the.test.data<-function(ts,type.of.ts){
  test.data<-ts$xx
  # library(zoo)
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



#' get formatted month of the original month 
#'  eg.c(11,12,13,14,15)---->c(11,12,1,2,3)
#' @param original.month  eg.c(11,12,13,14,15)
#'
#' @return eg.c(11,12,13,14,15)---->c(11,12,1,2,3)
#' @export
month_formatting<-function(original.month){
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


#' get formatted quartely of the original quarterly eg.c(3,4,5,7)---->c(3,4,1,3)
#' @param original.quarter eg. c(3,4,5,7)
#'
#' @return eg.c(3,4,5,7)---->c(3,4,1,3)
#' @export
quarter_formatting<-function(original.quarter){
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

#' formate date
#' @param original.date original date
#' @param type.of.ts 'M' or 'Q'
#' @return formatted date
#' @export
combined_date_formatting<-function(original.date,type.of.ts){
  if(type.of.ts=='M')
    return(month_formatting(original.date))
  else if (type.of.ts=='Q') {
    return(quarter_formatting(original.date))
  }
}
#' get the corresponding ts value according to the combinded months
#' test
#' @param ts train data of the ts, eg. 
#' @param combination.months  combined month, eg,c(1,2,3)
#' @param formatted.date.of.the.ts  formatted date for the train data of ts
#' @return new.ts : new ts constructed from the orginal ts
#' @export
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



#' decide to forecast how many points need to be forecast in the horizen in every new ts forecasting
#' test
#' @param combination.months combined month, eg,c(1,2,3)
#' @param formatted.date.of.the.test.data  formatted date for the test data of ts
#'
#' @return  result=list(new.horizen=counter,forecasts.position=forecasts.position)
#'          counter: the number of horizen for every new ts
#'          forecasts.position: the position of ervery forecasts
#' @export
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

#' forecast every new ts 
#' @param i number of months (the number of sub-seasonal)
#'  @param j  begin position
#' @param formatted.date.of.the.train.data formatted date of train data of ts
#' @param formatted.date.of.the.test.data formatted date of train data of ts
#' @return  forecasts of the new ts
#' @export
new_ts_forecast<-function(i,j,ts,formatted.date.of.the.train.data,formatted.date.of.the.test.data,method.option,level.value,type.of.ts){
  # formatted.date.of.the.train.data<-get.the.formatted.date.of.the.train.data(ts,type.of.ts)
  # formatted.date.of.the.test.data<-get.the.formatted.date.of.the.test.data(ts,type.of.ts)
  number.of.months<-i
  begin.position<-j
  end.position<-begin.position+number.of.months-1
  combination.dates<-seq(begin.position,end.position)
  #change to month format
  formatted.combination.months<-combined_date_formatting(combination.dates,type.of.ts)
  #begin to forecast
  #such as c(2,3,4)
  new.ts<-get.corresponding.ts.data.according.to.the.combination.months(ts,formatted.combination.months,formatted.date.of.the.train.data)
  result<-get.new.horizen(formatted.combination.months,formatted.date.of.the.test.data)
  new.horizen<-result$new.horizen
  # print('new_horizen')
  # print(new.horizen)
  forecasts.position<-result$forecasts.position
  #point and interval forecasts for new ts
  forecasts<-method_forecast(method.option,new.ts,new.horizen,level.value)
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


#' forecast with multiple approches with parellel computing for one time series
#' @param ts M4[[1]]
#' @param type.of.ts 'M' for monthly data and 'Q' for quarterly data
#' @param method.option 'ets' or 'arima'
#' @param level.value 95
#' @return  list(smape,mase) : smape and mase for every step forecasting(h=1, h=1-2,...)
#' @importFrom  foreach foreach
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @export
forecasting_with_multiple_approaches<-function(ts,type.of.ts,method.option,level.value){
  #get formatted date of the train data of the ts
  formatted.date.of.the.train.data<-get.the.formatted.date.of.the.train.data(ts,type.of.ts)
  formatted.date.of.the.test.data<-get.the.formatted.date.of.the.test.data(ts,type.of.ts)
  #
  n.cores=parallel::detectCores()-1
  cl <- parallel::makeCluster(n.cores)
  doParallel::registerDoParallel(cl)
  if(type.of.ts=='M'){
    # number.of.months=1:11
    # begin.position=1:12
    forecasts.results <-foreach::foreach(i =1:11, .combine = 'cbind',.export=c("month_formatting",
                                                                       "quarter_formatting",
                                                                       "combined_date_formatting",
                                                                       "new_ts_forecast",
                                                                       "get.corresponding.ts.data.according.to.the.combination.months",
                                                                       "get.new.horizen",
                                                                       "method_forecast")) %:%
      foreach::foreach(j = 1:12) %dopar% {
        new_ts_forecast(i,j,ts,formatted.date.of.the.train.data,formatted.date.of.the.test.data,method.option,level.value,type.of.ts)
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
    forecasts<-method_forecast(method.option,ts$x,horizen,level.value)
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
    forecasts.results <-foreach::foreach(i =1:3, .combine = 'cbind',.export=c("month_formatting",
                                                                      "quarter_formatting",
                                                                      "combined_date_formatting",
                                                                      "new_ts_forecast",
                                                                      "get.corresponding.ts.data.according.to.the.combination.months",
                                                                      "get.new.horizen",
                                                                      "method_forecast")) %:%
      foreach::foreach(j =1:4) %dopar% {
        new_ts_forecast(i,j,ts,formatted.date.of.the.train.data,formatted.date.of.the.test.data,method.option,level.value,type.of.ts)
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
    forecasts<-method_forecast(method.option,ts$x,horizen,level.value)
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
  parallel::stopCluster(cl)
  #model averaging
  final.point.forecasts<-colMeans(point.forecasts.final.m, na.rm=TRUE)
  final.upper.forecasts<-colMeans(upper.forecasts.final.m, na.rm=TRUE)
  final.lower.forecasts<-colMeans(lower.forecasts.final.m, na.rm=TRUE)
  #calculate accracy of every step
  #calculate accuracy for point forecasting
  smape<-smape_cal(ts$xx,final.point.forecasts)
  mase<-mase_cal(ts$x,ts$xx,final.point.forecasts)
  #calculate accuracy for interval forecasting
  msis<-msis_cal(ts$x,ts$xx,final.lower.forecasts,final.upper.forecasts,0.05)
  return(list(smape=smape,mase=mase,msis=msis,original.point.forecasts=point.forecasts.final.m,
              point.forecasts=final.point.forecasts,original.upper.forecasts=upper.forecasts.final.m,
              upper.forecasts=final.upper.forecasts,original.lower.forecasts=lower.forecasts.final.m,
              lower.forecasts=final.lower.forecasts,all.models=model.list))
}





#' forecast with parellel computing for batch data(many time series)
#' This function allows you
#' @param dataset eg. M4
#' @param method.option 'ets' or 'arima'
#' @param level.value 95
#' @param saving.path path of saving intermediate results
#' @param horizen 
#' @return  Null
#' @importFrom  foreach foreach
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @export
forecasting_with_mutiple_approches_for_batch_data_and_saving_results<-function(dataset,method.option,level.value,saving.path){
  # library(doParallel)
  data.index=1:length(dataset)
  id=dataset[[1]]$st
  type.of.ts=strsplit(id,'')[[1]][1]
  n.cores=parallel::detectCores()-1
  cl <- parallel::makeCluster(n.cores)
  doParallel::registerDoParallel(cl)
  horizen.length=dataset[[1]]$h
  if(length(data.index)==1){
    x <- foreach::foreach(x=data.index,.combine='rbind') %do% forecasting_with_multiple_approaches(dataset[[x]],type.of.ts,method.option,level.value)
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
    # library(Mcomp)
    x <- foreach::foreach(x=data.index,.combine='rbind') %do% forecasting_with_multiple_approaches(dataset[[x]],type.of.ts,method.option,level.value)
    
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
  parallel::stopCluster(cl)
  #calculate forecasting accuracy of specific horizen
  #M: 1, 1-6, 7-12,13-18,1-18
  #Q:1,1-3,4-6,7-8,1-8
  if(type.of.ts=='M'){
    #mean
    smape.accuracy.of.every.specific.horizen<-c(accuy_of_specific_horizen_cal(smape.accuracy.matrix,1,1),
                                                accuy_of_specific_horizen_cal(smape.accuracy.matrix,1,6),
                                                accuy_of_specific_horizen_cal(smape.accuracy.matrix,7,12),
                                                accuy_of_specific_horizen_cal(smape.accuracy.matrix,13,18),
                                                accuy_of_specific_horizen_cal(smape.accuracy.matrix,1,18))
    mase.accuracy.of.every.specific.horizen<-c(accuy_of_specific_horizen_cal(mase.accuracy.matrix,1,1),
                                               accuy_of_specific_horizen_cal(mase.accuracy.matrix,1,6),
                                               accuy_of_specific_horizen_cal(mase.accuracy.matrix,7,12),
                                               accuy_of_specific_horizen_cal(mase.accuracy.matrix,13,18),
                                               accuy_of_specific_horizen_cal(mase.accuracy.matrix,1,18))
    msis.accuracy.of.every.specific.horizen<-c(accuy_of_specific_horizen_cal(msis.accuracy.matrix,1,1),
                                               accuy_of_specific_horizen_cal(msis.accuracy.matrix,1,6),
                                               accuy_of_specific_horizen_cal(msis.accuracy.matrix,7,12),
                                               accuy_of_specific_horizen_cal(msis.accuracy.matrix,13,18),
                                               accuy_of_specific_horizen_cal(msis.accuracy.matrix,1,18))
    names(smape.accuracy.of.every.specific.horizen)<-c('1.smape','1-6','7-12','13-18','1-18')
    names(mase.accuracy.of.every.specific.horizen)<-c('1.mase','1-6','7-12','13-18','1-18')
    names(msis.accuracy.of.every.specific.horizen)<-c('1.msis','1-6','7-12','13-18','1-18')
    
    #median
    
    smape.median.of.every.specific.horizen<-c(median(smape.accuracy.matrix[,1]),
                                              cal_median_of_specific_horizen(smape.accuracy.matrix,1,6),
                                              cal_median_of_specific_horizen(smape.accuracy.matrix,7,12),
                                              cal_median_of_specific_horizen(smape.accuracy.matrix,13,18),
                                              cal_median_of_specific_horizen(smape.accuracy.matrix,1,18))
    mase.median.of.every.specific.horizen<-c(median(mase.accuracy.matrix[,1]),
                                             cal_median_of_specific_horizen(mase.accuracy.matrix,1,6),
                                             cal_median_of_specific_horizen(mase.accuracy.matrix,7,12),
                                             cal_median_of_specific_horizen(mase.accuracy.matrix,13,18),
                                             cal_median_of_specific_horizen(mase.accuracy.matrix,1,18))
    msis.median.of.every.specific.horizen<-c(median(msis.accuracy.matrix[,1]),
                                             cal_median_of_specific_horizen(msis.accuracy.matrix,1,6),
                                             cal_median_of_specific_horizen(msis.accuracy.matrix,7,12),
                                             cal_median_of_specific_horizen(msis.accuracy.matrix,13,18),
                                             cal_median_of_specific_horizen(msis.accuracy.matrix,1,18))
    names(smape.median.of.every.specific.horizen)<-c('1.smape.median','1-6','7-12','13-18','1-18')
    names(mase.median.of.every.specific.horizen)<-c('1.mase.median','1-6','7-12','13-18','1-18')
    names(msis.median.of.every.specific.horizen)<-c('1.msis.median','1-6','7-12','13-18','1-18')
  }else if(type.of.ts=='Q'){
    #mean
    smape.accuracy.of.every.specific.horizen<-c(accuy_of_specific_horizen_cal(smape.accuracy.matrix,1,1),
                                                accuy_of_specific_horizen_cal(smape.accuracy.matrix,1,3),
                                                accuy_of_specific_horizen_cal(smape.accuracy.matrix,4,6),
                                                accuy_of_specific_horizen_cal(smape.accuracy.matrix,7,8),
                                                accuy_of_specific_horizen_cal(smape.accuracy.matrix,1,8))
    mase.accuracy.of.every.specific.horizen<-c(accuy_of_specific_horizen_cal(mase.accuracy.matrix,1,1),
                                               accuy_of_specific_horizen_cal(mase.accuracy.matrix,1,3),
                                               accuy_of_specific_horizen_cal(mase.accuracy.matrix,4,6),
                                               accuy_of_specific_horizen_cal(mase.accuracy.matrix,7,8),
                                               accuy_of_specific_horizen_cal(mase.accuracy.matrix,1,8))
    msis.accuracy.of.every.specific.horizen<-c(accuy_of_specific_horizen_cal(msis.accuracy.matrix,1,1),
                                               accuy_of_specific_horizen_cal(msis.accuracy.matrix,1,3),
                                               accuy_of_specific_horizen_cal(msis.accuracy.matrix,4,6),
                                               accuy_of_specific_horizen_cal(msis.accuracy.matrix,7,8),
                                               accuy_of_specific_horizen_cal(msis.accuracy.matrix,1,8))
    names(smape.accuracy.of.every.specific.horizen)<-c('1.smape','1-3','4-6','7-8','1-8')
    names(mase.accuracy.of.every.specific.horizen)<-c('1.mase','1-3','4-6','7-8','1-8')
    names(msis.accuracy.of.every.specific.horizen)<-c('1.msis','1-3','4-6','7-8','1-8')
    #median
    
    smape.median.of.every.specific.horizen<-c(median(smape.accuracy.matrix[,1]),
                                              cal_median_of_specific_horizen(smape.accuracy.matrix,1,3),
                                              cal_median_of_specific_horizen(smape.accuracy.matrix,4,6),
                                              cal_median_of_specific_horizen(smape.accuracy.matrix,7,8),
                                              cal_median_of_specific_horizen(smape.accuracy.matrix,1,8))
    mase.median.of.every.specific.horizen<-c(median(mase.accuracy.matrix[,1]),
                                             cal_median_of_specific_horizen(mase.accuracy.matrix,1,3),
                                             cal_median_of_specific_horizen(mase.accuracy.matrix,4,6),
                                             cal_median_of_specific_horizen(mase.accuracy.matrix,7,8),
                                             cal_median_of_specific_horizen(mase.accuracy.matrix,1,8))
    msis.median.of.every.specific.horizen<-c(median(msis.accuracy.matrix[,1]),
                                             cal_median_of_specific_horizen(msis.accuracy.matrix,1,3),
                                             cal_median_of_specific_horizen(msis.accuracy.matrix,4,6),
                                             cal_median_of_specific_horizen(msis.accuracy.matrix,7,8),
                                             cal_median_of_specific_horizen(msis.accuracy.matrix,1,8))
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

#' baseline method for one ts
#'
#' @param ts time series
#' @param method.option 'ets' or 'arima'
#' @param level.value 95, 90, 85
#' @return calculate forecasts of baseline
#' @importFrom forecast ets auto.arima forecast
#' @export

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
  msis<-msis_cal(ts$x,ts$xx,lower.forecasts,upper.forecasts,0.05)
  
  return(list(smape=smape,mase=mase,msis=msis))
  
}


#' forecast for many ts using benchmark method with parallel computing
#' @param dataset eg. M4
#' @param data.index eg. 1:100
#' @param ts.type 'M' or 'Q'
#' @param horizen h
#' @param method.option 'ets' or 'arima'
#' @param level.value 95
#' @return  Null
#' @importFrom  foreach foreach
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @export

batch_data_baseline_method<-function(dataset,data.index,ts.type,horizen,method.option,level.value){
  # print('test')
  # library(Mcomp)
  n.cores=parallel::detectCores()-1
  cl <- parallel::makeCluster(n.cores)
  doParallel::registerDoParallel(cl)
  x <- foreach::foreach(x=data.index,.combine='rbind') %do% baseline.method(dataset[[x]],method.option,level.value)
  # print(x)
  parallel::stopCluster(cl)
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
    smape.accuracy.of.every.specific.horizen<-c(accuy_of_specific_horizen_cal(smape.accuracy.matrix,1,1),
                                                accuy_of_specific_horizen_cal(smape.accuracy.matrix,1,6),
                                                accuy_of_specific_horizen_cal(smape.accuracy.matrix,7,12),
                                                accuy_of_specific_horizen_cal(smape.accuracy.matrix,13,18),
                                                accuy_of_specific_horizen_cal(smape.accuracy.matrix,1,18))
    mase.accuracy.of.every.specific.horizen<-c(accuy_of_specific_horizen_cal(mase.accuracy.matrix,1,1),
                                               accuy_of_specific_horizen_cal(mase.accuracy.matrix,1,6),
                                               accuy_of_specific_horizen_cal(mase.accuracy.matrix,7,12),
                                               accuy_of_specific_horizen_cal(mase.accuracy.matrix,13,18),
                                               accuy_of_specific_horizen_cal(mase.accuracy.matrix,1,18))
    msis.accuracy.of.every.specific.horizen<-c(accuy_of_specific_horizen_cal(msis.accuracy.matrix,1,1),
                                               accuy_of_specific_horizen_cal(msis.accuracy.matrix,1,6),
                                               accuy_of_specific_horizen_cal(msis.accuracy.matrix,7,12),
                                               accuy_of_specific_horizen_cal(msis.accuracy.matrix,13,18),
                                               accuy_of_specific_horizen_cal(msis.accuracy.matrix,1,18))
    names(smape.accuracy.of.every.specific.horizen)<-c('1.smape','1-6','7-12','13-18','1-18')
    names(mase.accuracy.of.every.specific.horizen)<-c('1.mase','1-6','7-12','13-18','1-18')
    names(msis.accuracy.of.every.specific.horizen)<-c('1.msis','1-6','7-12','13-18','1-18')
    #median
    
    smape.median.of.every.specific.horizen<-c(median(smape.accuracy.matrix[,1]),
                                              cal_median_of_specific_horizen(smape.accuracy.matrix,1,6),
                                              cal_median_of_specific_horizen(smape.accuracy.matrix,7,12),
                                              cal_median_of_specific_horizen(smape.accuracy.matrix,13,18),
                                              cal_median_of_specific_horizen(smape.accuracy.matrix,1,18))
    mase.median.of.every.specific.horizen<-c(median(mase.accuracy.matrix[,1]),
                                             cal_median_of_specific_horizen(mase.accuracy.matrix,1,6),
                                             cal_median_of_specific_horizen(mase.accuracy.matrix,7,12),
                                             cal_median_of_specific_horizen(mase.accuracy.matrix,13,18),
                                             cal_median_of_specific_horizen(mase.accuracy.matrix,1,18))
    msis.median.of.every.specific.horizen<-c(median(msis.accuracy.matrix[,1]),
                                             cal_median_of_specific_horizen(msis.accuracy.matrix,1,6),
                                             cal_median_of_specific_horizen(msis.accuracy.matrix,7,12),
                                             cal_median_of_specific_horizen(msis.accuracy.matrix,13,18),
                                             cal_median_of_specific_horizen(msis.accuracy.matrix,1,18))
    names(smape.median.of.every.specific.horizen)<-c('1.smape.median','1-6','7-12','13-18','1-18')
    names(mase.median.of.every.specific.horizen)<-c('1.mase.median','1-6','7-12','13-18','1-18')
    names(msis.median.of.every.specific.horizen)<-c('1.msis.median','1-6','7-12','13-18','1-18')
  }else if(type.of.ts=='Q'){
    smape.accuracy.of.every.specific.horizen<-c(accuy_of_specific_horizen_cal(smape.accuracy.matrix,1,1),
                                                accuy_of_specific_horizen_cal(smape.accuracy.matrix,1,3),
                                                accuy_of_specific_horizen_cal(smape.accuracy.matrix,4,6),
                                                accuy_of_specific_horizen_cal(smape.accuracy.matrix,7,8),
                                                accuy_of_specific_horizen_cal(smape.accuracy.matrix,1,8))
    mase.accuracy.of.every.specific.horizen<-c(accuy_of_specific_horizen_cal(mase.accuracy.matrix,1,1),
                                               accuy_of_specific_horizen_cal(mase.accuracy.matrix,1,3),
                                               accuy_of_specific_horizen_cal(mase.accuracy.matrix,4,6),
                                               accuy_of_specific_horizen_cal(mase.accuracy.matrix,7,8),
                                               accuy_of_specific_horizen_cal(mase.accuracy.matrix,1,8))
    msis.accuracy.of.every.specific.horizen<-c(accuy_of_specific_horizen_cal(msis.accuracy.matrix,1,1),
                                               accuy_of_specific_horizen_cal(msis.accuracy.matrix,1,3),
                                               accuy_of_specific_horizen_cal(msis.accuracy.matrix,4,6),
                                               accuy_of_specific_horizen_cal(msis.accuracy.matrix,7,8),
                                               accuy_of_specific_horizen_cal(msis.accuracy.matrix,1,8))
    names(smape.accuracy.of.every.specific.horizen)<-c('1.smape','1-3','4-6','7-8','1-8')
    names(mase.accuracy.of.every.specific.horizen)<-c('1.mase','1-3','4-6','7-8','1-8')
    names(msis.accuracy.of.every.specific.horizen)<-c('1.msis','1-3','4-6','7-8','1-8')
    #median
    
    smape.median.of.every.specific.horizen<-c(median(smape.accuracy.matrix[,1]),
                                              cal_median_of_specific_horizen(smape.accuracy.matrix,1,3),
                                              cal_median_of_specific_horizen(smape.accuracy.matrix,4,6),
                                              cal_median_of_specific_horizen(smape.accuracy.matrix,7,8),
                                              cal_median_of_specific_horizen(smape.accuracy.matrix,1,8))
    mase.median.of.every.specific.horizen<-c(median(mase.accuracy.matrix[,1]),
                                             cal_median_of_specific_horizen(mase.accuracy.matrix,1,3),
                                             cal_median_of_specific_horizen(mase.accuracy.matrix,4,6),
                                             cal_median_of_specific_horizen(mase.accuracy.matrix,7,8),
                                             cal_median_of_specific_horizen(mase.accuracy.matrix,1,8))
    msis.median.of.every.specific.horizen<-c(median(msis.accuracy.matrix[,1]),
                                             cal_median_of_specific_horizen(msis.accuracy.matrix,1,3),
                                             cal_median_of_specific_horizen(msis.accuracy.matrix,4,6),
                                             cal_median_of_specific_horizen(msis.accuracy.matrix,7,8),
                                             cal_median_of_specific_horizen(msis.accuracy.matrix,1,8))
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

