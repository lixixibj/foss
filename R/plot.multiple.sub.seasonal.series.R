#forecasting with multiple approach
# library(forecast)
# library(Mcomp)
# library(M4comp2018)
# library(zoo)


#' title name 
#' @param begin.position begin.position
#' @param num.of.seasons num.of.seasons
#' @param ts.type ts.type
#' @return title name

tile.name<-function(begin.position,num.of.seasons,ts.type){
  seasons=seq(begin.position,begin.position+num.of.seasons-1,1)
  #change to month format
  seasons<-format_combined_date(seasons,ts.type)
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

#' get formatted date of the train data
#' eg. #type.of.ts=='Quarterly', change month to quarterly format such as 2 3 4 1 2 3 4 1
#
#' @param ts train data of time series eg. M4[[1]]
#' @param type.of.ts  type of ts eg. 'M' or'Q'
#'
#' @return formatted.date.of.test.data eg. #type.of.ts=='Quarterly', change month to quarterly format such as 2 3 4 1 2 3 4 1
#' @importFrom zoo as.Date.ts
#' @export
get.the.formatted.date.of.the.train.data1<-function(ts,type.of.ts){
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
#
#' @param ts test data of time series eg. M4[[1]]
#' @param type.of.ts  type of ts eg. 'M' or'Q'
#'
#' @return formatted.date.of.test.data eg. #type.of.ts=='Quarterly', change month to quarterly format such as 2 3 4 1 2 3 4 1
#' @importFrom zoo as.Date.ts
#' @export
get.the.formatted.date.of.the.test.data1<-function(ts,type.of.ts){
  test.data<-ts$xx
  all.date.of.the.test.data<-zoo::as.Date.ts(test.data)
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
format.month1<-function(original.month){
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

#' get formatted quartely of the original quarterly 
#  eg.c(3,4,5,7)---->c(3,4,1,3)
#' @param original.quarter eg. c(3,4,5,7)
#'
#' @return eg.c(3,4,5,7)---->c(3,4,1,3)
#' @export
format_quarter<-function(original.quarter){
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

#' format combined date1
#  eg.c(3,4,5,7)---->c(3,4,1,3)
#' @param original.date eg. c(3,4,5,7)
#' @param type.of.ts 'M' or 'Q'
#' @return eg.c(3,4,5,7)---->c(3,4,1,3)
#' @export
format_combined_date<-function(original.date,type.of.ts){
  if(type.of.ts=='M')
    return(format.month1(original.date))
  else if (type.of.ts=='Q') {
    return(format_quarter(original.date))
  }
}
#' get the corresponding ts value according to the combinded months
#' @param ts train data of the ts, eg. M4[[1]]
#'  @param combination.months  combined month, eg,c(1,2,3)
#'  @param formatted.date.of.the.ts  formatted date for the train data of ts
#'
#' @return new.ts : new ts constructed from the orginal ts
#' @export
get.corresponding.ts.data.according.to.the.combination.months1<-function(ts,combination.months,formatted.date.of.the.ts){
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
#' @param combination.months combined month, eg,c(1,2,3)
#'  @param formatted.date.of.the.test.data  formatted date for the test data of ts
#'
#' @return  result=list(new.horizen=counter,forecasts.position=forecasts.position)
#'          counter: the number of horizen for every new ts
#'          forecasts.position: the position of ervery forecasts
#' @export
get.new.horizen1<-function(combination.months,formatted.date.of.the.test.data){
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
get.different.sub.seasonal.seqence.for.one.ts1<-function(i,j,ts,formatted.date.of.the.train.data,formatted.date.of.the.test.data,type.of.ts){
  # formatted.date.of.the.train.data<-get.the.formatted.date.of.the.train.data(ts,type.of.ts)
  # formatted.date.of.the.test.data<-get.the.formatted.date.of.the.test.data(ts,type.of.ts)
  number.of.months<-i
  begin.position<-j
  end.position<-begin.position+number.of.months-1
  combination.dates<-seq(begin.position,end.position)
  #change to month format
  formatted.combination.months<-format_combined_date(combination.dates,type.of.ts)
  #begin to forecast
  #such as c(2,3,4)
  new.ts<-get.corresponding.ts.data.according.to.the.combination.months1(ts,formatted.combination.months,formatted.date.of.the.train.data)

  return(new.ts)
}


#' plot different sub seasona series and save
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param ts time series,type: list
#' @param type.of.ts time series type 'M' or 'Q'
#' @return plot an figure
#' @export
plot_different_sub_seasonal_seqence_for_one_ts<-function(ts,type.of.ts){
  #get formatted date of the train data of the ts
  formatted.date.of.the.train.data<-get.the.formatted.date.of.the.train.data1(ts,type.of.ts)
  formatted.date.of.the.test.data<-get.the.formatted.date.of.the.test.data1(ts,type.of.ts)
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
        timm.series<-get.different.sub.seasonal.seqence.for.one.ts1(num.of.sub.seasons,begin.position,ts,
                                                              formatted.date.of.the.train.data,
                                                              formatted.date.of.the.test.data,
                                                              type.of.ts)
        print('test1')
        print(timm.series)
        print('test2')
        #plot.ts(new.ts,xy.labels = '11')
        #plot(new.ts,main = 'Q1')
        title.name1=tile.name(begin.position,num.of.sub.seasons,type.of.ts)
        plot(timm.series,xlab = 'Period',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
      }
      
      
    }
    title.name1='M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11+M12'
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
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
        timm.series<-get.different.sub.seasonal.seqence.for.one.ts1(num.of.sub.seasons,begin.position,ts,
                                                              formatted.date.of.the.train.data,
                                                              formatted.date.of.the.test.data,
                                                              type.of.ts)
        print(timm.series)
        #plot.ts(new.ts)
        title.name1=tile.name(begin.position,num.of.sub.seasons,type.of.ts)
        plot(timm.series,main = title.name1,xlab = 'Period',ylab = '',type = 'o')
    }

    
    }
    plot(ts$x,main='Q1+Q2+Q3+Q4',type = 'o')
    plot(ts$x,main='Q1+Q2+Q3+Q4',type = 'o')
    plot(ts$x,main='Q1+Q2+Q3+Q4',type = 'o')
    plot(ts$x,main='Q1+Q2+Q3+Q4',type = 'o')
    dev.off()
  
  }
}

#' plot different sub seasonal series
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param ts time series,type: list
#' @param type.of.ts time series type 'M' or 'Q'
#' @return plot an figure
#' @export
plot_different_sub_seasonal_series<-function(ts,type.of.ts){
  #get formatted date of the train data of the ts
  formatted.date.of.the.train.data<-get.the.formatted.date.of.the.train.data1(ts,type.of.ts)
  formatted.date.of.the.test.data<-get.the.formatted.date.of.the.test.data1(ts,type.of.ts)
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
        timm.series<-get.different.sub.seasonal.seqence.for.one.ts1(num.of.sub.seasons,begin.position,ts,
                                                                   formatted.date.of.the.train.data,
                                                                   formatted.date.of.the.test.data,
                                                                   type.of.ts)
        print('test1')
        print(timm.series)
        print('test2')
        #plot.ts(new.ts,xy.labels = '11')
        #plot(new.ts,main = 'Q1')
        title.name1=tile.name(begin.position,num.of.sub.seasons,type.of.ts)
        plot(timm.series,xlab = 'Period',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
      }
      
      
    }
    title.name1='M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11+M12'
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
    plot(ts$x,xlab = 'Time',ylab = '',type = 'o',main = title.name1,cex.main=0.9)
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
        timm.series<-get.different.sub.seasonal.seqence.for.one.ts1(num.of.sub.seasons,begin.position,ts,
                                                                   formatted.date.of.the.train.data,
                                                                   formatted.date.of.the.test.data,
                                                                   type.of.ts)
        #print(timm.series)
        #plot.ts(new.ts)
        title.name1=tile.name(begin.position,num.of.sub.seasons,type.of.ts)
        plot(timm.series,main = title.name1,xlab = 'Period',ylab = '',type = 'o')
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

