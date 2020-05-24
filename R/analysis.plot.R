#begin to plot
#1. decomposition plot (it is completed by other codes)
#2.ETS components or arima components
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
#plot ets components for one time series
plot.ets.components.for.one.ts<-function(ts.index,ts.type,intermediate.results){
  
  if(ts.type=='Q'){
    all.models=intermediate.results[[ts.index]]$all.models
    #components container
    components.array<-array(0,c(3,16))
    components.char<-array(0,c(3,16))
    for (i in 1:13) {
      #get ets model type
      model=all.models[[i]]
      est.components=model$components
      model.name=est.components[1:3]
      # mn <- nchar(est.method)
      # model.name <- substring(est.method, seq(1,mn,1), seq(1,mn,1))
      #1.component numbers
      comps.number=project.char.to.number.for.ets.method(model.name)
      #2.component char
      for (j in 1:3) {
        components.array[j,i]=comps.number[j]
        components.char[j,i]=model.name[j]
      }
    }
    #add the same 
    for (j in 1:3) {
      components.array[,j+13]=components.array[,13]
      components.char[,j+13]=components.char[,13]
    }
    #get
    
    #begin to plot
    image(1:16, 1:3,matrix(t(components.array),ncol=3), axes=FALSE, col=brewer.pal(8,"PuBu")[2:6], #rev(heat.colors(5)),
          ylab="Components", xlab="Seasons", main='ETS Components',breaks=c(-1,0,1,1.5,2,2.5),cex.lab=1.3,cex.main=1.3)
    axis(2, at=1:3, labels=list("Error","Trend","Season","Xreg")[(3+0):1])
    axis(1, at=1:16,labels = list('Q1','Q2','Q3','Q4',
                                  'Q1,2','Q2,3','Q3,4','Q4,1',
                                  'Q1,2,3','Q2,3,4','Q3,4,1','Q4,1,2',
                                  'Q1-4','Q1-4','Q1-4','Q1-4'),cex.axis=0.75)
    box()
    
    #get comps.char shape(3*16)
    for (i in 1:4){
      for (AL in 1:16){
        if (i==1){
          lines(c(AL-0.5+1-1,AL-0.5+1-1),c(0,4+0),col="black")
        }
        if (i<4 & AL<=16){
          #text(AL+1-1,i+0,'test')
          text(AL+1-1,i+0,components.char[i,AL])
        }
      }
      lines(c(1-0.5,16+0.5),c(i-0.5,i-0.5),col="black")
    }}else if(ts.type=='M'){
      all.models=intermediate.results[[ts.index]]$all.models
      #components container
      components.array<-array(0,c(3,144))
      components.char<-array(0,c(3,144))
      for (i in 1:133) {
        #get ets model type
        model=all.models[[i]]
        est.components=model$components
        model.name=est.components[1:3]
        # mn <- nchar(est.method)
        # model.name <- substring(est.method, seq(1,mn,1), seq(1,mn,1))
        #1.component numbers
        comps.number=project.char.to.number.for.ets.method(model.name)
        #2.component char
        for (j in 1:3) {
          components.array[j,i]=comps.number[j]
          components.char[j,i]=model.name[j]
        }
      }
      #add the same 
      for (j in 1:11) {
        components.array[,j+133]=components.array[,133]
        components.char[,j+133]=components.char[,133]
      }
      #get
      
      #begin to plot
      image(1:144, 1:3,matrix(t(components.array),ncol=3), axes=FALSE, col=brewer.pal(8,"PuBu")[2:6], #rev(heat.colors(5)),
            ylab="Components", xlab="Seasons", main='ETS Components',breaks=c(-1,0,1,1.5,2,2.5),cex.lab=1.3,cex.main=1.3)
      axis(2, at=1:3, labels=list("Error","Trend","Season","Xreg")[(3+0):1])
      axis(1, at=1:144,,cex.axis=0.75)
      box()
      
      #get comps.char shape(3*16)
      for (i in 1:4){
        for (AL in 1:144){
          if (i==1){
            lines(c(AL-0.5+1-1,AL-0.5+1-1),c(0,4+0),col="black")
          }
          if (i<4 & AL<=144){
            #text(AL+1-1,i+0,'test')
            text(AL+1-1,i+0,components.char[i,AL])
          }
        }
        lines(c(1-0.5,144+0.5),c(i-0.5,i-0.5),col="black")
      }
    }
  }


#plot ets components for one time series
plot.ets.components<-function(ts.type,forecast.results){
  
  if(ts.type=='Q'){
    all.models=forecast.results$all.models
    #components container
    components.array<-array(0,c(3,16))
    components.char<-array(0,c(3,16))
    for (i in 1:13) {
      #get ets model type
      model=all.models[[i]]
      est.components=model$components
      model.name=est.components[1:3]
      # mn <- nchar(est.method)
      # model.name <- substring(est.method, seq(1,mn,1), seq(1,mn,1))
      #1.component numbers
      comps.number=project.char.to.number.for.ets.method(model.name)
      #2.component char
      for (j in 1:3) {
        components.array[j,i]=comps.number[j]
        components.char[j,i]=model.name[j]
      }
    }
    #add the same 
    for (j in 1:3) {
      components.array[,j+13]=components.array[,13]
      components.char[,j+13]=components.char[,13]
    }
    #get
    
    #begin to plot
    image(1:16, 1:3,matrix(t(components.array),ncol=3), axes=FALSE, col=brewer.pal(8,"PuBu")[2:6], #rev(heat.colors(5)),
          ylab="Components", xlab="Seasons", main='ETS Components',breaks=c(-1,0,1,1.5,2,2.5),cex.lab=1.3,cex.main=1.3)
    axis(2, at=1:3, labels=list("Error","Trend","Season","Xreg")[(3+0):1])
    axis(1, at=1:16,labels = list('Q1','Q2','Q3','Q4',
                                  'Q1,2','Q2,3','Q3,4','Q4,1',
                                  'Q1,2,3','Q2,3,4','Q3,4,1','Q4,1,2',
                                  'Q1-4','Q1-4','Q1-4','Q1-4'),cex.axis=0.75)
    box()
    
    #get comps.char shape(3*16)
    for (i in 1:4){
      for (AL in 1:16){
        if (i==1){
          lines(c(AL-0.5+1-1,AL-0.5+1-1),c(0,4+0),col="black")
        }
        if (i<4 & AL<=16){
          #text(AL+1-1,i+0,'test')
          text(AL+1-1,i+0,components.char[i,AL])
        }
      }
      lines(c(1-0.5,16+0.5),c(i-0.5,i-0.5),col="black")
    }}else if(ts.type=='M'){
      all.models=forecast.results$all.models
      #components container
      components.array<-array(0,c(3,144))
      components.char<-array(0,c(3,144))
      for (i in 1:133) {
        #get ets model type
        model=all.models[[i]]
        est.components=model$components
        model.name=est.components[1:3]
        # mn <- nchar(est.method)
        # model.name <- substring(est.method, seq(1,mn,1), seq(1,mn,1))
        #1.component numbers
        comps.number=project.char.to.number.for.ets.method(model.name)
        #2.component char
        for (j in 1:3) {
          components.array[j,i]=comps.number[j]
          components.char[j,i]=model.name[j]
        }
      }
      #add the same 
      for (j in 1:11) {
        components.array[,j+133]=components.array[,133]
        components.char[,j+133]=components.char[,133]
      }
      #get
      
      #begin to plot
      image(1:144, 1:3,matrix(t(components.array),ncol=3), axes=FALSE, col=brewer.pal(8,"PuBu")[2:6], #rev(heat.colors(5)),
            ylab="Components", xlab="Seasons", main='ETS Components',breaks=c(-1,0,1,1.5,2,2.5),cex.lab=1.3,cex.main=1.3)
      axis(2, at=1:3, labels=list("Error","Trend","Season","Xreg")[(3+0):1])
      axis(1, at=1:144,,cex.axis=0.75)
      box()
      
      #get comps.char shape(3*16)
      for (i in 1:4){
        for (AL in 1:144){
          if (i==1){
            lines(c(AL-0.5+1-1,AL-0.5+1-1),c(0,4+0),col="black")
          }
          if (i<4 & AL<=144){
            #text(AL+1-1,i+0,'test')
            text(AL+1-1,i+0,components.char[i,AL])
          }
        }
        lines(c(1-0.5,144+0.5),c(i-0.5,i-0.5),col="black")
      }
    }
}
#test
#plot anlysis images for the result
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# saving.path='intermediate.results/M3/M_ets/'
# save.path=paste(saving.path,'intermediate.results.rda',sep = '')
# load(file = save.path)
# ts.index=200
# ts.type='M'
# image.path='test.svg'
# svg(image.path,width=22,height=5)
# plot.ets.components.for.one.ts(ts.index,ts.type,intermediate.results)
# dev.off()

#plot arima components for all sub-seasonal ts
# library(Mcomp)
# #
# library(forecast)
# model=auto.arima(M3[[646]]$x)
# model.name=arimaorder(model)
# model.name
# model.temp=unname(model.name)
# hhh=toString(model.name[1])


plot.arima.components.for.one.ts<-function(ts.index,ts.type,intermediate.results){
  
  if(ts.type=='Q'){
    all.models=intermediate.results[[ts.index]]$all.models
    #components container
    components.array<-array(0,c(3,16))
    components.char<-array(0,c(3,16))
    for (i in 1:13) {
      #get ets model type
      model=all.models[[i]]
      #est.components=model$components
      arima.components=arimaorder(model)
      # model.name=est.components[1:3]
      # mn <- nchar(est.method)
      # model.name <- substring(est.method, seq(1,mn,1), seq(1,mn,1))
      #1.component numbers
      #comps.number=project.char.to.number.for.ets.method(model.name)
      comps.number=arima.components
      #2.component char
      for (j in 1:3) {
        components.array[j,i]=comps.number[j]
        components.char[j,i]=toString(comps.number[j])
      }
    }
    #add the same 
    for (j in 1:3) {
      components.array[,j+13]=components.array[,13]
      components.char[,j+13]=components.char[,13]
    }
    #get
    
    #begin to plot
    image(1:16, 1:3,matrix(t(components.array),ncol=3), axes=FALSE, col=brewer.pal(8,"PuBu")[2:6], #rev(heat.colors(5)),
          ylab="Parameters", xlab="Seasons", main='ARIMA p,d,q',breaks=c(-1,0,1,1.5,2,2.5))
    axis(2, at=1:3, labels=list("p","d","q","Xreg")[(3+0):1])
    axis(1, at=1:16,labels = list('Q1','Q2','Q3','Q4',
                                  'Q1,2','Q2,3','Q3,4','Q4,1',
                                  'Q1,2,3','Q2,3,4','Q3,4,1','Q4,1,2',
                                  'Q1-4','Q1-4','Q1-4','Q1-4'),cex.axis=0.75)
    box()
    
    #get comps.char shape(3*16)
    for (i in 1:4){
      for (AL in 1:16){
        if (i==1){
          lines(c(AL-0.5+1-1,AL-0.5+1-1),c(0,4+0),col="black")
        }
        if (i<4 & AL<=16){
          #text(AL+1-1,i+0,'test')
          text(AL+1-1,i+0,components.char[i,AL])
        }
      }
      lines(c(1-0.5,16+0.5),c(i-0.5,i-0.5),col="black")
    }
  }else if (ts.type=='M'){
    all.models=intermediate.results[[ts.index]]$all.models
    #components container
    components.array<-array(0,c(3,144))
    components.char<-array(0,c(3,144))
    for (i in 1:133) {
      #get ets model type
      model=all.models[[i]]
      est.components=model$components
      model.name=est.components[1:3]
      # mn <- nchar(est.method)
      # model.name <- substring(est.method, seq(1,mn,1), seq(1,mn,1))
      #1.component numbers
      comps.number=project.char.to.number.for.ets.method(model.name)
      #2.component char
      for (j in 1:3) {
        components.array[j,i]=comps.number[j]
        components.char[j,i]=model.name[j]
      }
    }
    #add the same 
    for (j in 1:11) {
      components.array[,j+133]=components.array[,133]
      components.char[,j+133]=components.char[,133]
    }
    #get
    
    #begin to plot
    image(1:144, 1:3,matrix(t(components.array),ncol=3), axes=FALSE, col=brewer.pal(8,"PuBu")[2:6], #rev(heat.colors(5)),
          ylab="Components", xlab="Seasons", main='ETS Components',breaks=c(-1,0,1,1.5,2,2.5))
    axis(2, at=1:3, labels=list("Error","Trend","Season","Xreg")[(3+0):1])
    axis(1, at=1:144)
    box()
    
    #get comps.char shape(3*16)
    for (i in 1:4){
      for (AL in 1:144){
        if (i==1){
          lines(c(AL-0.5+1-1,AL-0.5+1-1),c(0,4+0),col="black")
        }
        if (i<4 & AL<=144){
          #text(AL+1-1,i+0,'test')
          text(AL+1-1,i+0,components.char[i,AL])
        }
      }
      lines(c(1-0.5,144+0.5),c(i-0.5,i-0.5),col="black")
    }
    ################
      
    }
}


plot.arima.components<-function(ts.type,forecast.results){
  
  if(ts.type=='Q'){
    all.models=forecast.results$all.models
    #components container
    components.array<-array(0,c(3,16))
    components.char<-array(0,c(3,16))
    for (i in 1:13) {
      #get ets model type
      model=all.models[[i]]
      #est.components=model$components
      arima.components=arimaorder(model)
      # model.name=est.components[1:3]
      # mn <- nchar(est.method)
      # model.name <- substring(est.method, seq(1,mn,1), seq(1,mn,1))
      #1.component numbers
      #comps.number=project.char.to.number.for.ets.method(model.name)
      comps.number=arima.components
      #2.component char
      for (j in 1:3) {
        components.array[j,i]=comps.number[j]
        components.char[j,i]=toString(comps.number[j])
      }
    }
    #add the same 
    for (j in 1:3) {
      components.array[,j+13]=components.array[,13]
      components.char[,j+13]=components.char[,13]
    }
    #get
    
    #begin to plot
    image(1:16, 1:3,matrix(t(components.array),ncol=3), axes=FALSE, col=brewer.pal(8,"PuBu")[2:6], #rev(heat.colors(5)),
          ylab="Parameters", xlab="Seasons", main='ARIMA p,d,q',breaks=c(-1,0,1,1.5,2,2.5))
    axis(2, at=1:3, labels=list("p","d","q","Xreg")[(3+0):1])
    axis(1, at=1:16,labels = list('Q1','Q2','Q3','Q4',
                                  'Q1,2','Q2,3','Q3,4','Q4,1',
                                  'Q1,2,3','Q2,3,4','Q3,4,1','Q4,1,2',
                                  'Q1-4','Q1-4','Q1-4','Q1-4'),cex.axis=0.75)
    box()
    
    #get comps.char shape(3*16)
    for (i in 1:4){
      for (AL in 1:16){
        if (i==1){
          lines(c(AL-0.5+1-1,AL-0.5+1-1),c(0,4+0),col="black")
        }
        if (i<4 & AL<=16){
          #text(AL+1-1,i+0,'test')
          text(AL+1-1,i+0,components.char[i,AL])
        }
      }
      lines(c(1-0.5,16+0.5),c(i-0.5,i-0.5),col="black")
    }
  }else if (ts.type=='M'){
    all.models=forecast.results$all.models
    #components container
    components.array<-array(0,c(3,144))
    components.char<-array(0,c(3,144))
    for (i in 1:133) {
      #get ets model type
      arima.components=arimaorder(model)
      # model.name=est.components[1:3]
      # mn <- nchar(est.method)
      # model.name <- substring(est.method, seq(1,mn,1), seq(1,mn,1))
      #1.component numbers
      #comps.number=project.char.to.number.for.ets.method(model.name)
      comps.number=arima.components
      #2.component char
      for (j in 1:3) {
        components.array[j,i]=comps.number[j]
        components.char[j,i]=model.name[j]
      }
    }
    #add the same 
    for (j in 1:11) {
      components.array[,j+133]=components.array[,133]
      components.char[,j+133]=components.char[,133]
    }
    #get
    
    #begin to plot
    image(1:144, 1:3,matrix(t(components.array),ncol=3), axes=FALSE, col=brewer.pal(8,"PuBu")[2:6], #rev(heat.colors(5)),
          ylab="Parameters", xlab="Seasons", main='ARIMA p,d,q',breaks=c(-1,0,1,1.5,2,2.5))
    axis(2, at=1:3, labels=list("Error","Trend","Season","Xreg")[(3+0):1])
    axis(1, at=1:144)
    box()
    
    #get comps.char shape(3*16)
    for (i in 1:4){
      for (AL in 1:144){
        if (i==1){
          lines(c(AL-0.5+1-1,AL-0.5+1-1),c(0,4+0),col="black")
        }
        if (i<4 & AL<=144){
          #text(AL+1-1,i+0,'test')
          text(AL+1-1,i+0,components.char[i,AL])
        }
      }
      lines(c(1-0.5,144+0.5),c(i-0.5,i-0.5),col="black")
    }
    ################
    
  }
}

plot.model.components<-function(ts.type,forecast.results,method.name){
  if(method.name=='arima'){
    plot.arima.components(ts.type,forecast.results)
  }else{
    plot.ets.components(ts.type,forecast.results)
  }
}
# #test
# ts.index=300
# ts.type='Q'
# intermediate.results=intermediate.results
# plot.arima.components.for.one.ts(ts.index,ts.type,intermediate.results)


#plot anlysis images for the result
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# saving.path='intermediate.results/M3/M_ets/'
# save.path=paste(saving.path,'intermediate.results.rda',sep = '')
# load(file = save.path)
# ts.index=200
# ts.type='M'
# image.path='test.svg'
# svg(image.path,width=22,height=5)
# plot.ets.components.for.one.ts(ts.index,ts.type,intermediate.results)
# dev.off()

#plot training data, testing and forecasting value


#plot train and test time series

plot.train.and.test.of.one.ts<-function(ts.index,ts.type,intermediate.results){
  library(Mcomp)
  if(ts.type=='Q'){
    j=ts.index
    forecasts=intermediate.results[[j]]$point.forecasts
    forecasts.of.single.method=intermediate.results[[j]]$original.point.forecasts[16,]
    test1=M3[[j+645]]$xx
    for (n in 1:length(test1)) {
      test1[n]=forecasts.of.single.method[n]
    }
    train=M3[[j+645]]$x
    test2=M3[[j+645]]$xx
    for (n in 1:length(test2)) {
      test2[n]=forecasts[n]
    }
    single.forecasts=test1
    predicted.ts=test2
    autoplot(M3[[j+645]]$x,series = 'Training data',xlab = "Time", ylab = "",main='Time series and forecast') + 
      autolayer(M3[[j+645]]$xx,series = 'Testing data')+
      autolayer(predicted.ts,series = 'Multiple forecast')+
      autolayer(single.forecasts,series = 'Standard forecast')+theme(text = element_text(size=15))+scale_color_discrete(breaks=c('Training data','Testing data','Standard forecast','Multiple forecast'))
      
  }else if(ts.type=='M'){
    j=ts.index
    forecasts=intermediate.results[[j]]$point.forecasts
    forecasts.of.single.method=intermediate.results[[j]]$original.point.forecasts[144,]
    test1=M3[[j+1401]]$xx
    for (n in 1:length(test1)) {
      test1[n]=forecasts.of.single.method[n]
    }
    train=M3[[j+1401]]$x
    test2=M3[[j+1401]]$xx
    for (n in 1:length(test2)) {
      test2[n]=forecasts[n]
    }
    single.forecasts=test1
    predicted.ts=test2
    autoplot(M3[[j+1401]]$x,series = 'Training data',xlab = "Time", ylab = "",main='Time series and forecast') + 
      autolayer(M3[[j+1401]]$xx,series = 'Testing data')+
      autolayer(predicted.ts,series = 'Multiple forecast')+
      autolayer(single.forecasts,series = 'Standard forecast')+theme(text = element_text(size=15))+scale_color_discrete(breaks=c('Training data','Testing data','Standard forecast','Multiple forecast'))
  }
}
#test




#plot forecasts of each components(here just for M3 dataset)
plot.forecasts.of.each.new.series<-function(ts.index,ts.type,intermediate.results){
  library(Mcomp)
  if(ts.type=='Q'){
    library(zoo)
    library(MASS)
    library(reshape2)
    library(reshape)
    j=ts.index
    seasons.name=c('Q1','Q2','Q3','Q4',
                   'Q1,2','Q2,3','Q3,4','Q4,1',
                   'Q1,2,3','Q2,3,4','Q3,4,1','Q4,1,2',
                   'Q1,2,3,4')
    original.point.forecasts=intermediate.results[[j]]$original.point.forecasts
    original.point.forecasts=head(original.point.forecasts,-3)
    original.point.forecasts.df=data.frame(original.point.forecasts)
    names(original.point.forecasts.df)=as.yearqtr(time(M3[[j+645]]$xx))
    #original.point.forecasts.df$season=LETTERS[seq( from = 1, to = 16 )]
    original.point.forecasts.df$Seasons=seasons.name
    df <- melt(original.point.forecasts.df, id = c('Seasons'))
    # print(df)
    # colnames(df)
    #df$variable <- as.Date(df$variable, format = "%d-%b-%y")
    #df remove na
    df=df[complete.cases(df), ]
    ggplot(df, aes(x =variable , y = value, group = Seasons,
                   colour = Seasons)) + geom_point() + geom_line()+
      ggtitle("Forecast of each sub-seasonal time series") +
      xlab("Time") + ylab("")+scale_color_discrete(breaks=seasons.name)+theme(text = element_text(size=15))
    
  }else if(ts.type=='M'){
    library(zoo)
    library(MASS)
    library(reshape2)
    library(reshape)
    j=ts.index
    #seasons.name=LETTERS[seq( from = 1, to = 144 )]
    seasons.name=c()
    for (i in 1:133) {
      seasons.name=c(seasons.name,toString(i))
    }
    original.point.forecasts=intermediate.results[[j]]$original.point.forecasts
    original.point.forecasts=head(original.point.forecasts,-11)
    original.point.forecasts.df=data.frame(original.point.forecasts)
    names(original.point.forecasts.df)=as.yearmon(time(M3[[j+1401]]$xx))
    #original.point.forecasts.df$season=LETTERS[seq( from = 1, to = 16 )]
    original.point.forecasts.df$season=seasons.name
    df <- melt(original.point.forecasts.df, id = c('season'))
    # print(df)
    # colnames(df)
    #df$variable <- as.Date(df$variable, format = "%d-%b-%y")
    #df remove na
    df=df[complete.cases(df), ]
    ggplot(df, aes(x =variable , y = value, group = season,
                   colour = season)) + geom_point() + geom_line()+
      ggtitle("Forecast of each sub-seasonal time series") +
      xlab("Time") + ylab("")+scale_color_discrete(breaks=seasons.name)+theme(text = element_text(size=15))
    
  }

}

#plot forecasts of each components(here just for M3 dataset)
plot.forecasts.of.new.mutiple.series<-function(ts.object,ts.type,forecasts.result){
  if(ts.type=='Q'){
    library(zoo)
    library(MASS)
    library(reshape2)
    library(reshape)
    seasons.name=c('Q1','Q2','Q3','Q4',
                   'Q1,2','Q2,3','Q3,4','Q4,1',
                   'Q1,2,3','Q2,3,4','Q3,4,1','Q4,1,2',
                   'Q1,2,3,4')
    original.point.forecasts=forecasts.result$original.point.forecasts
    original.point.forecasts=head(original.point.forecasts,-3)
    original.point.forecasts.df=data.frame(original.point.forecasts)
    names(original.point.forecasts.df)=as.yearqtr(time(ts.object$xx))
    #original.point.forecasts.df$season=LETTERS[seq( from = 1, to = 16 )]
    original.point.forecasts.df$Seasons=seasons.name
    df <- melt(original.point.forecasts.df, id = c('Seasons'))
    # print(df)
    # colnames(df)
    #df$variable <- as.Date(df$variable, format = "%d-%b-%y")
    #df remove na
    df=df[complete.cases(df), ]
    ggplot(df, aes(x =variable , y = value, group = Seasons,
                   colour = Seasons)) + geom_point() + geom_line()+
      ggtitle("Forecast of each sub-seasonal time series") +
      xlab("Time") + ylab("")+scale_color_discrete(breaks=seasons.name)+theme(text = element_text(size=15))
    
  }else if(ts.type=='M'){
    library(zoo)
    library(MASS)
    library(reshape2)
    library(reshape)
    #seasons.name=LETTERS[seq( from = 1, to = 144 )]
    seasons.name=c()
    for (i in 1:133) {
      seasons.name=c(seasons.name,toString(i))
    }
    original.point.forecasts=forecasts.result$original.point.forecasts
    original.point.forecasts=head(original.point.forecasts,-11)
    original.point.forecasts.df=data.frame(original.point.forecasts)
    names(original.point.forecasts.df)=as.yearmon(time(ts.object$xx))
    #original.point.forecasts.df$season=LETTERS[seq( from = 1, to = 16 )]
    original.point.forecasts.df$season=seasons.name
    df <- melt(original.point.forecasts.df, id = c('season'))
    # print(df)
    # colnames(df)
    #df$variable <- as.Date(df$variable, format = "%d-%b-%y")
    #df remove na
    df=df[complete.cases(df), ]
    ggplot(df, aes(x =variable , y = value, group = season,
                   colour = season)) + geom_point() + geom_line()+
      ggtitle("Forecast of each sub-seasonal time series") +
      xlab("Time") + ylab("")+scale_color_discrete(breaks=seasons.name)+theme(text = element_text(size=15))
    
  }
  
}


#plot for batch ts
plot.forecasts.for.batch.ts<-function(ts.type,intermediate.results,saving.path){
  if(ts.type=='Q'){
    for (i in 1:length(intermediate.results)) {
      p1=plot.forecasts.of.each.new.series(i,'Q',intermediate.results)
      p2=plot.train.and.test.of.one.ts(i,'Q',intermediate.results)
      #add lable
      image.index=paste(i,'ts.svg',sep = '')
      image.path=saving.path
      #image.path='/Users/xushengxiang/Desktop/lixixi/forecasting-with-multi-steps/code/examples-M3/quarterly-ets-forecasts/'
      image.path=paste(image.path,image.index,sep = '')
      svg(image.path,width=15,height=5)
      grid.arrange(p2, p1, nrow = 1)
      dev.off()
    }
  }else if(ts.type=='M'){
    for (i in 1:length(intermediate.results)) {
      p1=plot.forecasts.of.each.new.series(i,'M',intermediate.results)
      p2=plot.train.and.test.of.one.ts(i,'M',intermediate.results)
      #add lable
      image.index=paste(i,'ts.svg',sep = '')
      image.path=saving.path
      #image.path='/Users/xushengxiang/Desktop/lixixi/forecasting-with-multi-steps/code/examples-M3/quarterly-ets-forecasts/'
      image.path=paste(image.path,image.index,sep = '')
      svg(image.path,width=12,height=10)
      grid.arrange(p2, p1, nrow = 2)
      dev.off()
    }
  }
}


#plot for batch ts
plot.ets.components.for.batch.ts<-function(ts.type,intermediate.results,dataset,saving.path){
  if(ts.type=='Q'){
    for (i in 1:length(intermediate.results)) {
      #add lable
      image.index=paste(i,'ts.svg',sep = '')
      image.path=saving.path
      #image.path='/Users/xushengxiang/Desktop/lixixi/forecasting-with-multi-steps/code/examples-M3/quarterly-ets-forecasts/'
      image.path=paste(image.path,image.index,sep = '')
      svg(image.path,width=15,height=5)
      plot.ets.components.for.one.ts(i,ts.type,intermediate.results)
      dev.off()
    }
  }else if(ts.type=='M'){
    for (i in 1:length(intermediate.results)) {
      #add lable
      image.index=paste(i,'ts.svg',sep = '')
      image.path=saving.path
      #image.path='/Users/xushengxiang/Desktop/lixixi/forecasting-with-multi-steps/code/examples-M3/quarterly-ets-forecasts/'
      image.path=paste(image.path,image.index,sep = '')
      svg(image.path,width=22,height=5)
      plot.ets.components.for.one.ts(i,ts.type,intermediate.results)
      dev.off()
    }
  }
}
#plot for batch ts
plot.arima.components.for.batch.ts<-function(ts.type,intermediate.results,dataset,saving.path){
  if(ts.type=='Q'){
    for (i in 1:length(intermediate.results)) {
      #add lable
      image.index=paste(i,'ts.svg',sep = '')
      image.path=saving.path
      #image.path='/Users/xushengxiang/Desktop/lixixi/forecasting-with-multi-steps/code/examples-M3/quarterly-ets-forecasts/'
      image.path=paste(image.path,image.index,sep = '')
      svg(image.path,width=15,height=5)
      plot.arima.components.for.one.ts(i,ts.type,intermediate.results)
      dev.off()
    }
  }
}
#test
library(RColorBrewer)
library(Mcomp)

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
#plot anlysis images for the result
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# saving.path='intermediate.results/M3/M_ets/'
# save.path=paste(saving.path,'intermediate.results.rda',sep = '')
# load(file = save.path)





#test code
# ts.type='M'
# dataset=M3
# saving.path='/Users/xushengxiang/Desktop/lixixi/forecasting-with-multi-steps/code/examples-M3/monthly-ets-forecasts/'
# plot.forecasts.for.batch.ts(ts.type,intermediate.results,saving.path)
# saving.path.components='/Users/xushengxiang/Desktop/lixixi/forecasting-with-multi-steps/code/examples-M3/monthly-ets-components/'
# plot.ets.components.for.batch.ts(ts.type,intermediate.results,dataset,saving.path.components)

# ###test
# ts.index=2
# ts.type='Q'
# plot.forecasts.of.each.new.series(ts.index,ts.type,intermediate.results)
# plot.ets.components.for.one.ts(ts.index,ts.type,intermediate.results)

