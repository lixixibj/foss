
#' project char to number for ets method
#' @param components model components
#' @return char
#' @export
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
#' plot ets components for one ts
#' @param ts.index time series index
#' @param ts.type 'M'or'Q'
#' @param intermediate.results intermediate.results
#' @importFrom RColorBrewer brewer.pal
#' @return char
#' @export
plot_ets_components_for_one_ts<-function(ts.index,ts.type,intermediate.results){
  
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
    image(1:16, 1:3,matrix(t(components.array),ncol=3), axes=FALSE, col=RColorBrewer::brewer.pal(8,"PuBu")[2:6], #rev(heat.colors(5)),
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
      image(1:144, 1:3,matrix(t(components.array),ncol=3), axes=FALSE, col=RColorBrewer::brewer.pal(8,"PuBu")[2:6], #rev(heat.colors(5)),
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


#' plot ets components updated version
#' @param ts.type 'M'or'Q'
#' @param forecast.results intermediate.results
#' @return plot an figure
#' @importFrom RColorBrewer brewer.pal
#' @export
plot_ets_components<-function(ts.type,forecast.results){
  
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
    image(1:16, 1:3,matrix(t(components.array),ncol=3), axes=FALSE, col=RColorBrewer::brewer.pal(8,"PuBu")[2:6], #rev(heat.colors(5)),
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
      image(1:144, 1:3,matrix(t(components.array),ncol=3), axes=FALSE, col=RColorBrewer::brewer.pal(8,"PuBu")[2:6], #rev(heat.colors(5)),
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

#' plot arima components for one ts
#' @param ts.index time series index
#' @param ts.type 'M'or'Q'
#' @param intermediate.results intermediate.results
#' @return plot an figure
#' @importFrom forecast arimaorder
#' @importFrom RColorBrewer brewer.pal
#' @export
plot_arima_components_for_one_ts<-function(ts.index,ts.type,intermediate.results){
  
  if(ts.type=='Q'){
    all.models=intermediate.results[[ts.index]]$all.models
    #components container
    components.array<-array(0,c(3,16))
    components.char<-array(0,c(3,16))
    for (i in 1:13) {
      #get ets model type
      model=all.models[[i]]
      #est.components=model$components
      arima.components=forecast::arimaorder(model)
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
    image(1:16, 1:3,matrix(t(components.array),ncol=3), axes=FALSE, col=RColorBrewer::brewer.pal(8,"PuBu")[2:6], #rev(heat.colors(5)),
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
    image(1:144, 1:3,matrix(t(components.array),ncol=3), axes=FALSE, col=RColorBrewer::brewer.pal(8,"PuBu")[2:6], #rev(heat.colors(5)),
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

#' plot arima components updated version
#' @param ts.type 'M'or'Q'
#' @param forecast.results intermediate.results
#' @return plot an figure
#' @importFrom forecast arimaorder
#' @importFrom RColorBrewer brewer.pal
#' @export
plot_arima_components<-function(ts.type,forecast.results){
  
  if(ts.type=='Q'){
    all.models=forecast.results$all.models
    #components container
    components.array<-array(0,c(3,16))
    components.char<-array(0,c(3,16))
    for (i in 1:13) {
      #get ets model type
      model=all.models[[i]]
      #est.components=model$components
      arima.components=forecast::arimaorder(model)
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
    image(1:16, 1:3,matrix(t(components.array),ncol=3), axes=FALSE, col=RColorBrewer::brewer.pal(8,"PuBu")[2:6], #rev(heat.colors(5)),
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
      arima.components=forecast::arimaorder(model)
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
    image(1:144, 1:3,matrix(t(components.array),ncol=3), axes=FALSE, col=RColorBrewer::brewer.pal(8,"PuBu")[2:6], #rev(heat.colors(5)),
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
#' plot model components
#'
#' @param ts.type time series type 'M' or 'Q'
#' @param forecast.results forecasting result list
#' @param method.name 'ets' or 'arima'
#' @return A matrix of the infile
#' @export
plot_model_components<-function(ts.type,forecast.results,method.name){
  #reset your graphics devices
  par(mfrow=c(1,1))
  if(method.name=='arima'){
    plot_arima_components(ts.type,forecast.results)
  }else{
    plot_ets_components(ts.type,forecast.results)
  }
}



#' plot forecasts of new mutiple series
#'
#' @param ts.object time series,type: list
#' @param ts.type time series type 'M' or 'Q'
#' @param forecasts.result list
#' @return plot an figure
#' @importFrom ggplot2 ggplot aes geom_point geom_line ggtitle xlab ylab scale_color_discrete theme element_text scale_x_discrete
#' @importFrom zoo as.yearqtr
#' @importFrom reshape2 melt
#' @importFrom directlabels geom_dl dl.combine
#' @export
plot_forecasts_of_new_mutiple_series<-function(ts.object,ts.type,forecasts.result){
  if(ts.type=='Q'){
    # library(zoo)
    # library(MASS)
    # library(reshape2)
    # library(reshape)
    seasons.name=c('Q1','Q2','Q3','Q4',
                   'Q1,2','Q2,3','Q3,4','Q4,1',
                   'Q1,2,3','Q2,3,4','Q3,4,1','Q4,1,2',
                   'Q1,2,3,4')
    original.point.forecasts=forecasts.result$original.point.forecasts
    original.point.forecasts=head(original.point.forecasts,-3)
    original.point.forecasts.df=data.frame(original.point.forecasts)
    names(original.point.forecasts.df)=zoo::as.yearqtr(time(ts.object$xx))
    #original.point.forecasts.df$season=LETTERS[seq( from = 1, to = 16 )]
    original.point.forecasts.df$Seasons=seasons.name
    df <- reshape2::melt(original.point.forecasts.df, id = c('Seasons'))
    # print(df)
    # colnames(df)
    #df$variable <- as.Date(df$variable, format = "%d-%b-%y")
    #df remove na
    df=df[complete.cases(df), ]
    # ggplot2::ggplot(df, ggplot2::aes(x =variable , y = value, group = Seasons,
    #                colour = Seasons)) + ggplot2::geom_point() + ggplot2::geom_line()+
    #   ggplot2::ggtitle("Forecast of each sub-seasonal time series") +
    #   ggplot2::xlab("Time") + ggplot2::ylab("")+
    #   ggplot2::scale_color_discrete(breaks=seasons.name)+
    #   ggplot2::theme(text = ggplot2::element_text(size=15))
    ggplot2::ggplot(df, ggplot2::aes(x =variable , y = value, group = Seasons,colour = Seasons)) + 
      ggplot2::geom_point() + 
      ggplot2::geom_line()+
      ggplot2::ggtitle("Forecast of each sub-seasonal time series") +
      ggplot2::xlab("Time") + 
      ggplot2::ylab("")+
      ggplot2::scale_color_discrete(guide='none')+
      ggplot2::scale_x_discrete(expand=c(0, 1)) +
      directlabels::geom_dl(aes(label = Seasons), method = list(directlabels::dl.combine("first.points", "last.points"), cex = 0.8))+
      ggplot2::theme(text = ggplot2::element_text(size=15))
    
  }else if(ts.type=='M'){
    # library(zoo)
    # library(MASS)
    # library(reshape2)
    # library(reshape)
    #seasons.name=LETTERS[seq( from = 1, to = 144 )]
    seasons.name=c()
    for (i in 1:133) {
      seasons.name=c(seasons.name,toString(i))
    }
    original.point.forecasts=forecasts.result$original.point.forecasts
    original.point.forecasts=head(original.point.forecasts,-11)
    original.point.forecasts.df=data.frame(original.point.forecasts)
    names(original.point.forecasts.df)=zoo::as.yearmon(time(ts.object$xx))
    #original.point.forecasts.df$season=LETTERS[seq( from = 1, to = 16 )]
    original.point.forecasts.df$Seasons=seasons.name
    df <- reshape2::melt(original.point.forecasts.df, id = c('Seasons'))
    # print(df)
    # colnames(df)
    #df$variable <- as.Date(df$variable, format = "%d-%b-%y")
    #df remove na
    df=df[complete.cases(df), ]
    # ggplot2::ggplot(df, ggplot2::aes(x =variable , y = value, group = season,
    #                colour = season)) + ggplot2::geom_point() + ggplot2::geom_line()+
    #   ggplot2::ggtitle("Forecast of each sub-seasonal time series") +
    #   ggplot2::xlab("Time") + ggplot2::ylab("")+
    #   ggplot2::scale_color_discrete(breaks=seasons.name)+
    #   ggplot2::theme(text = ggplot2::element_text(size=15))
    ggplot2::ggplot(df, ggplot2::aes(x =variable , y = value, group = Seasons,colour = Seasons)) + 
      ggplot2::geom_point() + 
      ggplot2::geom_line()+
      ggplot2::ggtitle("Forecast of each sub-seasonal time series") +
      ggplot2::xlab("Time") + 
      ggplot2::ylab("")+
      ggplot2::scale_color_discrete(guide='none')+
      ggplot2::scale_x_discrete(expand=c(0, 1)) +
      directlabels::geom_dl(aes(label = Seasons), method = list(directlabels::dl.combine("first.points", "last.points"), cex = 0.8))+
      ggplot2::theme(text = ggplot2::element_text(size=15))
    
  }
  
}


#' plot forecasts for batch.ts
#'
#' @param ts.type 'M' or 'Q'
#' @param intermediate.results intermediate.results
#' @param saving.path path for saving figures
#' @return plot an figure
#' @export
plot_forecasts_for_batch_ts<-function(ts.type,intermediate.results,saving.path){
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


#' plot ets components for batch ts
#'
#' @param ts.type 'M' or 'Q'
#' @param intermediate.results intermediate.results
#' @param dataset M3, M1, or M4
#' @param saving.path path for saving figures
#' @return plot an figure
#' @export
plot_ets_components_for_batch_ts<-function(ts.type,intermediate.results,dataset,saving.path){
  if(ts.type=='Q'){
    for (i in 1:length(intermediate.results)) {
      #add lable
      image.index=paste(i,'ts.svg',sep = '')
      image.path=saving.path
      #image.path='/Users/xushengxiang/Desktop/lixixi/forecasting-with-multi-steps/code/examples-M3/quarterly-ets-forecasts/'
      image.path=paste(image.path,image.index,sep = '')
      svg(image.path,width=15,height=5)
      plot_ets_components.for.one.ts(i,ts.type,intermediate.results)
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
      plot_ets_components.for.one.ts(i,ts.type,intermediate.results)
      dev.off()
    }
  }
}
#' plot arima components for batch ts
#'
#' @param ts.type 'M' or 'Q'
#' @param intermediate.results intermediate.results
#' @param dataset M3, M1, or M4
#' @param saving.path path for saving figures
#' @return plot an figure
#' @export
plot_arima_components_for_batch_ts<-function(ts.type,intermediate.results,dataset,saving.path){
  if(ts.type=='Q'){
    for (i in 1:length(intermediate.results)) {
      #add lable
      image.index=paste(i,'ts.svg',sep = '')
      image.path=saving.path
      #image.path='/Users/xushengxiang/Desktop/lixixi/forecasting-with-multi-steps/code/examples-M3/quarterly-ets-forecasts/'
      image.path=paste(image.path,image.index,sep = '')
      svg(image.path,width=15,height=5)
      plot_arima_components_for_one_ts(i,ts.type,intermediate.results)
      dev.off()
    }
  }
}

#' plot time series and its forecast
#'
#' @param training.value ts object
#' @param testing.value ts object
#' @param forecast.results list
#' @return plot an figure
#' @importFrom ggplot2 autolayer autoplot
#' @export
plot_time_series_and_its_forecast<-function(training.value,testing.value,forecast.results){
  point.forecasts=testing.value
  for (i in 1:length(point.forecasts)) {
    point.forecasts[i]=forecast.results$point.forecasts[i]
  }
  ggplot2::autoplot(training.value,series = 'Training data',xlab = "Time", ylab = "",main='Time series and forecast')+
    ggplot2::autolayer(testing.value,series = 'Testing data')+
    ggplot2::autolayer(point.forecasts,series = 'Multiple forecast')
}
