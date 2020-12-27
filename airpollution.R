pollutantmean <- function(directory, pollutant, id = 1:332) {
  setwd(paste("C:/Users/paduz/Documents/R/airpollutionUSA",directory,sep = "/"))
  files <- dir()
  list <- list.files()
  df <- do.call(rbind,lapply(list[id],read.csv))
  dfna <- df[complete.cases(df[pollutant]),]
  mean(dfna[,pollutant])
  #dfna[pollutant]
}


complete <- function(directory, id = 1:332){
  setwd(paste("C:/Users/paduz/Documents/R/airpollutionUSA",directory,sep = "/"))
  files <- dir()
  list <- list.files()
  df <- data.frame()
  for(i in id){
    monitor <- read.csv(list[i])
    complete <- monitor[complete.cases(monitor[,2:3]),]
    nobs<- (nrow(complete))*(ncol(complete)-3)
    df<- rbind(df,data.frame(list[i],nobs))
  }
  df
}



corr <- function(directory, threshold =0){
  setwd(paste("C:/Users/paduz/Documents/R/airpollutionUSA",directory,sep = "/"))
  list <- list.files()
  correlation <- c()
  for(i in list){
    monitor <- read.csv(i)
    complete <- monitor[complete.cases(monitor[,2:3]),]
    nobs<- (nrow(complete)-2)*(ncol(complete)-3)
    if(nobs >=threshold){
      correlation <- rbind(correlation, cor(complete[,2],complete[,3]))
    }
  }
  correlation
}


