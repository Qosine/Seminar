ContactsTrendCheck <- function(data){
  #Function that will run regressions for the columns of contacts_

  library(lubridate)
  
  data <- read.csv(file.choose())
  
  data$id_date = yday(substr(data$id_date,1,10))
  #keep only the contacts data for convenience
  data <- data[,substr(colnames(data),1,8)=="contacts" | colnames(data)=="id_date"|substr(colnames(data),1,3)=="kpi" ]

  
  #run regression per column to see if variables have a significant trend over time
  # y = ax + b
  fit <- list()
  for (i in 2:length(data)){
    fit[i-1] <- lm(data[,i] ~ data[,1], data=data)
    print(fit[i-1])
    # a = fit[i]$coefficients[]
  }  
  
  #superbowl
  fit <- lm(data[,65] ~ data[,1], data=data)
  summary(fit)  
  plot(fit)
  
  #slope van dependent , kpi. vergelijken
  fit2 <- lm(data[,3] ~ data[,1], data=data)
  summary(fit2)
  
  }