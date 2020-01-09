AwarenessGraph = function(data){
  ##Function that will graph the change of the brand attraction over time
  
  #libraries
  library(ggplot2)
  library(dplyr)
  
  #vector of unique dates
  vUniqueDates = as.vector(unique(substr(data$id_date, 1, 10)))
  #how many unique dates (int)
  iNoUniqueDates = length(unique(substr(data$id_date, 1, 10)))

  iCount <- vector()
  for (i in 1:iNoUniqueDates){
    #count number of times each specific date appears
    iCount[i] = sum(substr(data$id_date, 1, 10)==vUniqueDates[i]) 
  }
  
  #take only the dates, not the time
  data$id_date <- substr(data$id_date, 1, 10)
  
  
  #sum 'digital Audio' by date
  dSumOfDigAudio <- aggregate(data$mc_digitalaudio_all, by=list(Date=data$id_date), FUN=sum)
  dSumOfDigAudio = dSumOfDigAudio[2]
  
  #weight the sum over number of obs's in group (date)
  dWeightedSumDigAudio= dSumOfDigAudio/iCount
  #create df with the 
  dfDataDigAud = data.frame(vUniqueDates, dWeightedSumDigAudio)
  dfDataDigAud$vUniqueDates = as.Date(dfDataDigAud$vUniqueDates)
  
  #plotting the mc_digital_audio_all versus time
  plot(dfDataDigAud, main = 'Weighted sum of mc_digital_audio_al versus time', 
       xlab = 'Time', ylab = 'Weighted sum' )  #plot shows 3 outliers which makes the rest of the plot unreadable
  
  #remove outliers of the weighted sum of the mc_digital_audio_all rows and plot again
  dfDataDigAud <- dfDataDigAud[dfDataDigAud$x<200,]
  plot(dfDataDigAud, main = 'Weighted sum of mc_digital_audio_al versus time', 
       xlab = 'Time', ylab = 'Weighted sum' ) 
}