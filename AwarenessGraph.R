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
  
  
  #Checking for Media Consumption trends
  ##'Digital Audio'###########################################################
  #sum 'Digital Audio' by date
  dSumOfDigAudio <- aggregate(data$mc_digitalaudio_all, by=list(Date=data$id_date), FUN=sum)
  dSumOfDigAudio = dSumOfDigAudio[2]
  
  #weight the sum over number of obs's in group (date)
  dWeightedSumDigAudio= dSumOfDigAudio/iCount
  #create df with the specific dates and weighted sums
  dfDataDigAud = data.frame(vUniqueDates, dWeightedSumDigAudio)
  dfDataDigAud$vUniqueDates = as.Date(dfDataDigAud$vUniqueDates)
  
  #plotting the mc_digital_audio_all versus time
  plot(dfDataDigAud, main = 'Weighted sum of mc_digital_audio_all versus time', 
       xlab = 'Survey (2019)', ylab = 'Weighted sum' )  #plot shows 3 outliers which makes the rest of the plot unreadable
  
  #remove outliers of the weighted sum of the mc_digital_audio_all rows and plot again
  dfDataDigAud <- dfDataDigAud[dfDataDigAud$x<200,]
  plot(dfDataDigAud, main = 'Weighted sum of mc_digitalaudio_all versus time', 
       xlab = 'Survey (2019)', ylab = 'Weighted sum' ) 
  
  ##'Online Display'###########################################################
  #sum 'Online Display' by date
  dSumOfOnlDisp <- aggregate(data$mc_onlinedisplay_all, by=list(Date=data$id_date), FUN=sum)
  dSumOfOnlDisp = dSumOfOnlDisp[2]
  
  #weight the sum over number of obs's in group (date)
  dWeightedSumOnlDisp = dSumOfOnlDisp/iCount
  #create df with the specific dates and weighted sums
  dfDataOnlDisp = data.frame(vUniqueDates, dWeightedSumOnlDisp)
  dfDataOnlDisp$vUniqueDates = as.Date(dfDataOnlDisp$vUniqueDates)
  
  #plotting the mc_digital_audio_all versus time
  plot(dfDataOnlDisp, main = 'Weighted sum of mc_onlinedisplay_all versus time', 
       xlab = 'Survey (2019)', ylab = 'Weighted sum' )  
  #plot shows 3 outliers which makes the rest of the plot unreadable
  
  #remove outliers of the weighted sum of the mc_onlinedisplay_all rows and plot again
  dfDataOnlDisp <- dfDataOnlDisp[dfDataOnlDisp$x<200,]
  plot(dfDataOnlDisp, main = 'Weighted sum of mc_onlinedisplay_all versus time', 
       xlab = 'Survey (2019)', ylab = 'Weighted sum') 
  
  ##'TV NBC'###########################################################
  #This variable is chosen because it has least zero-values
  #therefore might have more information
  #sum 'Online Display' by date
  dSumOfTvNBC <- aggregate(data$mc_tv_nbc, by=list(Date=data$id_date), FUN=sum)
  dSumOfTvNBC  = dSumOfTvNBC [2]
  
  #weight the sum over number of obs's in group (date)
  dWeightedSumTvNBC  = dSumOfTvNBC /iCount
  #create df with the specific dates and weighted sums
  dfDataTvNBC  = data.frame(vUniqueDates, dWeightedSumTvNBC)
  dfDataTvNBC$vUniqueDates = as.Date(dfDataTvNBC$vUniqueDates)
  
  #plotting the mc_tv_nbc versus time
  plot(dfDataTvNBC, main = 'Weighted sum of mc_tv_nbc versus time', 
       xlab = 'Survey (2019)', ylab = 'Weighted sum' )  
  #plot shows 2 outliers which makes the rest of the plot unreadable
  
  #remove outliers of the weighted sum of the mc_onlinedisplay_all rows and plot again
  dfDataTvNBC <- dfDataTvNBC[dfDataTvNBC$x<10,]
  plot(dfDataTvNBC, main = 'Weighted sum of mc_tv_nbc versus time', 
       xlab = 'Survey (2019)', ylab = 'Weighted sum') 
  
  
  
  
  
  #Checking for KPI trends
  
}