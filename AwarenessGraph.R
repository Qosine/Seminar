AwarenessGraph = function(data){
  ##Function that will graph the change of the brand attraction over time
  
  #libraries
  library(ggplot2)
  library(dplyr)
  library(data.table)
  
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
  
  #Checking for KPI trends
  ##'KPI Familiarity'#################################################
  #sum 'kpi_familiarity' by date
  dSumOfKpiFam <- aggregate(data$kpi_familiarity, by=list(Date=data$id_date), FUN=sum)
  dSumOfKpiFam = dSumOfKpiFam[2]
  
  #weight the sum over number of obs's in group (date)
  dWeightedSumKpiFam  = dSumOfKpiFam /iCount
  #*100 to get percentage because kpi_familiarity is binary
  dPercentageKpiFam = dWeightedSumKpiFam * 100
  
  #create df with the specific dates and percentages
  dfDataKpiFam  = data.frame(vUniqueDates, dPercentageKpiFam)
  dfDataKpiFam$vUniqueDates = as.Date(dfDataKpiFam$vUniqueDates)
  
  #plotting the mc_tv_nbc versus time
  plot(dfDataKpiFam, main = 'Percentage of KPI_familiarity versus time', 
       xlab = 'Survey (2019)', ylab = 'Percentage' )  
  #remove outliers of the weighted sum of the mc_onlinedisplay_all rows and plot again
  # dfDataKpiFam <- dfDataKpiFam[dfDataKpiFam$x<100,]
  # plot(dfDataKpiFam, main = 'Percentage of KPI_familiarity of versus time',
  #      xlab = 'Survey (2019)', ylab = 'Percentage')
  
  
  ##'KPI Awareness'#################################################
  #sum 'kpi_awareness' by date
  dSumOfKpiAwa <- aggregate(data$kpi_awareness, by=list(Date=data$id_date), FUN=sum)
  dSumOfKpiAwa = dSumOfKpiAwa[2]
  
  #weight the sum over number of obs's in group (date)
  dWeightedSumKpiAwa  = dSumOfKpiAwa /iCount
  #*100 to get percentage because kpi's are binary
  dPercentageKpiAwa = dWeightedSumKpiAwa * 100
  
  #create df with the specific dates and percentages
  dfDataKpiAwa = data.frame(vUniqueDates, dPercentageKpiAwa)
  dfDataKpiAwa$vUniqueDates = as.Date(dfDataKpiAwa$vUniqueDates)
  
  #plotting the mc_tv_nbc versus time
  plot(dfDataKpiAwa, main = 'Percentage of KPI_awareness versus time', 
       xlab = 'Survey (2019)', ylab = 'Percentage' )  
  
  #(Downside outliers??)
  
  ##'KPI Consideration'#############################################
  #sum 'kpi_awareness' by date
  dSumOfKpiCon <- aggregate(data$kpi_consideration, by=list(Date=data$id_date), FUN=sum)
  dSumOfKpiCon = dSumOfKpiCon[2]
  
  #weight the sum over number of obs's in group (date)
  dWeightedSumKpiCon  = dSumOfKpiCon /iCount
  #*100 to get percentage because kpi's are binary
  dPercentageKpiCon = dWeightedSumKpiCon * 100
  
  #create df with the specific dates and percentages
  dfDataKpiCon = data.frame(vUniqueDates, dPercentageKpiCon)
  dfDataKpiCon$vUniqueDates = as.Date(dfDataKpiCon$vUniqueDates)
  
  #plotting the mc_tv_nbc versus time
  plot(dfDataKpiCon, main = 'Percentage of KPI_consideration versus time', 
       xlab = 'Survey (2019)', ylab = 'Percentage' ) 
  
  #remove outliers of the weighted sum of the mc_onlinedisplay_all rows and plot again
  dfDataKpiCon <- dfDataKpiCon[dfDataKpiCon$x<100,]
  plot(dfDataKpiCon, main = 'Percentage of KPI_consideration of versus time',
       xlab = 'Survey (2019)', ylab = 'Percentage')
  
  #(Down and upside outliers??)
  

  
}