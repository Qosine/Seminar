########################################################################################
# Author      : Pointlogic Team 4, Case Studies in BA&QM
# Description : From an original sample, create a larger simulated population
#               by permuting columns (of the original data) independently,
#               and then sampling from the permuted columns (with replacement). 
# Usage       : Create functions, subsequently call simulate_population()
#               Function arguments
#               1) path             : Path of working directory
#               2) population_size  : Desired population size
#               3) seed             : Choice of seed (default 200127)
#       !!!        4) target_audience  : Whether to simulate target or nontarget
#                                     population (boolean, default TRUE)
#               5) target_gender_m  : Whether to set gender of target audience to male
#                                     (default TRUE)
#               6) min_age          : Lower age bound of the target audience, inclusive
#                                     (default 25)
#               7) max_age          : Upper age bound of the target audience, inclusive
#                                     (default 34)
#               8) replacement      : Whether to sample with replacement
#                                     (default TRUE)
#
########################################################################################

library(dplyr)

read_source_data <- function(path) {
  setwd(path)
  data <- read.csv("./cleaned_unified_sample.csv")
  return(data)
}

split_sample <- function(data,
                         li_target1,
                         li_target2) {
  
  # Target group 1
  if (li_target1$male==TRUE) {target_gender="male"}
  else {target_gender="female"}

  target1_sample = data[ ( data["sd_gender"]==target_gender
                           & data["sd_age"]>=li_target1$min_age
                           & data["sd_age"]<=li_target1$max_age ), ] 
  
  # Target group 2
  if ( !is.null(li_target2) ) {
    if (li_target2$male==TRUE) {target_gender="male"} else {target_gender="female"}
    target2_sample = data[ ( data["sd_gender"]==target_gender
                            & data["sd_age"]>=li_target2$min_age
                            & data["sd_age"]<=li_target2$max_age ), ]
  } else {target2_sample = NULL}
  
  nontarget_sample = setdiff(data, target1_sample, target2_sample) ##!!
  
  out = list(); 
  out$target1 = target1_sample; out$target2 = target2_sample
  out$nontarget = nontarget_sample
  
  return (out)
}

sum_contact_vars <- function(subsample) {
  
  # Extract columns of interest
  df_contacts = subsample[,93:182]
  
  # Sum across categories
  v_audiosum = rowSums(df_contacts[,1:5])
  v_digitalsum = rowSums(df_contacts[,6:12])
  v_programsum = df_contacts[,13]
  v_tvsum = rowSums(df_contacts[,14:81])
  v_vodsum = rowSums(df_contacts[,82:89])
  v_yousum = df_contacts[,90]
  
  # Return single dataframe
  m_contact_vars <- cbind(v_audiosum, v_digitalsum, v_programsum, v_tvsum,
                          v_vodsum, v_yousum)
  
  return(m_contact_vars)
}

draw_new_samples <- function(original_data,
                             population_size,
                             user_replace_choice = FALSE) {
  
  population = matrix( 0, population_size, ncol(original_data) ) # Allocate memory
  colnames(population) = c("v_audiosum", "v_digitalsum", "v_programsum",
                           "v_tvsum", "v_vodsum", "v_yousum","v_male","v_age3544","v_age4554","v_age55plus","v_havechildren","v_etn_cauc","v_income030","v_income3050","v_income5075","v_income75100","v_income100150","v_educ2","v_educ3","v_married","v_single","v_seperated"  )
  
  # Permute columns independently
  for( i in 1:ncol(original_data) ) {
    population[,i] = sample(original_data[,i],
                    size = population_size,
                    replace = user_replace_choice)
    }
  return(population)
}

## MAIN SCRIPT ##

simulate_population <- function(path,
                                population_size,
                                seed = 200127,
                                target_audience=TRUE,
                                target1_gender_m,
                                target1_min_age,
                                target1_max_age,
                                target2_gender_m=NULL,
                                target2_min_age=NULL,
                                target2_max_age=NULL,
                                replacement=TRUE) {
  
  data = read_source_data(path)
  li_target1 = list(); li_target1$male = target1_gender_m
  li_target1$min_age = target1_min_age; li_target1$max_age = target1_max_age
  
  li_target2 = NULL
  if(!all( sapply(list(target2_gender_m,
                       target2_min_age,
                       target2_max_age), is.null ))) {
    li_target2 = list(); li_target2$male = target2_gender_m
    li_target2$min_age = target2_min_age; li_target2$max_age = target2_max_age
  }
  
  subsamples = split_sample(data, li_target1, li_target2)
  

  
  target_contacts = sum_contact_vars(subsamples$target)
  nontarget_contacts = sum_contact_vars(subsamples$nontarget)
  
  #control variables
  v_male <- (ifelse(data$sd_gender == "male", 1,0))
  v_havechildren <- (ifelse(data$sd_havechildren == "yes", 1,0))
  v_age3544 <- (ifelse(data$sd_age <=44 & data$sd_age >=35, 1,0))
  v_age4554 <- (ifelse(data$sd_age <=54 & data$sd_age >=45, 1,0))
  v_age55plus <- (ifelse(data$sd_age >=55, 1,0))
  v_employed <- (ifelse(data$sd_employment == "employed", 1,0))
  v_income030 <- (ifelse(data$sd_householdincome == "[0,30)", 1,0))
  v_income3050 <- (ifelse(data$sd_householdincome == "[30,50)", 1,0))
  v_income5075 <- (ifelse(data$sd_householdincome == "[50,75)", 1,0))
  v_income75100 <- (ifelse(data$sd_householdincome == "[75,100)", 1,0))
  v_income100150 <- (ifelse(data$sd_householdincome == "[100,150)", 1,0))
  v_educ2 <- (ifelse(data$sd_education == "secondary", 1,0))
  v_educ3 <- (ifelse(data$sd_education == "tertiary", 1,0))
  v_etn_cauc <- (ifelse(data$sd_ethnicity_caucasian == "yes", 1,0))
  v_etn_afric <- (ifelse(data$sd_ethnicity_africanamerican == "yes", 1,0))
  v_etn_hisp <- (ifelse(data$sd_ethnicity_hispanic == "yes", 1,0))
  v_etn_asian <- (ifelse(data$sd_ethnicity_asian == "yes", 1,0))
  v_etn_native <- (ifelse(data$sd_ethnicity_nativeamerican == "yes", 1,0))
  v_etn_other <- (ifelse(data$sd_ethnicity_other == "yes", 1,0))
  v_married <- (ifelse(data$sd_maritalstatus == "married", 1,0))
  v_single <- (ifelse(data$sd_maritalstatus == "single", 1,0))
  v_seperated <- (ifelse(data$sd_maritalstatus == "seperated", 1,0))
  total_demographics <- split_sample(cbind(data, v_male, 
                              v_age3544, v_age4554, v_age55plus, v_havechildren, v_etn_cauc,
                              v_income030, v_income3050,v_income5075, v_income75100,v_income100150, v_educ2, v_educ3,v_married, v_single,v_seperated),li_target1, li_target2)
  target_demographics = total_demographics$target[,(ncol(data)+1):(ncol(data) +ncol(cbind(v_male, 
                                                             v_age3544, v_age4554, v_age55plus, v_havechildren, v_etn_cauc,
                                                             v_income030, v_income3050,v_income5075, v_income75100,v_income100150, v_educ2, v_educ3,v_married, v_single,v_seperated)))]
  nontarget_demographics = total_demographics$nontarget[,(ncol(data)+1):(ncol(data) +ncol(cbind(v_male, 
                                                                                               v_age3544, v_age4554, v_age55plus, v_havechildren, v_etn_cauc,
                                                                                               v_income030, v_income3050,v_income5075, v_income75100,v_income100150, v_educ2, v_educ3,v_married, v_single,v_seperated)))]
   
  # target_demographics = cbind(ifelse(sumbsample$target[,3]=="male",1,0))
  # nontarget_demographics = cbind(ifelse(sumbsample$nontarget[,3]=="male",1,0))
  # colnames(target_demographics) = c("male", "25-34","35-44","45-54", "55-99" )
  # colnames(nontarget_demographics) = c("male", "25-34","35-44","45-54", "55-99" )
  #print(head(target_contacts))
  
  if (target_audience==TRUE) {population = draw_new_samples(cbind(target_contacts,target_demographics) ,
                                                            population_size,
                                                            replacement)}
  else {population = draw_new_samples(cbind(nontarget_contacts, nontarget_demographics),
                                      population_size,
                                      replacement)}
  return(population)
}

# Set path
path = "."
#path = "D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Git/Seminar"

simulated_nontarget_population = simulate_population(path,
                                                     5*10^4,
                                                     target_audience = FALSE,
                                                     target1_gender_m = TRUE,
                                                     target1_min_age = 25,
                                                     target1_max_age = 34)


# Create overview of demographic groups over which we can loop
demographic_groups = rbind(c("Male", 25, 34),
                           c("Male", 35, 44),
                           c("Male", 45, 54),
                           c("Male", 55, 99),
                           c("Female", 25, 34),
                           c("Female", 35, 44),
                           c("Female", 45, 54),
                           c("Female", 55, 99))

datafile_names = c("male_25_34", "male_35_44", "male_45_54", "male_55_99",
                   "female_25_34", "female_35_44", "female_45_54", "female_55_99")

no_obs_to_simulate = 15*10^3


for (i in 1:nrow(demographic_groups)) {
  print(demographic_groups[i,])
  if (demographic_groups[i, 1] == "Male") {male_dummy = TRUE} else {male_dummy = FALSE}
  simulated_population = simulate_population(path,
                                             no_obs_to_simulate,
                                             target_audience = TRUE,
                                             target1_gender_m = male_dummy,
                                             target1_min_age = demographic_groups[i, 2],
                                             target1_max_age = demographic_groups[i, 3])
  datafile_string = paste(datafile_names[i], "_2.Rds", sep="")
  saveRDS(simulated_population, file = datafile_string)
  print("Simulated successfully")
}
