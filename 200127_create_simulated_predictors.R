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
                           "v_tvsum", "v_vodsum", "v_yousum" )
  
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
                                target1_gender_m=TRUE,
                                target1_min_age=25,
                                target1_max_age=34,
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
  
  if (target_audience==TRUE) {population = draw_new_samples(target_contacts,
                                                            population_size,
                                                            replacement)}
  else {population = draw_new_samples(nontarget_contacts,
                                      population_size,
                                      replacement)}
  return(population)
}
path = "~/Documents/Econometrie/Masters/Seminar Nielsen"
path = "D:/brian/Documents/EUR/19-20 Business Analytics and QM/Block 3/Seminar Case Studies/Data"
simulated_population = simulate_population(path, 50000)
rm(list=setdiff(ls(), "simulated_population"))