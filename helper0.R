# Library to help analyze Veteran Survey

library(tidyverse)

############################################## Single Selection Functions #####


# Get all responses for single select question
get_single_selection <- function(data, q.name)
  {
  responses <- 
    data %>%
    select(id = Respondent.ID, question = q.name) %>%
    slice(2:nrow(.)) %>%
    rename(response = question)
  
  return(responses)
  }



############################################## Select All Functions ###########

# Get all responses for choose all question
get_choose_all <- function(data, q.name, f.response)
  {
  responses <- 
    data %>%
    select(
      "Respondent.ID",
      q.name:f.response)
  
  colnames(responses) <- responses[1, ]
  responses <- responses[-1, ]
  responses <- responses %>% rename(id = 1)
  
  return(responses)
  }


# Turn output of get_choose_all into a 'tidy' data set for further processing
tidy_choose_all <- function(data)
  {
  tidy_data <- 
    data %>%
    gather(key = chosen, value = checked, 2:ncol(.)) %>%
    filter(checked >= 1) %>%
    select(-checked)
  
  return(tidy_data)
  }


