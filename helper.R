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
    rename(response = question) %>%
    filter(response > 0)
  
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

# Clean question data for plotting

clean_choose_all <- function(data)
{
  q_data %>%
    group_by(chosen) %>%
    summarize(count = n()) %>%
    mutate(percent_responded = round(100*(count / q_n), 2)) %>%
    arrange(desc(count))
}

# Plot the clean data
plot_choose_all <- function(data)
{
  r <- 
    ggplot(data, aes(x = reorder(chosen, count), y = percent_responded)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    geom_text(aes(label= paste(round(percent_responded, 0), count, sep = "%:"))) +
    xlab("Response") +
    ylab("% Responded, Count")
}


clean_single_select <- function(data)
{
q_plot <-
  data %>%
  group_by(response) %>%
  summarize(count = n()) %>%
  mutate(percent_responded = round(100*(count / q_n), 2)) %>%
  arrange(desc(count))

}

plot_single_select <- function(data)
{
  r <- 
    ggplot(data, aes(x = reorder(response, count), y = percent_responded)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    geom_text(aes(label= paste(round(percent_responded, 0), count, sep = "%:"))) +
    xlab("Response") +
    ylab("% Responded, Count")
  r
}

# Get all responses for single select question
get_single_selection_2 <- function(data, q.name)
{
  responses <- 
    data %>%
    select(id = "id", question = q.name) %>%
    slice(2:nrow(.)) %>%
    rename(response = question) %>%
    filter(response > 0)
  
  return(responses)
}


clean_single_select_2 <- function(data)
{
  q_plot <-
    data %>%
    group_by(response) %>%
    summarize(count = n()) %>%
    mutate(percent_responded = round(100*(count / sum(count)), 2)) %>%
    arrange(desc(count))
  
}


