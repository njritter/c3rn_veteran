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
    gather(key = response, value = checked, 2:ncol(.)) %>%
    filter(checked >= 1) %>%
    select(-checked)
  
  return(tidy_data)
  }

# Clean question data for plotting
clean_choose_all <- function(data, q_n)
{
  data %>%
    group_by(response) %>%
    summarize(count = n()) %>%
    mutate(percent_responded = round(100*(count / q_n), 2)) %>%
    arrange(desc(count))
}

# Plot the choose all clean data
plot_choose_all <- function(data, t, q_n)
{
  r <- 
    ggplot(data, aes(x = reorder(response, count), y = percent_responded)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    geom_text(aes(label= paste(round(percent_responded, 0), count, sep = "%:"))) +
    xlab("Response") +
    ylab("% Responded, Count") +
    labs(title = t, subtitle = q_n)
  
}


# Plot single select data for unfiltered data
plot_single_select <- function(data, t, q_n)
{
  r <- 
    ggplot(data, aes(x = reorder(response, count), y = percent_responded)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    geom_text(aes(label= paste(round(percent_responded, 0), count, sep = "%:"))) +
    xlab("Response") +
    ylab("% Responded, Count") +
    labs(title = t, subtitle = q_n)
  r
}

# Plot single select data for unfiltered data
plot_single_select_0 <- function(data, t, q_n, q_mean)
{
  r <- 
    ggplot(data, aes(x = reorder(response, count), y = percent_responded)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    geom_text(aes(label= paste(round(percent_responded, 0), count, sep = "%:"))) +
    xlab("Response") +
    ylab("% Responded, Count") +
    labs(title = t, subtitle = paste(q_n, q_mean, sep = " Average = "))
  r
}

# Clean single select data for filtered plotting
clean_single_select_f <- function(data)
{
  q_plot <-
    data %>%
    group_by(response) %>%
    summarize(count = n()) %>%
    mutate(percent_responded = round(100*(count / qf_n), 2)) %>%
    arrange(desc(count))
  
}

# Plot single select data for filtered data
plot_single_select_f <- function(data, n)
{
  r <- 
    ggplot(data, aes(x = reorder(response, count), y = percent_responded)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    geom_text(aes(label= paste(round(percent_responded, 0), count, sep = "%:"))) +
    xlab("Response") +
    ylab("% Responded, Count") +
    labs(title = t, subtitle = n)
  r
}

# Get all responses for single select questions that are sub questions of a larger question
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

# Clean responses for single select questions that are sub questions of a larger question
clean_single_select <- function(data)
{
  q_plot <-
    data %>%
    group_by(response) %>%
    summarize(count = n()) %>%
    mutate(percent_responded = round(100*(count / sum(count)), 2)) %>%
    arrange(desc(count))
  
}

# Get all responses for single select question with potential 0s as responses
get_single_selection_0 <- function(data, q.name)
{
  responses <- 
    data %>%
    select(id = Respondent.ID, question = q.name) %>%
    slice(2:nrow(.)) %>%
    rename(response = question)
  
  return(responses)
}

# Get all responses for single select questions that are sub questions of a larger question
get_single_selection_3 <- function(data, q.name)
{
  responses <- 
    data %>%
    select(id = "id", question = q.name) %>%
    rename(response = question)
  
  return(responses)
}
