library(tidyverse) # Necessary import
# Set local working directory
setwd("~/Desktop/epistemic_analytics/hui_QE_institute/datasets")
# Read in datasets
df_low <- read.csv("./commitment_observations_low.csv")
df_mid <- read.csv("./commitment_observations_mid.csv")
df_high <- read.csv("./commitment_observations_high.csv")

# Function to subset strings
str_subber <- function(vec) {
  str_sub(vec, start = 2, end = -2)
}

# Format datasets into basic representation
df_low <- df_low %>%
  filter(X != 500) %>%
  rename(students = X) %>%
  pivot_longer(!students, names_to = "week", values_to = "codes") %>%
  mutate(week = str_sub(week, start = 2)) %>%
  mutate(codes = str_sub(codes, start = 2, end = -2)) %>%
  mutate(codes = str_split(codes, pattern = ", ")) %>%
  mutate(codes = lapply(codes, str_subber))  %>% 
  mutate(commitment_level = "low") %>%
  relocate(commitment_level, .after = week)
df_mid <- df_mid %>%
  filter(X != 500) %>%
  rename(students = X) %>%
  pivot_longer(!students, names_to = "week", values_to = "codes") %>%
  mutate(week = str_sub(week, start = 2)) %>%
  mutate(codes = str_sub(codes, start = 2, end = -2)) %>%
  mutate(codes = str_split(codes, pattern = ", ")) %>%
  mutate(codes = lapply(codes, str_subber)) %>% 
  mutate(commitment_level = "mid") %>%
  relocate(commitment_level, .after = week)
df_high <- df_high %>%
  filter(X != 500) %>%
  rename(students = X) %>%
  pivot_longer(!students, names_to = "week", values_to = "codes") %>%
  mutate(week = str_sub(week, start = 2)) %>%
  mutate(codes = str_sub(codes, start = 2, end = -2)) %>%
  mutate(codes = str_split(codes, pattern = ", ")) %>%
  mutate(codes = lapply(codes, str_subber)) %>%
  mutate(commitment_level = "high") %>%
  relocate(commitment_level, .after = week)

# Combine the three daatasets together 
df <- rbind(df_low, df_mid, df_high)

# Take the codes stored as a vector in each row of column "codes" and split it into 12 separate codes, to bind back into
# original df
codes_temp <- pull(df, codes)

column_names <- str_extract(codes_temp[[1]], "^[^_]+")
values <- str_extract(codes_temp[[1]], "\\d$")

result_df <- data.frame(matrix(0, nrow = length(codes_temp), ncol = length(column_names)))
colnames(result_df) <- low_column_names

for (i in seq_along(codes_temp)) {
  values <- str_extract(codes_temp[[i]], "\\d$")
  result_df[i, column_names] <- as.integer(values)
}

df <- cbind(df, result_df) %>%
  select(-codes) 

# Split the codes into two (code -> code1, code2). 
# Key: 
  # If 0: code1 = 0, code2 = 0
  # If 1: code1 = 1, code2 = 0
  # If 2: code1 = 0, code2 = 1
df <- df %>%
  mutate(`Task done on time 1` = case_when( (`Task done on time` == 0) ~ 0, 
                                            (`Task done on time` == 1) ~ 1, 
                                            (`Task done on time` == 2) ~ 0)) %>%
  mutate(`Task done on time 2` = case_when( (`Task done on time` == 0) ~ 0, 
                                            (`Task done on time` == 1) ~ 0, 
                                            (`Task done on time` == 2) ~ 1)) %>%
  select(-`Task done on time`) %>%
  mutate(`Work accepted by others 1` =  case_when( (`Work accepted by others`== 0) ~ 0, 
                                                   (`Work accepted by others` == 1) ~ 1, 
                                                   (`Work accepted by others` == 2) ~ 0)) %>%
  mutate(`Work accepted by others 2` =  case_when( (`Work accepted by others`== 0) ~ 0, 
                                                   (`Work accepted by others` == 1) ~ 0, 
                                                   (`Work accepted by others` == 2) ~ 1)) %>%
  select(-`Work accepted by others`) %>% 
  mutate(`Positive tone 1` = case_when( (`Positive tone` == 0) ~ 0, 
                                        (`Positive tone` == 1) ~ 1, 
                                        (`Positive tone` == 2) ~ 0)) %>%
  mutate(`Positive tone 2` = case_when( (`Positive tone` == 0) ~ 0, 
                                        (`Positive tone` == 1) ~ 0, 
                                        (`Positive tone` == 2) ~ 1)) %>%
  select(-`Positive tone`) %>%
  mutate(`Initiate conversations 1` = case_when( (`Initiate conversations` == 0) ~ 0, 
                                                 (`Initiate conversations` == 1) ~ 1, 
                                                 (`Initiate conversations` == 2) ~ 0)) %>%
  mutate(`Initiate conversations 2` = case_when( (`Initiate conversations` == 0) ~ 0, 
                                                 (`Initiate conversations` == 1) ~ 0, 
                                                 (`Initiate conversations` == 2) ~ 1)) %>%
  select(-`Initiate conversations`) %>%
  mutate(`Help others 1` = case_when( (`Help others` == 0) ~ 0, 
                                        (`Help others` == 1) ~ 1, 
                                        (`Help others` == 2) ~ 0)) %>%
  mutate(`Help others 2` = case_when( (`Help others` == 0) ~ 0, 
                                        (`Help others` == 1) ~ 0, 
                                        (`Help others` == 2) ~ 1)) %>%
  select(-`Help others`) %>%
  mutate(`Completes more tasks 1` = case_when( (`Completes more tasks` == 0) ~ 0, 
                                      (`Completes more tasks` == 1) ~ 1, 
                                      (`Completes more tasks` == 2) ~ 0)) %>%
  mutate(`Completes more tasks 2` = case_when( (`Completes more tasks` == 0) ~ 0, 
                                      (`Completes more tasks` == 1) ~ 0, 
                                      (`Completes more tasks` == 2) ~ 1)) %>%
  select(-`Completes more tasks`) %>%
  mutate(`Assigns tasks 1` = case_when( (`Assigns tasks` == 0) ~ 0, 
                                               (`Assigns tasks` == 1) ~ 1, 
                                               (`Assigns tasks` == 2) ~ 0)) %>%
  mutate(`Assigns tasks 2` = case_when( (`Assigns tasks` == 0) ~ 0, 
                                               (`Assigns tasks` == 1) ~ 0, 
                                               (`Assigns tasks` == 2) ~ 1)) %>%
  select(-`Assigns tasks`) %>%
  mutate(`Review work from others 1` = case_when( (`Review work from others` == 0) ~ 0, 
                                        (`Review work from others` == 1) ~ 1, 
                                        (`Review work from others` == 2) ~ 0)) %>%
  mutate(`Review work from others 2` = case_when( (`Review work from others` == 0) ~ 0, 
                                         (`Review work from others` == 1) ~ 0, 
                                         (`Review work from others` == 2) ~ 1)) %>%
  select(-`Review work from others`) %>%
  mutate(`Initiate meeting 1` = case_when( (`Initiate meeting` == 0) ~ 0, 
                                                  (`Initiate meeting` == 1) ~ 1, 
                                                  (`Initiate meeting` == 2) ~ 0)) %>%
  mutate(`Initiate meeting 2` = case_when( (`Initiate meeting` == 0) ~ 0, 
                                                  (`Initiate meeting` == 1) ~ 0, 
                                                  (`Initiate meeting` == 2) ~ 1)) %>%
  select(-`Initiate meeting`)
  
  
write.csv(df, "reformatted_hui_data.csv")
  
  
  
  





