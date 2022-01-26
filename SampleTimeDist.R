library(tidyverse)
Events_2019 <- read_csv("Data/PlayByPlay_2019/Events_2019.csv")

types <- Events_2019 %>% 
  count(EventType)

timing <- Events_2019 %>% 
  filter(str_detect(EventType, "timeout|assist|sub|free|reb|block|foul", negate=TRUE)) %>% 
  mutate(time = case_when(
    row_number()==1 ~ ElapsedSeconds,
    ElapsedSeconds - lag(ElapsedSeconds) < 0 ~ ElapsedSeconds,
    TRUE ~ ElapsedSeconds - lag(ElapsedSeconds)))  %>% 
  mutate(type = case_when(
    str_detect(EventType, "3") ~ "3 pointer",
    str_detect(EventType, "2") ~ "2 pointer",
    TRUE ~ EventType
  ))

timing_nest <- timing %>% 
  select(EventTeamID, type, time) %>% 
  filter(time <= 35) %>% 
  group_by(EventTeamID, type) %>% 
  nest() %>% 
  mutate(pdf = map(data, ~density(.x$time, from=0, to=35)))


sample_time <- function(id, event) {
  temp <- timing_nest %>%
    filter(EventTeamID == id,
           type == event) %>%
    pull(pdf)
  
  sample(temp[[1]]$x,
         1,
         TRUE,
         temp[[1]]$y)
}


