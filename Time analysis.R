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


types2 <- timing %>% 
  group_by(type) %>% 
  summarize(number = n(),
            ave = mean(time),
            dev = sd(time))


timing %>% 
  filter(type=="2 pointer",
         time<=35) %>% 
  ggplot(aes(x=time)) +
  geom_density()


df <- timing %>%
  filter(type == "2 pointer",
         time <= 35) %>% 
  pull(time)

pdf <- density(df)

# random.points <- approx(
#   cumsum(cdf$y)/sum(cdf$y),
#   cdf$x,
#   runif(1)
# )$y
# 
# random.points
# 
# hist(random.points)

x <- sample(pdf$x, 1, TRUE, pdf$y)
round(x)
