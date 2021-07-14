library(Lahman)
library(tidyverse)

baseball <- 
  left_join(Batting, People, by = "playerID") %>% 
  mutate(age = ifelse(birthMonth >= 7,
                      yearID - birthYear - 1, yearID - birthYear)) %>%
  filter(age %in% 20:40) %>% 
  mutate(OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
         SLG =  (H + X2B + 2*X3B + 3*HR) / AB,
         OPS = OBP + SLG) %>% 
  filter(!is.na(OPS))

# hist(baseball$age, col = "salmon")

# get the players that play multiple seasons
seasons <- baseball %>% 
  group_by(playerID) %>% 
  summarise(count = n()) %>% 
  filter(count >= 2)

baseball %>% 
  filter(playerID %in% seasons$playerID) %>% # only keeps players that played more than 2 seasons
  arrange(playerID, age) %>% 
  group_by(playerID) %>% 
  mutate(diffOPS = OPS - lag(OPS)) %>% 
  group_by(age) %>% 
  drop_na() %>% 
  summarise(avgDiff = mean(diffOPS)) %>% 
  mutate(cumDiffOPS = cumsum(avgDiff)) %>% 
  ggplot(aes(x = age, y = cumDiffOPS)) +
  geom_point() +
  geom_smooth(se = FALSE, size = 2) +
  theme_bw()

