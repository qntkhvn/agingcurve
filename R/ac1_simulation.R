library(Lahman)
library(tidyverse)
theme_set(theme_bw())

max_ops <- 1.421
baseball <- Batting %>% 
  left_join(People, by = "playerID") %>% 
  mutate(age = ifelse(birthMonth >= 7,
                      yearID - birthYear - 1, yearID - birthYear)) %>% 
  select(playerID, age, colnames(Batting)[6:22]) %>% 
  group_by(playerID, age) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  ungroup() %>% 
  battingStats() %>% 
  filter(OPS <= max_ops) %>%
  select(playerID, age, OPS) %>%
  mutate(
    scaledOPS = OPS / max(OPS),
    arcsin = asin(sqrt(scaledOPS))
  )

# Model
library(lme4)
mod <- lmer(arcsin ~ poly(age, 3, raw = TRUE) + (1 | playerID), 
            data = baseball)

vcor <- as.data.frame(VarCorr(mod))

# Simulation
set.seed(1234)
sim_career <- function() {
  age <- data.frame(age = 20:40)
  pred <- predict(mod, age, re.form = NA)
  
  shift <- rnorm(1, 0, vcor[1, 5])
  eps <- rnorm(nrow(age), 0, vcor[2, 5]) # variability across seasons
  
  career <- pred + shift + eps
  return(career)
}

sim_list <- c()
set.seed(1234)
for (i in 1:1000) {
  sim_list[[i]] <- tibble(player = i,
                        arcsin = sim_career(),
                        age = 20:40)
}

sim_df <- bind_rows(sim_list) %>%
  mutate(ops = max_ops * sin(arcsin) ^ 2)

sim_df[1:210, ] %>%
  ggplot(aes(x = age, y = ops, col = factor(player))) +
  geom_point() +
  geom_smooth(se = FALSE)
