library(Lahman)
library(tidyverse)
theme_set(theme_bw())

baseball <- Batting %>% 
  left_join(People, by = "playerID") %>% 
  mutate(age = ifelse(birthMonth >= 7,
                      yearID - birthYear - 1, yearID - birthYear)) %>% 
  select(playerID, age, colnames(Batting)[6:22]) %>% 
  group_by(playerID, age) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  ungroup() %>% 
  battingStats() %>% 
  filter(OPS <= 1.421) %>%
  select(playerID, age, OPS) %>%
  mutate(
    scaledOPS = OPS / max(OPS),
    arcsin = asin(sqrt(scaledOPS))
  )

maxOPS <- max(baseball$OPS) # max ops (B.Bonds)

# Model
library(lme4)
mod <- lmer(arcsin ~ poly(age, 3, raw = TRUE) + (1 | playerID), data = baseball)
summary(mod)

randeff <- as.data.frame(VarCorr(mod))

# Simulation
set.seed(1234)
sim <- function() {
  age <- data.frame(age = 20:40)
  pred <- predict(mod, age, re.form = NA)
  
  shift <- rnorm(1, 0, randeff[1, 5])
  eps <- rnorm(nrow(age), 0, randeff[2, 5]) # variability across seasons
  
  career <- pred + shift + eps
  return(career)
}

mylist <- c()
for (i in 1:1000) {
  mylist[[i]] <- data.frame(player = i,
                            arcsin = sim(),
                            age = 20:40)
}

simdata <- do.call(rbind, mylist) %>%
  mutate(ops = maxOPS * sin(arcsin) ^ 2)

simdata[1:210, ] %>%
  ggplot(aes(x = age, y = ops, col = factor(player))) +
  geom_point() +
  geom_smooth(se = FALSE)

