library(Lahman)
library(tidyverse)

getPlayers <- function(year){
  left_join(Batting, People, by = "playerID") %>% 
    filter(yearID >= year)
}

mlb <- Batting %>% 
  filter(playerID %in% getPlayers(1990)$playerID) %>% 
  left_join(People, by = "playerID") %>% 
  mutate(age = ifelse(birthMonth >= 7,
                      yearID - birthYear - 1, yearID - birthYear)) %>% 
  select(playerID, age, colnames(Batting)[6:22]) %>% 
  group_by(playerID, age) %>%
  summarise_all(sum, na.rm = TRUE) %>% # fix players that played for multiple teams in one season
  ungroup() %>% 
  battingStats()
  
# hist(mlb$PA)
# quantile(mlb$PA, seq(0, 1, by = 0.05))

mlb <- mlb %>% 
  filter(PA >= 100) %>% 
  mutate(scaledOPS = OPS / 1.421,
         arcsinOPS = asin(sqrt(scaledOPS)),
         pnum = as.integer(as.factor(playerID))) %>% 
  filter(age %in% 19:42)

mlb %>% 
  group_by(age) %>% 
  summarize(meanOPS = mean(OPS, na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = meanOPS)) +
  geom_point() +
  geom_smooth()
  
players <- unique(mlb$playerID)
agerange <- min(mlb$age):max(mlb$age)

dat <- data.frame(playerID = rep(players, length(agerange))) %>% 
  group_by(playerID) %>% 
  mutate(age = agerange) %>% 
  ungroup() %>% 
  arrange(playerID)

joined <- dat %>% 
  left_join(mlb) %>% 
  arrange(playerID) %>% 
  mutate(pnum = as.integer(factor(playerID)))

df1 <- joined %>% 
  select(pnum, age, arcsinOPS)

library(mice)

ini <- mice(df1, maxit = 0)
pred <- ini$pred
pred["arcsinOPS", ] <- c(-2, 2, 0)

meth <- ini$meth
meth <- c("", "", "2l.norm")
imp <- mice(df1, pred = pred, meth = meth)

densityplot(imp, ~arcsinOPS)
densityplot(imp, ~ arcsinOPS | .imp)

maxOPS <- max(mlb$OPS)
impdf <- complete(imp) %>% mutate(OPS = maxOPS*(sin(arcsinOPS))^2) %>% 
  mutate(playerID = joined$playerID)

plot(density(joined$OPS[!is.na(joined$OPS)]), main = "black: og \n red: plain")
lines(density(impdf$OPS), col = "red", lwd = 2)  

mlb %>%
  select(playerID, age, OPS) %>%
  mutate(type = "og") %>%
  full_join(impdf %>% select(playerID, age, OPS) %>% mutate(type = "imputed")) %>%
  group_by(type, age) %>%
  summarise(meanOps = mean(OPS, na.rm = TRUE)) %>%
  ggplot(aes(x = age, y = meanOps, color = type)) +
  geom_point() + geom_smooth(span = 1)

complete(imp, "long", include = TRUE) %>% 
  mutate(OPS = maxOPS*(sin(arcsinOPS))^2) %>% 
  group_by(.imp, age) %>%
  summarise(meanOps = mean(OPS, na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = meanOps, color = factor(.imp))) +
  geom_point() + geom_smooth() +
  ggtitle("Real data, imputed with no covariates")

##########################

# Covariates
# HR rate

# select(pnum, age, arcsinOPS, BA)
df2 <- joined %>% 
  mutate(HRrate = HR/AB,
         arcsinHRrate = asin(sqrt(HRrate))) %>% 
  select(pnum, age, arcsinOPS, arcsinHRrate)

ini2 <- mice(df2, maxit = 0)
pred2 <- ini2$pred
pred2["arcsinOPS", ] <- c(-2, 2, 0, 2)
pred2["arcsinHRrate", ] <- c(-2, 2, 2, 0)
imp2 <- mice(df2, pred = pred2, meth = "2l.norm", seed = 1000)

# densityplot(imp2, ~arcsinOPS)
# densityplot(imp2, ~ arcsinOPS | .imp)

# impdf2 <- complete(imp2) %>% mutate(OPS = maxOPS*(sin(arcsinOPS))^2) %>% 
#   mutate(playerID = joined$playerID)

complete(imp2, "long", include = TRUE) %>% 
  mutate(OPS = maxOPS*(sin(arcsinOPS))^2) %>% 
  group_by(.imp, age) %>%
  summarise(meanOps = mean(OPS, na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = meanOps, color = factor(.imp))) +
  geom_point() + geom_smooth(span = 1) +
  ggtitle("Real data, imputed with (arcsin) HR rate")



# fit <- with(imp2, loess(arcsinOPS ~ age))
# summary(pool(fit))






mlb %>%
  filter(pnum %in% 4:6) %>%
  select(pnum, playerID, age, OPS) %>%
  mutate(type = "og") %>%
  full_join(
    impdf2 %>% select(pnum, playerID, age, OPS) %>% mutate(type = "imputed") %>% filter(pnum %in% 4:6)
  ) %>%
  full_join(
    impdf %>% select(pnum, playerID, age, OPS) %>% mutate(type = "imputed_plain") %>% filter(pnum %in% 4:6)
  ) %>%
  ggplot(aes(x = age, y = OPS, col = type)) +
  geom_point() + geom_path() + facet_wrap( ~ playerID) + theme_bw()

plot(density(joined$OPS[!is.na(joined$OPS)]), main = "black og, red plain, blue BA")
lines(density(impdf$OPS), col = "red", lwd = 2)  
lines(density(impdf2$OPS), col = "blue", lwd = 2)

mlb %>% 
  select(playerID, age, OPS) %>% 
  mutate(type = "og") %>% 
  full_join(impdf %>% select(playerID, age, OPS) %>% mutate(type = "imputed_plain")) %>% 
  full_join(impdf2 %>% select(playerID, age, OPS) %>% mutate(type = "adding_BA")) %>% 
  group_by(type, age) %>% 
  summarise(meanOps = mean(OPS, na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = meanOps, col = type)) +
  geom_point() + geom_smooth()

### Covariates
# BA, PA, HR

df2new <- joined %>% 
  mutate(HRrate = HR/AB,
         arcsinHRrate = asin(sqrt(HRrate)),
         arcsinBABIP = asin(sqrt(BABIP))) %>% 
  select(pnum, age, arcsinOPS, arcsinHRrate, arcsinBABIP)

ini2new <- mice(df2new, maxit = 0)
pred2new <- ini2new$pred
pred2new["arcsinOPS", ] <- c(-2, 2, 0, 2, 2)
pred2new["arcsinHRrate", ] <- c(-2, 2, 2, 0, 2)
pred2new["arcsinBABIP", ] <- c(-2, 2, 2, 2, 0)
imp2new <- mice(df2new, pred = pred2new, method = "2l.norm")

complete(imp2new, "long", include = TRUE) %>% 
  mutate(OPS = maxOPS*(sin(arcsinOPS))^2) %>% 
  group_by(.imp, age) %>%
  summarise(meanOps = mean(OPS, na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = meanOps, color = factor(.imp))) +
  geom_point() + geom_smooth(span = 1)


densityplot(imp2new, ~arcsinOPS)
densityplot(imp2new, ~ arcsinOPS | .imp)

impdf2new <- complete(imp2new) %>% mutate(OPS = maxOPS*(sin(arcsinOPS))^2) %>% 
  mutate(playerID = joined$playerID)

mlb %>% 
  select(playerID, age, OPS) %>% 
  mutate(type = "og") %>% 
  full_join(impdf %>% select(playerID, age, OPS) %>% mutate(type = "imputed")) %>% 
  full_join(impdf2new %>% select(playerID, age, OPS) %>% mutate(type = "imputed_with_covariates")) %>% 
  group_by(type, age) %>% 
  summarise(meanOps = mean(OPS, na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = meanOps, col = type)) +
  geom_point() + geom_smooth()






mlb %>% 
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


mlb %>% 
  arrange(playerID, age) %>% 
  group_by(playerID) %>% 
  mutate(diffOPS = OPS - lag(OPS)) %>% 
  group_by(age) %>% 
  drop_na() %>% 
  summarise(avgDiff = mean(diffOPS)) %>% 
  ggplot(aes(x = age, y = avgDiff)) +
  geom_point() +
  geom_smooth(se = FALSE, size = 2) +
  theme_bw()


help(Lahman)


View(Fielding)



###



d <- simdata %>%
  select(-id) %>%
  group_by(player) %>%
  mutate(diff = ops - lag(ops))


d %>%
  filter(player == 10) %>%
  ggplot() +
  geom_point(aes(x = age, y = ops)) +
  geom_smooth(aes(x = age, y = ops)) +
  geom_smooth(aes(x = age, y = diff))


d %>%
  group_by(age) %>%
  summarise(avgdiff = mean(diff), avgops = mean(ops)) %>%
  ggplot(aes(x = age, y = avgdiff)) +
  geom_point() +
  geom_smooth()



# dropping
# 25 low ops and out

ops25 <- d %>%
  filter(age == 25 & ops < 0.2)

out25 <- d %>%
  mutate(
    ops = ifelse(player %in% ops25$player & age > 25, NA, ops),
    diff = ifelse(is.na(ops), NA, diff)
  )

out25 %>%
  group_by(age) %>%
  summarise(avg = mean(ops, na.rm = TRUE)) %>%
  ggplot(aes(x = age, y = avg)) +
  geom_smooth()


out25 %>%
  filter(player == 4) %>%
  ggplot() +
  geom_point(aes(x = age, y = ops)) +
  geom_smooth(aes(x = age, y = ops)) +
  geom_smooth(aes(x = age, y = diff))





out <- list()
for (i in 1:1000) {
  a <- loess(ops ~ age, data = subset(simdata, player == i), span = 2 / 3)
  out[[i]] <- data.frame(
    age = 20:40,
    ops = predict(a, data.frame(age = seq(20, 40, 1))),
    player = i
  )
}

dd <- do.call(rbind, out) %>%
  mutate(diff = ops - lag(ops))

set.seed(1)
dd %>%
  filter(player %in% sample(1:1000, size = 10)) %>%
  ggplot() +
  geom_point(aes(x = age, y = ops)) +
  geom_line(aes(x = age, y = ops)) +
  geom_point(aes(x = age, y = diff), color = "blue") +
  geom_line(aes(x = age, y = diff), color = "blue") +
  facet_wrap(~player)


###

View(simdata)

simdata_new <- simdata %>% 
  group_by(player) %>% 
  mutate(diffops = ops - lag(ops)) %>% 
  filter(age != 20)


simdata_new %>% 
  filter(player %in% 1:5) %>% 
  ggplot(aes(x = age, y = diffops, col = factor(player))) + 
  geom_point() + geom_smooth(se = FALSE) + theme_bw()


simdata_new %>% 
  group_by(age) %>% 
  summarise(avgDiff = mean(diffops, na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = avgDiff)) +
  geom_point() +
  geom_smooth(se = FALSE, size = 2) +
  theme_bw()


ops25 <- simdata_new %>% 
  filter(age == 25 & ops < 0.2)

out25 <- simdata_new %>% 
  mutate(ops = ifelse(player %in% ops25$player & age > 25, NA, ops))

out25 %>% 
  group_by(age) %>% 
  summarise(avgDiff = mean(diffops, na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = avgDiff)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm", formula = y ~ poly(x, 3), size = 2) +
  theme_bw()

