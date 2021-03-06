# Data


######### End of "fake data" creation #########

# _____________________________________________________________________
### Drop-out rules and Imputations

library(mice)

ops25 <- simdata %>%
  filter(age == 25 & ops < 0.2)

out25 <- simdata %>%
  mutate(ops = ifelse(player %in% ops25$player & age > 25, NA, ops)) %>%
  dplyr::select(player, age, ops)

out25 %>%
  group_by(age) %>%
  summarise(avg = mean(ops, na.rm = TRUE)) %>%
  ggplot(aes(x = age, y = avg)) +
  geom_smooth()

summary(out25)

4455 / 21000
# 21% missing

md.pattern(out25)
library(VIM)
aggr(out25)
matrixplot(out25)

ini <- mice(out25, m = 5, maxit = 0)
pred <- ini$pred
pred["ops", ] <- c(-2, 2, 0)

# 2l.2stage.pmm
# 2l.2stage.norm
meth <- ini$meth
meth <- c("", "", "2l.norm")
imp1 <- mice(out25, pred = pred, meth = meth, print = FALSE)

densityplot(imp1, ~ops)
densityplot(imp1, ~ ops | .imp)

imp_data_start1 <- complete(imp1) # negative OPS, so use a transformation

# # plot observed and imputed data
# plot(density(simdata$ops), xlim = c(-1, 2))
# lines(density(imp_data_start1$ops), col = "red", lwd = 2)
#
# plot(imp)
# # run additional iterations.
#
# # imp2 <- mice.mids(imp, maxit = 10, print = FALSE)
# # plot(imp2) # convergence is more convincing


# Try with arcsin OPS

out25_new <- simdata %>%
  mutate(arcsinops = ifelse(player %in% ops25$player & age > 25, NA, arcsin)) %>%
  select(player, age, arcsinops)

ini <- mice(out25_new, maxit = 0)
pred <- ini$pred
pred["arcsinops", ] <- c(-2, 0, 0)

meth <- ini$meth
meth <- c("", "", "2l.norm")
# 2l.2stage.pmm
# 2l.2stage.norm
imp2 <- mice(out25_new, pred = pred, meth = meth, m = 1, maxit = 1)

densityplot(imp2, ~arcsinops)
densityplot(imp2, ~ arcsinops | .imp)

imp_data_start2 <- complete(imp2) %>% mutate(ops = maxOPS * sin(arcsinops)^2)


mylist <- list()
for (i in 1:5) {
  mylist[[i]] <- complete(imp2, i)
  mylist[[i]]$imp <- i
}

imp_all <- do.call(rbind, mylist)

imp_all %>%
  mutate(ops = maxOPS * sin(arcsinops)^2) %>%
  filter(player %in% c(1, 3)) %>%
  ggplot(aes(x = age, y = ops, col = factor(player))) +
  geom_path()

# plot observed and imputed data
plot(density(simdata$ops), xlim = c(-1, 2))
lines(density(out25$ops[!is.na(out25$ops)]), col = "red", lwd = 2)
lines(density(imp_data_start2$ops), col = "red", lwd = 2)

simdata %>%
  dplyr::select(player, age, ops) %>%
  mutate(type = "og") %>%
  full_join(imp_data_start2 %>% dplyr::select(player, age, ops) %>% mutate(type = "imputed")) %>%
  full_join(out25 %>% dplyr::select(player, age, ops) %>% mutate(type = "missing")) %>%
  group_by(type, age) %>%
  summarise(meanOps = mean(ops, na.rm = TRUE)) %>%
  ggplot(aes(x = age, y = meanOps, col = type)) +
  geom_point() +
  geom_smooth()

# 2l.2stage.norm
library(micemd)

ini_2stage_norm <- mice(out25_new, maxit = 0)
pred_2stage_norm <- ini$pred
pred_2stage_norm["arcsinops", ] <- c(-2, 2, 0)

meth_2stage_norm <- ini$meth
meth_2stage_norm <- c("", "", "2l.2stage.norm")
# 2l.2stage.pmm
# 2l.2stage.norm
imp_2stage_norm <- mice(out25_new, pred = pred_2stage_norm, meth = meth_2stage_norm, print = FALSE)

densityplot(imp_2stage_norm, ~arcsinops)
densityplot(imp_2stage_norm, ~ arcsinops | .imp)

imp_data_2stage_norm <- complete(imp_2stage_norm) %>% mutate(ops = maxOPS * sin(arcsinops)^2)
# write.csv


imp_2stage_norm_list <- list()
for (i in 1:5) {
  imp_2stage_norm_list[[i]] <- complete(imp_2stage_norm, i)
  imp_2stage_norm_list[[i]]$imp <- i
}

imp_2stage_norm_list_all <- do.call(rbind, imp_2stage_norm_list)
#write.csv

imp_2stage_norm_list_all %>%
  mutate(ops = maxOPS * sin(arcsinops)^2) %>%
  filter(player %in% c(1, 3)) %>%
  ggplot(aes(x = age, y = ops, col = factor(player))) +
  geom_path()

# plot observed and imputed data
plot(density(simdata$ops), xlim = c(-1, 2))
lines(density(out25$ops[!is.na(out25$ops)]), col = "red", lwd = 2)
lines(density(imp_data_2stage_norm$ops), col = "blue", lwd = 2)

simdata %>%
  dplyr::select(player, age, ops) %>%
  mutate(type = "og") %>%
  full_join(imp_data_2stage_norm %>% dplyr::select(player, age, ops) %>% mutate(type = "imputed")) %>%
  full_join(out25 %>% dplyr::select(player, age, ops) %>% mutate(type = "missing")) %>%
  group_by(type, age) %>%
  summarise(meanOps = mean(ops, na.rm = TRUE)) %>%
  ggplot(aes(x = age, y = meanOps, col = type)) +
  geom_point() +
  geom_smooth()


# 2l.2stage.pmm
ini_2stage_pmm <- mice(out25_new, maxit = 0)
pred_2stage_pmm <- ini$pred
pred_2stage_pmm["arcsinops", ] <- c(-2, 2, 0)

meth_2stage_pmm <- ini$meth
meth_2stage_pmm <- c("", "", "2l.2stage.pmm")
# 2l.2stage.pmm
# 2l.2stage.norm
imp_2stage_pmm <- mice(out25_new, pred = pred_2stage_pmm, meth = meth_2stage_pmm, print = FALSE)

densityplot(imp_2stage_pmm, ~arcsinops)
densityplot(imp_2stage_pmm, ~ arcsinops | .imp)

imp_data_2stage_pmm <- complete(imp_2stage_pmm) %>% mutate(ops = maxOPS * sin(arcsinops)^2)


imp_2stage_pmm_list <- list()
for (i in 1:5) {
  imp_2stage_pmm_list[[i]] <- complete(imp_2stage_pmm, i)
  imp_2stage_pmm_list[[i]]$imp <- i
}

imp_2stage_pmm_list_all <- do.call(rbind, imp_2stage_pmm_list)

imp_2stage_pmm_list_all %>%
  mutate(ops = maxOPS * sin(arcsinops)^2) %>%
  filter(player %in% c(1, 3)) %>%
  ggplot(aes(x = age, y = ops, col = factor(player))) +
  geom_path()

# plot observed and imputed data
plot(density(simdata$ops), xlim = c(-1, 2))
lines(density(out25$ops[!is.na(out25$ops)]), col = "red", lwd = 2)
lines(density(imp_data_2stage_pmm$ops), col = "red", lwd = 2)

simdata %>%
  dplyr::select(player, age, ops) %>%
  mutate(type = "og") %>%
  full_join(imp_data_2stage_pmm %>% dplyr::select(player, age, ops) %>% mutate(type = "imputed")) %>%
  full_join(out25 %>% dplyr::select(player, age, ops) %>% mutate(type = "missing")) %>%
  group_by(type, age) %>%
  summarise(meanOps = mean(ops, na.rm = TRUE)) %>%
  ggplot(aes(x = age, y = meanOps, col = type)) +
  geom_point() +
  geom_smooth()



#################### DELTA

simdata_delta <- simdata %>% 
  group_by(player) %>% 
  mutate(PrevOPS = lag(ops),
         DiffOPS = ops - PrevOPS,
         isNegative = if_else(DiffOPS < 0, "yes", "no"))

p <- simdata_delta %>% 
  group_by(age) %>% 
  summarize(meanDiff = mean(DiffOPS, na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = meanDiff)) + 
  geom_point() +
  geom_smooth()

plotly::ggplotly(p)


# Drop
# talk about what to do with the dropout rates
# Changes within 0.1 = no change
# 3 negative OPS's in a row = drop


















# end of career

ops35 <- simdata %>%
  filter(age == 35 & ops < 0.3)

out35 <- simdata %>%
  mutate(ops = ifelse(player %in% ops35$player & age > 35, NA, ops)) %>%
  select(player, age, ops)

out35 %>%
  group_by(age) %>%
  summarise(avg = mean(ops, na.rm = TRUE)) %>%
  ggplot(aes(x = age, y = avg)) +
  geom_smooth()

summary(out35)

ini <- mice(out35, maxit = 0)
pred <- ini$pred
pred["ops", ] <- c(-2, 2, 0)

meth <- ini$meth
meth <- c("", "", "2l.norm")
imp3 <- mice(out35, pred = pred, meth = meth, print = FALSE)

densityplot(imp3, ~ops)

imp_data_end1 <- complete(imp3)

# plot observed and imputed data
plot(density(simdata$ops), xlim = c(-1, 2))
lines(density(imp_data_end1$ops), col = "red", lwd = 2)


## arcsin

out35_new <- simdata %>%
  mutate(arcsinops = ifelse(player %in% ops35$player & age > 35, NA, arcsin)) %>%
  select(player, age, arcsinops)

ini <- mice(out35_new, maxit = 0)
pred <- ini$pred
pred["arcsinops", ] <- c(-2, 2, 0)

meth <- ini$meth
meth <- c("", "", "2l.norm")
imp4 <- mice(out35_new, pred = pred, meth = meth, print = FALSE)

densityplot(imp4, ~arcsinops)

imp_data_end2 <- complete(imp4) %>% mutate(ops = maxOPS * sin(arcsinops)^2)

# plot observed and imputed data
plot(density(simdata$ops), xlim = c(-1, 2))
lines(density(imp_data_end2$ops), col = "red", lwd = 2)


# start of career, but come back

outback <- simdata %>%
  mutate(ops = ifelse(ops < 0.3 & age <= 25, NA, ops)) %>%
  select(player, age, ops)

outback %>%
  group_by(age) %>%
  summarise(avg = mean(ops, na.rm = TRUE)) %>%
  ggplot(aes(x = age, y = avg)) +
  geom_smooth()

summary(outback)

ini <- mice(outback, maxit = 0)
pred <- ini$pred
pred["ops", ] <- c(-2, 2, 0)

meth <- ini$meth
meth <- c("", "", "2l.norm")
imp5 <- mice(outback, pred = pred, meth = meth, print = FALSE)

densityplot(imp5, ~ops)

imp_data_outback1 <- complete(imp5)

# plot observed and imputed data
plot(density(simdata$ops), xlim = c(-1, 2))
lines(density(imp_data_outback1$ops), col = "red", lwd = 2)


## arcsin

outback_new <- simdata %>%
  mutate(arcsinops = ifelse(ops < 0.3 & age <= 25, NA, arcsin)) %>%
  select(player, age, arcsinops)

ini <- mice(out35_new, maxit = 0)
pred <- ini$pred
pred["arcsinops", ] <- c(-2, 2, 0)

meth <- ini$meth
meth <- c("", "", "2l.norm")
imp6 <- mice(out35_new, pred = pred, meth = meth, print = FALSE)

densityplot(imp6, ~arcsinops)

imp_data_outback2 <- complete(imp6) %>% mutate(ops = maxOPS * sin(arcsinops)^2)

# plot observed and imputed data
plot(density(simdata$ops), xlim = c(-1, 2))
lines(density(imp_data_outback2$ops), col = "red", lwd = 2)

simdata %>%
  select(player, age, ops) %>%
  mutate(type = "og") %>%
  full_join(imp_data_start2 %>% select(player, age, ops) %>% mutate(type = "start")) %>%
  full_join(imp_data_end2 %>% select(player, age, ops) %>% mutate(type = "end")) %>%
  full_join(imp_data_outback2 %>% select(player, age, ops) %>% mutate(type = "outback")) %>%
  group_by(type, age) %>%
  summarise(meanOps = mean(ops)) %>%
  ggplot(aes(x = age, y = meanOps, col = type)) +
  geom_point() +
  geom_smooth() +
  theme_bw()










## Some scratch work

# Drop-out methods

# if age = 30 and OPS < 0.2 then retire

drop_end1 <- simdata %>%
  filter(age == 30 & ops < 0.2)

drop_end2 <- simdata %>%
  filter(player %in% drop_end1$player) %>%
  filter(age >= 30)

drop_end <- anti_join(simdata, drop_end2) %>% mutate(type = "End_of_career")

# drop out at the start of career
# age 25 ops < 0.3, not good enough to remain in the league (go overseas etc.)

drop_start1 <- simdata %>%
  filter(age == 25 & ops < 0.2)

drop_start2 <- simdata %>%
  filter(player %in% drop_start1$player) %>%
  filter(age > 25)

drop_start <- anti_join(simdata, drop_start2) %>% mutate(type = "Start_of_career_out")

# Start: continue to be in minor league

dropstart1 <- simdata %>%
  filter(age == 23 & ops < 0.2)

dropstart2 <- simdata %>%
  filter(player %in% dropstart1$player) %>%
  filter(age <= 23)

dropstart <- anti_join(simdata, dropstart2) %>% mutate(type = "Start_of_career_minor")

# at age 30, 25% retire
players <- unique(simdata$player)
x <- sample(players, length(players) / 4)

dropAge30 <- simdata %>%
  filter(player %in% x) %>%
  filter(age >= 30)

# age age 35, 10% more retire (35% total)
# y <- sample(setdiff(players, x), length(players)/10)
#
# dropAge35 <- simdata %>%
#   filter(player %in% y) %>%
#   filter(age >= 35)
#
# drop_30_35 <- anti_join(simdata, union(dropAge30, dropAge35)) %>% mutate(type = "Drop_ages_30_35")

drop_30 <- anti_join(simdata, dropAge30) %>% mutate(type = "Drop_age_30")

# at age 35, 50% retire
x35 <- sample(unique(simdata$player), length(players) / 2)

dropAge35 <- simdata %>%
  filter(player %in% x35) %>%
  filter(age >= 35)

drop_35 <- anti_join(simdata, dropAge35) %>% mutate(type = "Drop_age_35")


# middle of career

drop_mid <- simdata %>%
  filter(!(age %in% 26:33 & ops < 0.1)) %>%
  mutate(type = "Mid_career_injury")

simdata %>%
  mutate(type = "Full_data") %>%
  full_join(drop_end) %>%
  full_join(drop_start) %>%
  full_join(dropstart) %>%
  full_join(drop_30) %>%
  full_join(drop_35) %>%
  full_join(drop_mid) %>%
  group_by(type, age) %>%
  summarise(meanOps = mean(ops)) %>%
  ggplot(aes(x = age, y = meanOps, col = type)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_bw()

# geom_point() + geom_smooth(method = "lm", formula = y ~ poly(x, 3))

# full_df <- simdata %>%
#   mutate(type = "Full_data") %>%
#   full_join(drop_end) %>%
#   full_join(drop_start) %>%
#   full_join(dropstart) %>%
#   full_join(drop_30) %>%
#   full_join(drop_35) %>%
#   full_join(drop_mid)


library(micemd)
?micemd

dropstart1_NA <- simdata %>%
  filter(age <= 23 & ops < 0.2) %>%
  mutate(ops = NA)

dropstart1_remained <- simdata %>%
  filter(!(id %in% dropstart1_NA$id))

dropstart1_full_NA <- dropstart1_NA %>%
  full_join(dropstart1_remained) %>%
  arrange(player, age)

dropstart1_full_NA %>%
  filter(player %in% c("player1", "player2", "player3", "player6", "player7")) %>%
  ggplot(aes(x = age, y = ops, col = player)) +
  geom_point() +
  theme_bw() +
  geom_smooth()
