# Different types of drop-out
#########################################

# 1) if age = 30 and OPS < 0.2 then retire
#########################################

drop_end1 <- sim_df %>%
  filter(age == 30 & ops < 0.2)

drop_end2 <- sim_df %>%
  filter(player %in% drop_end1$player) %>%
  filter(age >= 30)

drop_end <- anti_join(sim_df, drop_end2) %>% mutate(type = "End_of_career")

# drop out at the start of career
# age 25 ops < 0.3, not good enough to remain in the league (go overseas etc.)

drop_start1 <- sim_df %>%
  filter(age == 25 & ops < 0.2)

drop_start2 <- sim_df %>%
  filter(player %in% drop_start1$player) %>%
  filter(age > 25)

drop_start <- anti_join(sim_df, drop_start2) %>% mutate(type = "Start_of_career_out")

# Start: continue to be in minor league

dropstart1 <- sim_df %>%
  filter(age == 23 & ops < 0.2)

dropstart2 <- sim_df %>%
  filter(player %in% dropstart1$player) %>%
  filter(age <= 23)

dropstart <- anti_join(sim_df, dropstart2) %>% mutate(type = "Start_of_career_minor")

# at age 30, 25% retire
players <- unique(sim_df$player)
x <- sample(players, length(players) / 4)

dropAge30 <- sim_df %>%
  filter(player %in% x) %>%
  filter(age >= 30)

# age age 35, 10% more retire (35% total)
# y <- sample(setdiff(players, x), length(players)/10)
#
# dropAge35 <- sim_df %>%
#   filter(player %in% y) %>%
#   filter(age >= 35)
#
# drop_30_35 <- anti_join(sim_df, union(dropAge30, dropAge35)) %>% mutate(type = "Drop_ages_30_35")

drop_30 <- anti_join(sim_df, dropAge30) %>% mutate(type = "Drop_age_30")

# at age 35, 50% retire
x35 <- sample(unique(sim_df$player), length(players) / 2)

dropAge35 <- sim_df %>%
  filter(player %in% x35) %>%
  filter(age >= 35)

drop_35 <- anti_join(sim_df, dropAge35) %>% mutate(type = "Drop_age_35")


# middle of career

drop_mid <- sim_df %>%
  filter(!(age %in% 26:33 & ops < 0.1)) %>%
  mutate(type = "Mid_career_injury")

sim_df %>%
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
  geom_smooth(method = "loess")

# age >= 30, less than <= 0.2, get first year, then drop every year after

sim_df <- sim_df %>% 
  mutate(rowID = paste0("row", 1:nrow(sim_df)))

temp1 <- sim_df %>% 
  filter(age >= 30 & ops <= 0.2) %>% 
  group_by(player) %>% 
  slice(which.min(age))

temp2 <- sim_df %>% 
  mutate(first = if_else(rowID %in% temp1$rowID, 1, 0)) %>% 
  group_by(player) %>% 
  mutate(dum = cumsum(first)) %>% 
  ungroup() %>% 
  mutate(arcsinops = ifelse(dum == 1, NA, arcsin)) %>% 
  select(player, age, arcsinops)

temp2 %>% 
  group_by(age) %>% 
  summarise(meanOps = mean(ops, na.rm = TRUE)) %>%
  ggplot(aes(x = age, y = meanOps)) +
  geom_point() +
  geom_smooth(method = "loess")

# age <= 27, less than <= 0.2, get first year, then drop every year after


d1 <- sim_df %>% 
  filter(age <= 27 & ops <= 0.2) %>% 
  group_by(player) %>% 
  slice(which.min(age))

d2 <- sim_df %>% 
  mutate(first = if_else(rowID %in% d1$rowID, 1, 0)) %>% 
  group_by(player) %>% 
  mutate(dum = cumsum(first)) %>% 
  ungroup() %>% 
  mutate(arcsinops = ifelse(dum == 1, NA, arcsin)) %>% 
  select(player, age, arcsinops)

d2 %>% 
  group_by(age) %>% 
  summarise(meanOps = mean(ops, na.rm = TRUE)) %>%
  ggplot(aes(x = age, y = meanOps)) +
  geom_point() +
  geom_smooth(method = "loess")

# Different types of drop-out

# 1) if age >= 30 and OPS <= 0.2 then retire

sim_df <- sim_df %>% 
  mutate(rowID = paste0("row", 1:nrow(sim_df)))

temp1 <- sim_df %>% 
  filter(age >= 30 & ops <= 0.2) %>% 
  group_by(player) %>% 
  slice(which.min(age))

drop1 <- sim_df %>% 
  mutate(first = if_else(rowID %in% temp1$rowID, 1, 0)) %>% 
  group_by(player) %>% 
  mutate(drop = cumsum(first)) %>% 
  ungroup() %>% 
  mutate(arcsinops = ifelse(drop == 1, NA, arcsin))

# 2) if age = 25 and ops < 0.2, not good enough to remain in the league

temp2 <- sim_df %>%
  filter(age == 25 & ops < 0.2)

drop2 <- sim_df %>%
  mutate(ops = ifelse(player %in% temp2$player & age > 25, NA, ops),
         arcsinops = ifelse(is.na(ops), NA, arcsin))

# 3)
