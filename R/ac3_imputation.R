library(mice)
ops25 <- simdata %>%
  filter(age == 25 & ops < 0.2)

out25 <- simdata %>%
  mutate(ops = ifelse(player %in% ops25$player & age > 25, NA, ops)) %>%
  dplyr::select(player, age, ops)

summary(out25)

out25_new <- simdata %>%
  mutate(arcsinops = ifelse(player %in% ops25$player & age > 25, NA, arcsin)) %>%
  select(player, age, arcsinops)

ini <- mice(out25_new, maxit = 0)
pred <- ini$predictorMatrix
pred["arcsinops", ] <- c(-2, 2, 0)

imp2 <- mice(out25_new, predictorMatrix = pred, method = "2l.norm", seed = 123)

# densityplot(imp2, ~arcsinops)
# densityplot(imp2, ~ arcsinops | .imp)
# imp_data_start2 <- complete(imp2) %>% mutate(ops = maxOPS * sin(arcsinops)^2)
# 
# # plot observed and imputed data
# plot(density(simdata$ops), xlim = c(-1, 2))
# lines(density(out25$ops[!is.na(out25$ops)]), col = "red", lwd = 2)
# lines(density(imp_data_start2$ops), col = "red", lwd = 2)

complete(imp2, "long", include = TRUE) %>% 
  mutate(ops = maxOPS*(sin(arcsinops))^2,
         type = paste("Imputation", .imp)) %>% 
  select(player, age, ops, type) %>% 
  full_join(simdata %>% select(player, age, ops) %>% mutate(type = "Original")) %>%
  group_by(type, age) %>%
  summarise(meanOps = mean(ops, na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = meanOps, color = type)) +
  geom_point() + geom_smooth()




# delta/derivative

der <- simdata %>% 
  group_by(player) %>% 
  mutate(prev = lag(ops),
         diff = ops - prev)

temp <- der %>%
  filter(age == 25 & ops < 0.2)

drop25 <- der %>%
  mutate(ops = ifelse(player %in% temp$player & age > 25, NA, ops),
         arcsinops = ifelse(is.na(ops), NA, arcsin),
         prev = ifelse(is.na(ops), NA, prev),
         diff = ifelse(is.na(ops), NA, diff)) %>% 
  filter(age != 20) %>% 
  select(player, age, diff)


ini_der <- mice(drop25, maxit = 0)
pred_der <- ini_der$pred
pred_der["diff", ] <- c(-2, 2, 0)
imp_der <- mice(drop25, pred = pred_der, method = "2l.norm")

complete(imp_der, "long", include = TRUE) %>% 
  group_by(.imp, age) %>%
  summarise(mean_diff = mean(diff, na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = mean_diff, color = factor(.imp))) +
  geom_point() + geom_smooth(se = FALSE)













complete(imp2, "long", include = TRUE) %>% 
  mutate(ops = maxOPS*(sin(arcsinops))^2,
         type = paste("Imputation", .imp)) %>% 
  select(player, age, ops, type) %>% 
  full_join(simdata %>% select(player, age, ops) %>% mutate(type = "Original")) %>% 
  group_by(type, player) %>% 
  mutate(diff = ops - lag(ops)) %>% 
  ungroup() %>% 
  group_by(type, age) %>% 
  summarise(meanDiff = mean(diff, na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = meanDiff, color = type)) +
  geom_point() + geom_smooth(se = FALSE)
