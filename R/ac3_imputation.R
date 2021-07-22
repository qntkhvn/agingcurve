library(mice)

# out of the league at age 25 if ops below 0.2
ops25 <- sim_df %>%
  filter(age == 25 & ops < 0.2)

out25 <- sim_df %>%
  mutate(ops = ifelse(player %in% ops25$player & age > 25, NA, ops),
         arcsinops = ifelse(is.na(ops), NA, arcsin)) %>% 
  select(player, age, arcsinops)

ini_out25 <- mice(out25, maxit = 0)
pred_out25 <- ini_out25$predictorMatrix
pred_out25["arcsinops", ] <- c(-2, 2, 0)

imp_out25 <- out25 %>% 
  mice(predictorMatrix = pred_out25, 
       method = "2l.norm", 
       seed = 1234)

# densityplot(imp_out25, ~arcsinops)
# densityplot(imp_out25, ~ arcsinops | .imp)
# imp_data_start2 <- complete(imp_out25) %>% mutate(ops = maxOPS * sin(arcsinops)^2)
# 
# # plot observed and imputed data
# plot(density(sim_df$ops), xlim = c(-1, 2))
# lines(density(out25$ops[!is.na(out25$ops)]), col = "red", lwd = 2)
# lines(density(imp_data_start2$ops), col = "red", lwd = 2)

out25_comp <- imp_out25 %>% 
  complete("long", include = TRUE) %>% 
  mutate(ops = maxOPS * (sin(arcsinops)) ^ 2)


out25_comp %>% 
  mutate(type = paste("Imputation", .imp)) %>% 
  select(player, age, ops, type) %>% 
  full_join(sim_df %>% 
              select(player, age, ops) %>% 
              mutate(type = "Original")) %>%
  group_by(type, age) %>%
  summarise(ops = mean(ops, na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = ops, color = type)) +
  geom_point() + 
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom")



# rubin's combining rules

m <- 5
rubin_df <- complete(imp_out25, "long") %>% 
  filter(age == 20) %>% 
  group_by(.imp, age) %>%
  summarize(mean_arcsin = mean(arcsinops),
            sd_arcsin = sd(arcsinops),
            n = n(),
            se = sd_arcsin/sqrt(n)) %>% 
  ungroup() %>% 
  group_by(age) %>% 
  summarize(avg = mean(mean_arcsin),
            vw = sum(se) / m, # Within imputation variance
            vb = sum((mean_arcsin - avg) ^ 2) / (m - 1), # Between imputation variance
            vt = vw + vb + vb / m, # total variablitiy
            se_pooled = sqrt(vt), # se pooled
            lambda = (vb + vb / m) / vt,
            riv = (vb + vb / m) / vw,
            df_old = (m - 1) / lambda ^ 2, 
            upper = avg + qt(0.975, df_old) * se_pooled,
            lower = avg - qt(0.975, df_old) * se_pooled) %>% 
  transmute(age,
            ops = max_ops * (sin(avg)) ^ 2,
            ops_upper = max_ops * (sin(upper)) ^ 2,
            ops_lower = max_ops * (sin(lower)) ^ 2) %>% 
  mutate(type = "combined")

rubin_df %>% 
  ggplot(aes(x = age, y = ops)) +
  geom_point() +
  geom_errorbar(aes(ymin = ops_lower, ymax = ops_upper)) +
  ylim(c(0, 1))

p +
  geom_point(data = rubin_df, aes(x = age, y = ops)) +
  geom_smooth(data = rubin_df, aes(x = age, y = ops), se = FALSE) +
  geom_errorbar(data = rubin_df, aes(ymin = ops_lower, ymax = ops_upper))



# delta/derivative

# der <- sim_df %>% 
#   group_by(player) %>% 
#   mutate(prev = lag(ops),
#          diff = ops - prev)
# 
# temp <- der %>%
#   filter(age == 25 & ops < 0.2)
# 
# drop25 <- der %>%
#   mutate(ops = ifelse(player %in% temp$player & age > 25, NA, ops),
#          arcsinops = ifelse(is.na(ops), NA, arcsin),
#          prev = ifelse(is.na(ops), NA, prev),
#          diff = ifelse(is.na(ops), NA, diff)) %>% 
#   filter(age != 20) %>% 
#   select(player, age, diff)
# 
# 
# ini_der <- mice(drop25, maxit = 0)
# pred_der <- ini_der$pred
# pred_der["diff", ] <- c(-2, 2, 0)
# imp_der <- mice(drop25, pred = pred_der, method = "2l.norm")
# 
# complete(imp_der, "long", include = TRUE) %>% 
#   group_by(.imp, age) %>%
#   summarise(mean_diff = mean(diff, na.rm = TRUE)) %>% 
#   ggplot(aes(x = age, y = mean_diff, color = factor(.imp))) +
#   geom_point() + geom_smooth(se = FALSE)
# 
# complete(imp_out25, "long", include = TRUE) %>% 
#   mutate(ops = maxOPS*(sin(arcsinops))^2,
#          type = paste("Imputation", .imp)) %>% 
#   select(player, age, ops, type) %>% 
#   full_join(sim_df %>% select(player, age, ops) %>% mutate(type = "Original")) %>% 
#   group_by(type, player) %>% 
#   mutate(diff = ops - lag(ops)) %>% 
#   ungroup() %>% 
#   group_by(type, age) %>% 
#   summarise(meanDiff = mean(diff, na.rm = TRUE)) %>% 
#   ggplot(aes(x = age, y = meanDiff, color = type)) +
#   geom_point() + geom_smooth(se = FALSE)
