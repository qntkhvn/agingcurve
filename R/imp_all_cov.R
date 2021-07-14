maxG <- max(joined$G, na.rm = TRUE)
maxAB <- max(joined$AB, na.rm = TRUE)
maxR <- max(joined$R, na.rm = TRUE)
maxH <- max(joined$H, na.rm = TRUE)
maxX2B <- max(joined$X2B, na.rm = TRUE)
maxX3B <- max(joined$X3B, na.rm = TRUE)
maxHR <- max(joined$HR, na.rm = TRUE)
maxRBI <- max(joined$RBI, na.rm = TRUE)
maxSB <- max(joined$SB, na.rm = TRUE)
maxCS <- max(joined$CS, na.rm = TRUE)
maxBB <- max(joined$BB, na.rm = TRUE)
maxSO <- max(joined$SO, na.rm = TRUE)
maxIBB <- max(joined$IBB, na.rm = TRUE)
maxHBP <- max(joined$HBP, na.rm = TRUE)
maxSH <- max(joined$SH, na.rm = TRUE)
maxSF <- max(joined$SF, na.rm = TRUE)
maxGIDP <- max(joined$GIDP, na.rm = TRUE)
maxPA <- max(joined$PA, na.rm = TRUE)

df2_new <- joined %>% 
  mutate(HRrate = HR/AB,
         arcsinHRrate = asin(sqrt(HRrate)),
         arcsinBA = asin(sqrt(BA)),
         arcsinG = asin(sqrt(G/maxG)),
         arcsinAB = asin(sqrt(AB/maxAB)),
         arcsinR = asin(sqrt(R/maxR)),
         arcsinH = asin(sqrt(H/maxH)),
         arcsinX2B = asin(sqrt(X2B/maxX2B)),
         arcsinX3B = asin(sqrt(X3B/maxX3B)),
         arcsinHR = asin(sqrt(HR/maxHR)),
         arcsinRBI = asin(sqrt(RBI/maxRBI)),
         arcsinSB = asin(sqrt(SB/maxSB)),
         arcsinCS = asin(sqrt(CS/maxCS)),
         arcsinBB = asin(sqrt(BB/maxBB)),
         arcsinSO = asin(sqrt(SO/maxSO)),
         arcsinIBB = asin(sqrt(IBB/maxIBB)),
         arcsinHBP = asin(sqrt(HBP/maxHBP)),
         arcsinSH = asin(sqrt(SH/maxSH)),
         arcsinSF = asin(sqrt(SF/maxSF)),
         arcsinGIDP = asin(sqrt(GIDP/maxGIDP)),
         arcsinPA = asin(sqrt(PA/maxPA))) %>% 
  select(pnum, age, contains("arcsin"))

ini2_new <- mice(df2_new, maxit = 0)
pred2_new <- ini2_new$pred
pred2_new["arcsinOPS", ] <- c(-2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
pred2_new["arcsinHRrate", ] <- c(-2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
pred2_new["arcsinBA", ] <- c(-2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
pred2_new["arcsinG", ] <- c(-2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
pred2_new["arcsinAB", ] <- c(-2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
pred2_new["arcsinR", ] <- c(-2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
pred2_new["arcsinH", ] <- c(-2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
pred2_new["arcsinX2B", ] <- c(-2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
pred2_new["arcsinX3B", ] <- c(-2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
pred2_new["arcsinHR", ] <- c(-2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
pred2_new["arcsinRBI", ] <- c(-2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
pred2_new["arcsinSB", ] <- c(-2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2)
pred2_new["arcsinCS", ] <- c(-2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2, 2)
pred2_new["arcsinBB", ] <- c(-2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2, 2)
pred2_new["arcsinSO", ] <- c(-2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2, 2)
pred2_new["arcsinIBB", ] <- c(-2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2, 2)
pred2_new["arcsinHBP", ] <- c(-2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2, 2)
pred2_new["arcsinSH", ] <- c(-2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2, 2)
pred2_new["arcsinSF", ] <- c(-2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2, 2)
pred2_new["arcsinGIDP", ] <- c(-2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 2)
pred2_new["arcsinPA", ] <- c(-2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0)

set.seed(100)
start <- Sys.time()
imp2_new <- mice(df2_new, pred = pred2_new, method = "2l.norm")
end <- Sys.time()
end - start




imp_all_cov <- read_csv("~/Desktop/imp_all_cov.csv")

imp_all_cov %>% 
  group_by(.imp, age) %>%
  summarise(meanOps = mean(OPS, na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = meanOps, color = factor(.imp))) +
  geom_point() + geom_smooth(method = "lm",
                             formula = y ~ x + I(x^2) + I(x^3)) +
  ggtitle("Real data, imputed with covariates")









