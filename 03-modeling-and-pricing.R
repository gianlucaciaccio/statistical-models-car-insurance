rm(list = ls())
load("data-preparation.RData")
library(pscl)
library(MASS)
library(dplyr)
library(tidyverse)
library(AER)
library(caret)
library(randomForest)
library(vcd)
library(vcdExtra)
library(conflicted) # avoid conflicts among packages
conflicted::conflict_prefer(name = "select", winner = "dplyr")
conflicted::conflict_prefer(name = "filter", winner = "dplyr")


#### Variable selection with Random Forest ####
# Calculate outcomes for RF count data model selection
data_freqTRAIN <- data_freqTRAIN %>%
  mutate(claim = as.factor(ifelse(ClaimNb>0, "Claim", "NoClaim")),
         freq = (ClaimNb+1e-10)/Exposure)

data_freqTEST <- data_freqTEST %>%
  mutate(claim = as.factor(ifelse(ClaimNb>0, "Claim", "NoClaim")),
         freq = (ClaimNb+1e-10)/Exposure)

# Down-sampling data_freqTRAIN for classification RF
set.seed(987)
data_freqTRAINdown <- downSample(x = select(data_freqTRAIN, car_age:brand,gas),
                                 y = data_freqTRAIN$claim, list = FALSE,
                                 yname = "claim")


# Random Forest regression with 'freq' 
rf_freq <- randomForest(freq ~ driver_age + car_age + density +
                          region + brand + power + gas,
                        data = data_freqTRAIN, ntree = 50)

rf_freq

data.frame(rf_freq$importance) %>%
  arrange(desc(IncNodePurity))

varImpPlot(rf_freq)
abline(v = 6000, col = "red")

rf_freq.p <- predict(rf_freq, newdata = data_freqTEST, type = "response")
RMSE(rf_freq.p, data_freqTEST$freq)
MAE(rf_freq.p, data_freqTEST$freq)

save.image("modeling-RF.RData")

# Random Forest classification with 'claim' 
rf_claim <- randomForest(claim ~ driver_age + car_age + density +
                          region + brand + power + gas,
                         data = data_freqTRAINdown)

rf_claim

data.frame(rf_claim$importance) %>%
  arrange(desc(MeanDecreaseGini))

varImpPlot(rf_claim)
abline(v = 250, col = "red")

rf_claim.p <- predict(rf_claim, newdata = data_freqTEST, type = "response")
confusionMatrix(rf_claim.p, data_freqTEST$claim)

save.image("modeling-RF.RData")

# Random Forest regression with 'ClaimAmount' 
rf_cost <- randomForest(ClaimAmount ~ driver_age + car_age + density +
                          region + brand + power + gas,
                        data = data_costTRAIN)

rf_cost

data.frame(rf_cost$importance) %>%
  arrange(desc(IncNodePurity))

varImpPlot(rf_cost)
abline(v = 1e+09, col = "red")

rf_cost.p <- predict(rf_cost, newdata = data_costTEST, type = "response")
RMSE(rf_cost.p, data_costTEST$ClaimAmount)
MAE(rf_cost.p, data_costTEST$ClaimAmount)

save.image("random-forest-selection.RData")


# Variables selected for frequency and cost of claims:
# driver_age + car_age + density + region + brand


#### Model selection for claim frequency ####
rm(list = ls())
gc()
load("data-preparation.RData")

#### Reorder factors columns ####
# We set as references those with greater exposure  
# (following actuarial practice)
list_df <- lapply(list(data_freq = data_freq,
                       data_freqTRAIN = data_freqTRAIN,
                       data_freqTEST = data_freqTEST,
                       data_cost = data_cost,
                       data_costTRAIN = data_costTRAIN,
                       data_costTEST = data_costTEST),
                  function(x) {x %>%
                      mutate_if(is.factor,
                                ~fct_reorder(., .x = Exposure,
                                             .fun = sum, 
                                             .desc = TRUE))})
# check
lapply(list_df, function(x) {x %>%
    select_if(is.factor) %>%
    sapply(levels)})

# return list elements in global enviroment
list2env(list_df, globalenv())

# check
data_freq %>%
  select_if(is.factor) %>%
  sapply(levels)

rm(list_df)

# Poisson 
pois <- glm(ClaimNb ~ driver_age + car_age + density +
              region + brand + offset(log(Exposure)),
            data = data_freqTRAIN, family = poisson(link = "log"))

poisP <- predict(pois, newdata = data_freqTEST, type = "response")


# Check for over/under-dispersion 
dispersiontest(pois) # over-dispersion (linear var): yes
dispersiontest(pois, 2) # over-dispersion (quadratic var): yes (pvalue: 1%)

dispersiontest(pois, alternative = "less") # under-dispersion (linear var): no
dispersiontest(pois, alternative = "less", 2) # under-dispersion (quadratic var): no

# Test Poisson (H0) vs Zero Inflated Poisson (H1)
countreg::zitest(pois)


# Negative Binomial
nbin <- glm.nb(ClaimNb ~ driver_age + car_age + density +
                 region + brand + offset(log(Exposure)),
               data = data_freqTRAIN)

nbin$theta

nbinP <- predict(nbin, newdata = data_freqTEST, type = "response")


# Zero Inflated Poisson
zip <- zeroinfl(ClaimNb ~ driver_age + car_age + density +
                  region + brand + offset(log(Exposure)) | 
                  driver_age + car_age + density + region + 
                  brand, data = data_freqTRAIN, dist = "poisson")

zipP <- predict(zip, newdata = data_freqTEST, type = "response")


# Hurdle Poisson
hp <- hurdle(ClaimNb ~ driver_age + car_age + density +
               region + brand + offset(log(Exposure)) | 
               driver_age + car_age + density + region + 
               brand, data = data_freqTRAIN, dist = "poisson")

hpP <- predict(hp, newdata = data_freqTEST, type = "response")


# Zero Inflated Negative Binomial
zinb <- zeroinfl(ClaimNb ~ driver_age + car_age + density +
                   region + brand + offset(log(Exposure)) | 
                   driver_age + car_age + density + region + 
                   brand, data = data_freqTRAIN, dist = "negbin")

zinbP <- predict(zinb, newdata = data_freqTEST, type = "response")


# Hurdle Negative Binomial 
hnb <- hurdle(ClaimNb ~ driver_age + car_age + density +
                region + brand + offset(log(Exposure)) | 
                driver_age + car_age + density + region + 
                brand, data = data_freqTRAIN, dist = "negbin")

hnbP <- predict(hnb, newdata = data_freqTEST, type = "response")


# Compare models on training data using AIC
LRstats(pois, nbin, zip, hp, zinb, hnb, sortby = "AIC") # zinb


#### Calculate Dawid-Sebastiani scoring rule and Pearson Chi Square ####
countMetrics <- function(x, mu, theta = NULL) {
  
  if (is.null(theta)) {
    sigma <- mu
  } else {
    sigma <- mu * (1 + mu/theta)
  }
  
  metrics <- c(dss = mean((x-mu)^2/sigma + 2*log(sigma)),
               pchisq = sum((x-mu)^2/sigma))
  
  return(metrics)
  
}

# Model comparison on test sample
freq_testM <- data.frame(rbind(
  pois = countMetrics(data_freqTEST$ClaimNb, poisP),
  nbin = countMetrics(data_freqTEST$ClaimNb, nbinP, theta = nbin$theta),
  zip = countMetrics(data_freqTEST$ClaimNb, zipP), 
  hp = countMetrics(data_freqTEST$ClaimNb, hpP), 
  zinb = countMetrics(data_freqTEST$ClaimNb, zinbP,  theta = zinb$theta),
  hnb = countMetrics(data_freqTEST$ClaimNb, hnbP, theta = hnb$theta))) %>%
  arrange(dss)

freq_testM


#### Gamma regression for severity ####
# Standard
gamma <- glm(ClaimAmount ~ driver_age + car_age + density +
              region + brand, data = data_costTRAIN, 
             family = Gamma(link = "log"))

gammaP <- predict(gamma, newdata = data_costTEST, type = "response")


# With weights
gamma_W <- glm(ClaimAmount ~ driver_age + car_age + density +
                 region + brand, data = data_costTRAIN, 
               family = Gamma(link = "log"), weights = ClaimNb)

gamma_WP <- predict(gamma_W, newdata = data_costTEST, type = "response")


# With offset
gamma_Of <- glm(ClaimAmount ~ driver_age + car_age + density +
                  region + brand, data = data_costTRAIN, 
                family = Gamma(link = "log"), offset = log(ClaimNb))

gamma_OfP <- predict(gamma_Of, newdata = data_costTEST, type = "response")


# Comparison on training (AIC)
AIC(gamma, gamma_W, gamma_Of)

# Comparison on test (MAE, RMSE)
t(rbind(MAE = sapply(list(gamma = gammaP, gamma_W = gamma_WP,
                        gamma_Of = gamma_OfP), function(x) 
                          {MAE(x, data_costTEST$ClaimAmount)}),
        RMSE = sapply(list(gamma = gammaP, gamma_W = gamma_WP,
                           gamma_Of = gamma_OfP), function(x) 
                             {RMSE(x, data_costTEST$ClaimAmount)})))

# Better specification with offset term 
# ClimNb is like the 'exposure' term for the severity (average claim cost)
# See Ohlsson & Johansson (2010) [pag. 6]
# "Non-Life Insurance Pricing with Generalized Linear Models"

save.image("modeling-and-pricing.RData")

#### Pricing ####
data_freq <- data_freq %>%
  mutate(ClaimNbP = predict(zip, newdata = data_freq, type = "response"))

countMetrics(data_freq$ClaimNb, data_freq$ClaimNbP)


data_cost <- data_cost %>%
  mutate(ClaimAmountP = predict(gamma_Of, newdata = data_cost, type = "response"))

c(MAE = MAE(data_cost$ClaimAmountP, data_cost$ClaimAmount),
  RMSE = RMSE(data_cost$ClaimAmountP, data_cost$ClaimAmount))


data_pricing <- data_freq %>%
  left_join(data_cost)  %>%
  mutate(ClaimAmount = ifelse(is.na(ClaimAmount),0,ClaimAmount),
         ClaimAmountP = ifelse(is.na(ClaimAmountP),0,ClaimAmountP)) %>%
  group_by(driver_age, car_age, density, region, brand) %>%
  summarize(NumPolicy = n(),
            NumClaim = sum(ClaimNb),
            Exposure = sum(Exposure),
            CostClaim = sum(ClaimAmount),
            NumClaimP = sum(ClaimNbP),
            CostClaimP = sum(ClaimAmountP),
            Frequency = sum(NumClaim/Exposure),
            Severity = sum(CostClaim/NumClaim)) %>%
  ungroup() %>%
  mutate(Severity = ifelse(is.nan(Severity),0,Severity),
         Premium = Frequency*Severity,
         FrequencyP = NumClaimP/Exposure,
         SeverityP = CostClaimP/NumClaimP,
         SeverityP = ifelse(is.nan(SeverityP),0,SeverityP),
         PremiumP = FrequencyP*SeverityP)

c(MAE = MAE(data_pricing$PremiumP, data_pricing$Premium),
  RMSE = RMSE(data_pricing$PremiumP, data_pricing$Premium))

c(MAE = MAE(data_pricing$PremiumP[data_pricing$NumClaim>0],
            data_pricing$Premium[data_pricing$NumClaim>0]),
  RMSE = RMSE(data_pricing$PremiumP[data_pricing$NumClaim>0],
              data_pricing$Premium[data_pricing$NumClaim>0]))


save.image("modeling-and-pricing.RData")




#### Calculate Relativities ####
# Refitting the model choosen from Cross Validation on the full datasets
zip_rel <- update(zip, data = data_freq)
gamma_rel <- update(gamma_Of, data = data_cost)

# Calculate relativities using the coefficient estimates and setting
# the base levels for each risk factor/categorical variable equal to one

data_rel <- data_freq %>%
  select(driver_age,car_age,density,region,brand, Exposure, ClaimNb) %>%
  pivot_longer(cols = -c("Exposure","ClaimNb"), 
               names_to = "RiskFactor",
               values_to = "Level") %>%
  group_by(RiskFactor, Level) %>%
  summarize(Exposure = sum(Exposure), NumClaim = sum(ClaimNb)) %>%
  mutate(Variable  = paste(RiskFactor, Level, sep = "")) %>%
  ungroup()


freq_rel <- data.frame(Variable = names(zip$coefficients$count)[-1],
                       Rel_Freq=exp(zip$coefficients$count[1] +
                                      zip$coefficients$count[-1]) / 
                         exp(zip$coefficients$count[1]),
                       row.names = NULL)

sev_rel <- data.frame(Variable = names(gamma_rel$coefficients)[-1],
                      Rel_Sev=exp(gamma_rel$coefficients[1] +
                                    gamma_rel$coefficients[-1]) / 
                        exp(gamma_rel$coefficients[1]),
                      row.names = NULL)

data_rel <- data_rel %>%
  left_join(freq_rel, by = "Variable") %>%
  left_join(sev_rel, by = "Variable") %>%
  replace(is.na(.), 1) %>%
  mutate(Variable = NULL, Rel_Premium = Rel_Freq*Rel_Sev,
         Level = as.factor(c(levels(data_freq$Brand),
                             levels(data_freq$car_age),
                             levels(data_freq$density),
                             levels(data_freq$driver_age),
                             levels(data_freq$Region))))


print(data_rel, n = 33)


save.image("modeling-and-pricing.RData")
