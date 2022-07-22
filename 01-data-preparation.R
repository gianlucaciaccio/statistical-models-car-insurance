library(CASdatasets)
library(dplyr)
library(tidyr)
library(forcats)
library(purrr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(cowplot)
library(fitdistrplus)
library(caret)
data("freMTPLfreq")
data("freMTPLsev")
conflicted::conflict_prefer("summarise", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("margin", "ggplot2")

#### Choosing breaks to cut numeric/ordinal variables into categorical ####
# CarAge
prop.table(table(cut(freMTPLfreq$CarAge,
                     breaks = c(0,2,5,10,15,Inf),
                     include.lowest = T)))

# DriverAge
prop.table(table(cut(freMTPLfreq$DriverAge,
                     breaks = c(18,25,35,50,65,Inf),
                     include.lowest = T)))

# Density
prop.table(table(cut(freMTPLfreq$Density,
                     breaks = c(0,50,100,500,2000,5000,Inf),
                     include.lowest = T)))

# Power
prop.table(table(freMTPLfreq$Power))
prop.table(table(cut(as.numeric(freMTPLfreq$Power),
                     breaks = c(1,4,12),
                     include.lowest = T)))




#### Create data frame from freMTPLfreq with new columns ####
# brand: new Brand's level names
# region: new Region's level names
# convert all characters columns to factor
# Move "other" brand as the last level
data_freq <- freMTPLfreq %>% 
  mutate(PolicyID = as.integer(PolicyID),
         car_age = case_when(
           CarAge <= 2 ~ "[0, 2]",
           CarAge > 2 & CarAge <= 5  ~ "[3, 5]",
           CarAge > 5 & CarAge <= 10  ~ "[6, 10]",
           CarAge > 10 & CarAge <= 15  ~ "[11, 15]",
           TRUE ~ "[16+]"),
         driver_age = case_when(
           DriverAge <= 25 ~ "[18, 25]",
           DriverAge > 25 & DriverAge <= 35  ~ "[26, 35]",
           DriverAge > 35 & DriverAge <= 50  ~ "[36, 50]",
           DriverAge > 50 & DriverAge <= 65  ~ "[61, 65]",
           TRUE ~ "[66+]"),
         region = case_when(
           Region %in% "Aquitaine" ~ "Aq",
           Region %in% "Basse-Normandie" ~ "BNorm",
           Region %in% "Bretagne" ~ "Bre",
           Region %in% "Centre" ~ "Ce",
           Region %in% "Haute-Normandie" ~ "HNorm",
           Region %in% "Ile-de-France" ~ "IdF",
           Region %in% "Limousin" ~ "Lim",
           Region %in% "Nord-Pas-de-Calais" ~ "NPdC",
           Region %in% "Pays-de-la-Loire" ~ "PdlL",
           TRUE ~ "PC"),
         density = case_when(
           Density <= 50 ~ "[0, 50]",
           Density > 50 & Density <= 100  ~ "[51, 100]",
           Density > 100 & Density <= 500  ~ "[101, 500]",
           Density > 500 & Density <= 2000  ~ "[501, 2000]",
           Density > 2000 & Density <= 5000  ~ "[2001, 5000]",
           TRUE ~ "[5001+]"),
         power = case_when(
           Power %in% c("d","e","f","g") ~ "Low",
           TRUE ~ "Med-High"),
         brand = case_when(
           Brand %in% "Renault, Nissan or Citroen"  ~ "RNC",
           Brand %in% "Volkswagen, Audi, Skoda or Seat"  ~ "VASS",
           Brand %in% "Opel, General Motors or Ford" ~ "OGF",
           Brand %in% "Fiat" ~ "F",
           Brand %in% "Mercedes, Chrysler or BMW" ~ "MCB",
           Brand %in% "Japanese (except Nissan) or Korean" ~ "JK",
           TRUE ~ "Other")) %>%
  rename(gas=Gas) %>%
  mutate_if(is.character,as.factor) %>%
  mutate(car_age = fct_relevel(car_age,c("[0, 2]", "[3, 5]",
                                         "[6, 10]", "[11, 15]",
                                         "[16+]")),
         density = fct_relevel(density,c("[0, 50]", "[51, 100]",
                                         "[101, 500]", "[501, 2000]",
                                         "[2001, 5000]", "[5001+]")),
         power = fct_relevel(power,c("Low", "Med-High")),
         Brand = fct_relevel(Brand,"other",after = Inf),
         brand = fct_relevel(brand,"Other",after = Inf))

class(data_freq$PolicyID)


#### See new levels ####
data_freq %>%
  select_if(is.factor) %>%
  sapply(levels)


#### Create data frame from freMTPLsev with new columns ####
# This data set needs to be aggregated because is in long format, 
# i.e each row collects the cost of a sigle claim, but  we want to know
# the cost for each PolicyID

freMTPLsev %>%
  group_by(PolicyID) %>%
  filter(n()>1) %>%
  summarize(n=n())


# Check outliers in Claim Amount (1st and 100th percentile)
data_amount <- freMTPLsev %>%
  group_by(PolicyID) %>%
  mutate(NumPolicy = n(),
         MeanAmount = ClaimAmount/NumPolicy) %>%
  ungroup() %>%
  mutate(Quant = ntile(ClaimAmount, 100),
         Quant_OUT = ifelse(Quant <= 99,"Normal Claims","Large Claims"),
         Quant_OUT = fct_relevel(Quant_OUT,"Large Claims",after = Inf))


data_amount %>%
  group_by(Quant_OUT) %>%
  summarize(Min = min(ClaimAmount),
            QT1 = quantile(ClaimAmount, 0.25),
            Median = median(ClaimAmount),
            QT3 = quantile(ClaimAmount, 0.75),
            Max = max(ClaimAmount),
            Mean = mean(ClaimAmount),
            SD = sd(ClaimAmount),
            NumObs = n()) %>%
  mutate(Prop = NumObs/sum(NumObs)) %>%
  pivot_longer(-Quant_OUT,names_to = "Summary",values_to = "Values") %>%
  pivot_wider(id_cols = Summary,names_from = "Quant_OUT", values_from = "Values")

threshold_Amount <- data_amount %>%
  filter(Quant_OUT == "Normal Claims") %>%
  summarize(High = max(ClaimAmount))



#### Plot 1: Percentile vs log Claim Amount ####
data_amount %>% 
  ggplot(aes(x = Quant, y = log(ClaimAmount))) +
  geom_point(shape = 21, color = "dodgerblue4", size = 4) +
  geom_abline(slope = 0, intercept = log(threshold_Amount$High),
              color = "black") +
  annotate("text", x = 70, y = 10.1, size = 4,
           label = "Min value 100th percentile: \u20ac 16157") +
  theme_economist() +
  theme(line = element_blank(),
        axis.title.y = element_text(margin = margin(r = 30)),
        axis.title.x = element_text(margin = margin(t = 30)), 
        title = element_text(size = 12, margin = margin(b = 30))) +
  labs(x = "Percentile", y = "Claim Amount (log scale)",
       title = "Outliers by percentile")


#### Delete outliers: 1° and  > 100° percentile ####
OutPolicy <- data_amount %>%
  select(PolicyID, ClaimAmount) %>%
  group_by(PolicyID) %>%
  summarize(ClaimAmount = sum(ClaimAmount)) %>%
  ungroup() %>%
  left_join(count(data_amount,PolicyID, name = "NumPolicy")) %>%
  filter(NumPolicy == 1 & ClaimAmount > threshold_Amount$High |
           NumPolicy == 2 & ClaimAmount > 2*threshold_Amount$High |
           NumPolicy == 3 & ClaimAmount > 3*threshold_Amount$High) %>%
  select(PolicyID)

#### Create new aggregated data.frame for ClaimAmount: data_cost ####
data_cost <- data_amount %>%
  group_by(PolicyID) %>%
  summarize(ClaimAmount = sum(ClaimAmount)) %>%
  anti_join(OutPolicy)


# Delete outliers from the 'data_freq' too 
data_freq <- data_freq %>%
  anti_join(OutPolicy)

# Join to 'data_cost' variables for models form 'data_freq' 
data_cost <- data_cost %>%
  left_join(data_freq, by = "PolicyID")

table(data_freq$ClaimNb)
table(data_cost$ClaimNb)

#### Remove original data sets from CASdataset package ####
rm(freMTPLfreq, freMTPLsev)

#### Save entire workspace ####
save.image(file = "data-preparation.RData")


#### Split data_freq into train and test sample ####
set.seed(123)
freq_split <- createDataPartition(data_freq$ClaimNb, p = 0.7, list = FALSE)

data_freqTRAIN <- data_freq[freq_split,]
data_freqTEST <- data_freq[-freq_split,]

round(rbind(FULL=prop.table(table(data_freq$ClaimNb)),
      TRAIN=prop.table(table(data_freqTRAIN$ClaimNb)),
      TEST=prop.table(table(data_freqTEST$ClaimNb))),5)

round(rbind(FULL=prop.table(table(data_freq$driver_age)),
            TRAIN=prop.table(table(data_freqTRAIN$driver_age)),
            TEST=prop.table(table(data_freqTEST$driver_age))),3)

round(rbind(FULL=prop.table(table(data_freq$car_age)),
            TRAIN=prop.table(table(data_freqTRAIN$car_age)),
            TEST=prop.table(table(data_freqTEST$car_age))),3)

round(rbind(FULL=prop.table(table(data_freq$density)),
            TRAIN=prop.table(table(data_freqTRAIN$density)),
            TEST=prop.table(table(data_freqTEST$density))),3)

round(rbind(FULL=prop.table(table(data_freq$region)),
            TRAIN=prop.table(table(data_freqTRAIN$region)),
            TEST=prop.table(table(data_freqTEST$region))),3)

round(rbind(FULL=prop.table(table(data_freq$brand)),
            TRAIN=prop.table(table(data_freqTRAIN$brand)),
            TEST=prop.table(table(data_freqTEST$brand))),3)

round(rbind(FULL=prop.table(table(data_freq$power)),
            TRAIN=prop.table(table(data_freqTRAIN$power)),
            TEST=prop.table(table(data_freqTEST$power))),3)

round(rbind(FULL=prop.table(table(data_freq$gas)),
            TRAIN=prop.table(table(data_freqTRAIN$gas)),
            TEST=prop.table(table(data_freqTEST$gas))),3)


#### Split data_cost into train and test sample ####
set.seed(123)
cost_split <- createDataPartition(data_cost$ClaimAmount, p = 0.7, list = FALSE)

data_costTRAIN <- data_cost[cost_split,]
data_costTEST <- data_cost[-cost_split,]



round(rbind(FULL=prop.table(table(data_freq$ClaimNb)),
            TRAIN=prop.table(table(data_freqTRAIN$ClaimNb)),
            TEST=prop.table(table(data_freqTEST$ClaimNb))),5)

round(rbind(FULL=prop.table(table(data_cost$driver_age)),
            TRAIN=prop.table(table(data_costTRAIN$driver_age)),
            TEST=prop.table(table(data_costTEST$driver_age))),3)

round(rbind(FULL=prop.table(table(data_cost$car_age)),
            TRAIN=prop.table(table(data_costTRAIN$car_age)),
            TEST=prop.table(table(data_costTEST$car_age))),3)

round(rbind(FULL=prop.table(table(data_cost$density)),
            TRAIN=prop.table(table(data_costTRAIN$density)),
            TEST=prop.table(table(data_costTEST$density))),3)

round(rbind(FULL=prop.table(table(data_cost$region)),
            TRAIN=prop.table(table(data_costTRAIN$region)),
            TEST=prop.table(table(data_costTEST$region))),3)

round(rbind(FULL=prop.table(table(data_cost$brand)),
            TRAIN=prop.table(table(data_costTRAIN$brand)),
            TEST=prop.table(table(data_costTEST$brand))),3)

round(rbind(FULL=prop.table(table(data_cost$power)),
            TRAIN=prop.table(table(data_costTRAIN$power)),
            TEST=prop.table(table(data_costTEST$power))),3)

round(rbind(FULL=prop.table(table(data_cost$gas)),
            TRAIN=prop.table(table(data_costTRAIN$gas)),
            TEST=prop.table(table(data_costTEST$gas))),3)



#### Save entire workspace ####
save.image(file = "data-preparation.RData")
