#### Plot 1: Histogram Claim Number ####
ggplot(data_freqTRAIN, aes(ClaimNb)) +
  geom_histogram(binwidth = 0.5, color="black", fill="white") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_economist() +
  theme(axis.title.y = element_text(
    margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(
      margin = margin(t = 20, r = 0, b = 0, l = 0)), 
    title = element_text(size = 10, margin = margin(t = 0, r = 0,
                                                    b = 30, l = 0))) +
  labs(x = "Number of claims", y = "Number of policies",
       title = "Distribution of policies based on number of claims")



#### Mean, variance, dispersion ratio and zeros of ClaimNb ####
data_freqTRAIN %>%
  summarize(no_claim = sum(ClaimNb==0)/n(),
            claim = sum(ClaimNb>0)/n(),
            mean = mean(ClaimNb),
            var = var(ClaimNb),
            ratio = var/mean)

#### Mean, variance, dispersion ratio and zeros of ClaimNb by risk factors ####
data_freqTRAIN %>%
  select(ClaimNb, car_age:brand, gas) %>%
  pivot_longer(cols = -ClaimNb, names_to = "Category",
               values_to = "Level") %>%
  group_by(Category, Level) %>%
  summarize(no_claim = sum(ClaimNb==0)/n(),
            claim = sum(ClaimNb>0)/n(),
            mean = mean(ClaimNb),
            var = var(ClaimNb),
            ratio = var/mean) %>%
  ungroup() %>%
  print(n = 40)


#### Plot 2: Histogram Exposure ####
ggplot(data_freqTRAIN, aes(Exposure)) +
  geom_histogram(binwidth = 0.1, color="black", fill="white") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_economist() +
  theme(axis.title.y = element_text(
    margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(
      margin = margin(t = 20, r = 0, b = 0, l = 0)), 
    title = element_text(size = 10,
                         margin = margin(t = 0, r = 0, b = 30, l = 0))) +
  labs(x = "Duration of policy (exposure)",
       y = "Number of policies",
       title = "Distribution of policies based on exposure")


#### Plot 3: Histogram Claim Amount ####
ggplot(data_costTRAIN, aes(x=ClaimAmount)) +
  geom_histogram(bins = 125, fill="white", color="black") +
  theme_economist() +
  theme(axis.title.y = element_text(
    margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(
      margin = margin(t = 20, r = 0, b = 0, l = 0)), 
    title = element_text(size = 10, margin = margin(t = 0, r = 0,
                                                    b = 30, l = 0))) +
  labs(x = "Cost of claim", y = "Number of policies",
       title = "Distribution of policies based on cost of claims")



#### Plot 4: Distribution of indpendent variables ####
data_freqTRAIN %>%
  select(car_age:brand, gas) %>%
  pivot_longer(cols = everything(), names_to = "variable") %>%
  ggplot(aes(x=value)) +
  geom_bar(color = "black", fill = "white") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  facet_wrap(~ variable, scales = "free") +
  theme_economist() +
  theme(strip.text = element_text(size = 10, face = "bold.italic", 
                                  margin = margin(t = 4, b = 2)),
        axis.text = element_text(size = 6),
        panel.spacing = unit(1, "lines"),
        panel.grid = element_blank()) +
  labs(x = NULL, y = NULL, title = "Distribution of indpendent variables")



#### Plot 5: Average claim costs ####
data_costTRAIN %>%
  select(ClaimAmount, car_age:brand, gas) %>%
  pivot_longer(cols = -ClaimAmount, names_to = "variable") %>%
  group_by(variable, value) %>%
  summarize(sd = sd(ClaimAmount),
            ClaimAmount = mean(ClaimAmount)) %>%
  ggplot(aes(x=fct_rev(value), y = ClaimAmount)) +
  geom_point(color="black",size = 2, shape = 21, fill = NA) + 
  geom_segment(aes(x = value, xend = value, y = 1200, yend = ClaimAmount)) +
  coord_flip() +
  facet_wrap(~ variable, scales = "free") +
  theme_economist() +
  theme(strip.text = element_text(size = 10, face = "bold.italic", 
                                  margin = margin(t = 4, b = 2)),
        axis.text = element_text(size = 6),
        panel.spacing = unit(1, "lines"),
        panel.grid = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_line(size = rel(0.8)),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(-10, "points")) +
  labs(x = NULL, y = NULL, title = "Average Claim Costs")



#### Create poisson regression function from glm ####
glmPois <- partial(glm, formula = ClaimNb~value,
                   family = poisson(link = "log"),
                   offset = log (Exposure))

#### Create predict response function with standard errors ####
predPois <- partial(predict, type = "response", se.fit = TRUE)



#### Annualized Frequency Estimate Data ####
# Create a tibble with column lists where covariate for ClaimNb are in each row
# Iterate a poisson regression for each variable (row)
# Calculate predictions (response) and SE on newdata
# newdata: unique values of the varible and set Exposure = 1
# The predicted values represent the claim frequencies for annual policies
data_poispred <- bind_rows(
  data_freqTRAIN %>%  select(DriverAge, CarAge,Density, ClaimNb, Exposure) %>% 
    pivot_longer(c("DriverAge", "CarAge", "Density"),
                 names_to = "variable",values_to = c("value")) %>% 
    group_by(variable) %>%  nest(), 
  data_freqTRAIN %>% select(driver_age, car_age, density, region, brand, 
                       Power, power, gas, ClaimNb, Exposure) %>% 
    pivot_longer(c("driver_age", "car_age", "density",
                   "region", "brand", "Power", "power", "gas"),
                 names_to = "variable",values_to = c("value")) %>% 
    group_by(variable) %>% nest()) %>%
  mutate(newdata = map(data, ~ .x %>% select(value) %>% 
                         summarize(value = unique(value),
                                   Exposure = 1) %>%
                         arrange(value)),
         poisson = map(data, ~ glmPois(.x)),
         prediction = map2(.x =  poisson, .y = newdata, ~ predPois(.x, .y)),
         newdata = map2(.x =  newdata, .y = prediction,  ~ .x %>% 
                          mutate(Fit = .y$fit, SE = .y$se.fit))) %>%
  select(variable, newdata)


data_poispred




#### Plot 6: Frequency DriverAge ####
data_poispred %>%
  filter(variable=="DriverAge") %>%
  unnest(cols = c(newdata)) %>%
  ggplot(aes(x=value,y=Fit)) +
  geom_line(color="red",size=0.6) + 
  geom_line(aes(x=value,y=Fit-SE), linetype = "dashed", color="gray25") + 
  geom_line(aes(x=value,y=Fit+SE), linetype = "dashed", color="gray25") + 
  geom_ribbon(aes(x = value, ymin = Fit-SE, ymax = Fit+SE),
              alpha = 0.25, fill  = "gray40") +
  theme_economist() +
  theme(axis.title.y = element_text(
    margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(
      margin = margin(t = 20, r = 0, b = 0, l = 0)), 
    title = element_text(
      margin = margin(t = 0, r = 0, b = 30, l = 0))) +
  labs(x = "Age of the driver", y = "Frequency",
       title = "Annualized Claim Frequency Estimate (DriverAge)")




#### Plot 7: Frequency driver_age ####
data_poispred %>%
  filter(variable=="driver_age") %>%
  unnest(cols = c(newdata)) %>%
  ggplot(aes(x=value,y=Fit)) +
  geom_point(color="black",size=2) + 
  geom_errorbar(aes(ymin=Fit-SE, ymax=Fit+SE),color="black",
                width=.2,position=position_dodge(0.05)) +
  theme_economist() +
  theme(title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0))) +
  labs(x = "", y = "",
       title = "Annualized Claim Frequency Estimate (driver_age)") 




#### Plot 8: Frequency CarAge ####
data_poispred %>%
  filter(variable=="CarAge") %>%
  unnest(cols = c(newdata))  %>%
  ggplot(aes(x=value,y=Fit)) +
  geom_line(color="red",size=0.6) + 
  geom_line(aes(x=value,y=Fit-SE), linetype = "dashed", color="gray25") + 
  geom_line(aes(x=value,y=Fit+SE), linetype = "dashed", color="gray25") + 
  geom_ribbon(aes(x = value, ymin = Fit-SE, ymax = Fit+SE),
              alpha = 0.25, fill  = "gray40") +
  theme_economist() +
  theme(axis.title.y = element_text(
    margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(
      margin = margin(t = 20, r = 0, b = 0, l = 0)), 
    title = element_text(
      margin = margin(t = 0, r = 0, b = 30, l = 0))) +
  labs(x = "Age of the car", y = "Frequency",
       title = "Annualized Claim Frequency Estimate (CarAge)")





#### Plot 9: Frequency car_age ####
data_poispred %>%
  filter(variable=="car_age") %>%
  unnest(cols = c(newdata)) %>%
  ggplot(aes(x=value,y=Fit)) +
  geom_point(color="black",size=2) + 
  geom_errorbar(aes(ymin=Fit-SE, ymax=Fit+SE),color="black",
                width=.2,position=position_dodge(0.05)) +
  theme_economist() +
  theme(title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0))) +
  labs(x = "", y = "", title = "Annualized Claim Frequency Estimate (car_age)") 




#### Plot 10: Frequency Density ####
data_poispred %>%
  filter(variable=="Density") %>%
  unnest(cols = c(newdata))  %>%
  ggplot(aes(x=value,y=Fit)) +
  geom_line(color="red",size=0.6) + 
  geom_line(aes(x=value,y=Fit-SE), linetype = "dashed", color="gray25") + 
  geom_line(aes(x=value,y=Fit+SE), linetype = "dashed", color="gray25") + 
  geom_ribbon(aes(x = value, ymin = Fit-SE, ymax = Fit+SE),
              alpha = 0.25, fill  = "gray40") +
  theme_economist() +
  theme(axis.title.y = element_text(
    margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(
      margin = margin(t = 20, r = 0, b = 0, l = 0)), 
    title = element_text(
      margin = margin(t = 0, r = 0, b = 30, l = 0))) +
  labs(x = "Age of the driver", y = "Frequency",
       title = "Annualized Claim Frequency Estimate (Density)")




#### Plot 11: Frequency density ####
data_poispred %>%
  filter(variable=="density") %>%
  unnest(cols = c(newdata)) %>%
  ggplot(aes(x=value,y=Fit)) +
  geom_point(color="black",size=2) + 
  geom_errorbar(aes(ymin=Fit-SE, ymax=Fit+SE),color="black",
                width=.2,position=position_dodge(0.05)) +
  theme_economist() +
  theme(title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0))) +
  labs(x = "", y = "", title = "Annualized Claim Frequency Estimate (density)")






#### Plot 12: Frequency region ####
data_poispred %>%
  filter(variable=="region") %>%
  unnest(cols = c(newdata)) %>%
  ggplot(aes(x=value,y=Fit)) +
  geom_point(color="black",size=2) + 
  geom_errorbar(aes(ymin=Fit-SE, ymax=Fit+SE),color="black",
                width=.2,position=position_dodge(0.05)) +
  theme_economist() +
  theme(title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0))) +
  labs(x = "", y = "", title = "Annualized Claim Frequency Estimate (region)") 






#### Plot 13: Frequency brand ####
data_poispred %>%
  filter(variable=="brand") %>%
  unnest(cols = c(newdata)) %>%
  ggplot(aes(x=value,y=Fit)) +
  geom_point(color="black",size=2) + 
  geom_errorbar(aes(ymin=Fit-SE, ymax=Fit+SE),color="black",
                width=.2,position=position_dodge(0.05)) +
  theme_economist() +
  theme(title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0))) +
  labs(x = "", y = "", title = "Annualized Claim Frequency Estimate (brand)")




#### Plot 14: Frequency Power ####
data_poispred %>%
  filter(variable=="Power") %>%
  unnest(cols = c(newdata)) %>%
  ggplot(aes(x=value,y=Fit)) +
  geom_point(color="black",size=2) + 
  geom_errorbar(aes(ymin=Fit-SE, ymax=Fit+SE),color="black",
                width=.2,position=position_dodge(0.05)) +
  theme_economist() +
  theme(title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0))) +
  labs(x = "", y = "", title = "Annualized Claim Frequency Estimate (Power)")




#### Plot 15: Frequency power ####
data_poispred %>%
  filter(variable=="power") %>%
  unnest(cols = c(newdata))  %>%
  ggplot(aes(x=value,y=Fit)) +
  geom_point(color="black",size=2) + 
  geom_errorbar(aes(ymin=Fit-SE, ymax=Fit+SE),color="black",
                width=.2,position=position_dodge(0.05)) +
  theme_economist() +
  theme(title = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0))) +
  labs(x = "", y = "", title = "Annualized Claim Frequency Estimate (power)")




#### Plot 16: Frequency gas ####
data_poispred %>%
  filter(variable=="gas") %>%
  unnest(cols = c(newdata)) %>%
  ggplot(aes(x=value,y=Fit)) +
  geom_point(color="black",size=2) + 
  geom_errorbar(aes(ymin=Fit-SE, ymax=Fit+SE),color="black",
                width=.2,position=position_dodge(0.05)) +
  theme_economist() +
  theme(title = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0))) +
  labs(x = "", y = "", title = "Annualized Claim Frequency Estimate (gas)")




#### Save workspace ####
save.image(file = "exploratory-data-analysis.RData")
