## Relief Ultrasound analysis

## Packages

library(tidyverse); library(readxl); library(reliefdata)
library(lme4)


################## Data import #############################

ul.dat <- reliefdata::relief_thickness

rand <- read_excel("./data/relief_randomization.xlsx") %>%
  pivot_longer(names_to = "condition",
               values_to = "leg",
               cols = c(low.leg:mod.arm)) %>%
  mutate(Participant = as.character(Participant)) %>%
  select(participant = Participant, condition, leg) %>%
  filter(!condition %in% c("low.arm", "mod.arm")) %>%
  pivot_wider(names_from = condition,
              values_from = leg) %>%
  select(participant, low = low.leg, mod = mod.leg) %>%
  pivot_longer(names_to = "condition",
               values_to = "leg",
               cols = c(low, mod)) %>%
  print()







####################### Data sorting #####################
# The code below joins the condition (low/moderate training volume) into
# the UL data, and summarises all the three measurements taken per leg 
# per timepoint

ul.dat2 <- ul.dat %>%
  select(participant, leg, time, VLthick, VIthick, age.grp, allocation) %>%
  full_join(rand) %>%
  filter(!participant %in% c("3033", "3046")) %>% # removes participants with no data
  group_by(participant, leg, time, age.grp, allocation, condition) %>%
  summarise(VLthick = mean(VLthick),
            VIthick = mean(VIthick))




### Model from Daniel


dat <- ul.dat2 |> 
  mutate(time = factor(time, levels = c("pre", "mid", "post")), 
         age.grp = factor(age.grp, levels = c("yng", "old"))) |> 
  filter(allocation == "int") |> 
  print()
           
# A model without volume effect
m <- lmer(log(VLthick) ~ age.grp * time + (1 | participant), 
          data = dat)
  
summary(m)


# The overall difference between young and old in change from baseline in 
# percentage is
# (exp(coef) - 1) * 100


# Mid
(exp(coef(summary(m))[5,1]) - 1) * 100

# Post
(exp(coef(summary(m))[6,1]) - 1) * 100

# Use the same transformation to confidence intervals
# on the percentage point scale. 

ci <- confint(m)

# Mid
ci.lwr <- (exp(ci[7,1]) - 1) * 100
ci.upr <- (exp(ci[7,2]) - 1) * 100

# Post
ci.lwr <- (exp(ci[8,1]) - 1) * 100
ci.upr <- (exp(ci[8,2]) - 1) * 100

# The estimated within group change from baseline can we get from 
# marginal effects

library(marginaleffects)


# avg_slopes will give you within group estimates
# 
avg_slopes(m, by = "age.grp")






############### Change score calculation ############# 

## Log transformation for analysis

vl.logchange <- ul.dat %>%
  select(!VIthick) %>%
  group_by(participant, allocation, condition, time, age.grp) %>%
  summarise(meanVL = mean(VLthick, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = meanVL) %>%
  mutate(change.2 = log(mid) - log(pre),
         change.3 = log(post) - log(pre),
         pre = pre - mean(pre, na.rm = TRUE)) %>%
  select(participant, allocation, condition, age.grp, pre, change.2, change.3) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = c(pre:change.3)) %>%
  print()




## Non-transformed change scores for reporting

meanchange.vl <- ul.dat2 %>%
  select(!VIthick) %>%
  group_by(participant, allocation, condition, time, age.grp) %>%
  filter(!allocation == "con") %>%
  summarise(meanVL = mean(VLthick, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = meanVL) %>%
  mutate(change.2 = mid - pre,
         change.3 = post - pre,
         pre = pre - mean(pre, na.rm = TRUE)) %>%
  select(participant, allocation, condition, age.grp, pre, change.2, change.3) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = c(pre:change.3)) %>%
  group_by(age.grp, condition, time) %>%
  summarise(meanc = mean(change, na.rm = TRUE),
            sdc = sd(change, na.rm = TRUE)) %>%
  print()

## USE THESE DATA FOR REPORTING






##################### STATISTICS #########################

## T-tests for comparisons and CI?

# Age group comparisons

low.mid <- change.vl %>%
  select(participant, allocation, condition, age.grp, 
         time, change) %>%
  filter(!allocation == "con",
         !time == "change.3",
         condition == "low") %>%
  print()


low.post <- change.vl %>%
  select(participant, allocation, condition, age.grp, 
         time, change) %>%
  filter(!allocation == "con",
         !time == "change.2",
         condition == "low") %>%
  print()


mod.mid <- change.vl %>%
  select(participant, allocation, condition, age.grp, 
         time, change) %>%
  filter(!allocation == "con",
         !time == "change.3",
         condition == "mod") %>%
  print()

mod.post <- change.vl %>%
  select(participant, allocation, condition, age.grp, 
         time, change) %>%
  filter(!allocation == "con",
         !time == "change.2",
         condition == "mod") %>%
  print()





# Condition comparisons

yng.mid <- change.vl %>%
  select(participant, allocation, condition, age.grp, 
         time, change) %>%
  filter(!allocation == "con",
         !time == "change.3",
         age.grp == "yng") %>%
  print()


old.mid <- change.vl %>%
  select(participant, allocation, condition, age.grp, 
         time, change) %>%
  filter(!allocation == "con",
         !time == "change.3",
         age.grp == "old") %>%
  na.omit %>%
  print()


yng.post <- change.vl %>%
  select(participant, allocation, condition, age.grp, 
         time, change) %>%
  filter(!allocation == "con",
         !time == "change.2",
         age.grp == "yng") %>%
  na.omit %>%
  print()


old.post <- change.vl %>%
  select(participant, allocation, condition, age.grp, 
         time, change) %>%
  filter(!allocation == "con",
         !time == "change.2",
         age.grp == "old") %>%
  na.omit %>%
  print()




################# Tests ###############################

#################### Age ####################

# Yng vs. old low volume pre vs. mid change

t.test(change ~ age.grp, paired = FALSE, data = low.mid) 

# Yng vs. old low volume pre vs. post

t.test(change ~ age.grp, paired = FALSE, data = low.post) 

# Yng vs. old mod volume pre vs. week 3

t.test(change ~ age.grp, paired = FALSE, data = mod.mid) 

# Yng vs. old mod volume pre vs. post

t.test(change ~ age.grp, paired = FALSE, data = mod.post) 



#################### Time ####################

# Low vs. moderate volume in young pre vs. week 3

t.test(change ~ condition, paired = TRUE, data = yng.mid) 

# Low vs. moderate volume in young pre vs. post

t.test(change ~ condition, paired = TRUE, data = yng.post) 

# Low vs. moderate volume in old pre vs. week 3

t.test(change ~ condition, paired = TRUE, data = old.w3) 

# Low vs. moderate volume in old pre vs. post

t.test(change ~ condition, paired = TRUE, data = old.post)
