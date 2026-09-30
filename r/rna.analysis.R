## total RNA analysis


# Packages
library(tidyverse); library(readxl)

################## Data import #############################

totrna.dat <- reliefdata::relief_rnaconc

participants <- reliefdata::relief_participants %>%
  select(participant, age, allocation) %>%
  mutate(participant = as.numeric(participant))


rand <- read_excel("./data/relief_randomization.xlsx") %>%
  pivot_longer(names_to = "condition",
               values_to = "leg",
               cols = c(low.leg:mod.arm)) %>%
  select(participant = Participant, condition, leg) %>%
  filter(!condition %in% c("low.arm", "mod.arm")) %>%
  pivot_wider(names_from = condition,
              values_from = leg) %>%
  select(participant, low = low.leg, mod = mod.leg) %>%
  pivot_longer(names_to = "condition",
               values_to = "leg",
               cols = c(low, mod)) %>%
  full_join(participants) %>%
  mutate(age = as.character(age)) %>%
  mutate(age.grp = if_else(age < "65",
                           "yng",
                           if_else(age > "65",
                                   "old", age))) %>%
  print()






####################### Data sorting #####################

rna.dat <- totrna.dat %>%
  filter(!sample == "Blank1") %>%
  full_join(rand) %>%
  select(participant, leg, condition, allocation, time, conc, weight, purity,
         sample, age.grp) %>%
  #filter(!allocation == "con") %>%
  mutate(totalrna = conc * 4 * 25, # adds the dilution factor (1:4) and eluate
         rna.weight = totalrna/weight) %>%  
  select(participant:time, weight:rna.weight) %>%
  filter(!sample %in% c("68", "76", "111", "108", # No data
                        "157", "164", "166"),
         !participant %in% c("3024", "3072")) # Missing values
  #filter(!purity <1.9) %>% # consider filtering out samples of too low purity, not sure where to set the threshold

rna.dat %>%
  select(participant, sample, age.grp, purity, totalrna) %>%
  filter(sample == "161") %>%




############### Change score calculation ############# 

## Log transformation for analysis 

#change.dat <- rna.dat %>%
#  group_by(participant, allocation, condition, time, age.grp) %>%
#  summarise(meanrna = mean(totalrna, na.rm = TRUE)) %>%
#  pivot_wider(names_from = time,
#              values_from = meanrna) %>%
#  filter(!participant %in% c("3004", "3005", "3010", "3011",
#                             "3016", "3018", "3019", "3021",
#                             "3023", "3026", "3028", "3033",
#                             "3034", "3035", "3038", "3039",
#                             "3040", "3046", "3052", "3053",
#                             "3056", "3058", "3061", "3068",
#                             "3069", "3089", "3092", "3093")) %>% # missing values
#  mutate(change.2 = log(w3) - log(pre),
#         change.3 = log(post) - log(pre),
#         pre = pre - mean(pre, na.rm = TRUE)) %>%
#  select(participant, allocation, condition, age.grp, pre, change.2, change.3) %>%
#  pivot_longer(names_to = "time",
#               values_to = "change",
#               cols = c(pre:change.3)) %>%
#  print()



## Mean values at the respective time points

mean.values <- rna.dat %>%
  filter(allocation == "int") %>%
  group_by(time, age.grp) %>%
  summarise(meanrna = mean(totalrna, na.rm = TRUE),
            sdrna = sd(totalrna, na.rm = TRUE)) %>%
  print()


## Mean change from pre to w3, and pre to post

mean.datchange <- rna.dat %>%
  group_by(participant, allocation, condition, time, age.grp) %>%
  summarise(meanrna = mean(totalrna, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = meanrna) %>%
 # filter(!participant %in% c("3004", "3005", "3010", "3011",
 #                            "3016", "3018", "3019", "3021",
 #                            "3023", "3026", "3028", "3033",
 #                            "3034", "3035", "3038", "3039",
 #                            "3040", "3046", "3052", "3053",
 #                            "3056", "3058", "3061", "3068",
 #                            "3069", "3089", "3092", "3093")) %>% # missing values
  mutate(change.2 = w3 - pre,
         change.3 = post - pre,
         pre = pre - mean(pre, na.rm = TRUE)) %>%
  select(participant, allocation, condition, age.grp, pre, change.2, change.3) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = c(pre:change.3)) %>%
  filter(!allocation == "con") %>%
  group_by(age.grp, time) %>%
  summarise(meanc = mean(change, na.rm = TRUE),
            sdc = sd(change, na.rm = TRUE)) %>%
  print()





### Model from Daniel

dat <- rna.dat %>%
  mutate(time = factor(time, levels = c("pre", "w3", "post")),
         age.grp = factor(age.grp, levels = c("yng", "old"))) %>%
  filter(allocation == "int") %>%
  print()


# A model without volume effect
m <- lmer(log(totalrna) ~ age.grp * time + (1 | participant), 
          data = dat)

summary(m)



# The overall difference between young and old in change from baseline in 
# percentage is
# (exp(coef) - 1) * 100

# 3 week
(exp(coef(summary(m))[5,1]) - 1) * 100

# post
(exp(coef(summary(m))[6,1]) - 1) * 100

# Use the same transformation to confidence intervals
# on the percentage point scale. 

ci <- confint(m)

# 3 week
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

##################### STATISTICS #########################



#################### Age comparisons ####################

## T-tests for comparisons and CI?

# Yng vs. old low volume pre vs. w3

low.w3 <- change.dat %>%
  select(participant, allocation, condition, age.grp, 
         time, change) %>%
  filter(!allocation == "con",
         !time == "change.3",
         condition == "low") %>%
  print()

agelow.w3.res <- t.test(change ~ age.grp, paired = FALSE, data = low.w3) 

agelow.w3.ci <- agelow.w3.res$conf.int


# Yng vs. old low volume pre vs. post

low.post <- change.dat %>%
  select(participant, allocation, condition, age.grp, 
         time, change) %>%
  filter(!allocation == "con",
         !time == "change.2",
         condition == "low") %>%
  print()

agelow.post.res <- t.test(change ~ age.grp, paired = FALSE, data = low.post) 

agelow.post.ci <- agelow.post.res$conf.int
  

# Yng vs. old moderate volume pre vs. w3

mod.w3 <- change.dat %>%
  select(participant, allocation, condition, age.grp, 
         time, change) %>%
  filter(!allocation == "con",
         !time == "change.3",
         condition == "mod") %>%
  print()

agemod.w3.res <- t.test(change ~ age.grp, paired = FALSE, data = mod.w3) 

agemod.w3.ci <- agemod.w3.res$conf.int


# Yng vs. old moderate volume pre vs. post

mod.post <- change.dat %>%
  select(participant, allocation, condition, age.grp, 
         time, change) %>%
  filter(!allocation == "con",
         !time == "change.2",
         condition == "mod") %>%
  print()

agemod.post.res <- t.test(change ~ age.grp, paired = FALSE, data = mod.post)

agemod.post.ci <- agemod.post.res$conf.int



################ Condition comparisons #################


# Low vs. moderate volume in young pre vs. week 3

yng.w3 <- change.dat %>%
  select(participant, allocation, condition, age.grp, 
         time, change) %>%
  filter(!allocation == "con",
         !time == "change.3",
         age.grp == "yng") %>%
  print()

condlow.w3.res <- t.test(change ~ condition, paired = FALSE, data = yng.w3) 

condlow.w3.ci <- condlow.w3.res$conf.int


# Low vs. moderate volume in young pre vs. post

yng.post <- change.dat %>%
  select(participant, allocation, condition, age.grp, 
         time, change) %>%
  filter(!allocation == "con",
         !time == "change.2",
         age.grp == "yng") %>%
  print()

condlow.post.res <- t.test(change ~ condition, paired = FALSE, data = yng.post) 

condlow.post.ci <- condlow.post.res$conf.int


# Low vs. moderate volume in old pre vs. week 3

old.w3 <- change.dat %>%
  select(participant, allocation, condition, age.grp, 
         time, change) %>%
  filter(!allocation == "con",
         !time == "change.3",
         age.grp == "old") %>%
  print()

condmod.w3.res <- t.test(change ~ condition, paired = FALSE, data = old.w3) 

condmod.w3.ci <- condmod.w3.res$conf.int


# Low vs. moderate volume in old pre vs. post

old.post <- change.dat %>%
  select(participant, allocation, condition, age.grp, 
         time, change) %>%
  filter(!allocation == "con",
         !time == "change.2",
         age.grp == "old") %>%
  print()

condmod.post.res <- t.test(change ~ condition, paired = FALSE, data = old.post)

condmod.post.ci <- condmod.post.res$conf.int






















