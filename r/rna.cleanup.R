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


saveRDS(rna.dat, "./data/data-gen/rna.dat.RDS")




############### Change score calculation ############# 

## Log transformation for analysis 

change.dat <- rna.dat %>%
  group_by(participant, allocation, condition, time, age.grp) %>%
  summarise(meanrna = mean(totalrna, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = meanrna) %>%
  #  filter(!participant %in% c("3004", "3005", "3010", "3011",
  #                             "3016", "3018", "3019", "3021",
  #                             "3023", "3026", "3028", "3033",
  #                             "3034", "3035", "3038", "3039",
  #                             "3040", "3046", "3052", "3053",
  #                             "3056", "3058", "3061", "3068",
  #                             "3069", "3089", "3092", "3093")) %>% # missing values
  mutate(change.2 = log(w3) - log(pre),
         change.3 = log(post) - log(pre),
         pre = pre - mean(pre, na.rm = TRUE)) %>%
  select(participant, allocation, condition, age.grp, pre, change.2, change.3) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = c(pre:change.3)) %>%
  print()

saveRDS(change.dat, "./data/data-gen/rna.logchange.RDS")


## Non-transformed change scores for reporting

mean.datchange <- rna.dat %>%
  group_by(participant, allocation, condition, time, age.grp) %>%
  summarise(meanrna = mean(totalrna, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = meanrna) %>%
  #  filter(!participant %in% c("3004", "3005", "3010", "3011",
  #                             "3016", "3018", "3019", "3021",
  #                             "3023", "3026", "3028", "3033",
  #                             "3034", "3035", "3038", "3039",
  #                             "3040", "3046", "3052", "3053",
  #                             "3056", "3058", "3061", "3068",
  #                             "3069", "3089", "3092", "3093")) %>% # missing values
  mutate(change.2 = w3 - pre,
         change.3 = post - pre,
         pre = pre - mean(pre, na.rm = TRUE)) %>%
  select(participant, allocation, condition, age.grp, pre, change.2, change.3) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = c(pre:change.3)) %>%
  filter(!allocation == "con") %>%
  group_by(condition, age.grp, time) %>%
  summarise(meanc = mean(change, na.rm = TRUE),
            sdc = sd(change, na.rm = TRUE)) %>%
  print()