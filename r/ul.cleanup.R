## Relief Ultrasound analysis

## Packages

library(tidyverse); library(readxl)

################## Data import #############################

ul.data <- reliefdata::relief_thickness

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

ul.dat <- ul.data %>%
  select(participant, leg, time, VLthick, VIthick, age.grp, allocation) %>%
  full_join(rand) %>%
  filter(!participant %in% c("3033", "3046")) %>% # removes participants with no data
  group_by(participant, leg, time, age.grp, allocation, condition) %>%
  summarise(VLthick = mean(VLthick),
            VIthick = mean(VIthick))


saveRDS(ul.dat, "./data/data-gen/ul.dat.RDS")





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

saveRDS(vl.logchange, "./data/data-gen/vl.logchange.RDS")



vi.logchange <- ul.dat %>%
  select(!VLthick) %>%
  group_by(participant, allocation, condition, time, age.grp) %>%
  summarise(meanVI = mean(VIthick, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = meanVI) %>%
  mutate(change.2 = log(mid) - log(pre),
         change.3 = log(post) - log(pre),
         pre = pre - mean(pre, na.rm = TRUE)) %>%
  select(participant, allocation, condition, age.grp, pre, change.2, change.3) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = c(pre:change.3)) %>%
  print()

saveRDS(vi.logchange, "./data/data-gen/vi.logchange.RDS")

