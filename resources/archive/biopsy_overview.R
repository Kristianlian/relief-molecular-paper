## Biopsy overview

# Script is used to count/check the amount of samples in our biopsy data

##--- DNA samples check --------------------------------------------------------



#--- Packages ------------------------------------------------------------------

library(tidyverse); library(readxl)


#--- Data ----------------------------------------------------------------------

biop_dat <- read_excel("./data/biopsy_overview.xlsx")


#--- Wrangling -----------------------------------------------------------------


# Long format
dna_long <- biop_dat %>%
  select(participant, leg, t1dna, t2dna, t3dna) %>%
  pivot_longer(cols = c(t1dna, t2dna, t3dna),
               names_to = "timepoint",
               values_to = "value") %>%
  filter(value != "no") #
# Suggests we have 374 samples in total


# Overall count per timepoint
dna_long %>%
  count(timepoint)

# 148 at baseline, 117 at 3w, 109 at post


# Count per participant (across both legs)
dna_long %>%
  count(participant, timepoint)
# "Full sets" equals two samples per timepoint (1 per leg)


# Who has a full set for at least one leg (all three timepoints, for at least one leg)

full_set1 <- dna_long %>%
  group_by(participant) %>%
  summarise(timepoints_present = n_distinct(timepoint)) %>%
  mutate(complete = timepoints_present == 3)

full_set1 %>%
  filter(complete == "TRUE") %>%
  count(complete)

# 56 participants with full set for at least 1 leg


# Who has a full set for both legs

full_set2 <- dna_long %>%
  group_by(participant, leg) %>%
  summarise(timepoints_present = n_distinct(timepoint), .groups = "drop") %>%
  group_by(participant) %>%
  summarise(complete = all(timepoints_present == 3) & n() == 2)

full_set2 %>%
  filter(complete == "TRUE") %>%
  count(complete)


# 34 participants with full set for both legs


# Who has at least baseline and week 3?

partial_set <- dna_long %>%
  group_by(participant, leg) %>%
  summarise(timepoints_present = list(unique(timepoint)), .groups = "drop") %>%
  group_by(participant) %>%
  summarise(twotps = all(map_lgl(timepoints_present,
                                              ~ all(c("t1dna", "t2dna") %in% .x))) & n() == 2) #twotps = two timepoints

partial_set %>%
  filter(twotps == "TRUE") %>%
  count(twotps)

# So, 48 participants has baseline and 3 week














