## Humac/peak torque analysis for PhD course



## Packages
library(tidyverse); library(reliefdata)



## Load data

humac.imp <- relief_humac

participant.dat <- relief_participants

condition <- relief_volume %>% 
  select(-arm) %>% 
  mutate(participant = as.character(participant)) %>% 
  print()


hum.dat <- humac.imp %>% 
  full_join(participant.dat) %>% 
  full_join(condition) %>%
  mutate(age = as.character(age)) %>%
  mutate(age.grp = if_else(age < 60,
                           "yng",
                           if_else(age > 60,
                                   "old", age)))
  
plot.dat <- hum.dat %>% 
  filter(speed == "60") %>%
  group_by(participant, time, leg, age, sex, allocation, age.grp, condition) %>%
  summarise(peakt = mean(pt, na.rm = TRUE)) %>% 
  print()

set.seed(1)

## Plot 1: Basic

fig1 <- plot.dat %>% 
  mutate(timep = factor(time, levels = c("pre", "mid", "post"))) %>% 
  
  ggplot(aes(timep, peakt)) +
  
  theme_classic() +
  
  geom_point(position = position_jitter(width = 0.2)) + 
  
  scale_y_continuous(limits = c(0, 300),
                     expand = c(0,0)) +
  scale_x_discrete(label = c("Baseline", "Midway", "Post")) +
  
  theme(axis.title.x = element_blank()) +
  
  labs(x = "", 
       y = "Peak Torque (Nm)",
       title = "Peak Torque in young vs. old with low vs. mod volumes")

ggsave(
  file = "fig1.pdf",
  plot = fig1,
  device = "pdf",
  path = "./figures",
  width = 7.7*2,
  height = 23*0.75,
  units = "cm",
  dpi = 600
)

# This plot looks ok, but it doesnt really convey any info other than peak
# torque values measured at different time points. Without any indicators of
# which observations belong to low/mod volume or young/old, it seems like 
# there is no change after 10 weeks of RT. 


## Plot 2: Colours!

plot.dat %>% 
  mutate(timep = factor(time, levels = c("pre", "mid", "post"))) %>% 
  
  ggplot(aes(timep, peakt,
             shape = condition, color = age.grp)) +
  
  theme_classic() +
  
  geom_point(position = position_jitter(width = 0.35)) + 
  
  scale_y_continuous(limits = c(0, 300),
                     expand = c(0,0),
                     breaks = c(0, 50, 100, 150, 200, 250, 300)) +
  scale_x_discrete(label = c("Baseline", "Midway", "Post")) +
  scale_alpha_manual(values = c(0.2, 0.8)) +
  
  theme(axis.title.x = element_blank()) +
  
  labs(x = "", 
       y = "Peak Torque (Nm)",
       title = "Peak Torque in young vs. old with low vs. mod volumes",
       shape = "Condition", color = "Age")

# So now we've added some colour and shape to the different levels of participants,
# and we can therefore distinguish between young and old age, low and mod 
# training loads, and control and intervention. However, the plot is quite busy,
# and its not easy to see a clear pattern. I think it looks "cool", but it 
# doesn't really convey the info in a good way still.


## Plot 3: Facet wrap it!

plot.dat %>% 
  mutate(timep = factor(time, levels = c("pre", "mid", "post"))) %>% 
  
  ggplot(aes(timep, peakt,
             shape = condition)) +
  
  theme_classic() +
  
  geom_point(position = position_jitter(width = 0.35)) + 
  
  facet_wrap( ~ age.grp, ncol = 1) +
  
  scale_y_continuous(limits = c(0, 300),
                     expand = c(0,0),
                     breaks = c(0, 50, 100, 150, 200, 250, 300)) +
  scale_x_discrete(label = c("Baseline", "Midway", "Post")) +
  
  theme(axis.title.x = element_blank()) +
  
  labs(x = "", 
       y = "Peak Torque (Nm)",
       title = "Peak Torque in young vs. old with low vs. mod volumes",
       shape = "Condition")

# While it would be very cool to have everything in one single facet, this
# makes it easier to actually get the info across. I would want to manage the
# legends a bit more, would be cool to get both the color/age and
# shape/condition in one legend.

## Plot 4: Mean

mean.dat <- plot.dat %>% 
  filter(allocation != "con") %>%
  group_by(time, condition, age.grp) %>%
  summarise(meanpt = mean(peakt, na.rm = TRUE),
            sdpt = sd(peakt, na.rm = TRUE)) %>%
  mutate(timep = factor(time, levels = c("pre", "mid", "post"))) %>% 
  ungroup() %>% 
  print()
  
  
 mean.dat %>% 
  ggplot(aes(timep, meanpt,
             shape = condition, color = age.grp)) +
  
  theme_classic() +
  
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = meanpt - sdpt, ymax = meanpt + sdpt), width = 0.5,
                position = position_dodge(width = 0.5)) +
 
  scale_y_continuous(limits = c(0, 250),
                     expand = c(0,0),
                     breaks = c(0, 50, 100, 150, 200, 250)) +
  scale_x_discrete(label = c("Baseline", "Midway", "Post")) +
  
  theme(axis.title.x = element_blank()) +
  
  labs(x = "", 
       y = "Peak Torque (Nm)",
       title = "Mean Peak Torque in young vs. old with low vs. mod volumes",
       shape = "Condition", color = "Age")

 # This makes it quite easy to see, but I really dont like the errorbar
 # aestethic. Ideally I would want to include all the individual data points,
 # perhaps at a low alpha, to show the variations. Again, would be nice to 
 # have one legend for all the groupings. 

 
 ## Plot 5: Mean and raw data points instead of errorbars
 
 # Data prep  (not sure if all of this is necessary)
 raw_dat <- plot.dat %>% 
   select(-age, -leg, -sex, -allocation) %>% 
   mutate(timep = factor(time, levels = c("pre", "mid", "post"))) %>%
   print()
 
 
 set.seed(1)
 
 # Colours
 colors <- c("#d7191c",
             "#fdae61",
             "#abd9e9",
             "#2c7bb6")
 
 # Plot 5.1 no errorbars
 mean.dat %>% 
   ggplot(aes(x = timep, y = meanpt,
              shape = condition, color = age.grp)) +
   
   # Theme
   theme_classic() +
   
   # Raw data points
   geom_point(data = raw_dat,
              aes(x = timep, y = peakt,
                  shape = condition, color = age.grp),
              size = 1.2,
              alpha = 0.5,
              position = position_jitter(width = 0.2)) +
   
   # Mean data points
   geom_point(size = 3, position = position_dodge(width = 0.2)) +
   
   
   # Scales
   scale_y_continuous(limits = c(0, 300),
                     expand = c(0,0),
                     breaks = c(0, 50, 100, 150, 200, 250, 300)) +
   scale_x_discrete(label = c("Baseline", "Midway", "Post")) +
   scale_color_manual(values = c("#d7191c", "#2c7bb6"),
                      labels = c("Old", "Young")) +
   scale_shape_discrete(name = "Volume", labels = c("Low", "Mod")) +
   
    
   theme(axis.title.x = element_blank()) +
    
   labs(x = "", 
        y = "Peak Torque (Nm)",
        title = "Mean Peak Torque in young vs. old with low vs. mod volumes",
        shape = "Condition", color = "Age")
 
 
 
 
 # Plot 5.2 errorbars
 mean.dat %>% 
   ggplot(aes(x = timep, y = meanpt,
              shape = condition, color = age.grp)) +
   
   # Theme
   theme_classic() +
   
   # Raw data points
   geom_point(data = raw_dat,
              aes(x = timep, y = peakt,
                  shape = condition, color = age.grp),
              size = 1.2,
              alpha = 0.5,
              position = position_jitter(width = 0.2)) +
   
   # Mean data points
   geom_point(size = 3, position = position_dodge(width = 0.3)) +
   geom_errorbar(aes(ymin = meanpt - sdpt, ymax = meanpt + sdpt), width = 0.5,
                 position = position_dodge(width = 0.3)) +
   
   
   # Scales
   scale_y_continuous(limits = c(0, 300),
                      expand = c(0,0),
                      breaks = c(0, 50, 100, 150, 200, 250, 300)) +
   scale_x_discrete(label = c("Baseline", "Midway", "Post")) +
   scale_color_manual(values = c("#d7191c", "#2c7bb6"),
                      labels = c("Old", "Young")) +
   scale_shape_discrete(name = "Volume", labels = c("Low", "Mod")) +
   
   
   theme(axis.title.x = element_blank()) +
   
   labs(x = "", 
        y = "Peak Torque (Nm)",
        title = "Mean Peak Torque in young vs. old with low vs. mod volumes",
        shape = "Condition", color = "Age")
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 