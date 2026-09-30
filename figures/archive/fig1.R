## Study design fig

# Packages 

library(tidyverse); 
library(grid); 
library(gridExtra); 
library(ggtext); 
library(readxl)
library(png); 
library(cowplot); 
library(magick); 
library(dplyr); 
library(forcats); 
library(ggplot2)

# Images

dxaimg <- readPNG("./figures/archive/dxa.fig.png")
biopsyimg <- readPNG("./figures/archive/biopsy.fig.png")
gtimg <- readPNG("./figures/archive/blood.vial.png")
mriimg <- readPNG("./figures/archive/mri.png")
strimg <- readPNG("./figures/archive/str.png")
rtimg <- readPNG("./figures/archive/rt.fig.png")
bigarrow <- readPNG("./figures/archive/big.arrow.png")
smallarrow <- readPNG("./figures/archive/small.arrow.png")
longarrow <- readPNG("./figures/archive/long.arrow.png")

# Study design fig
d.dat <- read_excel("./data/design.dat.xlsx", na = "NA")

line_size <- 0.2
htextsize <- 2.9
ltextsize <- 2.3
textsize <- 2
stextsize <- 1.7



d.fig <- d.dat %>%
  ggplot(aes(time, tp)) +
  scale_y_continuous(limits = c(0,10), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), expand = c(0,0)) +
  scale_x_continuous(limits = c(0,24), breaks = seq(1:24), expand = c(0,0)) +

### GENERAL LAYOUT
  
 # annotate("rect", xmin = 0, xmax = 24, ymin = 0, ymax = 10, alpha = 2, color = "black", fill = "#f0f4f8") +
#  annotate("segment", x = 0, xend = 24, y = 5, yend = 5, alpha = 1) + # horizontal divider
 # annotate("segment", x = 18.5, xend = 18.5, y = 10, yend = 5, alpha = 1) + # vertical divider
  
  # Rounded background with no border
  geom_rect(aes(xmin = 0, xmax = 24, ymin = 0, ymax = 10),
            fill = "#f0f4f8", color = "#f0f4f8", size = 0) +
  
  annotate("segment", x = 0, xend = 24, y = 5, yend = 5, 
           alpha = 1, color = "#7f8c8d", size = 0.5) + # horizontal divider
  
  annotate("segment", x = 18.5, xend = 18.5, y = 10, yend = 5, 
           alpha = 1, color = "#7f8c8d", size = 0.5) + # vertical divider


  annotate("segment", x = .2, xend = 23.8, y = .5, yend = .5, alpha = 2) + # timeline
  annotate("segment", x = .2, xend = .2, y = .3, yend = .7, alpha = 2) + # Week 1 left tick
  annotate("segment", x = 3.5, xend = 3.5, y = .3, yend = .7, alpha = 2) + # week 1 right tick
  annotate("segment", x = 10.9, xend = 10.9, y = .3, yend = .7, alpha = 2) + # mid left tick
  annotate("segment", x = 13.1, xend = 13.1, y = .3, yend = .7, alpha = 2) + # mid right tick
  annotate("segment", x = 20, xend = 20, y = .3, yend = .7, alpha = 2) + # Week 12 left tick
  annotate("segment", x = 23.8, xend = 23.8, y = .3, yend = .7, alpha = 2) + # week 12 right tick
  
### INCLUSION/RECRUITMENT PART
  
  # Recruitment heading
  annotate("text", x = 9.5, y = 9.7, label = "Recruitment", size = htextsize) +
  
  # Screening
  annotate("text", x = 1.4, y = 8.25, label = "Screening", size = ltextsize) + 
  annotate("text", x = 1.4, y = 7.75, label = "Inc./Ex. criteria", size = ltextsize) +
  annotate("segment", x =0.2, xend = 10, y = 7.25, yend = 7.25, alpha = 2) +
  draw_image(smallarrow, x = 2.5, y = 6.77, scale = 0.5) +
  
  # Inclusion
  annotate("text", x = 5, y = 8.25, label = "Inclusion (n = )", size = ltextsize) +
  annotate("text", x = 5, y = 7.75, label = "Infromed consent", size = ltextsize) +
  draw_image(smallarrow, x = 6.5, y = 6.77, scale = 0.5) +

  # Age stratification
  annotate("text", x = 8.5, y = 8.25, label = "Age", size = ltextsize) +
  annotate("text", x = 8.5, y = 7.75, label = "stratification", size = ltextsize) +
  annotate("segment", x = 10, xend = 11.5, y = 8.4, yend = 8.4, alpha = 2) + # upper arrow horizontal line
  annotate("segment", x = 10, xend = 10, y = 7.25, yend = 8.4, alpha = 2) + # upper arrow vertical line
  draw_image(smallarrow, x = 11, y = 7.905, scale = 0.5) + # upper arrow head
  annotate("segment", x = 10, xend = 11.5, y = 6.3, yend = 6.3, alpha = 2) + # lower arrow horizontal line
  annotate("segment", x = 10, xend = 10, y = 7.25, yend = 6.3, alpha = 2) + # lower arrow vertical line
  draw_image(smallarrow, x = 11, y = 5.8, scale = 0.5) + # lower arrow head
 
  # Groups
  annotate("text", x = 12.5, y = 8.4, label = "<30 yrs", size = ltextsize) + # young
  annotate("text", x = 12.5, y = 6.3, label = ">70 yrs", size = ltextsize) + # old

  # Randomization
  annotate("text", x = 14.75, y = 7.35, label = "Randomization", size = ltextsize) +
  annotate("segment", x = 13.5, xend = 16, y = 8.4, yend = 8.4, alpha = 2) + # upper arrow line
  draw_image(smallarrow, x = 15.5, y = 7.905, scale = 0.5) + # upper arrow head
  annotate("segment", x = 13.5, xend = 16, y = 6.3, yend = 6.3, alpha = 2) + # lower arrow line
  draw_image(smallarrow, x = 15.5, y = 5.8, scale = 0.5) + # lower arrow head
  
  # Volumes
  annotate("text", x = 16.75, y = 8.8, label = "LV", size = ltextsize) + # LV young
  annotate("text", x = 16.75, y = 8.1, label = "MV", size = ltextsize) + # MV young
  annotate("text", x = 16.75, y = 7, label = "LV", size = ltextsize) + # LV old
  annotate("text", x = 16.75, y = 6.3, label = "MV", size = ltextsize) + # MV old
  annotate("text", x = 16.75, y = 5.7, label = "CON", size = ltextsize) + # control old

  
  ## Symbols
  
  # Text
  annotate("text", x = 20.5, y = 9.6, label = "DXA/US", size = textsize) +
  annotate("text", x = 20.5, y = 8.9, label = "Biopsy", size = textsize) +
  annotate("text", x = 20.5, y = 7.9, label = "GT", size = textsize) +
  annotate("text", x = 20.5, y = 7, label = "MRI", size = textsize) +
  annotate("text", x = 20.5, y = 6.2, label = "STR", size = textsize) +
  annotate("text", x = 20.5, y = 5.4, label = "RT", size = textsize) +
  
  # Images
  draw_image(dxaimg, x = 21.5, y = 9.1, scale = 0.5) + # dxa and us image
  draw_image(biopsyimg, x = 21.5, y = 8.26, scale = 0.6) + # biopsy image
  draw_image(gtimg, x = 21.5, y = 7.42, scale = 0.6) + # blood vial/glucose tolerance image
  draw_image(mriimg, x = 21.5, y = 6.58, scale = 0.6) + # mri image
  draw_image(strimg, x = 21.5, y = 5.74, scale = 0.6) + # strength test image
  draw_image(rtimg, x = 21.5, y = 4.9, scale = 0.6) + # resistance training image

### INTERVENTION SEGMENT
  ## Baseline
  
  # Text
  annotate("text", x = 2, y = .2, label = "Week 1: Baseline", size = textsize) +
  annotate("text", x = .6, y = .7, label = "Pre 1", size = stextsize) + # baseline test day 1
  annotate("text", x = 1.75, y = .7, label = "Pre 2", size = stextsize) + # baseline test day 2
  annotate("text", x = 2.9, y = .7, label = "Pre 3", size = stextsize) + # baseline test day 3
  annotate("text", x = 3.9, y = .7, label = "Pre 4", size = stextsize) + # baseline test day 4
  
  # Images
  draw_image(dxaimg, x = .1, y = 3.4, scale = .9) + # dxa/us, pre 1
  draw_image(biopsyimg, x = .1, y = 2.2, scale = .9) + # biopsy, pre 1
  draw_image(gtimg, x = .1, y = 1, scale = .9) + # glucose, pre 1
  draw_image(strimg, x = 1.2, y = 1, scale = .9) + # strength, pre 2
  draw_image(mriimg, x = 2.3, y = 1, scale = .9) + # mri, pre 3
  
  
  ## Intervention
  
  #Timeline
  annotate("text", x = 12, y = 4.5, label = "Week 6: Mid", size = textsize) +
  
  # Text
  annotate("text", x = 7.5, y = 3.2, label = "2x/week for 5 weeks", size = textsize) + # Left arrow text
  annotate("text", x = 16.3, y = 3.2, label = "3x/week for 5 weeks", size = textsize) + # Right arrow text
  annotate("text", x = 8, y = .2, label = "Week 3", size = textsize) + # Week 3 text
  annotate("text", x = 12, y = .2, label = "Week 5-6: Mid", size = textsize) + # Mid text
  annotate("text", x = 11.4, y = .7, label = "Mid 1/2", size = stextsize) +
  annotate("text", x = 12.5, y = .7, label = "Mid 3", size = stextsize) +

  # Images
  draw_image(strimg, x = 3.4, y = 1, scale = .9) + # baseline strength test
  draw_image(rtimg, x = 4.3, y = 3, scale = 1.2) + # left RT image
  draw_image(longarrow, x = 7.7, y = 2.3, scale = 2, width = 8) + # left big arrow
  draw_image(biopsyimg, x = 7.5, y = .8, scale = 1) + # 3 week biopsy
  draw_image(strimg, x = 10.9, y = 1, scale = 1) + # mid strength test
  draw_image(dxaimg, x = 11.9, y = 1, scale = 1) + # mid dxa/US
  draw_image(rtimg, x = 13.2, y = 3, scale = 1.2) + # right RT image
 # draw_image(bigarrow, x = 14.3, y = 2.3, scale = 2, width = 3, height = 1) + # right big arrow
  draw_image(strimg, x = 19.2, y = 1, scale = 1) + # post strength test
  
  
  ## Post
  # Should include: Testday 1 (DXA, UL, blood sample, biopsy), test day 2 (Hb-mass + glucose tolerance), MRI
  
  # Text
  annotate("text", x = 22, y = .2, label = "Week 12: Post", size = textsize) + # Week 12 text
  annotate("text", x = 20.8, y = .7, label = "Post 1", size = stextsize) + # Post 1
  annotate("text", x = 22, y = .7, label = "Post 2", size = stextsize) + # Post 2
  annotate("text", x = 23.2, y = .7, label = "Post 3", size = stextsize) + # Post 3

  # Images
  draw_image(dxaimg, x = 20.3, y = 2.2, scale = 1) +
  draw_image(biopsyimg, x = 20.3, y = 1, scale = 1) +
  draw_image(gtimg, x = 21.45, y = 1, scale = 1) +
  draw_image(mriimg, x = 22.6, y = 1, scale = 1) +

 
  
  
  # General theme
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


fig1 <- plot_grid(d.fig,
                  NULL,
                  NULL,
                  NULL,
                  ncol = 1,
                  nrow = 3)



ggdraw(d.fig) +
  draw_image(dxaimg, x = 0.5, y = 0.5, scale = 0.05)



ggsave(
  file = "fig1.pdf",
  plot = fig1,
  device = "pdf",
  path = "./figures",
  width = 174,
  height = 234*0.75,
  units = "mm",
  dpi = 1200
)


