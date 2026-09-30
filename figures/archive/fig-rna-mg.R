# RNA and mg tissue relationship figure


library(tidyverse)
library(patchwork)

all_contrasts_fc <- readRDS("./data/data-gen/rna_mg_contrasts_fc_m4.rds")
# NB: use `fold_change` column for plotting — `value` is the standardized ystd scale

## Fold-change version

# Panel 1: Volume effect (2 facets, old vs. young)
all_contrasts_fc |> 
  filter(contrast_group == "volume effect") |> 
  mutate(age = if_else(grepl("^old", contrast), "Old", "Young")) |> 
  ggplot(aes(fold_change, color = delta)) + 
  geom_line(stat = "density") +
  geom_vline(xintercept = 1, lty = 2) +
  facet_wrap(~ age) +
  coord_cartesian(xlim = c(0.3, 3)) +
  labs(x = "Fold-change (moderate vs. low volume)",
       subtitle = "Positive = moderate volume > low volume (dashed line = no difference)")

# Panel 2: Age effect, intervention vs. control, volume x age (1 facet each)
all_contrasts_fc |> 
  filter(contrast_group %in% c("age effect", "intervention vs control", "volume x age")) |> 
  mutate(contrast_group = factor(contrast_group,
                                 levels = c("age effect", "intervention vs control", "volume x age"),
                                 labels = c("Age effect\n(positive = young > old)",
                                            "Intervention vs. control\n(positive = intervention > control)",
                                            "Volume x age\n(positive = young's volume effect > old's)"))) |> 
  ggplot(aes(fold_change, color = delta)) + 
  geom_line(stat = "density") +
  geom_vline(xintercept = 1, lty = 2) +
  facet_wrap(~ contrast_group) +
  coord_cartesian(xlim = c(0.3, 3)) +
  labs(x = "Fold-change") +
  theme(strip.text = element_text(size = 8))


## Log-scale version

# Panel 1: Volume effect (log-scale)
p1 <- all_contrasts_fc |> 
  filter(contrast_group == "volume effect") |> 
  mutate(age = if_else(grepl("^old", contrast), "Old", "Young")) |> 
  ggplot(aes(fold_change, color = delta)) + 
  geom_line(stat = "density") +
  geom_vline(xintercept = 1, lty = 2) +
  facet_wrap(~ age) +
  scale_x_log10(breaks = c(0.5, 1, 2, 3)) +
  labs(x = "Fold-change (moderate vs. low volume, log scale)",
       subtitle = "Positive = moderate volume > low volume (dashed line = no difference)")

# Panel 2: Age effect, intervention vs. control, volume x age (log-scale)
p2 <- all_contrasts_fc |> 
  filter(contrast_group %in% c("age effect", "intervention vs control", "volume x age")) |> 
  mutate(contrast_group = factor(contrast_group,
                                 levels = c("age effect", "intervention vs control", "volume x age"),
                                 labels = c("Age effect\n(positive = young > old)",
                                            "Intervention vs. control\n(positive = intervention > control)",
                                            "Volume x age\n(positive = young's volume effect > old's)"))) |> 
  ggplot(aes(fold_change, color = delta)) + 
  geom_line(stat = "density") +
  geom_vline(xintercept = 1, lty = 2) +
  facet_wrap(~ contrast_group) +
  scale_x_log10(breaks = c(0.5, 1, 2, 3)) +
  labs(x = "Fold-change (log scale)") +
  theme(strip.text = element_text(size = 8))

combined <- p1 / p2 + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

combined


#ggsave("figures/fig9-rna-mg.png",
#       plot = combined,
#       width = 8, height = 9, dpi = 300)










