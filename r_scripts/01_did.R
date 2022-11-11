library(tidyverse) # for data processing

prop99 <- read_rds("data/proposition99.rds")

# first plot
prop99 |> 
  mutate(cali = ifelse(state == "California", "California", "Other states")) |> 
  ggplot(aes(x = year, y = cigsale, group = state, colour = cali, alpha = cali)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 1988, lty = 2) +
  scale_alpha_manual(values = c(1, 0.4), guide = "none") +
  scale_colour_manual(values = c("darkgreen", "grey")) +
  theme_minimal() +
  annotate("label", x = 1988, y = 200, label = "Intervention") +
  labs(title = "Panel data for proposition 99",
       y = "Cigarette sales", x = "Year", colour = "State")

ggsave("figures/main_data_plot.png", width = 9, height = 5, bg = "white")
  


# pre-post
prop99 |> 
  filter(state = "California", year = )

  