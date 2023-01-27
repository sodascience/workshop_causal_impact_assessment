library(tidyverse)

prop99 <- read_rds("data/proposition99.rds")

# cigsales totals
prop99 |> 
  group_by(state) |> 
  summarize(total_cigsales = sum(cigsale)) |> 
  arrange(total_cigsales)


# First plot
prop99 |> 
  mutate(cali = ifelse(state == "California", "California", "Other states")) |> 
  arrange(desc(cali)) |> 
  mutate(state = as_factor(as.character(state))) |> 
  ggplot(aes(x = year, y = cigsale, group = state, colour = cali, alpha = cali)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 1988, lty = 2) +
  scale_alpha_manual(values = c(1, 0.4), guide = "none") +
  scale_colour_manual(values = c("darkgreen", "grey")) +
  theme_minimal() +
  annotate("label", x = 1988, y = 200, label = "Intervention") +
  ylim(0, 300) +
  labs(title = "Panel data for proposition 99",
       y = "Cigarette sales", x = "Year", colour = "State")

ggsave("figures/main_data_plot.png", width = 9, height = 6, bg = "white", dpi = 300)
  

# prepost time series figure
prop99 |> 
  filter(state == "California") |> 
  ggplot(aes(x = year, y = cigsale)) +
  geom_line(linewidth = 1, colour = "darkgreen") +
  geom_vline(xintercept = 1988, lty = 2) +
  theme_minimal() +
  annotate("label", x = 1988, y = 150, label = "Intervention") +
  labs(title = "Panel data for California",
       y = "Cigarette sales", x = "Year")

ggsave("figures/cali_plot.png", width = 9, height = 6, bg = "white", dpi = 300)


# compute pre-post estimate + wrong standard error
prop99_cali <- 
  prop99 |> 
  filter(state %in% "California", year >= 1976) |> 
  mutate(prepost = as_factor(ifelse(year <= 1988, "Pre", "Post"))) 

summary(lm(cigsale ~ prepost, data = prop99_cali))

# pre-post estimate
prepost_dat <- 
  prop99_cali |>
  group_by(prepost) |> 
  summarize(cigsale = mean(cigsale))
  
prepost_dat |> 
  ggplot(aes(x = prepost, y = cigsale, group = 1)) +
  geom_point(colour = "darkgreen", size = 2) + 
  geom_line(colour = "darkgreen", size = 1) +
  ylim(0, 140) +
  theme_minimal() +
  labs(x = "Time", y = "Cigarette sales",
       title = "Pre-post estimator") +
  geom_segment(x = 2.1, xend = 2.1, y = min(prepost_dat$cigsale), yend = max(prepost_dat$cigsale), 
               size = 1, colour = "darkgray") +
  annotate("text", x = 2.12, y = mean(prepost_dat$cigsale), hjust = 0, 
           label = round(diff(prepost_dat$cigsale), 2), colour = "darkgray")

ggsave("figures/prepost.png", width = 7, height = 5, bg = "white", dpi = 300)


# DiD
prop99_did <- 
  prop99 |> 
  filter(state %in% c("California", "Utah"), year >= 1976) |> 
  mutate(prepost = as_factor(ifelse(year <= 1988, "Pre", "Post")))


# did time series figure
prop99_did |> 
  ggplot(aes(x = year, y = cigsale, colour = state)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 1988, lty = 2) +
  theme_minimal() +
  scale_colour_manual(values = c("darkgreen", "darkgrey")) +
  annotate("label", x = 1988, y = 150, label = "Intervention") +
  labs(title = "Panel data for California and Utah",
       y = "Cigarette sales", x = "Year", colour = "")

ggsave("figures/cali_utah_plot.png", width = 9, height = 6, bg = "white", dpi = 300)

# get table
prop99_did |> 
  group_by(state, prepost) |> 
  summarize(cigsale = mean(cigsale)) |> 
  pivot_wider(names_from = prepost, values_from = cigsale) |> 
  ungroup()

# Now we want to know about uncertainty
# model with interaction effect
mod_did <- lm(cigsale ~ state * prepost, data = prop99_did)
summary(mod_did)

# with robust standard errors to account for autocorrelation
library(sandwich)
library(lmtest)
coeftest(mod_did, vcov. = vcovHAC)


# DiD estimate
did_dat <- 
  prop99_did |> 
  group_by(state, prepost) |> 
  summarize(cigsale = mean(cigsale)) |> 
  ungroup()

did_dat |> 
  ggplot(aes(x = prepost, y = cigsale, colour = state, group = state)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  scale_colour_manual(values = c("darkgreen", "darkgrey"), guide = "none") +
  ylim(0, 140) +
  theme_minimal() +
  labs(x = "Time", y = "Cigarette sales", title = "Diff-in-diff") +
  annotate("text", label = "California", y = 110, colour = "darkgreen", x = 1, hjust = 1) + 
  annotate("text", label = "Utah", y = 69, colour = "darkgrey", x = 1, hjust = 1) 

ggsave("figures/did_dat.png", width = 7, height = 5, bg = "white", dpi = 300)


# Actually make the DiD estimate
did_dat <- 
  bind_rows(did_dat, tibble(
    state = rep("California (est.)", 2),
    prepost = c("Pre", "Post"),
    cigsale = c(did_dat$cigsale[1], did_dat$cigsale[1] + did_dat$cigsale[4] - did_dat$cigsale[3]), 
  )) |> 
  mutate(prepost = as_factor(prepost), state = as_factor(state))

did_dat |> 
  ggplot(aes(x = prepost, y = cigsale, colour = state, group = state, linetype = state)) +
  geom_point(size = 2) + 
  geom_line(size = 1) +
  scale_colour_manual(values = c("darkgreen", "darkgrey", "darkgreen"), guide = "none") +
  scale_linetype_manual(values = c(1, 1, 2), guide = "none") +
  ylim(0, 140) +
  theme_minimal() +
  labs(x = "Time", y = "Cigarette sales",
       title = "Diff-in-diff estimator") +
  geom_segment(x = 2.1, xend = 2.1, y = 60.4, yend =  92.7, size = 1, colour = "darkgray") +
  annotate("text", x = 2.12, y = 78.05, hjust = 0, label = 60.4 - 92.7, colour = "darkgray") +
  annotate("text", label = "California", y = 110, colour = "darkgreen", x = 1, hjust = 1) + 
  annotate("text", label = "California (counterfactual)", y = 105, colour = "darkgreen", x = 2, hjust = 0.5) + 
  annotate("text", label = "Utah", y = 69, colour = "darkgrey", x = 1, hjust = 1) 

ggsave("figures/did.png", width = 7, height = 5, bg = "white", dpi = 300)
