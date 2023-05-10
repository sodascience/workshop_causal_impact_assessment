# Interrupted time series script
library(tidyverse)
library(fpp3)
library(CausalImpact)



# ----------------------------------------------------------
# ---------------- Data preparation ------------------------
# ----------------------------------------------------------

# Read the dataset
prop99 <- read_rds("data/proposition99.rds")

# create a simple dataset with only one cigsale in california
# as a time-series object (tsibble) because that's what the
# fpp3 package needs! Also include a pre-post variable and a
# normalized "year" variable starting at 0
prop99_ts <- 
  prop99 |> 
  filter(state == "California") |> 
  select(year, cigsale) |>
  mutate(prepost = factor(year > 1988, labels = c("Pre", "Post"))) |> 
  as_tsibble(index = year) |> 
  mutate(year0 = year - min(year))

# ----------------------------------------------------------
# --------------------------- ITS --------------------------
# ----------------------------------------------------------


# ----------- Exercise 1: ITS with a simple linear growth curve --------

# here try a very growth curve simple model
fit_growth <- lm(formula = cigsale ~ year, family = "gaussian", prop99_ts |> filter(prepost == "Pre"))

summary(fit_growth)
# here we see a negative slope, as we would expect

pred <- predict(fit_growth, prop99_ts, interval = "prediction")
pred_df <- bind_cols(prop99_ts, as_tibble(pred))

pred_df |> 
  ggplot(aes(x = year, y = cigsale)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "lightgrey") +
  geom_line(aes(y = fit)) +
  geom_line(linewidth = 1, color = "darkgreen") +
  geom_vline(xintercept = 1988, lty = 2) +
  theme_minimal() +
  annotate("label", x = 1988, y = 150, label = "Intervention") +
  labs(title = "Panel data for California",
       y = "Cigarette sales", x = "Year")

ggsave("figures/its_growth_plot.png", width = 9, height = 6, bg = "white", dpi = 300)


# ----------- Exercise 2: ITS with a more complex time-series model --------


# fit model to pre-intervention time series.
# here we allow fpp3 to do its own model selection based on information criteria
# by default uses AICC, this can also be chosen in different ways, see ARIMA function documentation
fit_arima <-  
  prop99_ts |> 
  filter(prepost == "Pre") |> 
  model(timeseries = ARIMA(cigsale, ic = "aicc"))

# timeseries is an ar1 model on the second difference. 2nd order diference accounts for non-linear trends/non-stationarity
fit_arima |> select(timeseries) |> report()

ar_fitted <-  fit_arima |> fitted()
# make forecasts for the post-intervention period and save this in an object
fcasts <- fit_arima |> forecast(new_data = prop99_ts |> filter(prepost == "Post"))

# predict(fit_arima, prop99_pre, interval = "prediction")
# now plot the original data and forecasts
fcasts |> 
  autoplot(prop99_ts) +
  geom_line(aes(y = cigsale), linewidth = 1, color = "darkgreen") +
  facet_grid(rows = vars(.model)) + 
  geom_vline(xintercept = 1988, linetype = 2) +
  ylim(-50, 150) +
  theme_minimal() + 
  annotate("label", x = 1988, y = 150, label = "Intervention")

ggsave("figures/its_arima_plot.png", width = 9, height = 6, bg = "white", dpi = 300)


# -----------------------------------------------------------
# ----------------- Regression Discontinuity ----------------
# -----------------------------------------------------------

# here try a very simple model
fit_rdd <- lm(cigsale ~ year0 + prepost + year0:prepost, prop99_ts)
summary(fit_rdd)

pred_rdd <- predict(fit_rdd, interval = "prediction")
pred_df <- bind_cols(prop99_ts, as_tibble(pred_rdd))

pred_df |> 
  ggplot(aes(x = year, y = cigsale)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, data = pred_df |> filter(prepost == "Pre")) +
  geom_line(aes(y = fit), data = pred_df |> filter(prepost == "Pre")) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, data = pred_df |> filter(prepost == "Post")) +
  geom_line(aes(y = fit), data = pred_df |> filter(prepost == "Post")) +
  geom_line(linewidth = 1, color = "darkgreen") +
  ylim(0, 150) +
  theme_minimal() + 
  geom_vline(xintercept = 1988, linetype = 2) +
  annotate("label", x = 1988, y = 150, label = "Intervention")

ggsave("figures/rdd_plot.png", width = 9, height = 6, bg = "white", dpi = 300)


# ----------------------------------------------------------
# ---------------------- ITS with Causal Impact  -----------
# ----------------------------------------------------------

# data with only cigarette sales from other states as potential covariates
prop99_cigonly <- 
  prop99 |> 
  pivot_wider(
    names_from = state, 
    values_from = c(cigsale, lnincome, beer, age15to24, retprice)
  ) |> 
  select(cigsale_California, year)

# Here, we create a causalimpact model for the cigarette sales data 
# using only cigarette sales from other states as potential covariates
pre_idx <- c(1, 19) # the first 18 years (1970 - 1988) are pre-intervention
post_idx <- c(20, 31) # the years after that (1989 - 2000)

impact_cigsale <- CausalImpact(
  data = prop99_cigonly, 
  pre.period = pre_idx, 
  post.period = post_idx, 
  model.args = list(prior.level.sd = 0.4)
)

# then, plot the causal impact model
plot(impact_cigsale)

# Now, we will investigate the model a bit more
impact_cigsale_model <- impact_cigsale$model$bsts.model

# which covariates are most important in the prediction?
plot(impact_cigsale_model, "coefficients")

# what is the value of those coefficients?
summary(impact_cigsale_model)$coefficients

# compare those time series to the true time series
plot(impact_cigsale_model, "components")




