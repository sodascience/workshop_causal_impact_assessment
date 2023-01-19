# Causal Impact script
library(tidyverse)
library(mice)
library(CausalImpact)
library(fpp3)

# ----------------------------------------------------------
# ---------------- Data preparation ------------------------
# ----------------------------------------------------------
# CausalImpact package needs data in a specific format:
# - there should be no missing values
# - the response variable must be in the first column
# - any potential covariates in subsequent columns
# - There should not be a "year" or index column

# Read the dataset
prop99 <- read_rds("data/proposition99.rds")

# impute missing values because causalimpact cannot deal with missingness
prop99_imputed <- 
  mice(prop99, m = 1) |> 
  complete() |> 
  as_tibble()

# data with all covariates from all states in donor pool
prop99_wide <- 
  prop99_imputed |> 
  pivot_wider(
    names_from = state, 
    values_from = c(cigsale, lnincome, beer, age15to24, retprice)
  ) |> 
  select(cigsale_California, everything(), -year)

# data with only cigarette sales from other states as potential covariates
prop99_cigonly <- 
  prop99_wide |> 
  select(contains("cigsale"))

# ----------------------------------------------------------
# ---------------------- Run model  ------------------------
# ----------------------------------------------------------

# Here, we create a causalimpact model for the cigarette sales data 
# using only cigarette sales from other states as potential covariates
pre_idx <- c(1, 19) # the first 18 years (1970 - 1988) are pre-intervention
post_idx <- c(20, 31) # the years after that (1989 - 2000)

impact_cigsale <- CausalImpact(
  data = prop99_cigonly, 
  pre.period = pre_idx, 
  post.period = post_idx
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
plot(impact_cigsale_model, "predictors")

# Now, we create a CausalImpact model using all potential control variable
impact_all <- CausalImpact(
  data = prop99_wide, 
  pre.period = pre_idx, 
  post.period = post_idx
)

# then, plot the causal impact model
plot(impact_all)

# Now, we will investigate the model a bit more
impact_all_model <- impact_all$model$bsts.model

# which covariates are most important in the prediction?
plot(impact_all_model, "coefficients")

# what is the value of those coefficients?
summary(impact_all_model)$coefficients

# compare those time series to the true time series
plot(impact_all_model, "predictors")

# Open questions:
# Is there actually regularization happening? What happened to spike and slab? coefficients look not regularized
# try out custom bsts model. how do we understand what's happening here?
# can we find an example where the pre-period time-series is actually used?
# with prior.level.sd we can make a random walk, even without predictors:
# plot(CausalImpact(data = prop99_cigonly[,1], pre.period = pre_idx, post.period = post_idx))
# versus
# plot(CausalImpact(data = prop99_cigonly[,1], pre.period = pre_idx, post.period = post_idx,
#                   model.args = list(prior.level.sd = 0.5)))

# where does the increasing uncertainty over time in the regression part of the model come from?
# plot(impact_all_model, "components")
# this is a property of time-series models like ARIMA, but we only see regression coefficients in bsts?



# ----------------------------------------------------------
# ---------------------- in fpp3: simple CITS  -------------
# ----------------------------------------------------------

# Here: very basic CITS analysis!
# create a simple dataset with only one potential covariate
# as a time-series object (tsibble) because that's what the
# fpp3 package needs!
prop99_ts <- 
  prop99_cigonly |> 
  select(cigsale_California, cigsale_Utah, cigsale_Illinois) |> 
  mutate(years = 1970:2000) |> 
  as_tsibble(index = "years")

# divide into pre and post-intervention, as a time
prop99_pre  <- prop99_ts[1:19,]
prop99_post <- prop99_ts[20:31,]

# fit data
fit_arima <-  prop99_pre |> 
  model( 
    timeseries = ARIMA(cigsale_California), # no regression!
    regression = ARIMA(cigsale_California ~ cigsale_Utah + cigsale_Illinois)
  )

# timeseries is an ar1 model on the second difference
report(fit_arima |> select(timeseries))

# regression is a linear regression on the first difference
report(fit_arima |> select(regression))

# make forecasts for the post-intervention period and save this in an object
fcasts <- fit_arima |> forecast(new_data = prop99_post)

# now plot the original data and forecasts
fcasts |> 
  autoplot(prop99_ts) + 
  geom_point(aes(y = cigsale_California), size = 0.5) +
  facet_grid(rows = vars(.model)) + 
  geom_vline(xintercept = 1989, linetype = "dotted", color = "blue")

# get the differences between the forecast and the real data to estimate causal effect
# as dist_normal object from package {distributional}
fc <- fcasts |> filter(.model == "regression") |> pull(cigsale_California) - prop99_post$cigsale_California
mfc <- sapply(fc, mean)

# and the cumulative difference 
cumsum(mfc)

