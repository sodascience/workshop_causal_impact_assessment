# Interrupted time series script
library(tidyverse)
library(fpp3)
# optional extra 
# library(CausalImpact)



# ----------------------------------------------------------
# ---------------- Data preparation ------------------------
# ----------------------------------------------------------

# Read the dataset
prop99 <- read_rds("data/proposition99.rds")

# data with all covariates from all states in donor pool (will be useful later in part 4)
prop99_wide <- 
  prop99 |> 
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
# ---------------------- in fpp3: simple ITS  -------------
# ----------------------------------------------------------

# create a simple dataset with only one potential covariate
# as a time-series object (tsibble) because that's what the
# fpp3 package needs!
prop99_ts <- 
  prop99_cigonly |> 
  select(cigsale_California) |> 
  mutate(years = 1970:2000) |> 
  as_tsibble(index = "years")

# divide into pre and post-intervention, as a time
prop99_pre  <- prop99_ts[1:19,]
prop99_post <- prop99_ts[20:31,]

# fit model, allow fpp3 to do its own model selection based on information criteria
fit_arima <-  prop99_pre |> 
  model( 
    timeseries = ARIMA(cigsale_California),
  )

# timeseries is an ar1 model on the second difference
report(fit_arima |> select(timeseries))

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

# -----------------------------------------------------------
# ----------------- Regression Discontinuity ----------------
# -----------------------------------------------------------

cigsale_California <- prop99_ts$cigsale_California
time <- seq(0,length(cigsale_California)-1,1)
int_dummy <- c(rep(0,19), rep(1,12))


lmobj <- lm(cigsale_California ~ time + int_dummy + time:int_dummy)
summary(lmobj)

plot(time, cigsale_California, type = "l", col = "green", lwd = 2)
abline(v = 19, col = "grey" ,lty = 2)
abline(a = lmobj$coefficients[1], b = lmobj$coefficients[2], col = "grey")

# line segments here instead?

# figure 1; just the growth curve + uncertainty
# figure 2; ITS model from fpp3 in uniform format
# figure 3: RDD (model fit above) + both slopes

# ----------------------------------------------------------
# ---------------------- ITS with Causal Impact  -----------
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




