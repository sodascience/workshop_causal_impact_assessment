library(tidyverse)
library(tidysynth)

prop99 <- read_rds("data/proposition99.rds")


# Create synthetic control object
prop99_syn <- 
  prop99 |> 
  synthetic_control(
    outcome = cigsale,
    unit = state,
    time = year,
    i_unit = "California",
    i_time = 1988,
    generate_placebos = TRUE
  )

# in tidysynth, the grab_* functions can be used to inspect parts of the model
# here, we inspect the outcome (cigsale) for the treated unit and the potential controls
grab_outcome(prop99_syn)
grab_outcome(prop99_syn, type = "controls")

# Now, generate the aggregate predictors used to estimate the weights
prop99_syn <- 
  prop99_syn |> 
  generate_predictor(
    time_window = 1980:1988,
    lnincome = mean(lnincome, na.rm = TRUE),
    retprice = mean(retprice, na.rm = TRUE),
    age15to24 = mean(age15to24, na.rm = TRUE)
  ) |> 
  generate_predictor(
    time_window = 1984:1988,
    beer = mean(beer, na.rm = TRUE)
  ) |> 
  generate_predictor(
    time_window = 1975,
    cigsale_1975 = cigsale
  ) |> 
  generate_predictor(
    time_window = 1980,
    cigsale_1980 = cigsale
  ) |> 
  generate_predictor(
    time_window = 1988,
    cigsale_1988 = cigsale
  )

# Check the predictors we created for the treated and the control units
grab_predictors(prop99_syn)
grab_predictors(prop99_syn, type = "controls")


# Then, we can create our weights matrix
# this uses a quadratic programming routine (ipop) for optimization
prop99_syn <- 
  prop99_syn |> 
  generate_weights(
    optimization_window = 1970:1988, # pre-intervention period
    margin_ipop = 0.2, sigf_ipo = 7, bound_ipop = 6
  )

# let's look at the learned unit weights and predictor weights
grab_unit_weights(prop99_syn)
grab_predictor_weights(prop99_syn)

# and we can create a plot as well
plot_weights(prop99_syn)


# Generate the synthetic control
prop99_syn_control <- generate_control(prop99_syn)

plot_trends(prop99_syn_control) 
plot_placebos(prop99_syn_control)
