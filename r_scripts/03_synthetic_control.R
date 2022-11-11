library(tidyverse)
library(tidysynth)

prop99 <- read_rds("data/proposition99.rds")


# Perform Synthetic control method
syncont <- 
  prop99 |> 
  synthetic_control(
    outcome = cigsale,
    unit = state,
    time = year,
    i_unit = "California",
    i_time = 1988,
    generate_placebos = TRUE
  )

# Now, generate the aggregate predictors used to estimate the weights
syncont <- 
  syncont |> 
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
  ) %>%
  
  generate_predictor(time_window=1980,
                     cigsale_1980 = cigsale) %>%
  
  generate_predictor(time_window=1988,
                     cigsale_1988 = cigsale) %>%
  
  
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window =1970:1988,
                   Margin.ipop=.02,Sigf.ipop=7,Bound.ipop=6) %>%
  
  # Generate the synthetic control
  generate_control()
