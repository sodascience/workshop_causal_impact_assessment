# Causal Impact script
library(CausalImpact)

prop99 <- read_rds("data/proposition99.rds")

prop99 <- as.data.frame(prop99)

# ----------------------------------------------------------
# ---------------- Data preparation ------------------------
# ----------------------------------------------------------

# causalimpact package needs data in the format:
# the response variable must be in the first column, and any covariates in subsequent columns

states <- unique(prop99$state)

years <- unique(prop99$year)

# which years are pre and post intervention (intervention took place in 1988)
n_t <- length(years)

# pre and post vectors in format causalimpact requires
pre <- c(1,(which(years == 1988)))
post <- c(which(years == 1989),n_t)

# outcome variable for california
cf_out <- prop99[which(prop99$state == "California"),"cigsale"]
length <- nrow(cf_out)

# control states (unsuitable states already omitted, see abadie table 2)
control_states <- states[states!= "California"]

# now obtain cigarette sales in all other states
cont_out <- sapply(control_states, function(s){
  prop99[which(prop99$state == s),"cigsale"]
})
colnames(cont_out) <- paste0("cigsale_", control_states)


# now obtain all other covariate values
# covnames <- c("retprice")
covnames <- c("lnincome", "beer","age15to24", "retprice")

# make a matrix of covariate values
cont_cov_list <- lapply(control_states, function(s){
  sapply(covnames,function(nm){
    prop99[which(prop99$state == s),nm]
  })
})

# stupid imputaiton method, because this thing can't deal with 

# lapply(cont_cov_list,function(l) apply(l,2,function(col) any(is.na(col))))


# data is missing for some variables for some years
# loop through states
for(s in 1:length(cont_cov_list)){
  # loop through variables
  for(col in 1:ncol(cont_cov_list[[s]])){
    # if no missing, skip ahead
    if(!any(is.na(cont_cov_list[[s]][,col]))) next
    # otherwise, find start and end points
    observed <- which(!is.na(cont_cov_list[[s]][,col]))
    start <- observed[1]
    if(start > 1){
    # impute missing at the start with whatever is first observed
    cont_cov_list[[s]][,col][1:(start-1)] <- cont_cov_list[[s]][,col][start]
    }
    end <- max(observed)
    if(length(cont_cov_list[[s]][,col]) > end){
      cont_cov_list[[s]][,col][(end+1):(length(cont_cov_list[[s]][,col]))] <- cont_cov_list[[s]][,col][end]
    }
  }
}

cont_cov <- do.call("cbind", cont_cov_list)

# write names of covariates per state
cnames <- expand.grid(covnames, control_states)

colnames(cont_cov)<- paste0(cnames[,1], "_", cnames[,2])

# put this all together
# include only cigarette sales
data_cigonly <- cbind(cf_out, cont_out)
# including cigarette sales and all covariates of the control series (omit covariates of california)
data_allcontrol <- cbind(cf_out, cont_out, cont_cov)

# ----------------------------------------------------------
# ---------------------- Run model  ------------------------
# ----------------------------------------------------------

impact1 <- CausalImpact(data = data_cigonly, pre.period = pre, post.period = post)
plot(impact1)
# bsts model summary through there
summary(impact1$model$bsts.model)

impact2 <- CausalImpact(data = data_allcontrol, pre.period = pre, post.period = post)
plot(impact2)
impact2

plot(impact2$model$bsts.model,"components")

impact3 <- CausalImpact(data = data_allcontrol[,1], pre.period = pre, post.period = post)
plot(impact3)


# to do here: try out different bsts models!

# ----------------------------------------------------------
# ---------------------- in fpp3: simple CITS  -------------------------
# ----------------------------------------------------------

# Here: very basic CITS analysis!
# not enough uncertainty 

# pre and post vectors in format causalimpact requires
end_pre <- which(years == 1988)
# post <- c(which(years == 1989),n_t)

datasimp <- as.data.frame(cbind(data_cigonly[,1], data_cigonly[,"cigsale_Utah"]))
colnames(datasimp) <- c("cali", "utah")

library(fpp3)
date <- years
# make pre-intervention data
pre.dat <- tibble(date=1:end_pre ,y = datasimp$cali[1:end_pre], x = datasimp$utah[1:end_pre])
pre.dat <- as_tsibble(pre.dat, index=date)

# make post-intervention data
post.dat <- tibble(date=(end_pre):nrow(datasimp) ,
                   y = datasimp$cali[(end_pre+1):nrow(datasimp)], 
                   x = datasimp$utah[(end_pre+1):nrow(datasimp)])
post.dat <- as_tsibble(post.dat, index=date)

# fit data
fit1 <- pre.dat %>% model(ARIMA(y ~ x))
report(fit1)
# see how the model is very similar to the one described above!

# make forecast for the post-intervention period and save this in an object
fcasts <- fit1 %>% forecast(new_data = post.dat)

# now plot the original data and forecasts
fcasts %>% autoplot(bind_rows(pre.dat,post.dat)) + autolayer(post.dat, y, colour = "black") +
  geom_vline(xintercept =end_pre+1, linetype = "dotted", color = "blue")


