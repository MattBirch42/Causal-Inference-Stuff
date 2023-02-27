# Vanilla diff-in-diff data with truly parallel trends and only one treatment cutoff

n_obs <- 25000

set.seed(42)
region <- sample(c(1,1,1,2,2,2,3,3,3,3,3,4,4,5,5,5,5,6,6,7,7,7), n_obs, replace = TRUE)

set.seed(123)
year <- sample(2009:2021, n_obs, replace = TRUE)

# Treatment occurs in regions 2 and 4 in years 2015 and later
dd1_data <- as.data.frame(cbind(region,year))
set.seed(31416)
dd1_data$score <- round(335 + 4.8*(dd1_data$year - 2000) + 
                        ifelse(dd1_data$region %in% c(2,4),-42,0) +
                        ifelse(dd1_data$region %in% c(2,4) & dd1_data$year>2014,36,0) +
                        rnorm(n_obs,0,2),0)

write.csv(dd1_data,'vanilla_did.csv', row.names = FALSE)


# diff-in-diff with more complicated trends for synthetic control group
# Treatment occurs in regions 2 and 4 in years 2015 and later
dd2_data <- dd1_data[,c('region','year')]
set.seed(31416)
dd2_data$score <- round(335 + 
                          ifelse(dd2_data$region %in% c(2,4),-42,0) +
                          ifelse(dd2_data$region %in% c(2,4) & dd2_data$year>2014,36,0) +
                          ifelse(dd2_data$region%%2 == 0, ((dd2_data$year - 2016)/3)^2, dd2_data$region*(dd2_data$year-2017)) + 
                          rnorm(n_obs,0,2),0) 

write.csv(dd2_data,'synthetic_did.csv', row.names = FALSE)

# diff-in-diff with multiple cutoffs but no need for synthetic controls
# Treatment occurs in regions 2 and 4 in years 2013 and 2015 respectively
dd3_data <- as.data.frame(cbind(region,year))
set.seed(31416)
dd1_data$score <- round(335 + 4.8*(dd3_data$year - 2000) + 
                          ifelse(dd3_data$region %in% c(2,4),-42,0) +
                          ifelse(dd3_data$region == 2 & dd3_data$year>2012,36,0) +
                          ifelse(dd3_data$region == 4 & dd3_data$year>2014,44,0) +
                          rnorm(n_obs,0,2),0)

write.csv(dd2_data,'multiple_did.csv', row.names = FALSE)



# one with additional covariates??