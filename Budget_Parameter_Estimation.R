
##### Packages Required ######
install.packages(c("fBasics","MASS","stats4"))
library(c("fBasics","MASS","stats4"))
############################

bdg <- read.csv(file = "C:\\Users\\halte\\Desktop\\R from home\\2015_budget_flat_file.csv",
                header = T,
                col.names = c("key","Year","Month","Day","ID","Date","Store.Name",
                              "Amount", "Description", "Class", "Method.of.Payment"))




# fitting distributions
# https://cran.r-project.org/doc/contrib/Ricci-distributions-en.pdf
# http://www.itl.nist.gov/div898/handbook/eda/section3/boxcoxno.htm

#----------------

options(digits = 4) #remove extra digits causing errors

groceries = bdg$Amount[bdg$Class == "Groceries"]

clean_values <- function(x) {
  
  x = x[!is.na(x)]
  x = trimws(as.character(x))
  x = gsub("\\$","", paste(x))
  
  for (i in 1:(length(x)-1)) {
    if (grepl("(", x[i], fixed = TRUE)) {
      # if there is a parentheses, set as negative
      # don't take any negative values
      x[i] = as.numeric(0)
    } else if (grepl("-",x[i],fixed = TRUE)) {
      # if it's an empty value, set it to 0
      x[i] = as.numeric(0)
    }
    i = i + 1
  }
  return(as.numeric(x))
}
# clean dollar amounts from csv to numeric R values

initial_plots <- function(x) {
  par(mfrow = c(2,3))
  hist(x)
  plot(density(x))
  plot(ecdf(x))
  boxplot(x)
  z.norm <- (x - mean(x)) / sd(x)  #based on Gaussian
  hist(z.norm, main = "Histogram of Normal Standardized Values")
  qqnorm(z.norm)
  abline(0,1)
  
  x = as.numeric(x)
  skewness(x)
  kurtosis(x)
}
# plot initial graphs evaluating distribution of spending category

#-----

par(mfrow = c(1,1)) #reset graphing grid for future

# http://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm

g <- clean_values(groceries)
initial_plots(g)

# Method of Moments
# doesn't exist for lognormal, skipping for weibull and gamma

# Maximum Likelihood Estimate
fitdistr(g[g!=0], "lognormal")
fitdistr(g[g!=0], "weibull")
fitdistr(g[g!=0], "gamma")

size = fitdistr(g[g!=0], "lognormal")[5]  #5th value is number of data points
shape_lest = fitdistr(g[g!=0], "lognormal")$estimate[1]
#fitdistr class - i can't save the vector value, which is a bloody pain
# these values are the printed results of the fitdistr lines
groceries_shape_lest = 3.275281
groceries_scale_lest = 0.966774
groceries_shape_weib_est = 1.37406
groceries_scale_weib_est = 41.18968
groceries_shape_gam_est = 1.5495527
groceries_rate_gam_est = 0.04102898

# Measure goodness of fit for the Weibull and lognormal
# estimate random values a few thousand times, test MSE values

score = matrix(data = c(0,0), nrow = 10000, ncol = 3)
colnames(score) = c("Lognormal Std.Error","Weibull Std.Error", "Gamma Std.Error")

for (i in 1:10000) {
  empirical    = g[g>0]
  test_lognorm = rlnorm(n = length(empirical), meanlog = groceries_shape_lest, sdlog = groceries_scale_lest)
  test_weibull = rweibull(n = length(empirical), shape = groceries_shape_weib_est, scale = groceries_scale_weib_est)
  test_gamma   = rgamma(n = length(empirical), shape = groceries_shape_gam_est, rate = groceries_rate_gam_est)
  
  logn_fitted  = abs(test_lognorm - empirical)
  weib_fitted  = abs(test_weibull - empirical)
  gamma_fitted = abs(test_gamma   - empirical)
  
  score[i,1] = mean(logn_fitted)
  score[i,2] = mean(weib_fitted)
  score[i,3] = mean(weib_fitted)

  i = i + 1
}

apply(X = score,MARGIN = 2, FUN = mean)
# Mean squared error, Weibull is best option

# but are these distributions really good fits? We can do chisq test to validate MSE
chisq.test(test_lognorm)
chisq.test(test_weibull)
chisq.test(test_gamma)
# doesn't tell us much, but weibull has smaller test statistic, going with that

## So weibull distribution with these parameters it is!!!


#------ Finding the best distribution for my food spending
food = bdg$Amount[bdg$Class == "Food"]

f <- clean_values(food)
f = as.numeric(f)
initial_plots(f)

fitdistr(f[f!=0], "lognormal")
fitdistr(f[f!=0], "weibull")
fitdistr(f[f!=0], "gamma")

food_shape_lest = 1.6780547
food_scale_lest = 0.819799
food_shape_weib_est = 1.1296036
food_scale_weib_est = 8.703707
food_shape_gam_est = 1.545948
food_rate_gam_est = 0.202005

# lognormal distribution analog estimations
logn_sd.hat <- sqrt( log(1 + (var.hat / mean.hat^2) )) 
logn_mean.hat <- log( mean.hat / sqrt(1 + (var.hat / mean.hat^2)) )

food_score = matrix(data = c(0,0), nrow = 10000, ncol = 3)
colnames(food_score) = c("Lognormal Std.Error","Weibull Std.Error", "Gamma Std.Error")

set.seed(123)

for (i in 1:10000) {
  empirical    = f[f>0]
  test_lognorm = rlnorm(n = length(empirical), meanlog = food_shape_lest, sdlog = food_scale_lest)
  test_weibull = rweibull(n = length(empirical), shape = food_shape_weib_est, scale = food_scale_weib_est)
  test_gamma   = rgamma(n = length(empirical), shape = food_shape_gam_est, rate = food_rate_gam_est)
  
  logn_fitted  = abs(test_lognorm - empirical)
  weib_fitted  = abs(test_weibull - empirical)
  gamma_fitted = abs(test_gamma   - empirical)
  
  food_score[i,1] = mean(logn_fitted)
  food_score[i,2] = mean(weib_fitted)
  food_score[i,3] = mean(weib_fitted)
  
  i = i + 1
}
apply(X = food_score,MARGIN = 2, FUN = mean)
# Mean squared error, Lognormal is best option

# but are these distributions really good fits? We can do chisq test to validate MSE
chisq.test(test_lognorm)
chisq.test(test_weibull)
chisq.test(test_gamma)



#------Annual Budget Estimations (100% confidence Interval)
# using lognormal distribution for food
# using weibull distribution for groceries

annual_estimates <- matrix(data = NA, nrow = 2, ncol = 2)
colnames(annual_estimates) = c("Min est","Max est")
rownames(annual_estimates) = c("Food","Groceries")

food_totals         = matrix(data = NA, nrow = 10000, ncol = 1)
grocery_totals      = matrix(data = NA, nrow = 10000, ncol = 1)

for (i in 1:10000) {
  food_lognorm      = rlnorm(n = length(empirical), meanlog = food_shape_lest, sdlog = food_scale_lest)
  groceries_weib    = rweibull(n = length(empirical), shape = groceries_shape_weib_est, 
                             scale = groceries_scale_weib_est )
  
  food_totals[i,1]    = sum(food_lognorm)
  grocery_totals[i,1] = sum(groceries_weib)
  
  i = i + 1
}

annual_estimates[1,1] = min(food_totals)
annual_estimates[1,2] = max(food_totals)
annual_estimates[2,1] = min(grocery_totals)
annual_estimates[2,2] = max(grocery_totals)
