# Class Activity: Apply
# Date: February 23, 2017
# Author: Hyunjoo Oh
#########################


##8.15 Class activity.  No loops allowed.

# Before starting, clean the environment.
rm(list=ls())

# 1) Make a three dimensional array with dim=c(20,5, 1000) and fill it with random data.  
## Think of this as 1000 random datasets with 20 observations and 5 covariates

my.array <- array(rnorm(1000), dim = c(20, 5, 1000)) 
my.array # a set of arrays with (ncol=5, n=row=20)X10 is created
dim(my.array)



# 2) Here is the vector of covariates
Beta <- matrix(c(1,2,0,4,0), ncol=1)
Beta

# Make a function to create "Y" values (for a linear model).  
# The Y-values should be a linear combination of the X's plus some random noise.
# The output should be a 20 by 1000 array.

library(plyr) # use plyr package
# by using "aaply" function, create Y
# ?aaply
# add some randomly distributed random noise
Y <- aaply(my.array, .margins = 3, .fun = function(x) x%*%Beta+rnorm(20) )
dim(Y)

Y <- t(Y) # change the dimension to 20 by 1000
dim(Y)




#3) Run 1,000 regressions across all of this simulated data.  
#Have as the output a 1000 by 6 matrix of estimated regression coefficients.

# make a function that create beta hat
get.beta.hat <- function(i, Y, x){
  coef(lm(Y[,i] ~ my.array[,,i], Y = Y, x = my.array))
}

# testing the function
get.beta.hat(1, Y, x)
coef(lm(Y[,1] ~ my.array[,,1])) #compare to this

# create the output a 1000 by 6 matrix of estimated regression coefficients,
# by using "laply" function
# ?laply
Beta.hat <- laply(.data=1:dim(my.array)[3], .fun = get.beta.hat, Y, my.array, .parallel = FALSE)
Beta.hat
str(Beta.hat)

#4) Create a density plot for each of the 6 coefficients (each of
#which should have been estimated 1,000 times).
dev.off()
# combined plots
par(mfrow=c(2,3)) 
# apply function to create density plots for columns
apply(Beta.hat[,1:6], MARGIN = 2, FUN = function(x) plot(density(x), main = paste("Density Plot of Beta-hat")))


# What does this represent?

# If we run the regression multiple times, we get normally distributed coefficients, 
# which would center around the true value.


#5) Re-run that code in parallel.  
# changing the laply( ~ , parallel=TRUE)
Beta.hat.parallel <- laply(.data=1:dim(my.array)[3], .fun = get.beta.hat, Y, my.array, .parallel = TRUE)
Beta.hat.parallel

#How does the system time compare for the parallel version?

library('doMC')
library('foreach')

registerDoMC(cores==4)

system.time(out1 <- laply(.data=1:dim(my.array)[3], .fun = get.beta.hat, Y, my.array, .parallel = FALSE))
##  user  system elapsed 
## 2.135   0.032   2.194 

system.time(out2 <- system.time(out <- laply(.data=1:dim(my.array)[3], .fun = get.beta.hat, Y, my.array, .parallel = TRUE)))
##  user  system elapsed 
## 3.045   0.048   3.154 


## NOTE: The new "hot" functionality is from Haldey's tidyverse (dplyr) etc.  
## See new book "R for Data Science"

