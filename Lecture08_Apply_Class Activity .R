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

# 2) Here is the vector of covariates
Beta <- matrix(c(1,2,0,4,0), ncol=1)
Beta

# Make a function to create "Y" values (for a linear model).  
# The Y-values should be a linear combination of the X's plus some random noise.
# The output should be a 20 by 1000 array.

##### OH??? 1000 array? [(20X5) X (5X1) + (20X1)]X10 = (20X1)X10

# Make an array as a dataframe (list) so that we can use "lapply" function
my.array.df <- NULL # make an empty list

my.array.df$V1 <- my.array[,,1] # depending on the thrid dimension
# each array will be put into the list
my.array.df$V2 <- my.array[,,2]
my.array.df$V3 <- my.array[,,3]
my.array.df$V4 <- my.array[,,4]
my.array.df$V5 <- my.array[,,5]
my.array.df$V6 <- my.array[,,6]
my.array.df$V7 <- my.array[,,7]
my.array.df$V8 <- my.array[,,8]
my.array.df$V9 <- my.array[,,9]
my.array.df$V10 <- my.array[,,10]

my.array.df
class(my.array.df) # it's a list now.
my.array.df$V1%*%Beta # check if we can apply matrix multiplication to this list.
# it works!
# Create a set of randomly distributed random noise
set.seed(1234567)
ee <- array(rnorm(100), dim = c(20, 1, 10)) # random noise as a set of arrays

ee.list <- NULL # random noise as a list
ee.list$V1 <- ee[,,1]
ee.list$V2 <- ee[,,2]
ee.list$V3 <- ee[,,3]
ee.list$V4 <- ee[,,4]
ee.list$V5 <- ee[,,5]
ee.list$V6 <- ee[,,6]
ee.list$V7 <- ee[,,7]
ee.list$V8 <- ee[,,8]
ee.list$V9 <- ee[,,9]
ee.list$V10 <- ee[,,10]
ee.list

my.array.df$V1%*%Beta+ee.list$V1 # check if this works.
# it works!

Y.list <- lapply(my.array.df, function(X) X%*%Beta) # create a list Y, by using lapply.
Y <- mapply("+", Y.list, ee.list) # X%*%Beta + ee
Y # matrix of (20X1)X10
class(Y) # class=matrix


#3) Run 1,000 regressions across all of this simulated data.  
#Have as the output a 1000 by 6 matrix of estimated regression coefficients.


#4) Create a density plot for each of the 6 coefficients (each of
#which should have been estimated 1,000 times).

# What does this represent?

#5) Re-run that code in parallel.  
#How does the system time compare for the parallel version?

## NOTE: The new "hot" functionality is from Haldey's tidyverse (dplyr) etc.  
## See new book "R for Data Science"

