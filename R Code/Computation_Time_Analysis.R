###############################
## Computation Time Analysis ##
###############################


## Create Functions. - Fast to run


## Quarter circle algorithm to estimate pi. Return estimate at final n.
generate_quarter_circle_estimates = function(n){
  
  sum = 0
  
  for (i in 1:n){
    x = runif(1)
    fx = sqrt(1-x^2)
    sum = sum +fx
  }
  
  
return((sum/n)*4)
}

## Benroulli number generator algorithm to estimate pi. Return estimate at final n.
generate_bernoulli_estimates = function(n){
  
  sum = 0
  
  for (i in 1:n){
    u = runif(1)
    x = 1/u-1
    fx = (x/(exp(x)-1))/u^2
    sum = sum +fx
  }
  
  return(sqrt((sum/n)*6))
}




##############################################
## Benchmarking - Takes about 10-15 minutes ##
##############################################

set.seed(1234)
## Compare both functions to find fastest over 100 mill simulations.
library(rbenchmark)
benchmark(generate_quarter_circle_estimates(10^8), order = "relative",replications = 1) ## 189.824
benchmark(generate_bernoulli_estimates(10^8), order = "relative",replications = 1) ## 190.441


#######################################
## Benchmarking - END .. Run to here ##
#######################################





#############################################
## Final Estimates - Takes about 5 minutes ##
#############################################

## Get an idea of accuracy at end of 100 mill simulations.
set.seed(1234)
qcircle_est = generate_quarter_circle_estimates(10^8) ## 3.141664
bern_est = generate_bernoulli_estimates(10^8) ## 3.141522


#########################################
## Final Estimates - END.. Run to here ##
#########################################






##############################################################
## C++ Code to run a billion simulations for quarter circle ##
##############################################################

## Very fast to run!

# install.packages("Rcpp")
# install.packages("inline")
library(Rcpp)
library(inline)

## Same logic as previous R function
body_quarter_circle <- '

  int n = as<int>(N);
  srand(123);
  double sum = 0;

  for(int i = 0;i<n;i++){
  
    double x = (float) rand()/RAND_MAX;
    double fx = sqrt(1-pow(x,2));
    sum = sum +fx;
    
  }
    
  return wrap((sum/n)*4);
'
quarter_circle_est <- cxxfunction ( signature ( N = "integer") ,body = body_quarter_circle ,plugin = "Rcpp" )
quarter_circle_est(10^9)

#####################
## c++ Run to here ##
#####################

