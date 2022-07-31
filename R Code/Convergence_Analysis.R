#################
## CONVERGENCE ##
#################

## Mean results below pulled from previous simulations. See results table in paper.

mean_est_a = c(3.14185125607,3.14159523747,3.14159267943,3.14159265385)
mean_est_b = c(3.188019,3.104113,3.128552,3.139004)
mean_est_c = c(3.06670,3.17200,3.15667,3.13901)
mean_est_d = c(3.117712,3.128193,3.137728,3.139134)
mean_est_e = c(3.342480,3.175820,3.008100,3.146920)
mean_est_f = c(3.150185,3.151452,3.140536,3.142993)
mean_est_g = c(3.093333,3.169333,3.147333,3.141888)
mean_est_h = c(3.093333,3.078667,3.125600,3.145613)

## Relative difference to Pi calculated for each methods mean estimates
rel_diff_a = abs(mean_est_a-pi)/pmax(mean_est_a,pi)
rel_diff_b = abs(mean_est_b-pi)/pmax(mean_est_b,pi)
rel_diff_c = abs(mean_est_c-pi)/pmax(mean_est_c,pi)
rel_diff_d = abs(mean_est_d-pi)/pmax(mean_est_d,pi)
rel_diff_e = abs(mean_est_e-pi)/pmax(mean_est_e,pi)
rel_diff_f = abs(mean_est_f-pi)/pmax(mean_est_f,pi)
rel_diff_g = abs(mean_est_g-pi)/pmax(mean_est_g,pi)
rel_diff_h = abs(mean_est_h-pi)/pmax(mean_est_h,pi)

## Custom colours to distinguish between methods on plots
cols = c('#e60049', '#0bb4ff', '#50e991', '#e6d800', '#9b19f5', '#ffa300', '#dc0ab4', '#b3d4ff')

## Create dataframe of the above results
N = rep(c(10^2,10^3,10^4,10^5),8)
rel_diff = c(rel_diff_a,rel_diff_b,rel_diff_c,rel_diff_d,rel_diff_e,rel_diff_f,rel_diff_g,rel_diff_h)
method = c(rep('a',4),rep('b',4),rep('c',4),rep('d',4),rep('e',4),rep('f',4),rep('g',4),rep('h',4))
data = data.frame(N,rel_diff,method)
data$method = as.factor(data$method)



############
## Linear ##
############

## Any linear relationships

## Simple scatterplot
plot(y = data$rel_diff,x = data$N, col = cols[data$method],pch = 19,
     ylab = 'Relative Difference',xlab = 'N',main = 'Relative Difference vs N (Linear)',
     ylim = c(min(data$rel_diff),max(data$rel_diff)))
legend('topright', legend = c('archimedes','buffon','quarter circle','q circle integral','gamma','bernoulli','gregory-leibniz','coin toss'),pch = 19, col=cols)



##############
## Semi Log ##
##############

## "It is useful for data with exponential relationships, 
## where one variable covers a large range of values, or to zoom in and visualize that 

## Transform rel_diff by taking log10
semilog_data = data
semilog_data$rel_diff = log10(data$rel_diff)

## Semi-Log plot
plot(y = semilog_data$rel_diff,x = semilog_data$N, col = cols[semilog_data$method],pch = 19,
     ylab = 'Log(Relative Difference)',xlab = 'N',main = 'Log(Relative Difference) vs N (Semi-Log)',
     ylim = c(min(semilog_data$rel_diff),max(semilog_data$rel_diff)+2))
legend('topright', legend = c('archimedes','buffon','quarter circle','q circle integral','gamma','bernoulli','gregory-leibniz','coin toss'),pch = 19, col=cols)



#############
## log Log ##
#############

## Useful for data related by Power functions – y=ax^k would appear as a straight line in a log–log graph

## Transform other axis by taking log10
loglog_data = semilog_data
loglog_data$N = log10(semilog_data$N)

## Log-Log Plot
plot(y = loglog_data$rel_diff,x = loglog_data$N, col = cols[loglog_data$method],
     ylab = 'Log(Relative Difference)',xlab = 'Log(n)',main = 'Log(Relative Difference) vs n (Log-Log)',pch = 19,
     ylim = c(min(loglog_data$rel_diff),max(loglog_data$rel_diff)+1))

lines(y = loglog_data[loglog_data$method =='a',]$rel_diff,x = loglog_data[loglog_data$method =='a',]$N, col = cols[1])
lines(y = loglog_data[loglog_data$method =='b',]$rel_diff,x = loglog_data[loglog_data$method =='b',]$N, col = cols[2])
lines(y = loglog_data[loglog_data$method =='c',]$rel_diff,x = loglog_data[loglog_data$method =='c',]$N, col = cols[3])
lines(y = loglog_data[loglog_data$method =='d',]$rel_diff,x = loglog_data[loglog_data$method =='d',]$N, col = cols[4])
lines(y = loglog_data[loglog_data$method =='e',]$rel_diff,x = loglog_data[loglog_data$method =='e',]$N, col = cols[5])
lines(y = loglog_data[loglog_data$method =='f',]$rel_diff,x = loglog_data[loglog_data$method =='f',]$N, col = cols[6])
lines(y = loglog_data[loglog_data$method =='g',]$rel_diff,x = loglog_data[loglog_data$method =='g',]$N, col = cols[7])
lines(y = loglog_data[loglog_data$method =='h',]$rel_diff,x = loglog_data[loglog_data$method =='h',]$N, col = cols[8])

legend('topright', legend = c('archimedes','buffon','quarter circle','q circle integral','gamma','bernoulli','gregory-leibniz','coin toss'),pch = 19, col=cols)




##############################################################
## log Log after ignoring Arch and High Variance Estimators ##
##############################################################
## Arch gone
## Gamma Gone so bad
## GL & Buffon gone variance

## Recreate Df without other methodsß
cols = c('#50e991', '#e6d800', '#ffa300', '#b3d4ff')
N = rep(c(10^2,10^3,10^4,10^5),4)
rel_diff = c(rel_diff_c,rel_diff_d,rel_diff_f,rel_diff_h)
method = c(rep('c',4),rep('d',4),rep('f',4),rep('h',4))
data = data.frame(N,rel_diff,method)
data$method = as.factor(data$method)

## Only consider log log
semilog_data = data
semilog_data$rel_diff = log10(data$rel_diff)
loglog_data = semilog_data
loglog_data$N = log10(semilog_data$N)

## log log
plot(y = loglog_data$rel_diff,x = loglog_data$N, col = cols[loglog_data$method],
     ylab = 'Log(Relative Difference)',xlab = 'Log(n)',main = 'Log(Relative Difference) vs n (Log-Log)',pch = 19,
     ylim = c(min(loglog_data$rel_diff),max(loglog_data$rel_diff)))
legend('topright', legend = c('quarter circle','q circle integral','bernoulli','coin toss'),pch = 19, col=cols)
lines(y = loglog_data[loglog_data$method =='c',]$rel_diff,x = loglog_data[loglog_data$method =='c',]$N, col = cols[1])
lines(y = loglog_data[loglog_data$method =='d',]$rel_diff,x = loglog_data[loglog_data$method =='d',]$N, col = cols[2])
lines(y = loglog_data[loglog_data$method =='f',]$rel_diff,x = loglog_data[loglog_data$method =='f',]$N, col = cols[3])
lines(y = loglog_data[loglog_data$method =='h',]$rel_diff,x = loglog_data[loglog_data$method =='h',]$N, col = cols[4])
