## This script runs the following five Monte Carlo methods:
# 1. Buffon's Experiment
# 2. Quarter Circle Integral
# 3. Bernoulli Numbers Generating Function Integral
# 4. Standard Normal Distribution Integral
# 5. Coin Toss on Grid


## Run each method one by one by highlighting from question title to end.
## both will be highlighted with loads of comment symbols.

###################
## Q(b) - Buffon ##
###################

# Number of Simulations
n = 100000

## Store results of simulations.
results_b = matrix(0,n,3)

## Stick size and gap length defined for ease to be 1.
## For next part using Lazarrini's technique this will be adjusted.
gap_length = 1
length_of_stick = 1

## Record needles that crossed in results matrix, one column per seed.
for(seed in 1:3){
  
  set.seed(1234+seed)
  
  # Generate random uniform on [0,1] and then angle on (-pi/2,pi/2).
  for (i in 1:n){
    
    u = runif(1)
    theta = runif(1,-pi/2,pi/2)
    v = u + length_of_stick*cos(theta)
    
    # if other end crosses boundary then 1 else 0.
    if(v>1){
      results_b[i,seed] = 1
    }
    else{
      results_b[i,seed] = 0
    }
  }
}

# Calculate and store estimate of Pi against number of simulations for each seed.
means_b = matrix(0,n,3)
for (i in 1:n){
  
  means_b[i,1] = (2*length_of_stick*i)/((gap_length)*(sum(results_b[1:i,1])))
  means_b[i,2] = (2*length_of_stick*i)/((gap_length)*(sum(results_b[1:i,2])))
  means_b[i,3] = (2*length_of_stick*i)/((gap_length)*(sum(results_b[1:i,3])))
  
}

# Calculate mean estimate across the three runs against N.
mean_est_b = apply(means_b,1,mean)

## Remove initial Infinity values from division by zero.
mean_est_b[1:3] = 6.3

# Similarly calculate Sample standard deviation against N for the mean estimate.
st_devs_b = rep(0,n)
for (i in 1:n){
  st_devs_b[i] = sd(mean_est_b[1:i])
}


# Mean Estimate for N at 'important' intervals.
mean(means_b[10^2,1:3]) # 3.188019
mean(means_b[10^3,1:3]) # 3.104113
mean(means_b[10^4,1:3]) # 3.128552
mean(means_b[10^5,1:3]) # 3.139004

# Vectors for plotting mean estimates with error bars. (Results at 'important' intervals)
N = c(10^2,10^3,10^4,10^5)
N_pi_est_b = c(mean_est_b[10^2],mean_est_b[10^3],mean_est_b[10^4],mean_est_b[10^5])
N_pi_std_b = c(st_devs_b[10^2],st_devs_b[10^3],st_devs_b[10^4],st_devs_b[10^5])


###########
## Plots ## 
###########


## 2 plots side by side ##
par(mfrow = c(1,2),oma=c(5,5,4,0),mar=c(0,1,0,2))


## PLOT 1 - CONVERGENCE OVER N ##

## Estimates over N for each seed
matplot(1:n,means_b, type = 'l', col ='green',ylim = c(pi-.1,pi+.1),axes=F, 
        ylab = 'Pi Estimate',xlab = 'Number of Simulations',main = '')
axis(2)
axis(1)
## Mean estimate & True Pi line
matplot(1:n,mean_est_b,type = 'l',col ='red',add = T)
abline(h = pi) 
legend('topright', legend = c('Simulation Runs 1:3','Mean Estimate','True Value for Pi'),lty = 1, lwd=2, col=c('green', 'red','black'))


## PLOT 2 - ERROR BAR PLOTS ##

matplot(1:4,N_pi_est_b,col ='red',pch = 19,ylim = c(pi-2,pi+2),ylab="",axes=F)
axis(2)
axis(side=1,at=1:4,labels=N)
abline(h = pi) 

## 68%
matplot(1:4,N_pi_est_b+N_pi_std_b,col ='orange',pch = 19,add = T)
matplot(1:4,N_pi_est_b-N_pi_std_b,col ='orange',pch = 19,add = T)

## 95%
matplot(1:4,N_pi_est_b+1.96*N_pi_std_b,col ='blue',pch = 19,add = T)
matplot(1:4,N_pi_est_b-1.96*N_pi_std_b,col ='blue',pch = 19,add = T)
legend('topright', legend = c('Mean Estimate','One Sigma Error Bars','Two Sigma Error Bars'),pch = 19, col=c('red', 'orange','blue'))

title(main = NULL, outer=TRUE,xlab = 'Number of Simulations',ylab = 'Pi Estimate')



######################################
## b) Lazarrinis Rigging of Results ##
######################################

# Using nice multiples and lengths to aim for famous rational estimate of pi.
n = 1309
length_of_stick = 3/4
test = matrix(rep(0,21),7,3)
set.seed(123)

## same logic as previous loop
for (i in 1:7){
  
  cnt = 0
  
  for(j in 1:(i*n)){
    
    u = runif(1)
    theta = runif(1,-pi/2,pi/2)
    v = u + length_of_stick*cos(theta)
    
    if(v>1){
      cnt = cnt +1  
    }
    
  }
  
  test[i,1] = i*n
  test[i,2] = cnt
  test[i,3] = 625*i
  
}

## Rename columns
colnames(test) = c('N','Crosses by Simulation','Required to Achieve Desired Accuracy')

## Calculate pi estimate at last n
pi_estimate_Lazzarini = (2*length_of_stick*test[7,1])/((gap_length)*(test[7,2]))

## Artificially high pi for circa 9000 needle drops.
pi_estimate_Lazzarini

##How accurate is this estimate?
percentage_diff = (abs(pi - pi_estimate_Lazzarini)/pi)*100


################
## END OF Qb) ##
################








############################################
## Q(d) - Quarter Circle by average value ##
############################################

# Number of Simulations
n = 100000

## Store fx simulations.
results_d = matrix(0,n,3)

## Populate fx values, one column per seed.
for(seed in 1:3){
  
  set.seed(1234+seed)
  
  # Generate random uniform on [0,1] and calculate associated fx. (formula of unit circle in this case)
  for (i in 1:n){
    x = runif(1)
    fx = sqrt(1-x^2)
    results_d[i,seed] = fx
  }

}

# Calculate and store estimate of Pi against number of simulations for each seed.
means_d = matrix(0,n,3)
for (i in 1:n){
  
  means_d[i,1] = mean(results_d[1:i,1])*4
  means_d[i,2] = mean(results_d[1:i,2])*4
  means_d[i,3] = mean(results_d[1:i,3])*4
  
}

# Calculate mean estimate across the three runs against N.
mean_est_d = apply(means_d,1,mean)

# Similarly calculate Sample standard deviation against N for the mean estimate.
st_devs_d = rep(0,n)
for (i in 1:n){
  st_devs_d[i] = sd(mean_est_d[1:i])
}

# Mean Estimate for N at 'important' intervals.
mean(means_d[10^2,1:3]) # 3.117712
mean(means_d[10^3,1:3]) # 3.128193
mean(means_d[10^4,1:3]) # 3.137728
mean(means_d[10^5,1:3]) # 3.139134

# Vectors for plotting mean estimates with error bars
N = c(10^2,10^3,10^4,10^5)
N_pi_est_d = c(mean_est_d[10^2],mean_est_d[10^3],mean_est_d[10^4],mean_est_d[10^5])
N_pi_std_d = c(st_devs_d[10^2],st_devs_d[10^3],st_devs_d[10^4],st_devs_d[10^5])


###########
## Plots ## 
###########

## 2 Plots. One convergence over all n. One with error bars.
par(mfrow = c(1,2),oma=c(5,5,4,0),mar=c(0,1,0,2))

## PLOT 1 - CONVERGENCE OVER N ##

## Estimates over N for each seed
matplot(1:n,means_d, type = 'l', col ='green',ylim = c(pi-.1,pi+.1), axes = F, 
        ylab = 'Pi Estimate',xlab = 'Number of Simulations',main = '')
axis(2)
axis(1)

## Mean estimate & True Pi line
matplot(1:n,mean_est_d,type = 'l',col ='red',add = T)
abline(h = pi) 
legend('topright', legend = c('Simulation Runs 1:3','Mean Estimate','True Value for Pi'),lty = 1, lwd=2, col=c('green', 'red','black'))

## PLOT 2 - ERROR BAR PLOT ##

matplot(1:4,N_pi_est_d,col ='red',pch = 19,ylim = c(pi-2,pi+2),ylab="",axes=F)
abline(h = pi)
axis(2)
axis(side=1,at=1:4,labels=N)

## 68%
matplot(1:4,N_pi_est_d+N_pi_std_d,col ='orange',pch = 19,add = T)
matplot(1:4,N_pi_est_d-N_pi_std_d,col ='orange',pch = 19,add = T)

## 95%
matplot(1:4,N_pi_est_d+1.96*N_pi_std_d,col ='blue',pch = 19,add = T)
matplot(1:4,N_pi_est_d-1.96*N_pi_std_d,col ='blue',pch = 19,add = T)
legend('topright', legend = c('Mean Estimate','One Sigma Error Bars','Two Sigma Error Bars'),pch = 19, col=c('red', 'orange','blue'))
title(main = NULL, outer=TRUE,xlab = 'Number of Simulations',ylab = 'Pi Estimate')

########################
## d) END OF QUESTION ##
########################







#######################
## f) x/(exp(x) − 1) ##
#######################

# Number of Simulations
n = 100000

## Store fx simulations.
results_f = matrix(0,n,3)

## Populate fx values, one column per seed.
for(seed in 1:3){
  
  set.seed(1234+seed)
  
  # Generate random uniform on [0,1] and calculate associated fx. (f(u) in this case)
  for (i in 1:n){
    u = runif(1)
    x = 1/u-1
    fx = (x/(exp(x)-1))/u^2
    results_f[i,seed] = fx
  }
  
}

# Calculate and store estimate of Pi against number of simulations for each seed.
means_f = matrix(0,n,3)
for (i in 1:n){
  
  means_f[i,1] = sqrt(mean(results_f[1:i,1])*6)
  means_f[i,2] = sqrt(mean(results_f[1:i,2])*6)
  means_f[i,3] = sqrt(mean(results_f[1:i,3])*6)
  
}

# Calculate mean estimate across the three runs against N.
mean_est_f = apply(means_f,1,mean)

# Similarly calculate Sample standard deviation against N for the mean estimate.
st_devs_f = rep(0,n)
for (i in 1:n){
  st_devs_f[i] = sd(mean_est_f[1:i])
}

# Mean Estimate for N at 'important' intervals.
mean(means_f[10^2,1:3])  # 3.150185
mean(means_f[10^3,1:3])  # 3.151452
mean(means_f[10^4,1:3])  # 3.140536
mean(means_f[10^5,1:3])  # 3.142993

# Vectors for plotting mean estimates with error bars
N = c(10^2,10^3,10^4,10^5)
N_pi_est_f = c(mean_est_f[10^2],mean_est_f[10^3],mean_est_f[10^4],mean_est_f[10^5])
N_pi_std_f = c(st_devs_f[10^2],st_devs_f[10^3],st_devs_f[10^4],st_devs_f[10^5])


###########
## Plots ## 
###########

## 2 Plots. One convergence over all n. One with error bars.
par(mfrow = c(1,2),oma=c(5,5,4,0),mar=c(0,1,0,2))

## PLOT 1 CONVERGENCE OVER N ##

## Estimates over N for each seed
matplot(1:n,means_f, type = 'l', col ='green',ylim = c(pi-.1,pi+.1), axes = F,
        ylab = 'Pi Estimate',xlab = 'Number of Simulations',main = '')
axis(2)
axis(1)
## Mean estimate & True Pi line
matplot(1:n,mean_est_f,type = 'l',col ='red',add = T)
abline(h = pi) 
legend('topright', legend = c('Simulation Runs 1:3','Mean Estimate','True Value for Pi'),lty = 1, lwd=2, col=c('green', 'red','black'))


## PLOT 2 ERROR BAR PLOTS ##

matplot(1:4,N_pi_est_f,col ='red',pch = 19,ylim = c(pi-2,pi+2),ylab="",axes=F)
abline(h = pi) 
axis(2)
axis(side=1,at=1:4,labels=N)

## 68%
matplot(1:4,N_pi_est_f+N_pi_std_f,col ='orange',pch = 19,add = T)
matplot(1:4,N_pi_est_f-N_pi_std_f,col ='orange',pch = 19,add = T)

## 95%
matplot(1:4,N_pi_est_f+1.96*N_pi_std_f,col ='blue',pch = 19,add = T)
matplot(1:4,N_pi_est_f-1.96*N_pi_std_f,col ='blue',pch = 19,add = T)
legend('topright', legend = c('Mean Estimate','One Sigma Error Bars','Two Sigma Error Bars'),pch = 19, col=c('red', 'orange','blue'))

title(main = NULL, outer=TRUE,xlab = 'Number of Simulations',ylab = 'Pi Estimate')

########################
## f) END OF QUESTION ##
########################







########################################
## f)  BONUS QUESTION STANDARD NORMAL ##
########################################

# Number of Simulations
n = 100000

## Store fx simulations.
results_f2 = matrix(0,n,3)

## Populate fx values, one column per seed.
for(seed in 1:3){
  
  set.seed(1234+seed)
  
  # Generate random uniform on [0,1] and calculate associated fx after change of limits. (really f(u))
  for (i in 1:n){
    u = runif(1)
    x = 1/u-1
    fx = (exp(-(1/2)*(x*x)))/(u^2)
    results_f2[i,seed] = fx
  }
  
}

# Calculate and store estimate of Pi against number of simulations for each seed.
means_f2 = matrix(0,n,3)
for (i in 1:n){
  
  means_f2[i,1] = mean(results_f2[1:i,1])^2*2
  means_f2[i,2] = mean(results_f2[1:i,2])^2*2
  means_f2[i,3] = mean(results_f2[1:i,3])^2*2
  
}

# Calculate mean estimate across the three runs against N.
mean_est_f2 = apply(means_f2,1,mean)

# Similarly calculate Sample standard deviation against N for the mean estimate.
st_devs_f2 = rep(0,n)
for (i in 1:n){
  st_devs_f2[i] = sd(mean_est_f2[1:i])
}

# Mean Estimate for N at 'important' intervals.
mean(means_f2[10^2,1:3])  # 3.293155
mean(means_f2[10^3,1:3])  # 3.279212
mean(means_f2[10^4,1:3])  # 3.175767
mean(means_f2[10^5,1:3])  # 3.151726

# Vectors for plotting mean estimates with error bars
N = c(10^2,10^3,10^4,10^5)
N_pi_est_f2 = c(mean_est_f2[10^2],mean_est_f2[10^3],mean_est_f2[10^4],mean_est_f2[10^5])
N_pi_std_f2 = c(st_devs_f2[10^2],st_devs_f2[10^3],st_devs_f2[10^4],st_devs_f2[10^5])


###########
## Plots ## 
###########

## 2 Plots
par(mfrow = c(1,2),oma=c(5,5,4,0),mar=c(0,1,0,2))


## PLOT 1 - CONVERGENCE OVER N ##

## Estimates over N for each seed
matplot(1:n,means_f2, type = 'l', col ='green',ylim = c(pi-.1,pi+.1), axes = F,
        ylab = 'Pi Estimate',xlab = 'Number of Simulations',main = '')
axis(2)
axis(1)

## Mean estimate & True Pi line
matplot(1:n,mean_est_f2,type = 'l',col ='red',add = T)
abline(h = pi) 
legend('topright', legend = c('Simulation Runs 1:3','Mean Estimate','True Value for Pi'),lty = 1, lwd=2, col=c('green', 'red','black'))


## PLOT 2 - ERROR BAR PLOTS ##

matplot(1:4,N_pi_est_f2,col ='red',pch = 19,ylim = c(pi-2,pi+2),ylab="",axes=F)
axis(2)
axis(side=1,at=1:4,labels=N)
abline(h = pi) 

## 68%
matplot(1:4,N_pi_est_f2+N_pi_std_f2,col ='orange',pch = 19,add = T)
matplot(1:4,N_pi_est_f2-N_pi_std_f2,col ='orange',pch = 19,add = T)

## 95%
matplot(1:4,N_pi_est_f2+1.96*N_pi_std_f2,col ='blue',pch = 19,add = T)
matplot(1:4,N_pi_est_f2-1.96*N_pi_std_f2,col ='blue',pch = 19,add = T)
legend('topright', legend = c('Mean Estimate','One Sigma Error Bars','Two Sigma Error Bars'),pch = 19, col=c('red', 'orange','blue'))
title(main = NULL, outer=TRUE,xlab = 'Number of Simulations',ylab = 'Pi Estimate')

################################
## f) BONUS Q END OF QUESTION ##
################################







######################
## h) Coin on grid ##
#####################

# Number of Simulations
n = 100000

## Store fx simulations.
results_h = matrix(0,n,3)

## Populate fx values, one column per seed.
for(seed in 1:3){
  
  set.seed(1234+seed)
  
  # generate random u,v on [0,1] to represent cord of centre of coin.
  # If itgoes into any of shaded regions it must of hit a corner!
  for (i in 1:n){
    u = runif(1,0,2)
    v = runif(1,0,2)
    bl = u^2+v^2 <= 1
    br = (u-2)^2+v^2 <= 1
    tl = (u)^2+(v-2)^2 <= 1
    tr = (u-2)^2+(v-2)^2 <= 1
    if(any(bl,br,tl,tr)){results_h[i,seed] = 1}  
    
  }
  
}

# Calculate and store estimate of Pi against number of simulations for each seed.
means_h = matrix(0,n,3)
for (i in 1:n){
  
  means_h[i,1] = 4*sum(results_h[1:i,1])/i
  means_h[i,2] = 4*sum(results_h[1:i,2])/i
  means_h[i,3] = 4*sum(results_h[1:i,3])/i
  
}

# Calculate mean estimate across the three runs against N.
mean_est_h = apply(means_h,1,mean)

# Similarly calculate Sample standard deviation against N for the mean estimate.
st_devs_h = rep(0,n)
for (i in 1:n){
  st_devs_h[i] = sd(mean_est_h[1:i])
}

# Mean Estimate for N at 'important' intervals.
mean(means_h[10^2,1:3])  # 3.093333
mean(means_h[10^3,1:3])  # 3.078667
mean(means_h[10^4,1:3])  # 3.1256
mean(means_h[10^5,1:3])  # 3.145613

# Vectors for plotting mean estimates with error bars
N = c(10^2,10^3,10^4,10^5)
N_pi_est_h = c(mean_est_h[10^2],mean_est_h[10^3],mean_est_h[10^4],mean_est_h[10^5])
N_pi_std_h = c(st_devs_h[10^2],st_devs_h[10^3],st_devs_h[10^4],st_devs_h[10^5])


###########
## Plots ## 
###########

## CONVERGENCE OVER N ##

par(mfrow = c(1,2),oma=c(5,5,4,0),mar=c(0,1,0,2))
## Estimates over N for each seed
matplot(1:n,means_h, type = 'l', col ='green',ylim = c(pi-.1,pi+.1), axes=F,
        ylab = 'Pi Estimate',xlab = 'Number of Simulations',main = '')
axis(2)
axis(1)
## Mean estimate & True Pi line
matplot(1:n,mean_est_h,type = 'l',col ='red',add = T)
abline(h = pi) 
legend('topright', legend = c('Simulation Runs 1:3','Mean Estimate','True Value for Pi'),lty = 1, lwd=2, col=c('green', 'red','black'))


## ERROR BAR PLOTS ##
matplot(1:4,N_pi_est_h,col ='red',pch = 19,ylim = c(pi-2,pi+2),ylab="",axes=F)
axis(2)
axis(side=1,at=1:4,labels=N)
abline(h = pi) 

## 68%
matplot(1:4,N_pi_est_h+N_pi_std_h,col ='orange',pch = 19,add = T)
matplot(1:4,N_pi_est_h-N_pi_std_h,col ='orange',pch = 19,add = T)

## 95%
matplot(1:4,N_pi_est_h+1.96*N_pi_std_h,col ='blue',pch = 19,add = T)
matplot(1:4,N_pi_est_h-1.96*N_pi_std_h,col ='blue',pch = 19,add = T)
legend('topright', legend = c('Mean Estimate','One Sigma Error Bars','Two Sigma Error Bars'),pch = 19, col=c('red', 'orange','blue'))

title(main = NULL, outer=TRUE,xlab = 'Number of Simulations',ylab = 'Pi Estimate')

########################
## h) END OF QUESTION ##
########################


