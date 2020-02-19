#The data was read in a text file where each column was 
#corresponding to the teacher id, rater id, and the ten 
#dimensions. Each row was corresponding to the ratings 
#given to a teacher t by rater r.     

#LOAD NECESSARY PACKAGES
library(R2jags)
library(rjags)
library(lattice) # Needed for scatterplot matrix
library(coda)
library(ggplot2)
library(dplyr)

# Set working directory
#setwd("//JAYASHRI/Users/winBugs Codes")
getwd()

#Reading in data 
dataNew<- read.table("data.txt")
# creates the colnames 
colnames(dataNew)<-c("obs","teacher","rater","itemS1","itemS2","itemS3","itemS4","itemS5",
                     "itemS6","itemS7","itemS8","itemS9","itemS10") 

#Defining the data columns to respective names
teacher = dataNew$teacher
rater = dataNew$rater
item1Score=dataNew[,4] 
item2Score=dataNew[,5] 
item3Score=dataNew[,6] 
item4Score=dataNew[,7] 
item5Score=dataNew[,8] 
item6Score=dataNew[,9] 
item7Score=dataNew[,10] 
item8Score=dataNew[,11] 
item9Score=dataNew[,12] 
item10Score=dataNew[,13] 

#Create the model and store the model in the file Model_Normal_UniformPrior.txt 

sink("Model_Normal_UniformPrior.txt")
cat("

    model 
      {
        # Level 1 definition 
            for (i in 1:N) { 
            y[i]  ~ dnorm(mu[i], tau) 
            mu[i] <-  beta0 + u2[teacher[i]]  + u3[rater[i]] 
           } 
       # Higher level definitions 
            for (j in 1:nT) { 
            u2[j] ~ dnorm(0,tau.u2) 
           } 
            for (j in 1:nR) { 
            u3[j] ~ dnorm(0,tau.u3) 
          } 
        # Priors for fixed effects 
          beta0 ~ dnorm(0, 0.001) 
        
        # Priors for random terms; tau = 1/(1.39)^2
        tau <-pow(sigma2, -1) 
        sigma2 ~ dunif(0,10) 
        tau.u2 <-pow(sigma2.u2, -1) 
        sigma2.u2 ~ dunif(0,10)
        tau.u3 <-pow(sigma2.u3, -1) 
        sigma2.u3 ~ dunif(0,10)
     }
   
    ",fill = TRUE)
sink()

# Setting up the data with all the 10 items in a for loop
# The model used for this analysis is a cross-classified model, where each item is 
# analyzed one at a time. Therefore, setting up the data nad running JAGS model in 
# such a for loop is efficient.  

dataY <- replicate(10, list())  # creates 10 lists
for(i in 4:13)                  # Starts with item1 (dataNew[,4]) to item 10 
{
  dataY[[i-3]]<-list(N=154, nT=42, nR=11, y=dataNew[,i], rater=rater, teacher=teacher) #for dataNew[,1](that is item1) we will have dataY[[1]] and so on
}

#Setting up Intial Values 
inits<-rep(list(list(beta0=3, sigma2= 0.72, sigma2.u2=1.13 ,tau.u3=2.12, 
                     u2=c(rep(0,42)), u3=c(rep(0,11)))),2)

#Parameters to monitor 
parameters <- c("beta0","u2","u3","sigma2","sigma2.u2","sigma2.u3")

# Running JAGS and obtaining the outputs. 
# We also save 10 csv files for each of the 10 items 
for(i in 1:10)    
{
temp_output <- jags(dataY[[i]], inits, parameters, model ="model_normal_Uniform.txt", n.chains=2, 
           n.iter=40000, n.burnin=500, n.thin=5, DIC=FALSE)
temp_summary=temp_output$BUGSoutput$model 
write.csv(temp_summary, paste("//JAYASHRI/Users/winBugs Codes/outI",i,"_Normal.csv",sep=""))
}

# Diagnostics for the model. 
# Checking the time series plots and ACF to test for convergence and Gelamn Diagnostics

temp_Normal=temp_output$BUGSoutput$sims.matrix

head(temp_Normal)
beta0 = temp_Normal[,1]
sigma2 = temp_Normal[,2]
sigma2.u2 = temp_Normal[,3]
sigma2.u3 = temp_Normal[,4]

par(mfrow=c(2,2))
plot(acf(sigma2))
plot(acf(sigma2.u2))
plot(acf(sigma2.u3))

#time series Plots
temp.mcmc<-as.mcmc(temp_Normal)

# The saved MCMC chain:
par(mfrow=c(2,2))
traceplot(temp_Normal)        #plots all the  mcmc chains for all the parameters of interest

#Gelman Diagnostics
out.mcmc <-as.mcmc(temp)     #use as.mcmmc to convert rjags object into mcmc.list
densityplot(out.mcmc)        # Plots density of all  chains 
par(mfrow=c(2,2)) 
gelman.plot(out.mcmc)
gelman.diag(out.mcmc)

# posterior density plots
par(mfrow=c(2,2))
plot(density(beta0),xlim=c(2,5),main="",xlab="beta0")  
plot(density(sigma2),xlim=c(0.2,2),main="",xlab="sigma2") 
plot(density(sigma2.u3),xlim=c(-0.1, 2),main="",xlab="rater variance")
plot(density(sigma2.u2),xlim=c(0,2),main="",xlab="teacher variance")  








