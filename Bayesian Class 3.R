# Class 3 Assignment/Quiz
# Feliciti Milne

library(ggplot2)
library(rstan)
library(rstanarm)
library(bayesplot)

# We will be using the data set BreastCancer in the mlbench package of R.
# The response variable is whether or not the sample is malignant or benign 
# (class).  Make all 9 explanatory variables as continuous.

library(mlbench)
data(BreastCancer)
BreastCancer$Cl.thickness <-as.numeric(BreastCancer$Cl.thickness)
BreastCancer$Cell.size <-as.numeric(BreastCancer$Cell.size)
BreastCancer$Cell.shape <-as.numeric(BreastCancer$Cell.shape)
BreastCancer$Marg.adhesion <-as.numeric(BreastCancer$Marg.adhesion)
BreastCancer$Epith.c.size <-as.numeric(BreastCancer$Epith.c.size)
BreastCancer$Bare.nuclei <-as.numeric(BreastCancer$Bare.nuclei)
BreastCancer$Bl.cromatin <-as.numeric(BreastCancer$Bl.cromatin)
BreastCancer$Normal.nucleoli <-as.numeric(BreastCancer$Normal.nucleoli)
BreastCancer$Mitoses <-as.numeric(BreastCancer$Mitoses)
BreastCancer$Class <-as.numeric(BreastCancer$Class)-1


# For the Bayesian analysis, be sure to use the seed 85208 with a burn-in of 
# 1000 and total number of iterations set at 2000 (with the default of 4 chains). 

bc_model<-stan_glm(Class~.-Id,
                   data=BreastCancer, 
                   seed=85208, 
                   family = binomial(link = logit),
                   chains = 4,      # number of Markov chains
                   warmup = 1000,   # number of warmup iterations per chain
                   iter = 2000,    # total number of iterations per chain
                   refresh = 0    # no progress shown
)

#Q1 
# Take a look at the posterior distributions of the beta coefficients.  
# Do any of the 95% probability intervals include 0? If so, list them.  
# Remove any variables whose probability intervals include 0 and rerun 
# the analysis.
summary(bc_model)

sims <- as.array(bc_model)
plot_title <- ggtitle("Posterior distributions", "with medians and 95% intervals")
mcmc_areas(sims, prob = 0.95) + plot_title

quant.fun1 <- function(x){ 
  temp=quantile(x,probs = 0.025) 
  return(temp)}
quant.fun2 <- function(x){ 
  temp=quantile(x,probs = 0.975) 
  return(temp)}
correct.output<-
  matrix(c(apply(sims,3,mean), 
           apply(sims,3,median), 
           apply(sims,3,sd),
           apply(sims,3,quant.fun1),
           apply(sims,3,quant.fun2)),
         ncol=5)
colnames(correct.output)<-c("mean","median","standard error","2.5%","97.5%")
rownames(correct.output)<-
  c("Intercept","Cl.thickness","Cell.size","Cell.shape","Marg.adhesion",
    "Epith.c.size","Bare.nuclei","Bl.cromatin","Normal.nucleoli", 
    "Mitoses")
correct.output

# Looks like Cell.shape, Cell.size, and Epith.c.size variables include 0 
# in their 95% probability interval. 

bc_model2<-stan_glm(Class~Cl.thickness +
                      Marg.adhesion +
                      Bare.nuclei +
                      Bl.cromatin +
                      Normal.nucleoli +
                      Mitoses,
                   data=BreastCancer, 
                   seed=85208,
                   family = binomial(link = logit),
                   chains = 4,      # number of Markov chains
                   warmup = 1000,   # number of warmup iterations per chain
                   iter = 2000,    # total number of iterations per chain
                   refresh = 0    # no progress shown
)
summary(bc_model2)
sims2 <- as.array(bc_model2)
plot_title <- ggtitle("Posterior distributions", "with medians and 95% intervals")
mcmc_areas(sims2, prob = 0.95) + plot_title

#Q2
# The traceplot for the intercept looks like it has converged.
mcmc_trace(sims2)

#Q3
# What is the mean of the posterior distribution for regression coefficient 
# for Cell Thickness (Cl.thickness)?
posterior_samples <- as.matrix(bc_model2)
mean(posterior_samples[, "Cl.thickness"])
#0.6911792

#Q4
# Get the posterior distribution for the odds ratio for Bl.cromatin.  
# What is the mean of this distribution?  (Recall how to calculate the 
# odds ratio using the coefficient in the Logistic regression) Keep 
# accuracy to two numbers beyond decimal place.
odds<- posterior_samples[, "Bl.cromatin"]/(1-posterior_samples[, "Bl.cromatin"])
mean(odds)
#2.05

#Q5
# Compare your final model (look at the means for the parameter estimates) 
# and compare them to a Logistic regression with the same variables.  
# They look similar?
correct.output2<-
  matrix(c(apply(sims2,3,mean), 
           apply(sims2,3,median), 
           apply(sims2,3,sd),
           apply(sims2,3,quant.fun1),
           apply(sims2,3,quant.fun2)),
         ncol=5)
colnames(correct.output2)<-c("mean","median","standard error","2.5%","97.5%")
rownames(correct.output2)<-
  c("Intercept","Cl.thickness","Marg.adhesion",
    "Bare.nuclei","Bl.cromatin","Normal.nucleoli", 
    "Mitoses")
print("Model 2")
correct.output2

#logistic regression model
lg_model <- glm(Class~Cl.thickness +
                  Marg.adhesion +
                  Bare.nuclei +
                  Bl.cromatin +
                  Normal.nucleoli +
                  Mitoses,
                  family=binomial(link='logit'),
                  data=BreastCancer)

#Q6 
# Copy and paste your stan code.
# Done

