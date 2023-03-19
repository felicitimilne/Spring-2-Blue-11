#remove.packages(c("StanHeaders","rstan"))
#install.packages("rstan", repos = c("https://mc-stan.org/r-packages", getOption("repos")))

library(ggplot2)
library(rstan)
library(AmesHousing)
library(dplyr)
library(titanic)

test <- '

data {
    int <lower=0> y;
    int <lower=0> n;
    }
parameters {
    real <lower=0, upper=1> p;
            }
model {
    p ~ beta(1,1);
    y ~ binomial(n, p);
}'

binom.data=list(n=150, y=100)

### This is how you run Stan
binom.stan=stan(model_code = test, data=binom.data,seed=12976)

### Pull off the posterior samples
post.samp.binom=extract(binom.stan) 
new.p=post.samp.binom$p

####Graph the posterior
p.post=data.frame(new.p)
ggplot(p.post,aes(new.p))+geom_histogram()+labs(title="Posterior distribution of p",y="Frequency",x="P")

#Assignment 1

#Q1
mean(p.post$new.p)

#Q2
sd(p.post$new.p)

#Q3
quantile(p.post$new.p, 0.025)

#Q4
binom.data2=list(n=600, y=400) #keep proportion 2/3

### This is how you run Stan
binom.stan2=stan(model_code = test, data=binom.data2,seed=12976)

### Pull off the posterior samples
post.samp.binom2=extract(binom.stan2) 
new.p2=post.samp.binom2$p

####Graph the posterior
p.post2=data.frame(new.p2)
ggplot(p.post2,aes(new.p2))+geom_histogram()+labs(title="Posterior distribution of p 2",y="Frequency",x="P")

#Q5

test5 <- '

data {
    int <lower=0> y;
    int <lower=0> n;
    }
parameters {
    real <lower=0, upper=1> p;
            }
model {
    p ~ beta(2,9);
    y ~ binomial(n, p);
}'

#Q5.1
binom.data5.1=list(n=9, y=6)

### This is how you run Stan
binom.stan5.1=stan(model_code = test5, data=binom.data5.1,seed=12976)

### Pull off the posterior samples
post.samp.binom5.1=extract(binom.stan5.1) 
new.p5.1=post.samp.binom5.1$p

####Graph the posterior
p.post5.1=data.frame(new.p5.1)
ggplot(p.post5.1,aes(new.p5.1))+geom_histogram()+labs(title="Posterior distribution of p 5.1",y="Frequency",x="P")


#Q5.2
binom.data5.2=list(n=1200, y=800)

### This is how you run Stan
binom.stan5.2=stan(model_code = test5, data=binom.data5.2,seed=12976)

### Pull off the posterior samples
post.samp.binom5.2=extract(binom.stan5.2) 
new.p5.2=post.samp.binom5.2$p

####Graph the posterior
p.post5.2=data.frame(new.p5.2)
ggplot(p.post5.2,aes(new.p5.2))+geom_histogram()+labs(title="Posterior distribution of p 5.2",y="Frequency",x="P")
