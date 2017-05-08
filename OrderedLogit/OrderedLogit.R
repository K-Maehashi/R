# Read Packages
library(rstan)
library(tidyverse)
library(bayesplot)
require(MASS)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Generate Data
num_data = 2000
age <- sample(20:50, num_data, replace=T)
gender <- sample(0:1, num_data, replace=T)
treatment <- sample(0:1, num_data, replace=T)
data <- data.frame(age, gender, treatment)

baseprob <- c(1,5,2,1.5,1)

genderboost_fun <- function(x){
  if (x==0){# Male
    return (c(1,1,1,1,1))
  }
  return (c(0.97,1.02,1,1.02,0.98))
}

treatmentboost <- c(0.5,0.3,1,1.5,3.8)

ageboost_fun <- function(x){
  if(x < 35){
    return (c(1,1,1,1,1))
  }
  if(x<45){
    return (c(0.98,0.98,1,1.02,1.03))
  }
  return (c(0.99,1.02,1,1.03,1.02))
}

makeout <- function(x){
  ageboost <- ageboost_fun(x[1])
  genderboost <- genderboost_fun(x[2])
  if(x[3]==0){# Control
   prob <- baseprob * ageboost * genderboost 
   prob <- prob / sum(prob)
   Y <- sample(1:5, 1, replace=T, prob)
   return (Y)
  }
  if(x[3]==1){# Treated
   prob <- baseprob * ageboost * genderboost * treatmentboost
   prob <- prob / sum(prob)
   Y <- sample(1:5, 1, replace=T, prob)
   return (Y)
  }
}

data$Y <- apply(data, 1, makeout)

# Analysis
covariates <- as.matrix(data[,c("gender", "age")])

#data %>%
#	dplyr::select_('-treatment') %>%
#	group_by(Y) %>%
#	summarise_each(funs(mean)) %>%
#	dplyr::select_('-Y') %>%
#	as.matrix() -> mean_covariates
data %>% summarise_each(funs(mean)) %>%
	dplyr::select_('-Y', '-treatment') -> mean_covariates


stan_data <- list(N=nrow(data), K=5, M=2, mean_cov=mean_covariates,
                  treatment=data$treatment,
                  covariates=covariates,
                  Y=data$Y)

model <- "
data{
  int<lower=0> N;
  int<lower=1> K;
  int<lower=1> M;
  matrix[N, M] covariates;
  int<lower=0, upper=1> treatment[N];
  matrix[1,M] mean_cov;
  int<lower=1,upper=K> Y[N];
}
parameters{
  vector[M] betaX; // beta for covariates
  real beta; // beta for treatment
  ordered[K-1] c;
}
model{
  for(i in 1:N){
    Y[i] ~ ordered_logistic(covariates[i,] * betaX + beta*treatment[i], c);
  }

}
generated quantities{
	real T1; // category 1 in treatment group
	real C1; // category 1 in control group
	real T2;
	real C2;
	real T5;
	real C5;
	real diff1;
	real diff2;
	real diff5;

	T1 = 1 - inv_logit(mean_cov[1,] * betaX + beta*1 - c[1]);
	C1 = 1 - inv_logit(mean_cov[1,] * betaX + beta*0 - c[1]);
	diff1 = T1 - C1;

	T2 = inv_logit(mean_cov[1,] * betaX + beta*1 - c[1]) - inv_logit(mean_cov[1,] * betaX + beta*1 - c[2]);
	C2 = inv_logit(mean_cov[1,] * betaX + beta*0 - c[1]) - inv_logit(mean_cov[1,] * betaX + beta*0 - c[2]);
	diff2 = T2 - C2;

	T5 =  inv_logit(mean_cov[1,] * betaX + beta*1 - c[4]);
	C5 =  inv_logit(mean_cov[1,] * betaX + beta*0 - c[4]);
	diff5 = T5 - C5;

}
"
ordered_model <- stan_model(model_code = model)

result <- sampling(ordered_model, data = stan_data, chains = 3)


posterior <- as.matrix(result)
mcmc_areas(posterior, 
           pars = c("T1", "C1", "diff1"), 
           prob = 0.95)

posterior <- as.matrix(result)
mcmc_areas(posterior, 
           pars = c("T2", "C2", "diff2"), 
           prob = 0.95)

posterior <- as.matrix(result)
mcmc_areas(posterior, 
           pars = c("T5", "C5", "diff5"), 
           prob = 0.95)


## MASS Ordered Logit
res <- polr(as.factor(Y)~treatment+age+gender, data=data, Hess=T)
summary(res)
