# Read Packages
library(rstan)
library(tidyverse)
library(bayesplot)
require(MASS)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Generate Data
	# Remember the shape of distribution (fat middle) and each stratum should have enough number of observations.
num_data <- 1000

age <- runif(num_data, min=2, max=5)
gender <- sample(0:1, num_data, replace=T)
income <- runif(num_data, min=2, max=8)
treatment <- sample(0:1, num_data, replace=T)
data <- data.frame(age, gender, income, treatment)
covariates <- as.matrix(data[,c("age", "gender", "income")]) # be careful with the order (should be consistent throughout the analysis)


makeY <- function(x){
	if(x < 14.5)
		return (1)
	if(x <18)
		return (2)
	if(x <23)
		return (3)
	return (4)
}

beta_ <- 2
betaX <- c(2.8, -1, 1.68)
l <- beta_ * treatment + covariates %*% betaX + rlogis(num_data, scale=1)
data$Y <- apply(l, 1, makeY)
summary(l)
table(data$Y, data$treatment)
plot(factor(data$Y)[data$treatment==0], l[data$treatment==0])
plot(factor(data$Y)[data$treatment==1], l[data$treatment==1])

#data %>%
#	dplyr::select_('-treatment') %>%
#	group_by(Y) %>%
#	summarise_each(funs(mean)) %>%
#	dplyr::select_('-Y') %>%
#	as.matrix() -> mean_covariates

# Analysis

data %>% summarise_each(funs(mean)) %>%
	dplyr::select_('-Y', '-treatment') -> mean_covariates


stan_data <- list(N=nrow(data), K=4, M=3, mean_cov=mean_covariates,
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
	real T3;
	real C3;
	real T4;
	real C4;
	real diff1;
	real diff2;
	real diff3;
	real diff4;

	T1 = 1 - inv_logit(mean_cov[1,] * betaX + beta*1 - c[1]);
	C1 = 1 - inv_logit(mean_cov[1,] * betaX + beta*0 - c[1]);
	diff1 = T1 - C1;

	T2 = inv_logit(mean_cov[1,] * betaX + beta*1 - c[1]) - inv_logit(mean_cov[1,] * betaX + beta*1 - c[2]);
	C2 = inv_logit(mean_cov[1,] * betaX + beta*0 - c[1]) - inv_logit(mean_cov[1,] * betaX + beta*0 - c[2]);
	diff2 = T2 - C2;

	T3 = inv_logit(mean_cov[1,] * betaX + beta*1 - c[2]) - inv_logit(mean_cov[1,] * betaX + beta*1 - c[3]);
	C3 = inv_logit(mean_cov[1,] * betaX + beta*0 - c[2]) - inv_logit(mean_cov[1,] * betaX + beta*0 - c[3]);
	diff3 = T3 - C3;

	T4 =  inv_logit(mean_cov[1,] * betaX + beta*1 - c[3]);
	C4 =  inv_logit(mean_cov[1,] * betaX + beta*0 - c[3]);
	diff4 = T4 - C4;

}
"
ordered_model <- stan_model(model_code = model)

result <- sampling(ordered_model, data = stan_data, chains = 3)


posterior <- as.matrix(result)
mcmc_areas(posterior, 
           pars = c("T1", "C1", "diff1"), 
           prob = 0.95)

mcmc_areas(posterior, 
           pars = c("T2", "C2", "diff2"), 
           prob = 0.95)

mcmc_areas(posterior, 
           pars = c("T3", "C3", "diff3"), 
           prob = 0.95)

mcmc_areas(posterior, 
           pars = c("T4", "C4", "diff4"), 
           prob = 0.95)

mcmc_areas(posterior, 
           pars = c("beta", "betaX[1]", "betaX[2]", "betaX[3]"), 
           prob = 0.95)

mcmc_areas(posterior, 
           pars = c("c[1]", "c[2]", "c[3]"), 
           prob = 0.95)

## MASS Ordered Logit
res <- polr(as.factor(Y)~treatment+age+gender+income, data=data, Hess=T)
summary(res)
