library(foreign)
library(rstan)
library(tidyverse)
library(bayesplot)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

data <- read.dta("http://www.ats.ucla.edu/stat/data/ologit.dta") 
data$apply <- as.numeric(data$apply)

# Analysis
covariates <- as.matrix(data[,c("pared", "gpa")])
data %>% summarise_each(funs(mean)) %>%
	dplyr::select_('-apply', '-public') -> mean_covariates

stan_data <- list(N=nrow(data), K=3, M=2, mean_cov=mean_covariates,
                  treatment=data$public,
                  covariates=covariates,
                  Y=data$apply)


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
	real diff1;
	real diff2;
	real diff3;

	T1 = 1 - inv_logit(mean_cov[1,] * betaX + beta*1 - c[1]);
	C1 = 1 - inv_logit(mean_cov[1,] * betaX + beta*0 - c[1]);
	diff1 = T1 - C1;

	T2 = inv_logit(mean_cov[1,] * betaX + beta*1 - c[1]) - inv_logit(mean_cov[1,] * betaX + beta*1 - c[2]);
	C2 = inv_logit(mean_cov[1,] * betaX + beta*0 - c[1]) - inv_logit(mean_cov[1,] * betaX + beta*0 - c[2]);
	diff2 = T2 - C2;

	T3 =  inv_logit(mean_cov[1,] * betaX + beta*1 - c[2]);
	C3 =  inv_logit(mean_cov[1,] * betaX + beta*0 - c[2]);
	diff3 = T3 - C3;

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
           pars = c("T3", "C3", "diff3"), 
           prob = 0.95)
