data{   #get the data we have
  int <lower=0> N; #number of obs
  int <lower=0> J; #number of gene level 
  int <lower=1,upper=J> gene[N];
  vector[N] x;  #x
  int <lower=0> y[N] ; #y
}

parameters{ #specify the parameter we want to know 
  vector[J] a;                  #random intercept when gene is the level 
  real <lower=0> sigma_a;       #variance of intercept
  real <lower=0> sigma_epsilon; #variance of dispersion
  real beta;                    #common slope;
  vector[N] epsilon_raw;
}

transformed parameters{ #specify the model we will use 
  vector[N] lambda;
  vector[N] epsilon;            #amount of dispersion 
  for (i in 1:N) 
       epsilon[i] <- sigma_epsilon * epsilon_raw[i];
       
  for (i in 1:N) 
       lambda[i] <- beta*x[i]+a[gene[i]]+epsilon[i]; #specify the group
}

model { #give the prior distribution
  
  beta ~ normal(0,10);
  a ~ normal(0, sigma_a);
  epsilon_raw~normal(0,1);
  y ~ poisson_log(lambda); #y and y_hat should have same type 
}

