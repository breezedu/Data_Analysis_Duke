## ShuaiQi's Project
## Date 11-04-2015
## Aim:
## @ authors:
## Data source:
## Models:
## Parameters:


#using glmer for poisson multilevel regression
install.packages("lme4")
install.packages("Matrix")

library("Matrix")
library("lme4")



my.control=glmerControl(optCtrl=list(maxfun=20) ) 
fitgene<-glmer(envarpfc~(1|gene)+envarp+1,data=table, family=poisson(link="log"),control = my.control) 
summary(fitgene)
result<-coef(fitgene)$gene
write.table(result, "C:/Users/shuaiqi/Desktop/duke/Andrew/project/geneglmer.txt"
, sep="\t")
result<-read.table("C:/Users/shuaiqi/Desktop/duke/Andrew/project/geneglmer.txt",head=T)
colnames(result)<-c("gene","randomef","fixslop")


#logistic regression and ROC for whole data set
mylogit <- glm(disease~randomef, data = genetest, family = binomial(logit))
summary(mylogit)
#model logit(p)= -1.90142-0.11059*randomef, the smaller the randomef is, the higher the prob
#is to getting disease 
# logit(p)=0----> -1.90142-0.11059*randomef=0----->randomef=-17.19342
prob<-predict(mylogit, type="response") 
logit<-genetest
logit$preprob=prob
head(logit)
#check the ROC for logistic regression 
library(pROC)
curve <- roc(disease ~ prob, data = logit,ci=T)
plot(curve)  
auc(curve) # Area under the curve


#logistic regression and ROC for OMIM subset 
loghap<-glm(disease~ randomef , data = hap, family = binomial(logit))
summary(loghap)
prob<-predict(loghap, type="response") 
hap$preprob=prob
head(hap)
#check the ROC for logistic regression 
library(pROC)
curve <- roc(disease~ prob, data = hap,ci=T,smooth=T)
plot(curve)  
auc(curve) # Area under the curve

logDneg<-glm(disease~randomef, data =Dneg, family = binomial(logit))
summary(logDneg)
prob<-predict(logDneg, type="response") 
Dneg$preprob=prob
head(Dneg)
#check the ROC for logistic regression 
library(pROC)
curve <- roc(disease ~ prob, data = Dneg,ci=T)
plot(curve)  
auc(curve) # Area under the curve

logdenove<-glm(disease~randomef, data =denove, family = binomial(logit))
summary(logdenove)

logrece<-glm(disease~randomef, data =rece, family = binomial(logit))
summary(logrece)

logomim<-glm(disease~randomef, data =omimall, family = binomial(logit))
summary(logomim)






     


#############################################################
## have to download and install RTools33.exe for windows OS
#############################################################
#use stan
install.packages("rstan")

library("rstan")


## For execution on a local, multicore CPU with excess RAM we recommend calling
## rstan_options(auto_write = TRUE)
## options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


gene_code<-"
data{   #get the data we have
int <lower=0> N; #number of obs
int <lower=0> J; #number of gene level 
int <lower=1,upper=J> gene[N];
vector[N] x;  #x
int <lower=0> y[N] ; #y
}

parameters{ #specify the parameter we want to know 
vector[J] a;  #random intercept when gene is the level 
real <lower=0> sigma_a;  #variance of intercept
real <lower=0> sigma_epsilon; #variance of dispersion
real beta;    #common slope;
vector[N] epsilon_raw;
}

transformed parameters{ #specify the model we will use 
vector[N] lambda;
vector[N] epsilon; #amount of dispersion 
for (i in 1:N) 
     epsilon[i]<-sigma_epsilon*epsilon_raw[i];
for (i in 1:N) 
     lambda[i] <- beta*x[i]+a[gene[i]]+epsilon[i];#specify the group
}

model { #give the prior distribution
  
  beta ~ normal(0,10);
  a ~ normal(0, sigma_a);
  epsilon_raw~normal(0,1);
  y ~ poisson_log(lambda); #y and y_hat should have same type 
}
"





N<-dim(table)[1]
J<-dim(table1)[1]
gene<-as.numeric(table$gene)
genelevel<-length(unique(gene))
index<-match(gene, unique(gene)) 
M1_table<-list(N=N, J=J, y=table$envarpfc,
x=table$envarp,gene=index)


## Jeff, add one fit0 here
fit0 <- stan(file = 'possion.gene.rstan .stan')

fit1 <- stan(model_code=gene_code, data=M1_table, iter=200, chains=4)

print(fit1, "a")
print (fit1, "beta")
answer1<-extract(fit1, permuted = TRUE)
effect<-answer1$a

write.table(effect, "D:/GitHubRepositories/DataStats/Data_Analysis_Duke/SQProject/effectstan.txt", sep="\t")

#check convergence 
pdf("D:/GitHubRepositories/DataStats/Data_Analysis_Duke/SQProject/traceplot.pdf")
traceplot(fit1,pars=c("a","beta"))
dev.off()


############
