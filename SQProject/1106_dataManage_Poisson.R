## ShuaiQi's Project
## Date 11-04-2015
## Aim:
## @ authors:
## Data source:
## Models:
## Parameters:


## Data source and copyright?
## Read in table from local hard drive:
## setup the working directory to where exon_level_process_v2.txt locates

##########################################
## Part One Read in the data
## create the tables
##########################################


table <- read.table("exon_level_process_v2.txt")
## table <- read.table("D:/GitHub/Stats/Data_Analysis_Duke/SQProject/exon_level_process_v2.txt")
# table<-read.table("C:/Users/shuaiqi/Desktop/duke/Andrew/data/for_asa/other_stuff/exon_level_process_v3.txt")


## assign column names
colnames(table) <- c("chr", "gene", "dom", "subdom", "exon", "gene.dom", 
             "gene.dom.subdom", 
             "envarp",    # pass
             "envarpf",   # pass functional
             "envarpfr",  # pass functional rare
             "emutr")     # mutation rate 


## attract data from the original table??
table<-within(table,envarpfc<-envarpf-envarpfr)#y

table<-within(table,gene<-factor(gene))

table<-within(table,gene.dom<-factor(gene.dom))

table<-within(table,gene.dom.subdom<-factor(gene.dom.subdom))



##
table<-table[1:5000,]
#for the use of counting number of gene
sumenvarp<-aggregate(table$envarp, by=list(Category=table$gene), FUN=sum)

sumenvarpfc<-aggregate(table$envarpfc, by=list(Category=table$gene), FUN=sum)[,2]

## simplify table1
table1<-data.frame(cbind(sumenvarp,sumenvarpfc))


## assign column names to table1
colnames(table1)<-c("gene","sumenvarp","sumenvarpfc")



#######################################################
## Part Two
## Call Stan to do the simutation
#######################################################


#install.packages("rstan")

library("rstan")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

##gene_code
gene_code <- "

	data{   #get the data we have
	int <lower=0> N; #number of obs
	int <lower=0> J; #number of gene level 
	int <lower=1,upper=J> gene[N];
	int <lower=0> x[N];  #x
	int <lower=0> y[N] ; #y

	} #end data


## parameters
parameters{ #specify the parameter we want to know 

	vector[J] a;  #random intercept when gene is the level 
	real <lower=0> sigma_a;  #variance of intercept
	real <lower=0> sigma_epsilon; #variance of dispersion
	real beta;    #common slope;
	vector[N] epsilon_raw;

	} #end parameters


## transformed parameters
transformed parameters{ #specify the model we will use 

	vector[N] lambda;
	vector[N] epsilon; #amount of dispersion 
	for (i in 1:N) 
     		epsilon[i]<-sigma_epsilon*epsilon_raw[i];
	for (i in 1:N) 
	     lambda[i] <- beta*x[i]+a[gene[i]]+epsilon[i];#specify the group
	
	} #end transformed parameters



model { #give the prior distribution
  
  beta ~ normal(0,10);
  a ~ normal(0, sigma_a);
  epsilon_raw~normal(0,1);
  y ~ poisson_log(lambda); #y and y_hat should have same type 
}
"


######################
# init the parameters
######################
N<-dim(table)[1]
J<-dim(table1)[1]
gene<-as.numeric(table$gene)
genelevel<-length(unique(gene))
index<-match(gene, unique(gene)) 
M1_table<-list(N=N, J=J, y=table$envarpfc,
x=table$envarp,gene=index)





## fit0<-stan(file='D:/GitHub/Stats/Data_Analysis_Duke/SQProject/possion.gene.rstan .stan')
## fit1<-stan(model_code="D:/GitHub/Stats/Data_Analysis_Duke/SQProject/possion.gene.rstan .stan", data=M1_table, iter=100, chains=4)


fit0 <- stan(file = "possion.gene.rstan .stan")
fit1 <- stan(fit=fit0, data = M1_table, iter = 200, chains=4)

## fit1 <- stan(model_code = gene_code, data=M1_table, iter=2000, chains=4)


print(fit1, "a")
print (fit1, "beta")
answer1<-extract(fit1, permuted = TRUE)
effect<-answer1$a
write.table(effect, "5000effectstan.txt", sep="\t")

#check convergence 
pdf("5000traceplot.pdf")
traceplot(fit1,pars=c("a","beta"))
dev.off()

##################
## END 
##################