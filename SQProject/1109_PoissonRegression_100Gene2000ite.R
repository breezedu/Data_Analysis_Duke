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
table<-table[1:100,]

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


## fit rstan()

## fit the model
fit0 <- stan(file = "possion.gene.rstan .stan")

## fit the model with data
fit1 <- stan(fit=fit0, data = M1_table, iter = 3000, chains=4)

## fit1 <- stan(model_code = gene_code, data=M1_table, iter=2000, chains=4)


print(fit1, "a")
print (fit1, "beta")
answer1<-extract(fit1, permuted = TRUE)
effect<-answer1$a
write.table(effect, "1109_100gene_effectstan.txt", sep="\t")

#check convergence 
pdf("1109_100gene_traceplot.pdf")
traceplot(fit1,pars=c("a","beta"))
traceplot(fit1, pars=c("beta", "beta"))
dev.off()

##################
## END 
##################