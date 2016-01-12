## ShuaiQi's Project
## Date 11-04-2015
## Aim:
## @ authors:
## Data source:
## Models:
## Parameters:


## Data source and copyright?
## Read in table from local hard drive:
table <- read.table("D:/GitHubRepositories/DataStats/Data_Analysis_Duke/SQProject/exon_level_process_v2.txt")


##  table<-read.table("C:/Users/shuaiqi/Desktop/duke/Andrew/data/for_asa/other_stuff/exon_level_process_v3.txt")


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



# table<-table[1:1000,]
# for the use of counting number of gene
sumenvarp<-aggregate(table$envarp, by=list(Category=table$gene), FUN=sum)

sumenvarpfc<-aggregate(table$envarpfc, by=list(Category=table$gene), FUN=sum)[,2]

## simplify table1
table1<-data.frame(cbind(sumenvarp,sumenvarpfc))


## assign column names to table1
colnames(table1)<-c("gene","sumenvarp","sumenvarpfc")




##########################################################
#for the use of counting number of gene.dom
sumenvarpgenedom<-aggregate(table$envarp, by=list(Category=table$gene.dom), FUN=sum)

sumenvarpfcgenedom<-aggregate(table$envarpfc, by=list(Category=table$gene.dom), FUN=sum)[,2]


## simplify table2
table2<-data.frame(cbind(sumenvarpgenedom,sumenvarpfcgenedom))

## assign column names to table2
colnames(table2)<-c("gene.dom","sumenvarpgenedom","sumenvarpfcgenedom")


#for the use of counting number of gene.dom.sub
sumenvarpgenedomsub<-aggregate(table$envarp, by=list(Category=table$gene.dom.subdom), FUN=sum)
sumenvarpfcgenedomsub<-aggregate(table$envarpfc, by=list(Category=table$gene.dom.subdom), FUN=sum)[,2]

## simply table3, assign column names
table3<-data.frame(cbind(sumenvarpgenedomsub,sumenvarpfcgenedomsub))
colnames(table3)<-c("gene.dom.subdom","sumenvarpgenedomsub","sumenvarpfcgenedomsub")






#domains_lin_response_txt for logsitic regression 
disease<-read.table("C:/Users/shuaiqi/Desktop/duke/Andrew/data/domains_lin_response.txt",sep='')

#disease<-disease[1:100,]
location<-as.character(disease$V1)
genes<-matrix(unlist(strsplit(location, ":",fixed=T)),ncol=3,byrow=T)[,1]

#unique(genes)
domains<-matrix(unlist(strsplit(location, ":",fixed=T)),ncol=3,byrow=T)[,2]

subs<-matrix(unlist(strsplit(location, ":",fixed=T)),ncol=3,byrow=T)[,3]

loggene<-data.frame(genes,disease$V2)

loggene<-within(loggene,gene<-factor(loggene$genes))

geneLof<-aggregate(loggene$disease.V2, by=list(Category=loggene$gene), FUN=sum)

colnames(geneLof)<- c("gene","non-Lof")

#check if the glmer and disease has same gene 
result<-read.table("C:/Users/shuaiqi/Desktop/duke/Andrew/project/geneglmer.txt",head=T)

colnames(result)<-c("gene","randomef","fixslop")


## get vectors for a1 and a2, but what are these two vectors?
a1 <- result$gene
a2 <- geneLof$gene

a1[!(a1%in%a2)] # in a1 but not in a2
a2[!(a2%in%a1)] # in a2 but not in a1

genel <- merge(result,geneLof,by.x="gene",by.y="gene")

disease <- rep(0, dim(genel)[1])

## for loop: 
for (i in 1:dim(genel)[1]){
    if (genel[i,4]!=0){
        disease[i]=1
        }
     if (genel[i,4]==0){
        disease[i]=0
        }
}

genetest <- cbind(genel,disease)
head(genetest)

#rvis<-read.csv("C:/Users/shuaiqi/Desktop/duke/Andrew/data/RVIS.csv",head=T)[,-3]
#gene1<-merge(rvis,geneLof,by.x="HGNC.gene",by.y="gene")
#R<-cbind(gene1,noLof1)
#hap<-merge(RVIS,omim,by.x="HGNC.gene",by.y="OMIM.Haploinsufficiency",sort=T) [,1:5]
#dim(hap)


#subset for gene level OMIM list
omim<-read.csv("C:/Users/shuaiqi/Desktop/duke/Andrew/data/OMIM and MGI gene lists.csv",head=T)[,-2]
omimall<-merge(genetest,omim,by.x="gene",by.y="OMIM.disease.genes",sort=T) [,1:5]

dim(omimall)

hap <- merge(genetest,omim,by.x="gene",by.y="OMIM.Haploinsufficiency",sort=T) [,1:5]

dim(hap)

Dneg <- merge(genetest,omim,by.x="gene",by.y="OMIM.Dominant.Negative",sort=T) [,1:5]
dim(Dneg)

denove <- merge(genetest,omim,by.x="gene",by.y="OMIM.de.novo",sort=T) [,1:5]
dim(denove)

rece <- merge(genetest,omim,by.x="gene",by.y="OMIM.Recessive",sort=T) [,1:5]
dim(rece)


#############
## END