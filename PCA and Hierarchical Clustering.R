######################################
##Principal Component Analysis (PCA)##
######################################

##Needed libraries
library(reshape2)
library(ggplot2)
library(FactoMineR)

#import csv data
ncaa=read.csv("/Users/Rachel/NCAA Analysis/Clustering Example/ncaa_teamdata_trim.csv", header=TRUE) #cleaned data and removed some attributes
data=ncaa[,-1] #remove unique identifiers for teams
data=data[,c(1:14,16:18)] #remove column 15, for some reason, col 15 is causing a problem with convergence of cluster algorithms

#PCA has more information, including graphs, but does not give eigenvectors (at least Rachel can't find them yet in PCA)
pca=PCA(data) #scale.unit=TRUE is default, some graphs are also default displayed
pca$eig #gives eigenvalues and corresponding % of variation
head(pca3$ind$coord) #PCs

#get eigenvectors
pca2=princomp(data, cor=TRUE)
head(pca2$scores) #PCs (verify same as PCA)
unclass(pca2$loadings) #eigenvectors for corresponding eigenvalues

###############################
###Plotting PCA observations###
###############################

#Some of these plots are default by PCA, but let's make them prettier
library(ggplot2)
scores=as.data.frame(pca2$scores)

##Plot PCA1 v PCA2 - two most essential dimensions
ggplot(data=scores, aes(x=Comp.1, y=Comp.2, label=rownames(scores)))+
  geom_hline(yintercept=0, color="gray65")+
  geom_vline(xintercept=0, color="gray65")+
  geom_text(color="tomato", alpha=0.8, size=4)+
  ggtitle("PCA plot of NCAA Team Statistics")

##Factor loadings plot (in a circle)

#First need a function that will create a circle
circle=function(center=c(0,0), npoints=100)
{
  r=1
  tt=seq(0,2*pi, length=npoints)
  xx=center[1]+r*cos(tt)
  yy=center[1]+r*sin(tt)
  return(data.frame(x=xx, y=yy))
}
corcir=circle(c(0,0), npoints=100)

#create data frame with correlations between variables and PCs
correlations=as.data.frame(cor(data[,c(1:14,16:18)], pca2$scores))

#data frame with arrows coordinates
arrows=data.frame(x1=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), y1=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), x2=correlations$Comp.1, y2=correlations$Comp.2)

#geom_path will do open circles
ggplot()+
  geom_path(data=corcir, aes(x=x,  y=y), color="gray65")+
  geom_segment(data=arrows, aes(x=x1, y=y1, xend=x2, yend=y2), color="gray65")+
  geom_text(data=correlations, aes(x=Comp.1, y=Comp.2, label=rownames(correlations)))+
  geom_hline(yintercept=0, color="gray65")+
  geom_vline(xintercept=0, color="gray65")+
  xlim(-1.1, 1.1)+
  ylim(-1.1, 1.1)+
  labs(x="pca1 axis", y="pc2 axis")+
  ggtitle("Circle of corrlations")
    
############################
###Hierarchical Clustering##
############################

#Agglomerative clustering (bottom-up)

#################
##Ward's Method##
#################

hc.ward=hclust(dist(data), method="ward") #prepare hierarchical cluster
plot(hc.ward, hang=0.2) #plot dendogram (hang just changes the visual proportionately, not needed)

##Colored Dendogram
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
op = par(bg = "#EFEFEF")

A2Rplot(hc.ward, k = 3, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black"), show.labels=FALSE) #color 3 groups
A2Rplot(hc.ward, k = 4, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black", "green"), show.labels=FALSE) #color 4 groups
A2Rplot(hc.ward, k = 5, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black", "green", "orange"), show.labels=FALSE) #color 5 groups
A2Rplot(hc.ward, k = 6, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black", "green", "orange", "purple"), show.labels=FALSE) #color 6 groups
A2Rplot(hc.ward, k = 7, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black", "green", "orange", "purple", "yellow"), show.labels=FALSE) #color 7 groups
A2Rplot(hc.ward, k = 8, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black", "green", "orange", "purple", "yellow", "green3"), show.labels=FALSE) #color 8 groups

#Does the hierarchical structure represent the data? Look at cophenetic correlation coefficient
d1=dist(data)
d2=cophenetic(hc.ward)
cor(d1,d2) #a correlation close to 1 means the hierarchical structure of the dendogram is a perfect representation of the data

####################
##Complete Linkage##
####################

hc.comp=hclust(dist(data)) #prepare hierarchical cluster, default method is "complete linkage"
par(mfrow=c(2,1)) #prepare plot location to allow for two plots side by side
plot(hc.comp, main="Complete Linkage") 
plot(hc.ward, main="Ward's Method")

##Colored dendogram
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
op = par(bg = "#EFEFEF")
A2Rplot(hc.comp, k = 3, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black"), show.labels=FALSE)
A2Rplot(hc.comp, k = 4, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black", "green"), show.labels=FALSE)
A2Rplot(hc.comp, k = 5, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black", "green", "orange"), show.labels=FALSE)
A2Rplot(hc.comp, k = 6, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black", "green", "orange", "purple"), show.labels=FALSE)
A2Rplot(hc.comp, k = 7, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black", "green", "orange", "purple", "yellow"), show.labels=FALSE)
A2Rplot(hc.comp, k = 8, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black", "green", "orange", "purple", "yellow", "green3"), show.labels=FALSE)

#Does the hierarchical structure represent the data? Look at cophenetic correlation coefficient
d1=dist(data)
d2=cophenetic(hc.comp)
cor(d1,d2) #a correlation close to 1 means the hierarchical structure of the dendogram is a perfect representation of the data

####################
##Average Linkage##
####################

hc.avg=hclust(dist(data), method="average") #prepare hierarchical cluster
par(mfrow=c(2,1)) #prepare plot location to allow for two plots
plot(hc.ward, main="Ward's Method")
plot(hc.avg, main="Average Linkage")

source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
op = par(bg = "#EFEFEF")
A2Rplot(hc.avg, k = 3, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black"), show.labels=FALSE)
A2Rplot(hc.avg, k = 4, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black", "green"), show.labels=FALSE)
A2Rplot(hc.avg, k = 5, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black", "green", "orange"), show.labels=FALSE)
A2Rplot(hc.avg, k = 6, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black", "green", "orange", "purple"), show.labels=FALSE)
A2Rplot(hc.avg, k = 7, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black", "green", "orange", "purple", "yellow"), show.labels=FALSE)
A2Rplot(hc.avg, k = 8, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black", "green", "orange", "purple", "yellow", "green3"), show.labels=FALSE)
A2Rplot(hc.avg, k = 9, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black", "green", "orange", "purple", "yellow", "green3", "red3"), show.labels=FALSE)
A2Rplot(hc.avg, k = 10, boxes = FALSE, col.up = "gray50", col.down = c("red", "blue", "black", "green", "orange", "purple", "yellow", "green3", "red3", "white"), show.labels=FALSE)

#Does the hierarchical structure represent the data? Look at cophenetic correlation coefficient
d1=dist(data)
d2=cophenetic(hc.avg)
cor(d1,d2)
