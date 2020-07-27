install.packages("readxl")
library("readxl")
airlines <- read_xlsx(file.choose())
summary(airlines)

# Normalizing continuous columns to bring them under same scale

airlines_norm<-scale(airlines[,2:12]) #excluding ID# column before normalizing
airlines_d <- dist(airlines_norm, method = "euclidean") # distance matrix

airlines_fit <- hclust(airlines_d, method="complete")
str(airlines_fit)

plot(airlines_fit, hang=-1)# display dendrogram

rect.hclust(airlines_fit, k=10, border="red")

airlines_groups <- cutree(airlines_fit, k=10) # cut tree into 10 clusters
airlines_groups

airlines_matrix<-as.matrix(airlines_groups) # groups or cluster numbers
airlines_final <- data.frame(airlines, airlines_matrix)
View(airlines_final)

airlines_final1<-airlines_final[,c(ncol(airlines_final),1:(ncol(airlines_final)-1))] # pushing last column to first
View(airlines_final1)

write.csv(airlines_final1, file="airlines_final1.csv",row.names = F)
getwd()

aggregate(airlines[,-1],by=list(airlines_final1$airlines_matrix),mean)

##############################################################3

#working as per K-Mean :

airlines_k_fit<-kmeans(airlines_norm,10) # 10 cluster solution
str(airlines_k_fit)

airlines_k_fit$cluster

airlines_k_final<-data.frame(airlines,airlines_k_fit$cluster) # append cluster
airlines_k_final

airlines_k_final1<-airlines_k_final[,c(ncol(airlines_k_final),1:(ncol(airlines_k_final)-1))]
aggregate(airlines[,2:12],by=list(airlines_k_fit$cluster),FUN=mean)

####################################

# Explain  - Requirement of K-selection
# selecting K for kmeans clustering using K selection
install.packages("kselection")
library(kselection)
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores = 4) # Take all the 4 cores to do this process to save time
airlines_k<-kselection(airlines_norm,parallel = TRUE,k_threshold = 0.95,max_centers = 13)
airlines_k
# K finds 10 clusters

airlines_twss<-NULL
for(i in 1:15)
  airlines_twss[i]=sum(kmeans(airlines_norm,centers = i)$tot.withinss)

window()
plot(1:15,airlines_twss,type="b",xlab="Number of clusters",ylab="within groups sum of squares")
title(sub="K-Means Clustering Scree-Plot")
