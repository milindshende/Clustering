crime <- read.csv(file.choose())
summary(crime)

# Normalizing continuous columns to bring them under same scale
crime_norm<-scale(crime[,2:5]) #excluding the state name column before normalizing
?dist()
crime_d <- dist(crime_norm, method = "euclidean") # distance matrix
?hclust
crime_fit <- hclust(crime_d, method="complete")
str(crime_fit)
crime_fit$order
crime_fit$labels
crime_fit$height

plot(crime_fit) # display dendrogram
plot(crime_fit, hang=-1)

?cutree
rect.hclust(crime_fit, k=4, border="red")
?rect.hclust
crime_groups <- cutree(crime_fit, k=4) # cut tree into 4 clusters
crime_groups

crime_zone<-as.matrix(crime_groups) # groups or cluster numbers
crime_final <- data.frame(crime, crime_zone)
View(crime_final)

crime_final1<-crime_final[,c(ncol(crime_final),1:(ncol(crime_final)-1))] # pushing last column to first
View(crime_final1)

write.csv(crime_final1, file="crime_final1.csv",row.names = F)
getwd()

aggregate(crime[,-1],by=list(crime_final1$crime_zone),mean)

##############################################################3

#working as per K-Mean :

crime_k_fit<-kmeans(crime_norm,4) # 4 cluster solution
str(crime_k_fit)

crime_k_fit$cluster

crime_k_final<-data.frame(crime,crime_k_fit$cluster) # append cluster crime 
crime_k_final

crime_k_final1<-crime_k_final[,c(ncol(crime_k_final),1:(ncol(crime_k_final)-1))]
aggregate(crime[,2:5],by=list(crime_k_fit$cluster),FUN=mean)

####################################

# Explain  - Requirement of K-selection
# selecting K for kmeans clustering using K selection
install.packages("kselection")
library(kselection)
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores = 4) # Take all the 4 cores to do this process to save time
crime_k<-kselection(crime_norm,parallel = TRUE,k_threshold = 0.95,max_centers = 13)
crime_k
# K finds 2 clusters

twss<-NULL
for(i in 1:14)
  twss[i]=sum(kmeans(normalized_data,centers = i)$tot.withinss)

window()
plot(1:14,twss,type="b",xlab="Number of clusters",ylab="within groups sum of squares")
title(sub="K-Means Clustering Scree-Plot")
