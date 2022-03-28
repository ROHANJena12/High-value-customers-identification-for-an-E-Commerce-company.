library(tidyverse)
data <- read.csv("Ecommerce.csv")
str(data)
summary(data)
sum(is.na(data))
dim(data)
#Removing Blank Column and remove records where Customer ID is NA
data$X <- NULL
data <- na.omit(data)
str(data)
dim(data)
#Import Libraries
#install.packages("rfm")
library(rfm)
library(ggpubr)
library(tidyverse)
library(gridExtra)
library(dplyr)
library(lubridate)
library(ggplot2)
library(cluster)
library(NbClust)
library(factoextra)
library(data.table)
library(fpc)
data$InvoiceDate <- as.Date(data$InvoiceDate,format="%d-%b-%y")
class_vars <- sapply(data, class)
data[ class_vars == 'character'] <- lapply(data[class_vars == 'character'], as.factor)
str(data)
cur_date <- date("2021-11-20")
dim(data)
sum(is.na(data))
# Final Data Set for Calculating RFM Result

finaldata <-cbind(subset(data, UnitPrice>0,select= c(CustomerID,InvoiceDate,UnitPrice)),cur_date)
str(finaldata)
#Calulation of Recency, Frequency and Monetary Score usng RFM Library.

rfmresult <- rfm_table_order(finaldata,CustomerID,InvoiceDate,UnitPrice,cur_date)
findata <- data.frame(rfmresult$rfm)
str(findata)
#Creating data frame with RFM data.

culstdata1 <- subset(findata, select = c(recency_days,transaction_count,amount))
str(culstdata1)
#Data Scaling
mns <- apply(culstdata1,2, mean)
sds <- apply (culstdata1,2,sd)
stdata <-scale(culstdata1,center = mns, scale=sds)
summary(stdata)
#Applying K Means Clustering Method
# Will use first 5 number of cluster since we have maximum RFM Score is 5.

set.seed(222)
kmeanclust5 <- kmeans(stdata,5)
kmeanclust5$centers
clusplot(stdata, kmeanclust5$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=5, lines=0)

#Calculating quality of partition

partqual5 <- round(100*kmeanclust5$betweenss/kmeanclust5$totss,2)
partqual5
##Applying K Means Clustering Method with Start Position

# Will use first 5 number of cluster since we have maximum RFM Score is 5.

set.seed(333)
kmeanclust5pt <- kmeans(stdata,5,50)
kmeanclust5pt$centers
clusplot(stdata, kmeanclust5pt$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=5, lines=0)

#Calculating quality of partition

patqual5pt <- round(100*kmeanclust5pt$betweenss/kmeanclust5pt$totss,2)
patqual5pt

#Although Partition quality is good but still we will go for check optimum number of Cluster.
# Elbow Method
elbow <- fviz_nbclust(stdata, kmeans, method = 'wss')+
  geom_vline(xintercept = 5, linetype=2)+
  labs(subtitle = "Elbow Method")

#Silhoutte Method
Silh <- fviz_nbclust(stdata, kmeans, method = 'silhouette')+
  labs(subtitle = "silhouette")

grid.arrange(elbow,Silh, nrow=1)

#By Silhoutte method seems 4 Cluster is seems right number of cluster.
#By Elbow method seems 5 Cluster is seems right number of cluster.
#Drawing Silhoutte Plot to check the quality

set.seed(555)
silmet5 <- kmeans(stdata, centers = 5, nstart = 50)
silplot5 <- silhouette(silmet5$cluster, dist(stdata))
sil1 <- fviz_silhouette(silplot5)

set.seed(666)
silmet4 <- kmeans(stdata, centers = 4, nstart = 50)
silplot4 <- silhouette(silmet4$cluster, dist(stdata))
sil2 <-fviz_silhouette(silplot4)

grid.arrange(sil1,sil2, nrow=1)
#Cluster Plotting.

kclust5 <- fviz_cluster(silmet5, stdata, ellipse.type = "norm")
kclust4<- fviz_cluster(silmet4, stdata, ellipse.type = "norm")
grid.arrange(kclust5,kclust4, nrow=1)

#By observation of methods, seems that 4 number of cluster is okay.
#####Kmeans Cluster Algorithm####

set.seed(222)
Finalkmeanclust4 <- kmeans(stdata,4)
kplot4 <- silhouette(Finalkmeanclust4$cluster, dist(stdata))
fviz_cluster(Finalkmeanclust4, stdata, ellipse.type = "norm")

partq4 <- round(100*Finalkmeanclust4$betweenss/Finalkmeanclust4$totss,2)
partq4
## Partition Quality is low with 4 cluster as compare than 5 Cluster, We will go with 5 Cluster for Now.
#Clustering with 5 Clusters
set.seed(888)
Finalkmeanclust5 <- kmeans(stdata,5)
kplot <- silhouette(Finalkmeanclust5$cluster, dist(stdata))
fviz_cluster(Finalkmeanclust5, stdata, ellipse.type = "norm")

partq5 <- round(100*Finalkmeanclust5$betweenss/Finalkmeanclust5$totss,2)
partq5
####Hierarchical Clustering####
##Single Method

hclustering.s <- hclust(dist(stdata), method = 'single')
hclustering.s$height
h1<- plot(hclustering.s)
rect.hclust(hclustering.s,
            k=5,
            border = 'red')
#Complete Method
hclustering.c <- hclust(dist(stdata), method = 'complete')
hclustering.c$height
h2<- plot(hclustering.c)
rect.hclust(hclustering.c,
            k=5,
            border = 'red')
#Average Method
hclustering.a <- hclust(dist(stdata), method = 'average')
round(hclustering.a$height,3)
h3<- plot(hclustering.a)
rect.hclust(hclustering.a,
            k=5,
            border = 'red')
#Extracting Principle Components for visualization for Hierarchical as Visualization is not meaning fule with above graphs.

group <- cutree(hclustering.c, 5)
pccomp <- princomp(stdata)
predpc <- predict(pccomp, stdata)[,1:3]
summary(pccomp)
#herarichiel plot
cp_dt<- cbind(as.data.table(predpc),cluster = as.factor(group), label=stdata)
hplot1 <- ggplot(cp_dt,aes(Comp.1,Comp.2))+
  geom_point(aes(color = cluster),size=3)+
  ggtitle("Herarchiel Clustering")+
  theme_classic()

#K means Clusing Plot
cp_dt<- cbind(as.data.table(predpc),cluster = as.factor(Finalkmeanclust5$cluster), label=stdata)
hplot2 <- ggplot(cp_dt,aes(Comp.1,Comp.2))+
  geom_point(aes(color = cluster),size=3)+
  ggtitle("KMeans Clustering")+
  theme_classic()

grid.arrange(hplot1,hplot2, nrow=1)

## By looking the both clustering technique, K Means is looks better than Hierarchical Clustering.
## Number of User Per Cluster

table(Finalkmeanclust5$cluster)
CulsterID = Finalkmeanclust5$cluster
GroupedCustomer <- cbind(findata,CulsterID)
str(GroupedCustomer)
summary(GroupedCustomer)
GroupedCustomer['CustomerRating'] <- ifelse(GroupedCustomer$rfm_score==555, "Highly Valued",
                                            ifelse(between(GroupedCustomer$rfm_score,333,554),"Average Valued","Low Valued"))
GroupedCustomer %>%
  group_by(CulsterID)%>%
  summarise(Count=n())%>%
  ggplot(aes(CulsterID,Count))+
  geom_bar(stat = 'identity',fill='grey', width = .5)+
  geom_text(aes(label=(Count)),vjust=1.0,color="white",size=3.0)+
  ggtitle("Customer in Clusters")+
  xlab("Culsters") + ylab('Count of Customers')+
  theme_classic()

#Cluster 4 has Maximum Customers.
#Provide the number of customers who are highly valued.
#Considering the high value customer who has RFM Score is 555.

GroupedCustomer['CustomerRating'] <- ifelse(GroupedCustomer$rfm_score==555, "Highly Valued",
                                            ifelse(between(GroupedCustomer$rfm_score,333,554),"Average Valued","Low Valued"))

GroupedCustomer %>%
  group_by(CustomerRating)%>%
  summarise(Count=n())%>%
  ggplot(aes(CustomerRating,Count, fill=CustomerRating))+
  geom_bar(stat = 'identity', width = .5)+
  geom_text(aes(label=(Count)),vjust=1.0,color="white",size=3.0)+
  ggtitle("Customer in Clusters")+
  xlab("Culsters") + ylab('Count of Customers')+
  theme_classic()

##End of Project##
