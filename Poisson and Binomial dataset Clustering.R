library(cluster)
library(factoextra)
library(dplyr)
library(microbenchmark)
library(ggplot2)
library(factoextra)
library(fpc)
library(dbscan)
library(tictoc)
library(flexclust)
library(LICORS)
library(mclust)

X1=vector(mode = "numeric", length = 250)
dim(X1)
X1=as.matrix(X1)
dim(X1)
X2=vector(mode = "numeric", length = 250)
dim(X2)
X2=as.matrix(X2)
dim(X2)
set.seed(9001)
for(i in 1:250){
  X1[i,1] <- rpois(1, lambda = 3+i/10)
  X2[i,1] <- rpois(1, lambda = 3+i/10)
  
}
dim(X1)
Data1=cbind(X1,X2)
Data1=as.data.frame(Data1)
names(Data1)[1]='X1'
names(Data1)[2]='X2'
str(Data1)
ggplot(Data1, aes(x=X1, y=X2)) + geom_point()

set.seed(9002)
X1=vector(mode = "numeric", length = 250)
dim(X1)
X1=as.matrix(X1)
dim(X1)
for(i in 1:250){
  X1[i,1] <- rpois(1, lambda = 4+i/12)
  X2[i,1] <- rpois(1, lambda = 40+i/12)
  }
dim(X1)
dim(X2)
Data2=cbind(X1,X2)
Data2=as.data.frame(Data2)
names(Data2)[1]='X1'
names(Data2)[2]='X2'
Data=rbind(Data1,Data2)
ggplot(Data, aes(x=X1, y=X2)) + geom_point()

set.seed(9003)
X1=vector(mode = "numeric", length = 250)
dim(X1)
X1=as.matrix(X1)
dim(X1)
for(i in 1:250){
  X1[i,1] <- rpois(1, lambda = 50)
  X2[i,1] <- rpois(1, lambda = 30)
  
}
dim(X1)
dim(X2)
Data3=cbind(X1,X2)
Data3=as.data.frame(Data3)
names(Data3)[1]='X1'
names(Data3)[2]='X2'
str(Data3)
Data=rbind(Data,Data3)
ggplot(Data, aes(x=X1, y=X2)) + geom_point()

set.seed(9005)
X1=vector(mode = "numeric", length = 250)
dim(X1)
X1=as.matrix(X1)
dim(X1)
for(i in 1:250){
  X1[i,1] <- rpois(1, lambda = 60+i/20)
  X2[i,1] <- rpois(1, lambda = 60)
  
}
dim(X1)
Data4=cbind(X1,X2)
Data4=as.data.frame(Data4)
names(Data4)[1]='X1'
names(Data4)[2]='X2'
str(Data4)
Data=rbind(Data,Data4)
ggplot(Data, aes(x=X1, y=X2)) + geom_point()


seeds_df_sc <- as.data.frame(scale(Data))
dist_mat <- dist(seeds_df_sc, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)
cut_avg <- cutree(hclust_avg, k = 4)


suppressPackageStartupMessages(library(dplyr))
seeds_df_cl <- mutate(Data, cluster = cut_avg)
count(seeds_df_cl,cluster)



suppressPackageStartupMessages(library(ggplot2))
fviz_cluster(list(data = Data, cluster = cut_avg),stand = FALSE)


accuracy=1-(16/1000)
accuracy

run=microbenchmark(hi=hclust_avg <- hclust(dist_mat, method = 'average'),times=30L) 
run

#k means

set.seed(9777)
### Elbow method (look at the knee)
# Elbow method for kmeans
fviz_nbclust(Data, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

# Average silhouette for kmeans
fviz_nbclust(Data, kmeans, method = "silhouette")

# K-means clustering
# +++++++++++++++++++++
km.res <- kmeans(Data, 4, algorithm = "Lloyd")
kmeansrun <- microbenchmark("kmeans" = km.res1 <- 
                            kmeans(Data, 4, algorithm = "Lloyd"),
                            times=30L) 
kmeansrun

# Visualize kmeans clustering
# use repel = TRUE to avoid overplotting
fviz_cluster(km.res, Data, ellipse.type = "norm",stand=FALSE)

#!!!!!!!!!!!
#the clusters according to the caption must be matched with the order that 
#the constructed data came out
#Otherwise the following code will not provide the right results


cluster1=as.data.frame(Data[km.res$cluster==3,])
dim(cluster1)
cluster2=as.data.frame(Data[km.res$cluster==1,])
dim(cluster2)
cluster3=as.data.frame(Data[km.res$cluster==2,])
dim(cluster3)
cluster4=as.data.frame(Data[km.res$cluster==4,])
dim(cluster4)

as.data.frame(Data1[,1])
as.vector(Data1[,1])
as.vector(cluster1[,1])
q1=which(Data1[,1]%in%cluster1[,1])

as.data.frame(Data1[,2])
as.vector(Data1[,2])
as.vector(cluster1[,2])
q2=which(Data1[,2]%in%cluster1[,2])
q3=as.vector(which(q1%in%q2))
kmeanscluster1_correct=length(q3)

as.data.frame(Data2[,1])
as.vector(Data2[,1])
as.vector(cluster2[,1])
q11=which(Data2[,1]%in%cluster2[,1])

as.data.frame(Data2[,2])
as.vector(Data2[,2])
as.vector(cluster2[,2])
q22=which(Data2[,2]%in%cluster2[,2])
q33=as.vector(which(q11%in%q22))
kmeanscluster2_correct=length(q33)

as.data.frame(Data3[,1])
as.vector(Data3[,1])
as.vector(cluster3[,1])
q111=which(Data3[,1]%in%cluster3[,1])

as.data.frame(Data3[,2])
as.vector(Data3[,2])
as.vector(cluster3[,2])
q222=which(Data3[,2]%in%cluster3[,2])
q333=as.vector(which(q111%in%q222))
kmeanscluster3_correct=length(q333)

as.data.frame(Data4[,1])
as.vector(Data4[,1])
as.vector(cluster4[,1])
q1111=which(Data4[,1]%in%cluster4[,1])

as.data.frame(Data4[,2])
as.vector(Data4[,2])
as.vector(cluster4[,2])
q2222=which(Data4[,2]%in%cluster4[,2])
q3333=as.vector(which(q1111%in%q2222))
kmeanscluster4_correct=length(q3333)

kmeans_Correct_Clustering=(kmeanscluster1_correct+kmeanscluster2_correct
                          +kmeanscluster3_correct
                          +kmeanscluster4_correct)/1000
kmeans_Correct_Clustering

##############################################################################
#############################################################################
#############################################################################
#############################################################################

# PAM clustering
# ++++++++++++++++++++


set.seed(9320)

### Elbow method (look at the knee)
# Elbow method for pam
fviz_nbclust(Data, pam, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

# Average silhouette for pam
fviz_nbclust(Data, pam, method = "silhouette")

pam.res <- pam(Data, 4)
pamrun <- microbenchmark("Pam" = pam.res1 <-
                          pam(Data, 4),times=30L) 
pamrun

# Visualize pam clustering
fviz_cluster(pam.res, geom = "point", ellipse.type = "norm",
             stand=FALSE)

#!!!!!!!!!!!
#the clusters according to the caption must be matched with the order that 
#the constructed data came out
#Otherwise the following code will not provide the right results

cluster1=as.data.frame(Data[pam.res$cluster==1,])
dim(cluster1)
cluster2=as.data.frame(Data[pam.res$cluster==2,])
dim(cluster2)
cluster3=as.data.frame(Data[pam.res$cluster==3,])
dim(cluster3)
cluster4=as.data.frame(Data[pam.res$cluster==4,])
dim(cluster4)

as.data.frame(Data1[,1])
as.vector(Data1[,1])
as.vector(cluster1[,1])
q1=which(Data1[,1]%in%cluster1[,1])

as.data.frame(Data1[,2])
as.vector(Data1[,2])
as.vector(cluster1[,2])
q2=which(Data1[,2]%in%cluster1[,2])
q3=as.vector(which(q1%in%q2))
Pamcluster1_correct=length(q3)

as.data.frame(Data2[,1])
as.vector(Data2[,1])
as.vector(cluster2[,1])
q11=which(Data2[,1]%in%cluster2[,1])

as.data.frame(Data2[,2])
as.vector(Data2[,2])
as.vector(cluster2[,2])
q22=which(Data2[,2]%in%cluster2[,2])
q33=as.vector(which(q11%in%q22))
Pamcluster2_correct=length(q33)

as.data.frame(Data3[,1])
as.vector(Data3[,1])
as.vector(cluster3[,1])
q111=which(Data3[,1]%in%cluster3[,1])

as.data.frame(Data3[,2])
as.vector(Data3[,2])
as.vector(cluster3[,2])
q222=which(Data3[,2]%in%cluster3[,2])
q333=as.vector(which(q111%in%q222))
Pamcluster3_correct=length(q333)

as.data.frame(Data4[,1])
as.vector(Data4[,1])
as.vector(cluster4[,1])
q1111=which(Data4[,1]%in%cluster4[,1])

as.data.frame(Data4[,2])
as.vector(Data4[,2])
as.vector(cluster4[,2])
q2222=which(Data4[,2]%in%cluster4[,2])
q3333=as.vector(which(q1111%in%q2222))
Pamcluster4_correct=length(q3333)

Pam_Correct_Clustering=(Pamcluster1_correct+Pamcluster2_correct
                        +Pamcluster3_correct
                        +Pamcluster4_correct)/1000
Pam_Correct_Clustering
############################################################################
##############################################################################
#############################################################################
#############################################################################

#CLARA

set.seed(9533)
### Elbow method (look at the knee)
# Elbow method for clara
fviz_nbclust(Data, clara, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

fviz_nbclust(Data, clara, method = "silhouette") +
  geom_vline(xintercept = 4, linetype = 2)


clarax <- clara(Data, 4, samples=50)
clarax
clararun <- microbenchmark("clara" = clarax1 <-
                            clara(Data,4,samples=50),times=30L) 
clararun

clarax$clusinfo

# Print components of clara.res
print(clarax)

# Visualize cluster
fviz_cluster(clarax, 
             ellipse.type = "t", # Concentration ellipse
             geom = "point",stand=FALSE
)

#!!!!!!!!!!!
#the clusters according to the caption must be matched with the order that the
#constructed data came out
#Otherwise the following code will not provide the right results

cluster1=as.data.frame(Data[clarax$cluster==1,])
dim(cluster1)
cluster2=as.data.frame(Data[clarax$cluster==2,])
dim(cluster2)
cluster3=as.data.frame(Data[clarax$cluster==3,])
dim(cluster3)
cluster4=as.data.frame(Data[clarax$cluster==4,])
dim(cluster4)

as.data.frame(Data1[,1])
as.vector(Data1[,1])
as.vector(cluster1[,1])
q1=which(Data1[,1]%in%cluster1[,1])

as.data.frame(Data1[,2])
as.vector(Data1[,2])
as.vector(cluster1[,2])
q2=which(Data1[,2]%in%cluster1[,2])
q3=as.vector(which(q1%in%q2))
Claracluster1_correct=length(q3)

as.data.frame(Data2[,1])
as.vector(Data2[,1])
as.vector(cluster2[,1])
q11=which(Data2[,1]%in%cluster2[,1])

as.data.frame(Data2[,2])
as.vector(Data2[,2])
as.vector(cluster2[,2])
q22=which(Data2[,2]%in%cluster2[,2])
q33=as.vector(which(q11%in%q22))
Claracluster2_correct=length(q33)

as.data.frame(Data3[,1])
as.vector(Data3[,1])
as.vector(cluster3[,1])
q111=which(Data3[,1]%in%cluster3[,1])

as.data.frame(Data3[,2])
as.vector(Data3[,2])
as.vector(cluster3[,2])
q222=which(Data3[,2]%in%cluster3[,2])
q333=as.vector(which(q111%in%q222))
Claracluster3_correct=length(q333)

as.data.frame(Data4[,1])
as.vector(Data4[,1])
as.vector(cluster4[,1])
q1111=which(Data4[,1]%in%cluster4[,1])

as.data.frame(Data4[,2])
as.vector(Data4[,2])
as.vector(cluster4[,2])
q2222=which(Data4[,2]%in%cluster4[,2])
q3333=as.vector(which(q1111%in%q2222))
Claracluster4_correct=length(q3333)

Clara_Correct_Clustering=(Claracluster1_correct+Claracluster2_correct
                          +Claracluster3_correct
                          +Claracluster4_correct)/1000
Clara_Correct_Clustering

##############################################################################
##############################################################################
#############################################################################
#DBSCAN

data("multishapes")

df <- multishapes[, 1:2]
plot(df)
ggplot(data = df, aes(x = x , y = y )) +
  geom_point()
set.seed(123)


km.res <- kmeans(df, 5, algorithm = "Hartigan-Wong",nstart = 25)
fviz_cluster(km.res, df, frame = FALSE, geom = "point")


#find epsilon
dbscan::kNNdistplot(df, k =  5)
abline(h = 0.15, lty = 2)

library("fpc")
# Compute DBSCAN using fpc package
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)
# Plot DBSCAN results
plot(db, df, main = "DBSCAN", frame = FALSE)

fviz_cluster(db, df, stand = FALSE, frame = FALSE, geom = "point")


#############################################3
# Data dataset

set.seed(9534)
# Load the data

#parameter minpt=4
dbscan::kNNdistplot(Data, k =  4)
abline(h = 3, lty = 2)

set.seed(9123)
# fpc package
res.fpc <- fpc::dbscan(Data, eps = 3, MinPts = 4)
# dbscan package
res.db <- dbscan::dbscan(Data, 3, 4)
dbscanrun5 <- microbenchmark("dbscan" = dbscan5 <- 
                              dbscan::dbscan(Data,3,4),times=30L) 
dbscanrun5
fviz_cluster(res.db, Data, geom = "point",stand=FALSE)

#!!!!!!!!!!!
#the clusters according to the caption must be matched with the order that 
#the constructed data came out
#Otherwise the following code will not provide the right results

cluster1=as.data.frame(Data[res.db$cluster==1,])
dim(cluster1)
cluster2=as.data.frame(Data[res.db$cluster==3,])
dim(cluster2)
cluster3=as.data.frame(Data[res.db$cluster==4,])
dim(cluster3)
cluster4=as.data.frame(Data[res.db$cluster==5,])
dim(cluster4)

as.data.frame(Data1[,1])
as.vector(Data1[,1])
as.vector(cluster1[,1])
q1=which(Data1[,1]%in%cluster1[,1])

as.data.frame(Data1[,2])
as.vector(Data1[,2])
as.vector(cluster1[,2])
q2=which(Data1[,2]%in%cluster1[,2])
q3=as.vector(which(q1%in%q2))
dbscan4cluster1_correct=length(q3)

as.data.frame(Data2[,1])
as.vector(Data2[,1])
as.vector(cluster2[,1])
q11=which(Data2[,1]%in%cluster2[,1])

as.data.frame(Data2[,2])
as.vector(Data2[,2])
as.vector(cluster2[,2])
q22=which(Data2[,2]%in%cluster2[,2])
q33=as.vector(which(q11%in%q22))
dbscan4cluster2_correct=length(q33)

as.data.frame(Data3[,1])
as.vector(Data3[,1])
as.vector(cluster3[,1])
q111=which(Data3[,1]%in%cluster3[,1])

as.data.frame(Data3[,2])
as.vector(Data3[,2])
as.vector(cluster3[,2])
q222=which(Data3[,2]%in%cluster3[,2])
q333=as.vector(which(q111%in%q222))
dbscan4cluster3_correct=length(q333)

as.data.frame(Data4[,1])
as.vector(Data4[,1])
as.vector(cluster4[,1])
q1111=which(Data4[,1]%in%cluster4[,1])

as.data.frame(Data4[,2])
as.vector(Data4[,2])
as.vector(cluster4[,2])
q2222=which(Data4[,2]%in%cluster4[,2])
q3333=as.vector(which(q1111%in%q2222))
dbscan4cluster4_correct=length(q3333)

dbscan4_Correct_Clustering=(dbscan4cluster1_correct+dbscan4cluster2_correct
                          +dbscan4cluster3_correct
                          +dbscan4cluster4_correct)/1000
dbscan4_Correct_Clustering

#parameter minpt=3
set.seed(9535)
dbscan::kNNdistplot(Data, k =  3)
abline(h = 2.5, lty = 2)

# fpc package
res.fpc <- fpc::dbscan(Data, eps = 2.5, MinPts = 3)
# dbscan package
res.db3 <- dbscan::dbscan(Data, 2.5, 3)
dbscanrun3 <- microbenchmark("dbscan" = dbscan3 <- 
                              dbscan::dbscan(Data,2.5,3),times=30L) 
dbscanrun3
fviz_cluster(res.db3, Data, geom = "point",stand=FALSE)

#!!!!!!!!!!!
#the clusters according to the caption must be matched with the order that the
#constructed data came out
#Otherwise the following code will not provide the right results

cluster1=as.data.frame(Data[res.db3$cluster==1,])
dim(cluster1)
cluster2=as.data.frame(Data[res.db3$cluster==5,])
dim(cluster2)
cluster3=as.data.frame(Data[res.db3$cluster==8,])
dim(cluster3)
cluster4=as.data.frame(Data[res.db3$cluster==9,])
dim(cluster4)

as.data.frame(Data1[,1])
as.vector(Data1[,1])
as.vector(cluster1[,1])
q1=which(Data1[,1]%in%cluster1[,1])

as.data.frame(Data1[,2])
as.vector(Data1[,2])
as.vector(cluster1[,2])
q2=which(Data1[,2]%in%cluster1[,2])
q3=as.vector(which(q1%in%q2))
dbscan4cluster1_correct=length(q3)

as.data.frame(Data2[,1])
as.vector(Data2[,1])
as.vector(cluster2[,1])
q11=which(Data2[,1]%in%cluster2[,1])

as.data.frame(Data2[,2])
as.vector(Data2[,2])
as.vector(cluster2[,2])
q22=which(Data2[,2]%in%cluster2[,2])
q33=as.vector(which(q11%in%q22))
dbscan4cluster2_correct=length(q33)

as.data.frame(Data3[,1])
as.vector(Data3[,1])
as.vector(cluster3[,1])
q111=which(Data3[,1]%in%cluster3[,1])

as.data.frame(Data3[,2])
as.vector(Data3[,2])
as.vector(cluster3[,2])
q222=which(Data3[,2]%in%cluster3[,2])
q333=as.vector(which(q111%in%q222))
dbscan4cluster3_correct=length(q333)

as.data.frame(Data4[,1])
as.vector(Data4[,1])
as.vector(cluster4[,1])
q1111=which(Data4[,1]%in%cluster4[,1])

as.data.frame(Data4[,2])
as.vector(Data4[,2])
as.vector(cluster4[,2])
q2222=which(Data4[,2]%in%cluster4[,2])
q3333=as.vector(which(q1111%in%q2222))
dbscan4cluster4_correct=length(q3333)

dbscan4_Correct_Clustering=(dbscan4cluster1_correct+dbscan4cluster2_correct
                            +dbscan4cluster3_correct+
                            dbscan4cluster4_correct)/1000
dbscan4_Correct_Clustering

#parameter minpt=5
dbscan::kNNdistplot(Data, k = 5)
abline(h = 3, lty = 2)

set.seed(9536)
# fpc package
res.fpc <- fpc::dbscan(Data, eps = 3, MinPts = 5)
# dbscan package
res.db5 <- dbscan::dbscan(Data, 3, 5)
dbscan5run <- microbenchmark("dbscan5" = dbscan5 <- 
                             dbscan::dbscan(Data,3,5),
                             times=30L) 
dbscan5run
fviz_cluster(res.db5, Data, geom = "point",stand=FALSE)

cluster1=as.data.frame(Data[res.db5$cluster==1,])
dim(cluster1)
cluster2=as.data.frame(Data[res.db5$cluster==2,])
dim(cluster2)
cluster3=as.data.frame(Data[res.db5$cluster==4,])
dim(cluster3)
cluster4=as.data.frame(Data[res.db5$cluster==5,])
dim(cluster4)

as.data.frame(Data1[,1])
as.vector(Data1[,1])
as.vector(cluster1[,1])
q1=which(Data1[,1]%in%cluster1[,1])

as.data.frame(Data1[,2])
as.vector(Data1[,2])
as.vector(cluster1[,2])
q2=which(Data1[,2]%in%cluster1[,2])
q3=as.vector(which(q1%in%q2))
dbscan5cluster1_correct=length(q3)

as.data.frame(Data2[,1])
as.vector(Data2[,1])
as.vector(cluster2[,1])
q11=which(Data2[,1]%in%cluster2[,1])

as.data.frame(Data2[,2])
as.vector(Data2[,2])
as.vector(cluster2[,2])
q22=which(Data2[,2]%in%cluster2[,2])
q33=as.vector(which(q11%in%q22))
dbscan5cluster2_correct=length(q33)

as.data.frame(Data3[,1])
as.vector(Data3[,1])
as.vector(cluster3[,1])
q111=which(Data3[,1]%in%cluster3[,1])

as.data.frame(Data3[,2])
as.vector(Data3[,2])
as.vector(cluster3[,2])
q222=which(Data3[,2]%in%cluster3[,2])
q333=as.vector(which(q111%in%q222))
dbscan5cluster3_correct=length(q333)

as.data.frame(Data4[,1])
as.vector(Data4[,1])
as.vector(cluster4[,1])
q1111=which(Data4[,1]%in%cluster4[,1])

as.data.frame(Data4[,2])
as.vector(Data4[,2])
as.vector(cluster4[,2])
q2222=which(Data4[,2]%in%cluster4[,2])
q3333=as.vector(which(q1111%in%q2222))
dbscan5cluster4_correct=length(q3333)

dbscan5_Correct_Clustering=(dbscan5cluster1_correct+dbscan5cluster2_correct
                            +dbscan5cluster3_correct
                            +dbscan5cluster4_correct)/1000
dbscan5_Correct_Clustering

#############################################################################
#############################################################################
#############################################################################
############################################################################

#k means ++
set.seed(9537)
a=kmeanspp(Data, k = 4, start = "random")

kmeanspprun <- microbenchmark("kmeanspp" = a1 <- 
                              kmeanspp(Data,k=4,start="random"),
                              times=30L) 
kmeanspprun
fviz_cluster(a, Data, geom = "point",stand=FALSE)

#!!!!!!!!!!!
#the clusters according to the caption must be matched with the order that 
#the constructed data came out
#Otherwise the following code will not provide the right results

cluster1=as.data.frame(Data[a$cluster==3,])
dim(cluster1)
cluster2=as.data.frame(Data[a$cluster==2,])
dim(cluster2)
cluster3=as.data.frame(Data[a$cluster==4,])
dim(cluster3)
cluster4=as.data.frame(Data[a$cluster==1,])
dim(cluster4)

as.data.frame(Data1[,1])
as.vector(Data1[,1])
as.vector(cluster1[,1])
q1=which(Data1[,1]%in%cluster1[,1])

as.data.frame(Data1[,2])
as.vector(Data1[,2])
as.vector(cluster1[,2])
q2=which(Data1[,2]%in%cluster1[,2])
q3=as.vector(which(q1%in%q2))
kmeansppcluster1_correct=length(q3)

as.data.frame(Data2[,1])
as.vector(Data2[,1])
as.vector(cluster2[,1])
q11=which(Data2[,1]%in%cluster2[,1])

as.data.frame(Data2[,2])
as.vector(Data2[,2])
as.vector(cluster2[,2])
q22=which(Data2[,2]%in%cluster2[,2])
q33=as.vector(which(q11%in%q22))
kmeansppcluster2_correct=length(q33)

as.data.frame(Data3[,1])
as.vector(Data3[,1])
as.vector(cluster3[,1])
q111=which(Data3[,1]%in%cluster3[,1])

as.data.frame(Data3[,2])
as.vector(Data3[,2])
as.vector(cluster3[,2])
q222=which(Data3[,2]%in%cluster3[,2])
q333=as.vector(which(q111%in%q222))
kmeansppcluster3_correct=length(q333)

as.data.frame(Data4[,1])
as.vector(Data4[,1])
as.vector(cluster4[,1])
q1111=which(Data4[,1]%in%cluster4[,1])

as.data.frame(Data4[,2])
as.vector(Data4[,2])
as.vector(cluster4[,2])
q2222=which(Data4[,2]%in%cluster4[,2])
q3333=as.vector(which(q1111%in%q2222))
kmeansppcluster4_correct=length(q3333)

kmeanspp_Correct_Clustering=(kmeansppcluster1_correct+kmeansppcluster2_correct
                            +kmeansppcluster3_correct
                            +kmeansppcluster4_correct)/1000
kmeanspp_Correct_Clustering
#############################################################################
#############################################################################
#############################################################################
#############################################################################

# OPTICS

### run OPTICS
set.seed(9538)
res <- optics(Data, eps = 40,  minPts = 30)
res
opticsrun <- microbenchmark("optics" = a2 <- 
                            optics(Data,eps=10,minPts=10),
                            times=30L) 
opticsrun
### get order
res$order

### plot produces a reachability plot
plot(res)
### extract a DBSCAN clustering by cutting the reachability plot at eps_cl
res <- extractDBSCAN(res, eps_cl = 6.5)
res

plot(res)  ## black is noise
hullplot(Data, res)
#look at the results of the hullplot in order to manually add the correct 
#classification
Correct_Clustering_Percentage_optics=(243+250+229+231)/1000
Correct_Clustering_Percentage_optics
### re-cut at a lower eps threshold
res <- extractDBSCAN(res, eps_cl = 7)
res
plot(res)
hullplot(Data, res)

##############################################################################
#############################################################################
#############################################################################
#############################################################################
#gaussian mixture models
set.seed(9101010)
mod <- Mclust(Data)
summary(mod$BIC)
BIC <- mclustBIC(Data)
plot(BIC)
mod1 <- Mclust(Data, x = BIC)
summary(mod1, parameters = TRUE)


dens <- densityMclust(Data)
summary(dens)

mod <- Mclust(Data, G = 4, modelName = "VVV")
summary(mod, parameters = TRUE)

gmmrun <- microbenchmark("gmm" = gmm1 <-
                         Mclust(Data, G = 4, modelName = "VVI"),
                         times=30L) 
gmmrun


dens1 <- densityMclust(Data, modelNames = "VVV", G = 4)
plot(mod, what = "classification", main = FALSE)

#!!!!!!!!!!!
#the clusters according to the caption must be matched with the order that the
#constructed data came out
#Otherwise the following code will not provide the right results

cluster1=as.data.frame(Data[mod$classification==1,])
dim(cluster1)
cluster2=as.data.frame(Data[mod$classification==2,])
dim(cluster2)
cluster3=as.data.frame(Data[mod$classification==3,])
dim(cluster3)
cluster4=as.data.frame(Data[mod$classification==4,])
dim(cluster4)

as.data.frame(Data1[,1])
as.vector(Data1[,1])
as.vector(cluster1[,1])
q1=which(Data1[,1]%in%cluster1[,1])

as.data.frame(Data1[,2])
as.vector(Data1[,2])
as.vector(cluster1[,2])
q2=which(Data1[,2]%in%cluster1[,2])
q3=as.vector(which(q1%in%q2))
gmmcluster1_correct=length(q3)

as.data.frame(Data2[,1])
as.vector(Data2[,1])
as.vector(cluster2[,1])
q11=which(Data2[,1]%in%cluster2[,1])

as.data.frame(Data2[,2])
as.vector(Data2[,2])
as.vector(cluster2[,2])
q22=which(Data2[,2]%in%cluster2[,2])
q33=as.vector(which(q11%in%q22))
gmmcluster2_correct=length(q33)

as.data.frame(Data3[,1])
as.vector(Data3[,1])
as.vector(cluster3[,1])
q111=which(Data3[,1]%in%cluster3[,1])

as.data.frame(Data3[,2])
as.vector(Data3[,2])
as.vector(cluster3[,2])
q222=which(Data3[,2]%in%cluster3[,2])
q333=as.vector(which(q111%in%q222))
gmmcluster3_correct=length(q333)

as.data.frame(Data4[,1])
as.vector(Data4[,1])
as.vector(cluster4[,1])
q1111=which(Data4[,1]%in%cluster4[,1])

as.data.frame(Data4[,2])
as.vector(Data4[,2])
as.vector(cluster4[,2])
q2222=which(Data4[,2]%in%cluster4[,2])
q3333=as.vector(which(q1111%in%q2222))
gmmcluster4_correct=length(q3333)

gmm_Correct_Clustering=(gmmcluster1_correct+gmmcluster2_correct
                        +gmmcluster3_correct
                        +gmmcluster4_correct)/1000
gmm_Correct_Clustering




