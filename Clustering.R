#na shmeiwthei oti sto iris dataset uparxoun 3 diaforetika eidh

#install.packages(c("cluster", "factoextra")) exoun egkatastathei me dikaiwmata diaxeiristh
library(cluster)
library(factoextra)
##############################################################################
#############################################################################
#############################################################################

# Time Speed kai texnika datasets

#prwto dataset (sunexh,sunexh) dedomena apo kanonikes katanomes kai gia tis
#tuxaies metavlhtes

set.seed(124)
X1 <- rnorm(200, 7, 0.5)
X2 <- rnorm(200, 4.5, 1)
DataNormal1=cbind(X1,X2)
DataNormal1=as.data.frame(DataNormal1)
X1=rnorm(300, 12.5, 1.4)
X2=rnorm(300, 13.5, 1)
DataNormal2=cbind(X1,X2)
DataNormal2=as.data.frame(DataNormal2)
DataNormal<-rbind(DataNormal1, DataNormal2)
X1=rnorm(100, 12.5, 1.1)
X2=rnorm(100, 6, 0.4)
DataNormal3=cbind(X1,X2)
DataNormal3=as.data.frame(DataNormal3)
DataNormal<-rbind(DataNormal, DataNormal3)
X1=rnorm(400, 4.2, 1.1)
X2=rnorm(400, 16.3, 1.3)
DataNormal4=cbind(X1,X2)
DataNormal4=as.data.frame(DataNormal4)
DataNormal<-rbind(DataNormal, DataNormal4)

library(ggplot2)
ggplot(DataNormal, aes(x=X1, y=X2)) + geom_point()


#deutero dataset (diakrita,sunexh) apo Poisson kai kanonikh

#parathrhse oti stis times pou paragontai apo thn poisson se kathe epanalhpsh
#dineis diaforetikh timh sto lamda gia na apofugeis polles parathrhseis na exoun
#sth suntetagmanh X1 sxedon tis idies times

#enas logos pou mporei na dikaiologei to parapanw se pragmatikes katastaseis
#tha htan h montelopoihsh twn afixewn kapou stis opoies omws parathreitai mia
#diafora sto ruthmo kathws gia paradeigma proxwraei h mera, exoume dhladh ena 
#dunamiko susthma afixewn
set.seed(124)
X1=vector(mode = "numeric", length = 250)
dim(X1)
X1=as.matrix(X1)
dim(X1)
for(i in 1:250){
X1[i,1] <- rpois(1, lambda = 3+i/10)
}
dim(X1)
X2=rnorm(250, 19.2, 2)
Data1=cbind(X1,X2)
Data1=as.data.frame(Data1)
names(Data1)[1]='X1'
str(Data1)
ggplot(Data1, aes(x=X1, y=X2)) + geom_point()

set.seed(124)
X1=vector(mode = "numeric", length = 250)
dim(X1)
X1=as.matrix(X1)
dim(X1)
for(i in 1:250){
  X1[i,1] <- rpois(1, lambda = 4+i/12)
}
dim(X1)

X2=rnorm(250, 5.2, .4)
X2=as.data.frame(X2)
dim(X2)
Data2=cbind(X1,X2)
Data2=as.data.frame(Data2)
Data=rbind(Data1,Data2)
ggplot(Data, aes(x=X1, y=X2)) + geom_point()

set.seed(124)
X1=vector(mode = "numeric", length = 250)
dim(X1)
X1=as.matrix(X1)
dim(X1)
for(i in 1:250){
  X1[i,1] <- rpois(1, lambda = 15+i/7)
}
dim(X1)
X2=rnorm(250, 40.2, 2.2)
Data1=cbind(X1,X2)
Data1=as.data.frame(Data1)
names(Data1)[1]='X1'
str(Data1)
Data=rbind(Data,Data1)
ggplot(Data, aes(x=X1, y=X2)) + geom_point()

set.seed(124)
X1=vector(mode = "numeric", length = 250)
dim(X1)
X1=as.matrix(X1)
dim(X1)
for(i in 1:250){
  X1[i,1] <- rpois(1, lambda = 60+i/20)
}
dim(X1)
X2=rnorm(250, 30.1, 2.2)
Data1=cbind(X1,X2)
Data1=as.data.frame(Data1)
names(Data1)[1]='X1'
str(Data1)
Data=rbind(Data,Data1)
ggplot(Data, aes(x=X1, y=X2)) + geom_point()

 #k means

# NOT RUN {
set.seed(123)

# Data preparation
# +++++++++++++++
data("iris")
head(iris)
plot(iris$Petal.Length, iris$Petal.Width, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)],
     main="Edgar Anderson's Iris Data")# Remove species column (5) and scale the data
iris.scaled <- scale(iris[, -5])
# Optimal number of clusters in the data
# ++++++++++++++++++++++++++++++++++++++
# Examples are provided only for kmeans, but
# you can also use cluster::pam (for pam) or
#  hcut (for hierarchical clustering)

### Elbow method (look at the knee)
# Elbow method for kmeans
fviz_nbclust(iris.scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

# Average silhouette for kmeans
fviz_nbclust(iris.scaled, kmeans, method = "silhouette")

### Gap statistic
library(cluster)
set.seed(123)
# Compute gap statistic for kmeans
# we used B = 10 for demo. Recommended value is ~500
gap_stat <- clusGap(iris.scaled, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 10)
print(gap_stat, method = "firstmax")
# K-means clustering
# +++++++++++++++++++++
km.res <- kmeans(iris.scaled, 3, algorithm = "Hartigan-Wong", nstart = 10)

#an anti gia Hartigan-Wong epilexeis Lloyd to nstart de xreiazetai
#to nstart einai as poume 10 efarmoges tou algorithmou apo tis opoies krateitai
#h kaluterh
# Visualize kmeans clustering
# use repel = TRUE to avoid overplotting
fviz_cluster(km.res, iris[, -5], ellipse.type = "norm")


# Change the color palette and theme
fviz_cluster(km.res, iris[, -5],
             palette = "Set2", ggtheme = theme_minimal())
#to deutero einai kalutero giati apofeygei tis elleipseis pou mpainoun sthn perioxh
#allwn sustadwn.

# }
# NOT RUN {
# Show points only
fviz_cluster(km.res, iris[, -5], geom = "point")
# Show text only
fviz_cluster(km.res, iris[, -5], geom = "text")


km1 <- kmeans(iris.scaled, centers=3, nstart = 1, algorithm = "Lloyd")
library(tripack)
library(RColorBrewer)
CL5 <- brewer.pal(3, "Pastel1")
V <- voronoi.mosaic(km1$centers[,1],km1$centers[,2])
P <- voronoi.polygons(V)
plot(iris.scaled,6*xlim=0:1,9*ylim=0:1,xlab="",ylab="",col=CL5[km1$cluster])
points(km1$centers[,1],km1$centers[,2],pch=3,cex=1.5,lwd=2)
plot(V,add=TRUE)



##############################################################################
#############################################################################
#############################################################################
#############################################################################

# PAM clustering
# ++++++++++++++++++++



require(cluster)
# Optimal number of clusters in the data
# ++++++++++++++++++++++++++++++++++++++
# Examples are provided only for kmeans, but
# you can also use cluster::pam (for pam) or
#  hcut (for hierarchical clustering)

### Elbow method (look at the knee)
# Elbow method for pam
fviz_nbclust(iris.scaled, pam, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

# Average silhouette for pam
fviz_nbclust(iris.scaled, pam, method = "silhouette")

### Gap statistic
library(cluster)
# Compute gap statistic for pam
# we used B = 10 for demo. Recommended value is ~500
gap_stat <- clusGap(iris.scaled, FUN = pam, nstart = 10,
                    K.max = 10, B = 10)
print(gap_stat, method = "firstmax")





pam.res <- pam(iris.scaled, 3)
# Visualize pam clustering
fviz_cluster(pam.res, geom = "point", ellipse.type = "norm")
############################################################################
##############################################################################
#############################################################################
#############################################################################

# Hierarchical clustering
# ++++++++++++++++++++++++
# Optimal number of clusters in the data
# ++++++++++++++++++++++++++++++++++++++
# Examples are provided only for kmeans, but
# you can also use cluster::pam (for pam) or
#  hcut (for hierarchical clustering)

### Elbow method (look at the knee)
# Elbow method for hier
fviz_nbclust(iris.scaled, hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

# Average silhouette for hier
fviz_nbclust(iris.scaled, hcut, method = "silhouette")

### Gap statistic
library(cluster)
set.seed(123)
# Compute gap statistic for hier
# we used B = 10 for demo. Recommended value is ~500
gap_stat <- clusGap(iris.scaled, FUN = hcut, nstart = 25,
                    K.max = 10, B = 10)
print(gap_stat, method = "firstmax")



# Use hcut() which compute hclust and cut the tree
hc.cut <- hcut(iris.scaled, k = 3, hc_method = "complete")
# Visualize dendrogram
fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
# Visualize cluster
fviz_cluster(hc.cut, ellipse.type = "convex")

# }
# NOT RUN {

# mallon oi texnikes 
# }
#############################################################################
###############################################################################
############################################################################
################################################################################
#############################################################################
#CLARA



# NOT RUN {
## generate 500 objects, divided into 2 clusters.
x <- rbind(cbind(rnorm(200,0,8), rnorm(200,0,8)),
           cbind(rnorm(300,50,8), rnorm(300,50,8)))
x=as.data.frame(x)
plot(x)
### Elbow method (look at the knee)
# Elbow method for clara
fviz_nbclust(x, clara, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

fviz_nbclust(x, clara, method = "silhouette") +
  geom_vline(xintercept = 3, linetype = 2)

fviz_nbclust(x, clara, method = "gap_stat") +
  geom_vline(xintercept = 3, linetype = 2)

#des oti o silhouette coefficient sumfwnei me to gap statistic enw h methodos
#elbow proteinei tria clusters.

clarax <- clara(x, 2, samples=50)
clarax
clarax$clusinfo

# Print components of clara.res
print(clarax)

# Add clustering result to the Data
head(dd, n = 4)
library(dplyr)
glimpse(clarax)

# Visualize cluster
#pata enter molis sou vgalei hit Return
fviz_cluster(clarax, 
             ellipse.type = "t", # Concentration ellipse
             geom = "point"
)

## 'xclara' is an artificial data set with 3 clusters of 1000 bivariate
## objects each.
xclara=as.data.frame(data(xclara))
(clx3 <- clara(xclara, 3))
## "better" number of samples
cl.3 <- clara(xclara, 3, samples=100)
## but that did not change the result here:
stopifnot(cl.3$clustering == clx3$clustering)
## Plot similar to Figure 5 in Struyf et al (1996)
# }
# NOT RUN {
plot(clx3, ask = TRUE)
# }
# NOT RUN {
## Try 100 times *different* random samples -- for reliability:
nSim <- 100
nCl <- 3 # = no.classes
set.seed(421)# (reproducibility)
cl <- matrix(NA,nrow(xclara), nSim)
for(i in 1:nSim)
  cl[,i] <- clara(xclara, nCl, medoids.x = FALSE, rngR = TRUE)$cluster
tcl <- apply(cl,1, tabulate, nbins = nCl)
## those that are not always in same cluster (5 out of 3000 for this seed):
(iDoubt <- which(apply(tcl,2, function(n) all(n < nSim))))
if(length(iDoubt)) { # (not for all seeds)
  tabD <- tcl[,iDoubt, drop=FALSE]
  dimnames(tabD) <- list(cluster = paste(1:nCl), obs = format(iDoubt))
  t(tabD) # how many times in which clusters
}
# }
##############################################################################
##############################################################################
#############################################################################
#DBSCAN
library(ggplot2)
library(factoextra)
library(fpc)
library(dbscan)
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


#isodynama
fviz_cluster(db, df, stand = FALSE, frame = FALSE, geom = "point")

# iris dataset


# Load the data
data("iris")
iris <- as.matrix(iris[, 1:4])

dbscan::kNNdistplot(iris, k =  4)
abline(h = 0.4, lty = 2)

set.seed(123)
# fpc package
res.fpc <- fpc::dbscan(iris, eps = 0.4, MinPts = 4)
# dbscan package
res.db <- dbscan::dbscan(iris, 0.4, 4)

fviz_cluster(res.fpc, iris, geom = "point")



#############################################################################
#############################################################################
#############################################################################
############################################################################

#k means ++
library(flexclust)
library(LICORS)
a=kmeanspp(iris, k = 3, start = "random", iter.max = 100, nstart = 10)

fviz_cluster(a, iris, geom = "point")
#############################################################################
#############################################################################
#############################################################################
#############################################################################

# OPTICS

#gia kalutero kwdika phgaine kai koita se mia selida manning pou exeis 
#apothhkeusei stous selidodeiktes tou paliou upologisth


### run OPTICS
iris.scaled <- scale(iris[, -5])
res <- optics(iris.scaled, eps = 10,  minPts = 10)
res

### get order
res$order

### plot produces a reachability plot
plot(res)

### identify clusters by cutting the reachability plot (black is noise)
res <- optics_cut(res, eps_cl = .065)
res

plot(res)
plot(x, col = res$cluster+1L)

### re-cutting at a higher eps threshold
res <- optics_cut(res, eps_cl = .1)
res
plot(res)
plot(x, col = res$cluster+1L)

### identify clusters of varying density hierarchically using the Xi method
res <- opticsXi(res, xi = 0.05)
res

plot(res)
plot(x, col = res$cluster+1L)
# better visualization of the nested structure using convex hulls
hullplot(x, res)

# Xi cluster structure
res$clusters_xi

### use OPTICS on a precomputed distance matrix
d <- dist(x)
res <- optics(x, eps = 1, minPts = 10)
plot(res)


##############################################################################
#############################################################################
#############################################################################
#############################################################################

#CLARANS

set.seed(1)
iris.scaled <- scale(iris[, -5])

results <- clara(iris.scaled, 3, metric = "euclidean", 
                 samples = 20, pamLike = FALSE )

fviz_cluster(results, iris, geom = "point")
#gia diaforetiko megethos deigmatos allazei kai to apotelesma
#to 4 einai o arithmos twn clusters
# nomizw prepei na exeis pamlike=FALSE gia na exeis CLARANS
#to eides apo mia selida stous selidodeiktes tou paliou upologisth





















