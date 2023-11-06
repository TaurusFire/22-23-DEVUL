library(psych)
library(factoextra)
library(cluster)
library(mclust)
library(tidyverse)
library(igraph)

dim(wholesale)
#440 rows 6 vars

sum(is.na(wholesale))
#no missing values


par(mfrow=c(1,6))
par(mar=c(5,5,1,1))
boxplot(wholesale$Fresh, ylim = c(0,12), main = "Fresh", ylab = "Annual spending (m.u.)")
boxplot(wholesale$Milk, ylim = c(0,12), main = "Milk")
boxplot(wholesale$Grocery, ylim = c(0,12), main = "Groceries")
boxplot(wholesale$Frozen, ylim = c(0,12), main = "Frozen")
boxplot(wholesale$Detergents_Paper, ylim = c(0,12), main = "DetsPaper")
boxplot(wholesale$Delicassen, ylim = c(0,12), main = "Deli")
#boxplots, all on common axes. can compare structure of distributions like this.
#notice the groceries outlier. might be best to just remove it completely.
#duplicate point for the grocery statistic

summary(wholesale$Grocery)

#why does fresh have so many outliers? look at multiple dimensions?
pairs.panels(wholesale,
             method = "pearson",
             hist.col = "turquoise",
             density = TRUE
             )
#scatterplots, histograms and correlations between different pairs of variables.
#most vars look somewhat normally distributed
#detergents is multimodal

#get rid of extreme value
wholesale2 = wholesale[-76,]

#need to think about scaling the data before using PCA
# Do we need to use Covariance matrix (Σ) or Correlation matrix (scale the data)? 
#We can check this by assessing the standard deviation of the variables

sd = apply(wholesale2,2,sd)
sd
#no need to scale, sds are not that different, and the same units are used.
#use covariance matrix

pcaObj = prcomp(wholesale2, scale = FALSE)
summary(pcaObj)

# eigenvalues
pcaObj$sdev^2
# PC loadings
pcaObj$rotation
# PC score
pcaObj$x



#proportion of variance
summary(pcaObj)$importance[2,]
#scree plot
fviz_screeplot(pcaObj, addlabels = TRUE)
#elbow at 3rd PC - less pronounced drop after 3rd PC, so opt for 3 principal dimensions



#how much contribution to each PC?
#pc1
fviz_contrib(pcaObj, choice = "var", axes = 1, top = 10)
#pc2
fviz_contrib(pcaObj, choice = "var", axes = 2, top = 10)
#pc3
fviz_contrib(pcaObj, choice = "var", axes = 3, top = 10)

#k-means clustering and hierarchical clustering

#how many clusters K will be decided using a scree plot
#total within-cluster sum of squares is plotted against number of cluster
#not sure where to start, consider 2 clusters

#have to do it on the dimension reduced data
#means or medoids? - think about how many outliers there are
#loads of outliers. try medoids?

scores = pcaObj$x

#finding the optimal k for different clustering methods

#k means
fviz_nbclust(scores[,1:3], kmeans, method = "silhouette", linecolor = 'darkgreen')+labs(title = "K-means")
fviz_nbclust(scores[,1:3], kmeans, method = "wss", linecolor = 'red') 
?fviz_nbclust

#k medoids
fviz_nbclust(scores[,1:3], pam, method = "silhouette")+labs(title = "K-means")
fviz_nbclust(scores[,1:3], pam, method = "wss")+labs(title = "K-means")

#hierarchical
fviz_nbclust(scores[,1:3], hcut, method = "silhouette", linecolor = 'darkgreen')+labs(title = "Hierarchical Clustering")
fviz_nbclust(scores[,1:3], hcut, method = "wss", linecolor = 'red')
?fviz_nbclust


#kmeans suggests k=2, but hierarchical suggests k=4 



#k means model, 2 means

#provide an nstart value to try loads of different initial cluster assignments
set.seed(5)
k2means = kmeans(scores[,1:3], centers=2, nstart = 10)
fviz_cluster(k2means, scores, stand = FALSE, ellipse.type = "norm")
#choose.vars = c("PC1", "PC2"), 

#percentage of variance explained by the two cluster means
pcvarexplained2means = (k2means$betweenss / k2means$totss) *100
pcvarexplained2means
#37.13742

#k means model, 3 means
#provide an nstart value to try loads of different initial cluster assignments
set.seed(5)
k3means = kmeans(scores[,1:3], centers=3, nstart = 1000)
fviz_cluster(k3means, scores, stand = FALSE, ellipse.type = "norm")
pcvarexplained3means = (k3means$betweenss / k3means$totss) *100
pcvarexplained3means
#49.90758

for (i in 2:10){
  test = kmeans(scores[,1:3], centers = i, nstart = 10)
  print(c(i,(test$betweenss/test$totss)*100))
}


#k medoids?? less sensitive to outliers which could be useful
#provide an nstart value to try loads of different initial cluster assignments
set.seed(5)
?pam
k2medoids = pam(scores, k=2, stand = FALSE, nstart = 50)
fviz_cluster(k2medoids, scores[,1:3], stand = FALSE, ellipse.type = "norm")



# hierarchical clustering model

#test using different types of linkage.

#COMPLETE LINKAGE
#cut so that we get four clusters
?hclust
hc.complete.four = hclust(dist(scores[,1:3]), method = "complete")
dendros.complete.four = as.dendrogram(hc.complete.four)
plot(dendros.complete.four, main = "Wholesale Data - Complete linkage",
     ylab = "Height")
abline(h = 11.5, col = 'red')
hc.complete.four.cuts = cutree(hc.complete.four, 4)
fviz_cluster(list(data=scores, cluster=hc.complete.four.cuts), stand = FALSE,
             ellipse.type = "norm")
fviz_dend(hc.complete.four, k = 4, k_colors = "jco", type = "circular", repel = TRUE)

table(k3means$cluster)

table(hc.complete.four.cuts)
?fviz_dend
#use agglomerative coefficient to choose the best linkage between complete,
#single and
#clearly going to be complete linkage

#The AC measures the dissimilarity of an object to the first cluster it joins, 
#divided by the dissimilarity of the final merger in the cluster analysis, 
#averaged across all samples. The AC represents the strength of the clustering 
#structure; values closer to 1 indicates that clustering structure is more 
#balanced while values closer to 0 suggests less well balanced clusters. 
#The higher the value, the better.

m <- c("average","single","complete")
names(m) <- c("average","single","complete")
# function to compute coefficient
ac <- function(x){
  agnes(scores, method = x)$ac
}
map_dbl(m,ac)
agnes(scores, method = 'average')$ac
agnes(scores, method = 'single')$ac
agnes(scores, method = 'complete')$ac

?agnes

#use complete!!
