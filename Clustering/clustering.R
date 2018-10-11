# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
WineScale <- wine
WineScale$Type = NULL
WS <- scale(WineScale)
head(WS)
# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

  wssplot <- function(data, nc=15, seed=1234){
  	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
                 	      for (i in 2:nc){
  		        set.seed(seed)
  	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  	                
  		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
  	                        ylab="Within groups sum of squares")
  	   }

wssplot(WS)

# Exercise 2:
#   * How many clusters does this method suggest?  

### Three Clusters as there is a signifiant change in slope/gradient at that point

#   * Why does this method work? What's the intuition behind it?

### The within group sum of squares is a measure of the variability of the observations 
### within each cluster. A cluster that has a small sum of squares is more compact than 
### a cluster that has a large sum of squares

#   * Look at the code for wssplot() and figure out how it works

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(WS, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
### Three 

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(WS, centers = 3)
fit.km
fit.km$size

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$cluster
# compares to the actual wine types in wine$Type.

WSTable <- table(fit.km$cluster, wine$Type)
WSTable
# Would you consider this a good clustering?

### We can quantify the agreement between type and cluster, using an adjusted Rank index 
### provided by the flexclust package.

install.packages("flexclust")
library(flexclust)
randIndex(WSTable)
### Yes it is as it almost acurately maps the type of wine with its cluster group 
### with an index of 0.897495 which is approximately 90%

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library

library(cluster)
clusplot(WS, fit.km$cluster, color = TRUE, shade = T, lines = 0)

# * Would you consider this a good clustering?


### Yes . This clustering approach has a variability of 55.41%
 