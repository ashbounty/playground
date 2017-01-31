### Function starts with representation of each data point as one cluster
### In each iteration function computes distances between the clusters and groups the two with the smallest distance
### Function repeats this process until there is only one cluster that includes all data points

### Output:
### A list with one list element for each iteration
### Each list element contains information about the clustering in this specific iteration
### $clusterAssigment:  Data frame that assigns one cluster to each data point
### $clusterCenters:    Data frame that includes the center coordinates of each cluster
### $numberOfClusters:  Numeric that gives the number of total clusters
### $maximalHeight:     Numeric that gives the maximal distance between two cluster points

hclustFunction <- function(x, y, cluster = 3) {
  
  # Initialize variables
  
  n = 0
  clusterAssignment <- data.frame(x=x, y=y, cluster=1:length(x))
  clusterCenters <- data.frame(cluster=1:length(x), x=x, y=y)
  numberOfClusters <- nrow(clusterCenters)
  maximalHeight <- 0
  results <- list()
  
  # Start algorithm and repeat until numberOfClusters==1
  
  while(numberOfClusters > 1) {
    
    n <- n +1
    
    # Prepare the distance matrix
    
    distanceCluster <- matrix(nrow = numberOfClusters, ncol = numberOfClusters)
    
    # Calculate the difference between each clusterCenter
    
    for(i in 1:numberOfClusters) {
      for(j in 1:numberOfClusters) {
        if(j > i) distanceCluster[i,j] <- sqrt( (clusterCenters[i,2]-clusterCenters[j,2])^2 
                                                + (clusterCenters[i,3]-clusterCenters[j,3])^2 )
      }
    }
    
    # Look for the clusterIDs of the two clusters with the smallest distance in clusterCenters data frame
    
    c1 <- clusterCenters$cluster[which.min(distanceCluster) %% numberOfClusters]
    c2 <- clusterCenters$cluster[(which.min(distanceCluster) - which.min(distanceCluster) %% numberOfClusters) / numberOfClusters + 1]
    
    # Reassign clusterID to data points
    
    clusterID <- min(c1, c2)
    clusterAssignment[which(clusterAssignment$cluster %in% c(c1,c2)),3] <- clusterID
    
    # Recalculate cluster centers
    
    xNewCenter <- with(clusterAssignment, mean(subset(x, cluster == clusterID)))
    yNewCenter <- with(clusterAssignment, mean(subset(y, cluster == clusterID)))
    
    # Update clusterCenters data frame
    
    clusterCenters <- clusterCenters[-which(clusterCenters$cluster %in% c(c1, c2)),]    # eliminate old entries
    clusterCenters <- rbind(clusterCenters, c(clusterID, xNewCenter, yNewCenter))       # add entry with new coordinates
    names(clusterCenters) <- c("cluster", "x", "y")
    
    # Save cluster information of last iteration in new list element
    
    maximalHeight <- min(distanceCluster, na.rm = TRUE)
    numberOfClusters <- nrow(clusterCenters)
    results <- append(results, list(list(clusterAssignment=clusterAssignment, 
                                         clusterCenters=clusterCenters, 
                                         maximalHeight=maximalHeight, 
                                         numberOfClusters=numberOfClusters)))
  }
  
  results
}


### Just a quick function that returns clusterAssignment data frame for a specific number of clusters

hclustChoose <- function(x, n = 3) {
  for(i in 1:length(x)) {
    if(x[[i]]$numberOfClusters == n) return(x[[i]]$clusterAssignment)
  }
}



### Testing

testClust <- hclustFunction(iris$Sepal.Length, iris$Petal.Length)
testToPlot <- hclustChoose(testClust, n = 4)
ggplot(testToPlot, aes(x, y, colour = factor(cluster))) + geom_point()
