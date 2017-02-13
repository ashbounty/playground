## ownKMeans() applies a k-means algorithm nrep times to a bunch of 2-dimensional data points
## It uses different random seeds for each iteration and returns the cluster result with the lowest SSE

## The k-means algorithm itself sets random center points for each of the n starting clusters
## It assigns each data point to the cluster with the lowest distance between the data point and the cluster center
## It recalculates the cluster centers on the basis of the cluster data point assignment
## The algorithm iterates this process as long as there are changes in the cluster data point assignment


ownKMeans <- function(x, y, nclus=5, dist="euclidean", nrep=500) {
        
        
        # Implement distance function
        
        distance <- function(a, b, method) {
                
                if(method == "euclidean") {
                        sqrt( (a[,1]-b[,1])^2 + (a[,2]-b[,2])^2 )
                        
                } else if(method == "manhatten") {
                        abs(a[,1]-b[,1])+abs(a[,2]-b[,2])
                }
        }
        
        
        # Implement list with output
        
        result <- list(input, cluster, sse=Inf, iterations=0)
        
        
        # Start looping nrep times through seeds
        
        for(i in 1:nrep) {
                
                # Set seed
                
                set.seed(i)
                
                # Initialize data frames
                
                input <- data.frame(x = x, y = y, cluster_number = NA, cluster_distance = NA)
                cluster <- data.frame(x = runif(nclus, min(x), max(x)), y = runif(nclus, min(y), max(y)), sse = NA)
                
                # Initialize iteration counter
                
                counter <- 0
                
                
                # Start single cluster algorithm for specific seed
                
                repeat {
                        
                        # Save old assignment vector for comparison
                        
                        old_assign <- input[,3] 
                        
                        # Calculate cluster assignment and distance
                        
                        for(i in 1:nrow(input)) {
                                input[i,3] <- which.min( distance(input[i,], cluster, method=dist) )
                                input[i,4] <- min( distance(input[i,], cluster, method=dist), na.rm=T )
                        }
                        
                        # Check if assignment has changed, if not quit the loop
                        
                        if(identical(old_assign, input[,3])) break
                        
                        # Calculate center point and SSE for each cluster
                        
                        for(i in 1:nrow(cluster)) {
                                cluster_points <- subset(input, cluster_number==i)
                                
                                if(nrow(cluster_points)>0) {
                                        cluster[i,1] <- mean(cluster_points[,1])
                                        cluster[i,2] <- mean(cluster_points[,2])
                                        cluster[i,3] <- sum(distance(cluster[i,], cluster_points, method=dist), na.rm=T)
                                } else {
                                        cluster[i,3] <- NA 
                                }
                        }
                        
                        # Update counter
                        
                        counter <- counter + 1
                }
                
                # Calculate Total SSE
                
                sse <- sum(cluster$sse, na.rm=T)
                
                # Refresh output list if best SSE so far
                
                if(sse < result$sse) result <- list(input=input, cluster=cluster, sse=sse, iterations=counter)
        }
        
        
        # Print the best cluster result
        
        result
}



## plotKmeans() takes a object that is returned by the ownKmeans() function and plots the results

plotKmeans <- function(x) {
        
        require(ggplot2)
        
        dfInput <- x[[1]]
        dfCluster <- x[[2]]
                
        g <- ggplot(dfInput, aes(x, y, color = factor(cluster_number))) 
        g + geom_point() + 
                geom_point(data = dfCluster, aes(x, y, color = factor(1:nrow(dfCluster))), size = 10, shape=2)
        
}



## Example

a <- c(rnorm(30, 10, 5), rnorm(40, 30, 5), rnorm(20, 50,5))
b <- c(rnorm(30, 40, 5), rnorm(40, 20, 5), rnorm(20, 50,5))

clust <- ownKMeans(a, b, nclus = 5, dist = "euclidean", nrep = 50)

plotKmeans(clust)
