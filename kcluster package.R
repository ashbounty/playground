## kcluster(x, y, max_clus=5, nrep=1000) is the main function of this package.
## It repeatedly forms clusters of data points (x,y) based on a k-means algorithm.
## The results are saved in the list object, that is returned by the kcluster().

## Input parameters:
## x, y are the coordinate vectors of the attributes, on which the clustering will be applied
## max_clus is the maximal number of clusters
## nrep is the number of iterations of algorithm application

## Output:
## A list object in which each element includes the results of one clustering iteration
## Each list element includes four result outputs:
## $data is a data frame of the (x, y) input vectors and their cluster assignment 
## $clus is a data frame of the (x, y) coordinates of the cluster centers
## $nclus is the number of clusters
## $sse is the sum of squared errors for the clustering

kcluster <- function(x, y, max_clus=5, nrep=1000) {
    
    # Create list for the results of each application of the algorithm
    results <- list()
    
    for(rs in 1:nrep) {
        
        # Set random seed
        set.seed(rs)

        # Start with random cluster centers
        xcen <- runif(n = max_clus, min = min(x), max = max(x))
        ycen <- runif(n = max_clus, min = min(y), max = max(y))
        
        # Put data points and cluster assignment in "data"
        # Put cluster center coordinates in "clus"
        data <- data.frame(xval = x, yval = y, clus = NA)
        clus <- data.frame(name = 1:max_clus, xcen = xcen, ycen = ycen)

        finish <- FALSE
 
        while(finish == FALSE) {
            
            # Assign cluster with minimal distance to each data point
            for(i in 1:length(x)) {
                dist <- sqrt((x[i]-clus$xcen)^2 + (y[i]-clus$ycen)^2)
                data$clus[i] <- which.min(dist)
            }
        
            xcen_old <- clus$xcen
            ycen_old <- clus$ycen
            
            # Calculate new cluster centers
            for(i in 1:max_clus) {
                clus[i,2] <- mean(subset(data$xval, data$clus == i))
                clus[i,3] <- mean(subset(data$yval, data$clus == i))
            }
            
            # Stop the algorithm if there is no change in cluster center coordinates
            if(identical(xcen_old, clus$xcen) & identical(ycen_old, clus$ycen) ) finish <- TRUE
        }
        
        # Convert cluster assignment to factor
        data$clus <- as.factor(data$clus)
        
        # Calculate final number of clusters
        nclus <- length(unique(data$clus))
        
        # Calculate the SSE for the clustering
        sse <- 0
        for(i in 1:max_clus) {
            sse <- sse + sum((subset(data$xval, data$clus==i)-clus[i,2])^2 
                             + (subset(data$yval, data$clus==i)-clus[i,3])^2)
        }
        
        # Append list with result from last alogrithm application 
        results <- append(results, list(list(data = data, 
                                             clus = clus, 
                                             nclus = nclus,
                                             sse = sse)))
    }
    results
}


## findbest(x, n="all") is a function to extract the best cluster result from a
## list object, that has been returned by the kcluster() function.

## Input parameters:
## x is the name of the list object, that has been returned by the kcluster() function
## n is the desired number of clusters, for n = "all" findbest() returns the best cluster
## result, independent of the specific number of clusters

## Output:
## A list object that includes information about the best cluster result of a kcluster() object
## The list includes four result outputs:
## $data is a data frame of the (x, y) input vectors and their cluster assignment 
## $clus is a data frame of the (x, y) coordinates of the cluster centers
## $nclus is the number of clusters
## $sse is the sum of squared errors for the clustering

findbest <- function(x, n="all") {
    
    best <- list(data = NA, clus = NA, sse = Inf)
    
    if(n=="all") { 
        for(i in 1:length(x)) {
            if(x[[i]]$sse < best$sse) best <- x[[i]] 
        }
    } else {
        for(i in 1:length(x)) { 
            if(x[[i]]$nclus != n) next
            if(x[[i]]$sse < best$sse) best <- x[[i]]  
        }
    }
    
    best
}


## Exemplary application of the algorithm:
xval <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
yval <- rnorm(12, mean = rep(c(1,2,1), each = 4), sd = 0.2)

res <- kcluster(xval, yval, max_clus = 5, nrep = 500)

best <- findbest(res, n=3)

ggplot(data = best$data, aes(xval, yval, colour = clus)) + 
    geom_point(size = 2) +
    geom_point(data = best$clus[!is.nan(best$clus[[2]]),], aes(xcen, ycen, colour = as.factor(name)), pch = 3, size = 5)

