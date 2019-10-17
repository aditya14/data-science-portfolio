library(ggplot2)

# The K-Means function that accepts data and number of clusters k as arguments (default k as 4)
my_kmeans <- function(my_data, K = 4, convergence_crit = 10e-10) {
  
  # Set initial centroids points by taking rows randomly (where each row contains a data point)
  centroids <- my_data[sample(nrow(my_data),K), ]
  
  # Set initial value for the mean sum of squared differences
  ssd <- 10e10
  
  # Vector of zeros for cluster
  cluster <- integer(nrow(my_data))
  
  # Set initial state for checking convergence
  converged <- FALSE
  
  # Initializing the counter
  n <- 1
  
  # Run the loop as long as the mean sum of squared differences is bigger than the convergence critera (which is the initial default value for the sum of squared differences)
  while(ssd >= convergence_crit && converged == FALSE) {
    # Increment counter
    n <- n + 1
    
    # Check for convergence, and end loop if true, or if it's been run 80 times.
    if (ssd <= convergence_crit) {
      converged <- TRUE
    }
    if (n > 80) {
      converged <- TRUE
    }
    
    # Store the centroid values into previous_centroids
    previous_centroids <- centroids
    
    # For each point in the data,
    for (i in 1:nrow(my_data)) {
      initial_dist <- 10e10
      # Calculate the Euclidean distance from the point to each centroid.
      for (j in 1:nrow(centroids)) {
        euc_distance <- sum((centroids[j, ] - my_data[i, ]) ^ 2)
        # Assign the point to whichever cluster is closest
        if (euc_distance <= initial_dist) {
          # Storing the cluster number in an array
          cluster[i] <- j
          initial_dist <- euc_distance
        }
      }
    }
    
    # Calculating the mean of the points in each cluster
    for (i in 1:nrow(centroids)) {
      centroids[i, ] <- apply(my_data[cluster == i, ], 2, mean)
    }
    
    # Assign ssd value to see if any points have changed places
    ssd <- mean((previous_centroids - centroids) ^ 2)
  }
  
  # Assigning final centroid values
  centroids <- data.frame(centroids, cluster = 1:K)
  return(list(my_data=data.frame(my_data, cluster), centroids = centroids))
}
# end of Kmeans function

DF <- as.matrix(read.csv("~/Documents/Data Science/R Projects/KMeansClustering/dataset.csv", header = FALSE))

# Randomly assign clusters
k <- 6
DF1=data.frame(DF,cluster=as.factor(c(rep(1:k,length.out=nrow(DF)))))

# Run my_kmeans function.
res <- my_kmeans(DF, K = k) 

# For the graph
res$my_data$isCentroid <- FALSE
res$centroids$isCentroid <- TRUE
graph_data <- rbind(res$centroids, res$my_data)
cluster_graph <- ggplot(graph_data, aes(x = V1, y = V2, color = as.factor(cluster), size = isCentroid, alpha = isCentroid)) + geom_point()

cluster_graph + labs(title = "Data points grouped by cluster", color = "Cluster #", size = "Centroid Point") + guides(alpha = FALSE)

# Get total within group sum of squares
ks <- 2:10

# Initialize vector for within group sum of squares
wss = vector(mode = "numeric", length = 9)

for(k in ks) {
  res=my_kmeans(DF,K=k)
  
  # Merging clusters with data
  my_data <- merge(res$centroids, res$my_data, by="cluster")
  
  # Calculating within group sum of squares for each cluster count possibility
  wss[k-1] = sum(apply(my_data, 1, function(x) { (x[2] - x[4])^2 + (x[3] - x[5])^2 }))
}

# Plotting the elbow curve
plot(ks, wss, type="o", xlab = "Number of Clusters",
     ylab = "Within groups sum of squares",
     main = "Elbow Curve",
     pch = 20, cex = 1)

#Cleanup
rm(graph_data, ks, k, wss, DF1)

## Based on the elbow graph, the optimial number of clusters is 4, then 6.
