library(igraph)
library(pracma)
library(Matrix)
library(ggplot2)

rm(list=ls())
# Part a ############################################################
#####################################################################
print("Question 1 Part a ==================================================")

# assign number of nodes
n <- 900

# given edge-formation probabilities
probabilities <- c(0.002, 0.006, 0.012, 0.045, 0.1)

# run through each given edge-formation probability
for (p in probabilities) {
  # network of n where every edge is created with the same constant probability.
  g1 <- erdos.renyi.game(n, p, type="gnp", directed=F);
  g1_distribution <- degree.distribution(g1)
  g1_degree <- degree(g1)
  
  # set up the data frames for plotting in ggplot
  g1_distribution_ind <- c(1:length(g1_distribution))
  g1_binonmial_dist <- dbinom(g1_distribution_ind,n,p)
  g1_distribution_df <- data.frame(degree=g1_distribution_ind, probability=g1_distribution)
  g1_binonmial_dist_df <- data.frame(degree=g1_distribution_ind, probability=g1_binonmial_dist)
  # plot the degree distribution
  png(sprintf("Question1_parta_dist%5.3f.png",p))
  print(ggplot(data = g1_distribution_df, aes(x=g1_distribution_ind, y=g1_distribution))+
          geom_bar(stat="identity", fill="royalblue",color="black", width=.7)+
          geom_point(data = g1_binonmial_dist_df, aes(x=g1_distribution_ind, y=g1_binonmial_dist), size=2)+
          geom_line(data = g1_binonmial_dist_df, aes(x=g1_distribution_ind, y=g1_binonmial_dist))+
          labs(title=sprintf("Degree Probability Distribution for n=%d and p=%.3f",n,p), x="Degree", y="Probability")+
          theme(plot.title = element_text(hjust = 0.5)))
  dev.off()
  
  # plot the graph for visualization
  png(sprintf("Question1_parta_plot%5.3f.png",p))
  plot(g1, main=sprintf("Erdos-Renyi Undirected Random Graph with n=%d and p=%.3f",n,p), 
       vertex.size=2, vertex.label=NA, edge.arrow.size=0.2)
  dev.off()
  
  # calculate theoretical mean and variance assuming binomial distribution
  theoretical_mean <- n*p
  theoretical_variance <- n*p*(1-p)
  
  # print the observed mean and variance
  print("----------")
  print(sprintf("p = %5.3f: observed mean = %5.3f", p, mean(g1_degree)))
  print(sprintf("p = %5.3f: theoretical mean = %5.3f", p, theoretical_mean))
  print(sprintf("p = %5.3f: observed variance = %5.3f", p, var(g1_degree)))
  print(sprintf("p = %5.3f: theoretical variance = %5.3f", p, theoretical_variance))
  
}

# Part b ############################################################
#####################################################################
print("Question 1 Part b ==================================================")
# assign number of nodes
n <- 900

# given probabilities for creating an edge
probabilities <- c(0.002, 0.006, 0.012, 0.045, 0.1)

# run through each given edge-formation probability
for (p in probabilities) {
  # to numerically estimate the probability that a generated network is connected,
  # realize multiple networks and count how many are connected
  connected <- 0
  # realize 1000 random networks
  for (i in seq(1,1000,1)){
    g2 <- erdos.renyi.game(n, p, type="gnp", directed = F)
    # check if the randomly generated network is connected
    if (is.connected(g2)){
      connected <- connected + 1
    }
  }
  g2_prob_connected <- connected/1000
  print(sprintf("For graph with nodes=%d and p=%5.3f:",n,p))
  print("----------")
  print(sprintf("The probability that the network is connected is: %5.3f", g2_prob_connected))
  
  # find the clusters of the graph
  g2_clusters <- clusters(g2)
  
  # find index of the biggest cluster, or the Giant Connected Component (GCC)
  ind <- which.max(g2_clusters$csize)
  
  # grab the GCC subgraph
  g2_gcc <- induced.subgraph(g2, which(g2_clusters$membership == ind))
  
  print(sprintf("Last generated graph is connected: %s", is.connected(g2)))
  print(sprintf("Nodes of the GCC: %d", vcount(g2_gcc)))
  print(sprintf("Edges of the GCC: %d", ecount(g2_gcc)))
  print(sprintf("Diameter of the GCC: %d",diameter(g2_gcc)))
  cat("\n")
}

# Part c ############################################################
#####################################################################
print("Question 1 Part c ==================================================")
# assign number of nodes
n <- 900

# set edge-formation parameters to sweep through
p_min <- 0
p_max <- 0.018
p_step <- 0.0001
probabilities <- seq(p_min, p_max, p_step)
num_probabilities <- length(probabilities)

# create 100 random networks for each p
num_networks <- 100
networks <- seq(1,num_networks)

# create object to hold gcc sizes per p
GCC_psizes <- matrix(0,nrow=length(probabilities), ncol=length(networks))
# create vector for sum of GCC sizes per p
GCC_psum <- vector(mode="numeric", length=length(probabilities))
# create vector for average GCC size per p
GCC_avg <- vector(mode="numeric", length=length(probabilities))
# create vector to hold total gcc sizes for normalized plot
GCC_sizes <- vector(mode="numeric", length=length(probabilities)*num_networks)

i<-0
j<-0
# sweep over values of p=0 to p_max. p_max should be roughly determined
for (p in probabilities){
  i <- i+1
  cat("i =",i,"\r")
  
  cum_size = 0
  for (r in networks){
    j <- j+1
    # generate a random graph
    g3 <- erdos.renyi.game(n, p, type="gnp")
    
    # find the clusters
    g3_clusters <- clusters(g3)
    
    # find the index of the biggest cluster, or the GCC
    ind <- which.max(g3_clusters$csize)
    # specify vertices of the original graph which will form the subgraph
    g3_gcc <- induced.subgraph(g3, which(g3_clusters$membership == ind))
    
    # store gcc sizes per p
    GCC_psizes[i,r] <- length(g3_gcc)
    # store running total gcc sizes
    GCC_sizes[j] <- length(g3_gcc)
    # accumulate the gcc sizes
    # cum_size <- cum_size + length(g.gcc)
  }
  
  GCC_psum[i] <- sum(GCC_psizes[i,])
  GCC_avg[i] <- GCC_psum[i] / num_networks
  # store the average size of the gcc's for that probability
  # gcc.sizes[which(probabilities==p)] <- cum_size / num_networks
}

# plot the normalized GGC sizes
px <- rep(probabilities, times=1, each=num_networks)
xticks <- seq(p_min, p_max, 0.001)
yticks <- seq(0,1,0.1)
png("Question1_partc_NormGCC.png")
plot(px, GCC_sizes / n, pch = 19, main="Normalized GCC size vs. p",xlab="p",ylab="Normalized GCC size", type="l",xaxt="n",yaxt="n")
axis(side=2, at=yticks, labels=sprintf("%2.1f", yticks), las=2)
axis(side=1, at=xticks, labels=sprintf("%5.3f", xticks), las=2)
lines(GCC_sizes)
grid()
dev.off()

# plot the average normalized GGC sizes
png("Question1_partc_NormGCC.png")
plot(probabilities,GCC_avg / n, pch=19, main="Avg Normalized GCC size vs. p",xlab="p",ylab="Normalized GCC size", type="l",xaxt="n",yaxt="n")
axis(side=2, at=yticks, labels=sprintf("%2.1f", yticks), las=2)
axis(side=1, at=xticks, labels=sprintf("%5.3f", xticks), las=2)
grid()
dev.off()

# Part d ############################################################
#####################################################################
print("Question 1 Part d ==================================================")

# average degree of nodes
c_values <- c(0.5, 1, 1.15, 1.25, 1.35)

# sweep over number of nodes from 100 to 10000
n_min <- 100
n_max <- 10000
n_step <- 100
nodes <- seq(n_min, n_max, n_step)

num_networks = 10
# create GCC object to store GCC sizes
GCC_c_sizes <- matrix(0,nrow=length(c_values), ncol=length(nodes))
GCC_n_avg <- vector(mode="numeric", length=length(nodes))
GCC_r_sizes <- vector(mode="numeric", length=length(nodes))

q <- 0
for (c in c_values){
  q <- q+1
  cat("q =",q,"\r")
  
  i <- 0
  for (n in nodes){
    i <- i+1
    
    # calculate edge-formation probability as a function of c and n
    p=c/n
    
    j <- 0
    # expected/average size of GCC calculated over 100 realizations
    for (r in seq(1,num_networks,1)){
      j <- j+1
      # create ER random graph
      g4 <- erdos.renyi.game(n, p, type="gnp")
      # find the clusters
      g4_clusters <- clusters(g4)
      # find the index of the biggest cluster, or the GCC
      ind <- which.max(g4_clusters$csize)
      # specify vertices of the original graph which will form the subgraph
      g4_gcc <- induced.subgraph(g4, which(g4_clusters$membership == ind))
      # add to the running total of GCC sizes to calculate the average
      GCC_r_sizes[j] <- length(g4_gcc)
    }
    # calculate average GCC size 
    GCC_n_avg[i] <- sum(GCC_r_sizes) / num_networks
  }
  GCC_c_sizes[q,] <- GCC_n_avg
}

# plot for c=0.5
xticks <- seq(0, n_max, 1000)
yticks <- seq(0,30,5)
png(sprintf("Question1_partd_GCC_c=%.1f.png",c_values[1]))
plot(nodes, GCC_c_sizes[1,], main=sprintf("Expected GCC size vs nodes for c=%2.1f",c_values[1]),
     xlab="nodes",ylab="Expected GCC size",xaxt="n",yaxt="n", pch=19)
axis(side=2, at=yticks, labels=sprintf("%d", yticks), las=2)
axis(side=1, at=xticks, labels=sprintf("%d", xticks), las=2)
dev.off()

# plot for c=1
xticks <- seq(0, n_max, 1000)
yticks <- seq(0,500,50)
png(sprintf("Question1_partd_GCC_c=%.1f.png",c_values[2]))
plot(nodes, GCC_c_sizes[2,], main=sprintf("Expected GCC size vs nodes for c=%2.1f",c_values[2]),
     xlab="nodes",ylab="Expected GCC size",xaxt="n",yaxt="n", pch=19)
axis(side=2, at=yticks, labels=sprintf("%d", yticks), las=2)
axis(side=1, at=xticks, labels=sprintf("%d", xticks), las=2)
dev.off()

# plot for c=1.15, 1.25, 1.35
xticks <- seq(0, n_max, 1000)
yticks <- seq(0,5000,500)
png(sprintf("Question1_partd_GCC_c=%.1f,%.1f,%.1f.png",c_values[3],c_values[4],c_values[5]))
plot(nodes, GCC_c_sizes[3,], main=sprintf("Expected GCC size vs nodes for c=%3.2f, %3.2f, and %3.2f",c_values[3],c_values[4],c_values[5]),
     xlab="nodes",ylab="Expected GCC size",xaxt="n",yaxt="n", col="blue", pch=19)
axis(side=2, at=yticks, labels=sprintf("%d", yticks), las=2)
axis(side=1, at=xticks, labels=sprintf("%d", xticks), las=2)

points(nodes, GCC_c_sizes[4,], col="orange", pch=19)
points(nodes, GCC_c_sizes[5,], col="purple", pch=19)
legend(6000, 800, legend=c("c = 1.15", "c = 1.25","c = 1.35"), col=c("blue", "orange","purple"), lty=1, cex=1)
dev.off()

