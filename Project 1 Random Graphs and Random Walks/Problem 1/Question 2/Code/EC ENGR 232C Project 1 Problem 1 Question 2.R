library(igraph)
library(pracma)
library(Matrix)
library(ggplot2)

rm(list=ls())
# Part a ############################################################
#####################################################################
print("Question 2 Part a ==================================================")

# create an undirected network with n=1050 nodes with m=1 edges brought in
n <- 1050
m <- 1

# to check if the networks are always connected, run multiple realizations
networks <- seq(1,10000,1)
connected <- logical(length(networks))

# use the barabasi.game method to generate random graph using preferential attachment
# run through multiple realizations to check if the networks are connected
for (i in networks){
  cat("i=",i,"\r")
  g1 <- barabasi.game(n=n, m=m, directed = F)
  connected[i] <- is.connected(g1)
}
print(sprintf("Percentage of networks connected out of %d: %d%%",length(networks),sum(connected)/length(connected)*100))
print(sprintf("Is such a network always connected?: %s", all(connected)))

# generate random graph using preferential attachment
g1 <- barabasi.game(n=n, m=m, directed = F)
png("Question2_parta_plot.png")
plot(g1, main=sprintf("Preferential Attachment Model \nUndirected Graph with n=%d and m=%d",n,m), 
     vertex.size=4, vertex.label=NA, edge.arrow.size=0.2)
dev.off()

# plot the degree distribution
g1_distribution <- degree.distribution(g1)
g1_dist_ind <- c(1:length(g1_distribution))
g1_dist_freq <- g1_distribution*n
g1_dist_df <- data.frame(ind=g1_dist_ind, freq=g1_dist_freq)

# plot bar of the degree distribution
png(sprintf("Question2_parta_bardist_n=%d,m=%d.png",n,m))
print(ggplot(g1_dist_df, aes(x=g1_dist_ind, y=g1_dist_freq ))+
        geom_bar(stat="identity", fill="royalblue", color="black", width=.7)+
        # geom_text(aes(x=g1_dist_ind, y=g1_dist_freq+10,label=g1_dist_freq),color="black",size=4,show.legend = T)+
        labs(title=sprintf("Degree distribution for n=%d and m=%d",n,m), x="Degree", y="Frequency")+
        theme(plot.title = element_text(hjust = 0.5)))
dev.off()

# Part b ############################################################
#####################################################################
print("Question 2 Part b ==================================================")

# Use fast greedy method to find the community structure. Measure modularity. Define and compute Assortivity.
g1_communities <- cluster_fast_greedy(g1)
png("Question2_partb_communities.png")
plot(g1, main=sprintf("Preferential Attachment Model \nCommunities in an Undirected Graph with n=%d and m=%d",n,m), 
     mark.groups=groups(g1_communities), vertex.color="gold", vertex.frame.color="black", 
     vertex.size=2, vertex.label="", edge.arrow.size=.2)

# for a more colorful plot
# plot(g1_communities, g1, main=sprintf("Preferential Attachment Model \nCommunities in an Undirected Graph with n=%d and m=%d",n,m), vertex.color="gold", vertex.frame.color="black", vertex.size=2, vertex.label="", edge.arrow.size=.2)

dev.off()

# find the measured modularity
g1_communities_modularity <- modularity(g1_communities)
print(sprintf("Community modularity: %.3f", g1_communities_modularity))

# create dataframe for plotting sizes of all the communities
g1_communities_ind <- c(1:length(g1_communities))
g1_communities_sizes <- as.vector(sizes(g1_communities)) #needed to be coerced with as.vector!!
g1_communities_df <- data.frame(ind=g1_communities_ind, sizes=g1_communities_sizes)

# ggplot the communities sizes
png("Question2_partb_hist.png")
ggplot(g1_communities_df, aes(x=g1_communities_ind, y=g1_communities_sizes))+
  geom_bar(stat="identity", fill="royalblue",color="black", width=.7)+
  labs(title=sprintf("Communities sizes for n=%d and m=%d",n,m), x="Community number", y="Size")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

# Part c ############################################################
#####################################################################
print("Question 2 Part c ==================================================")

# Try to generate a larger network with 10500 nodes using the same model
n <- 10500
g2 <- barabasi.game(n=n, m=m, directed = F)

# plot the graph
png("Question2_partc_plot.png")
plot(g2, main=sprintf("Preferential Attachment Model \nUndirected Graph with n=%d and m=%d",n,m), 
     vertex.size=4, vertex.label=NA, edge.arrow.size=0.2)
dev.off()

# plot the graph with community groupings
g2_communities <- cluster_fast_greedy(g2)
png("Question2_partc_communities.png")
plot(g2, main=sprintf("Preferential Attachment Model \nCommunities in an Undirected Graph with n=%d and m=%d",n,m), 
     mark.groups=groups(g2_communities), vertex.color="gold", vertex.frame.color="black", 
     vertex.size=2, vertex.label="", edge.arrow.size=.2)

# for a more colorful plot
# plot(g2_communities, g2, main=sprintf("Preferential Attachment Model \nCommunities in an Undirected Graph with n=%d and m=%d",n,m), vertex.color="gold", vertex.frame.color="black", vertex.size=2, vertex.label="", edge.arrow.size=.2)

dev.off()

# find the measured modularity
g2_communities_modularity <- modularity(g2_communities)
print(sprintf("Community modularity: %.3f", g2_communities_modularity))

# plot the histogram of communities sizes
g2_communities_ind <- c(1:length(g2_communities))
g2_communities_sizes <- as.vector(sizes(g2_communities))
g2_communities_df <- data.frame(ind=g2_communities_ind, sizes=g2_communities_sizes)
png("Question2_partc_hist.png", width=1000, height=600)
ggplot(g2_communities_df, aes(x=g2_communities_ind, y=g2_communities_sizes))+
  geom_bar(stat="identity", fill="royalblue",color="black", width=.7)+
  labs(title=sprintf("Communities sizes for n=%d and m=%d",n,m), x="Community number", y="Size")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

# Part d ############################################################
#####################################################################
print("Question 2 Part d ==================================================")

# plot the degree distribution in a log-log scale for both n = 1050; 10500, then estimate the slope of the plot using linear regression.
n_values <- c(1050, 10500)

for (n in n_values){
  g3 <- barabasi.game(n=n, m=m, directed = F)
  g3_distribution <- degree.distribution(g3)
  g3_degree <- degree(g3)
  
  # need to remove the zeros because they don't contribute insight to the Log-Log plot
  ind <- which(g3_distribution != 0)
  log_x <- log(seq(1,length(g3_distribution),1))[ind]
  log_y <- log(g3_distribution)[ind]
  g3_dist_df <- data.frame(logx=log_x, logy=log_y)
  
  # estimate the slope using linear regression
  model <- lm(log_y ~ log_x)
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  print(sprintf("n=%d: Slope of linear regression for log-log degree distribution = %.3f",n,slope))
  print(sprintf("n=%d: Intercept of linear regression for log-log degree distribution = %.3f",n,intercept))
  
  # plot the Log-Log of the degree distribution
  png(sprintf("Question2_partd_loglogdist_n=%d,m=%d.png",n,m))
  print(ggplot(g3_dist_df, aes(x=logx, y=logy))+
    geom_point()+
    geom_abline(intercept=intercept, slope=slope, col="red")+
    labs(title=sprintf("Log-Log Degree Distribution for n=%d and m=%d",n,m), x="Log(Degree)", y="Log(Probability)")+
    theme(plot.title = element_text(hjust = 0.5)))
  dev.off()
  
  # create dataframe for the degree distribution
  g3_dist_ind <- c(1:length(g3_distribution))
  g3_dist_freq <- g3_distribution*n
  g3_dist_df <- data.frame(ind=g3_dist_ind, freq=g3_dist_freq)
  
  # plot bar of the degree distribution
  png(sprintf("Question2_partd_bardist_n=%d,m=%d.png",n,m))
  print(ggplot(g3_dist_df, aes(x=g3_dist_ind, y=g3_dist_freq ))+
    geom_bar(stat="identity", fill="royalblue", color="black", width=.7)+
      # geom_text(aes(x=g3_dist_ind, y=g3_dist_freq+10,label=g3_dist_freq),color="black",size=4,show.legend = T)+
    labs(title=sprintf("Degree distribution for n=%d and m=%d",n,m), x="Degree", y="Frequency")+
    theme(plot.title = element_text(hjust = 0.5)))
  dev.off()
}

# Part e ############################################################
#####################################################################
print("Question 2 Part e ==================================================")

# networks generated in 2(a) and 2(c), Randomly pick a node i, and then randomly pick a 
# neighbor j of that node. Plot the degree distribution of nodes j that are picked with 
# this process, in the log-log scale

iterations <- seq(1,1000,1) #iterations to calculate expected degree
# consider the two networks generated in part(a) and part(c)
for (gi in list(g1,g2)){
  node_j_degrees <- vector(mode="numeric", length=length(iterations))
  
  # loop through multiple iterations 
  for (i in iterations){
    
    # randomly pick a node i in graph
    node_i <- sample(vcount(gi),1)
    # find all of node i neighbors
    neighbors_i <- neighbors(gi, node_i)
    # randomly pick a node j neighbor of node i
    if (length(neighbors_i)==1){
      node_j <- neighbors_i   
    }else{
      node_j <- sample(neighbors_i, 1)
    }
    
    # keep a running account of the node j degrees
    node_j_degrees[i] <- degree(gi, node_j)
  }
  
  # calculate the node j degree distribution
  node_j_degree_dist <- as.vector(prop.table(table(node_j_degrees)))
  
  # plot the node j degree distribution in Log-Log
  log_x <- log(seq(1,length(node_j_degree_dist),1))
  log_y <- log(node_j_degree_dist)
  node_j_degrees_df <- data.frame(logx=log_x, logy=log_y)
  
  # estimate the slope using linear regression
  model <- lm(log_y ~ log_x)
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  print(sprintf("n=%d: Slope of linear regression for log-log degree distribution = %.3f",vcount(gi),slope))
  print(sprintf("n=%d: Intercept of linear regression for log-log degree distribution = %.3f",vcount(gi),intercept))
  
  # plot the Log-Log of the degree distribution
  png(sprintf("Question2_parte_loglog_nodej_dist_n=%d,m=%d.png",vcount(gi),m))
  print(ggplot(node_j_degrees_df, aes(x=logx, y=logy))+
    geom_point()+
    geom_abline(intercept=intercept, slope=slope, col="red")+
    labs(title=sprintf("Log-Log Degree Distribution of node j for n=%d and m=%d",vcount(gi),m), x="Log(Degree)", y="Log(Probability)")+
    theme(plot.title = element_text(hjust = 0.5)))
  dev.off()
  
  # plot histogram of the node j degree distribution
  node_j_degrees_df <- data.frame(x=node_j_degrees)
  png(sprintf("Question2_parte_hist_nodej_dist_n=%d,m=%d.png",vcount(gi),m))
  print(ggplot(node_j_degrees_df, aes(x = node_j_degrees)) +
    geom_histogram(fill = "royalblue",color="black") +
    scale_x_continuous(breaks = seq(0, ceiling(max(node_j_degrees_df$x)/10)*10, 5))+
    labs(title=sprintf("Degree distribution for n=%d and m=%d",vcount(gi),m), x="Degree", y="Frequency")+
    theme(plot.title = element_text(hjust = 0.5)))
  dev.off()
}

# Part f ############################################################
#####################################################################
print("Question 2 Part f ==================================================")

# Estimate the expected degree of a node that is added at time step i for 1<=i<=1050
n <- 1050
iterations <- seq(1,1000,1) #iterations to calculate expected degree
sum_degree <- vector(mode="numeric", length=n)
expected_degree <- vector(mode="numeric", length=n)

for (i in iterations){
  g4 <- barabasi.game(n=n, m=m, directed=F)
  # keep running sum of each iteration's degree frequency for nodes i=0 to n
  sum_degree <- sum_degree + degree(g4)
}
# divide the degree sums but number of iterations to find expected degree
expected_degree <- sum_degree/length(iterations)
expected_degree_df <- data.frame(node=seq(1,n,1), degree=rev(expected_degree))
# plot relationship between age of nodes and their expected degree
png(sprintf("Question2_partf_deg_n=%d,m=%d.png",n,m))
ggplot(expected_degree_df, aes(x=node, y=degree))+
  geom_bar(stat="identity", fill="royalblue", width=.7)+
  labs(title=sprintf("Degree Distribution vs. Age of Node for n=%d and m=%d",n,m), x="Age of Node\n (inverse of node #)", y="Degree")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


# Part g ############################################################
#####################################################################
print("Question 2 Part g ==================================================")

# Part h ############################################################
#####################################################################
print("Question 2 Part h ==================================================")
# Generate a preferential attachment network with n=1050 and m=1
# take its degree sequence and create a new network with the same sequence, through
# stub matching procedure. plot both networks, mark communities, measure modularity
n <- 1050
m <- 1

# generate a network
g5 <- barabasi.game(n=n, m=m, directed = F)
g5_degree <- degree(g5)
g5_communities <- cluster_fast_greedy(g5)
g5_communities_modularity <- modularity(g5_communities)
print(sprintf("Community modularity of graph n=%d and m=%d: %.3f",n,m,g5_communities_modularity))
# plot graph
png(sprintf("Question2_parth_plot_n=%d,m=%d.png",n,m))
plot(g5, main=sprintf("Communities in Undirected Graph with n=%d and m=%d",n,m), 
     mark.groups=groups(g5_communities), vertex.color="gold", vertex.frame.color="black", 
     vertex.size=2, vertex.label="", edge.arrow.size=.2)

# for a more colorful plot
# plot(g5_communities, g5, main=sprintf("Communities in Undirected Graph with n=%d and m=%d",n,m), vertex.color="gold", vertex.frame.color="black", vertex.size=2, vertex.label="", edge.arrow.size=.2)

dev.off()
# plot bar of community sizes
png(sprintf("Question2_parth_bar_n=%d,m=%d.png",n,m))
g5_communities_sizes <- as.vector(sizes(g5_communities))
g5_communities_ind <- c(1:length(sizes(g5_communities)))
g5_communities_df <- data.frame(community=g5_communities_ind, size=g5_communities_sizes)
ggplot(g5_communities_df, aes(x=g5_communities_ind, y=g5_communities_sizes))+
  geom_bar(stat="identity", fill="royalblue", color="black", width=.7)+
  labs(title=sprintf("Community sizes for graph with n=%d and m=%d",n,m), x="Community", y="Size")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


# use sample_degseq() to create a graph with given vertex degrees
g5_degseq <- sample_degseq(g5_degree, method="simple.no.multiple")
g5_degseq_degree <- degree(g5_degseq)
g5_degseq_communities <- fastgreedy.community(g5_degseq)
g5_degseq_communities_modularity <- modularity(g5_degseq_communities)
print(sprintf("Community modularity of degree sequenced graph n=%d and m=%d: %.3f",n,m,g5_degseq_communities_modularity))

# plot
png(sprintf("Question2_parth_plot_degseq_n=%d,m=%d.png",n,m))
plot(g5_degseq, main=sprintf("Communities in Undirected Graph with n=%d and m=%d\n(method=simple.no.multiple)",n,m), 
     mark.groups=groups(g5_degseq_communities), vertex.color="gold", vertex.frame.color="black", 
     vertex.size=2, vertex.label="", edge.arrow.size=.2)

# for a more colorful plot
# plot(g5_degseq_communities, g5_degseq, main=sprintf("Communities in Undirected Graph with n=%d and m=%d\n(method=simple.no.multiple)",n,m), vertex.color="gold", vertex.frame.color="black", vertex.size=2, vertex.label="", edge.arrow.size=.2)

dev.off()

# plot bar of community sizes
g5_degseq_communities_sizes <- as.vector(sizes(g5_degseq_communities))
g5_degseq_communities_ind <- c(1:length(sizes(g5_degseq_communities)))
g5_degseq_communities_df <- data.frame(community=g5_degseq_communities_ind, size=g5_degseq_communities_sizes)
png(sprintf("Question2_parth_degseq_bar_n=%d,m=%d.png",n,m))
ggplot(g5_degseq_communities_df, aes(x=g5_degseq_communities_ind, y=g5_degseq_communities_sizes))+
  geom_bar(stat="identity", fill="royalblue", color="black", width=.7)+
  labs(title=sprintf("Community sizes for degree sequenced graph with n=%d and m=%d\n(method=simple.no.multiple)",n,m), x="Community", y="Size")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()



