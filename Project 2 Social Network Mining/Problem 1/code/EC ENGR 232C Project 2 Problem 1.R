library(igraph)
library(pracma)
library(Matrix)
library(ggplot2)

rm(list=ls())
# 1. Facebook Network ###########################################
#####################################################################

print("Question 1 ========================================================")
# read in the facebook network data from facebook_combined.txt
fb_network <- read_graph("facebook_combined.txt", format='ncol', directed = F)
writeLines(paste("Nodes in the Facebook Network: ", vcount(fb_network)))
writeLines(paste("Edges in the Facebook Network: ", ecount(fb_network)))
writeLines(paste("Is the Facebook Network connected?: ", is_connected(fb_network)))

# find the clusters of the graph
fb_network_clusters <- clusters(fb_network)
# find index of the biggest cluster, or the Giant Connected Component (GCC)
ind <- which.max(fb_network_clusters$csize)
# grab the GCC subgraph
fb_network_gcc <- induced.subgraph(fb_network, which(fb_network_clusters$membership == ind))

writeLines(paste("Facebook Network GCC size: ", length(fb_network_gcc)))

print("Question 2 ========================================================")
writeLines(paste("Facebook Network GCC diameter: ", diameter(fb_network_gcc)))


print("Question 3 ========================================================") 
# plot the visualization of the facebook network
png(sprintf("../plots/Question_3/Question3_fb_network_visualization.png"))
plot(fb_network_gcc, main="Facebook Network", vertex.size=4, vertex.label=NA, edge.arrow.size=0.2,
     layout=layout.fruchterman.reingold, edge.color="grey50")
dev.off()

# find the degree distribution of the network
fb_network_degree <- degree(fb_network)
fb_network_deg_df <- data.frame(prop=fb_network_degree)

# plot bar of the degree distribution
png(sprintf("../plots/Question_3/Question3_degree_distribution.png"), width=1000, height=600)
print(ggplot(fb_network_deg_df, aes(x=fb_network_degree, y=after_stat(count)))+
        geom_histogram(fill = "royalblue",color="black") +
        scale_x_continuous(breaks = seq(0, ceiling(max(fb_network_deg_df$prop)/10)*10, 100)) +
        stat_bin(aes(x=fb_network_degree, y=after_stat(count), label=after_stat(ifelse(count == 0, "", count))), geom ="text", vjust = -1) +
        labs(title=sprintf("Degree Distribution for Facebook Network")) +
        theme(plot.title = element_text(hjust = 0.5)))
dev.off()

writeLines(sprintf("Average Degree %.3f",mean(fb_network_degree)),"../plots/Question_3/Question_3.txt",sep="\n")

print("Question 4 ========================================================")
fb_network_distribution <- degree_distribution(fb_network)
ind <- which(fb_network_distribution != 0) #find ind of values != 0
log_x <- log(seq(1,length(fb_network_distribution),1))[ind]
log_y <- log(fb_network_distribution)[ind]
fb_network_dist_df <- data.frame(logx=log_x, logy=log_y)

# estimate the slope using linear regression
model <- lm(log_y ~ log_x)
intercept <- coef(model)[1]
slope <- coef(model)[2]
writeLines(sprintf("Slope of linear regression for log-log degree distribution = %.3f",slope),"../plots/Question_4/Question_4.txt",sep="\n")
cat(sprintf("Intercept of linear regression for log-log degree distribution = %.3f",intercept),file="../plots/Question_4/Question_4.txt",append=T)

# plot the Log-Log of the degree distribution
png(sprintf("../plots/Question4_loglog_dist.png"), width=1000, height=600)
print(ggplot(fb_network_dist_df, aes(x=log_x, y=log_y))+
        geom_point()+
        geom_abline(intercept=intercept, slope=slope, col="red")+
        labs(title=sprintf("Log-Log Degree Distribution for Facebook Network"), x="Log(Degree)", y="Log(Probability)")+
        theme(plot.title = element_text(hjust = 0.5)))
dev.off()

# 2. Personalized Network ###########################################
#####################################################################
print("Question 5 ========================================================")
# grab the personalized network/induced subgraph of node 1
v1 <- "0"
fb_network_v1 <- make_ego_graph(fb_network, order=1,nodes=v1)[[1]] #[[1]] returns object as an 'igraph' class, without is returned as a list
png(sprintf("../plots/Question_5/Question5_node1_subgraph.png"))
plot(fb_network_v1, main="Personalized Network of Node 1", vertex.size=5, vertex.label=NA, edge.arrow.size=0.2, 
     layout=layout.fruchterman.reingold, edge.color="grey50")
dev.off()

# print the nodes and edges count
writeLines(sprintf("Nodes of Node 1 personalized subgraph = %d",vcount(fb_network_v1)),"../plots/Question_5/Question_5.txt",sep="\n")
cat(sprintf("Edges of Node 1 personalized subgraph = %d",ecount(fb_network_v1)),file="../plots/Question_5/Question_5.txt",append=T)

print("Question 6 ========================================================")
# diameter of the personalized network
writeLines(sprintf("Diameter of Node 1 personalized subgraph = %d",diameter(fb_network_v1)),"../plots/Question_6/Question_6.txt",sep="\n")
# Given the condition that the order of the induced subgraph was 1, a trivial upper bound for this graph is 2
# and a trivial lower bound of the graph is 1.

print("Question 7 ========================================================")
# a diameter of a trivial lower bound = 1 means that node 1 has friends that have no mutual friend
# a diameter of a trivial upper bound = 2 means that node 1 has at least one friend with a mutual other friend.


# 3. Code node's personalized network ###############################
#####################################################################
print("Question 8 ========================================================")
core_nodes <- which(fb_network_degree > 200)
writeLines(sprintf("Core nodes in the Facebook network = %d",length(core_nodes)),"../plots/Question_8/Question_8.txt",sep="\n")
cat(sprintf("Average degree of the core nodes = %.3f",sum(fb_network_degree[core_nodes])/length(core_nodes)),file="../plots/Question_8.txt",append=T)


print("Question 9 ========================================================")
writeLines(sprintf("Community modularity scores: "),"../plots/Question_9.txt",sep="\n")
core_nodes_id <- c('0', '107', '348', '483', '1086')

for (core_node in core_nodes_id){
  # generate subgraph for the core nodes
  g_sub <- make_ego_graph(fb_network, order=1, nodes=core_node)[[1]]
  
  
  # plot the community structure using the Fast-greedy method
  g_fastgreedy_comm <- cluster_fast_greedy(g_sub)
  cat(sprintf("Fast-Greedy Community modularity for core node %d = %.3f\n",1+as.integer(core_node),modularity(g_fastgreedy_comm)),file="../plots/Question_9.txt",append=T)
  png(sprintf("../plots/Question_9/Question9_fastgreedy_node_%d.png",1+as.integer(core_node)))
  plot(g_sub, main=sprintf("Fast-Greedy Community Structure for Core Node %d",1+as.integer(core_node)), mark.groups=groups(g_fastgreedy_comm), vertex.color=(g_fastgreedy_comm$membership),
       vertex.color="gold", vertex.frame.color="black", vertex.size=4, vertex.label="", edge.arrow.size=.2, edge.color="grey50", edge.width=0.3, layout=layout.fruchterman.reingold)
  dev.off()
  
  # plot the community structure using the Edge-betweenness method
  g_edgebetween_comm <- cluster_edge_betweenness(g_sub)
  cat(sprintf("Edge-Betweenness Community Modularity for Core Node %d = %.3f\n",1+as.integer(core_node),modularity(g_edgebetween_comm)),file="../plots/Question_9.txt",append=T)
  png(sprintf("../plots/Question_9/Question9_edgebetween_node_%d.png",1+as.integer(core_node)))
  plot(g_sub, main=sprintf("Edge-Betweenness Community Structure for Core Node %d",1+as.integer(core_node)), mark.groups=groups(g_edgebetween_comm), vertex.color=(g_edgebetween_comm$membership),
       vertex.color="gold", vertex.frame.color="black", vertex.size=4, vertex.label="", edge.arrow.size=.2, edge.color="grey50", edge.width=0.3, layout=layout.fruchterman.reingold)
  dev.off()
  
  # plot the community structure using the Custer Infomap method
  g_infomap_comm <- cluster_infomap(g_sub)
  cat(sprintf("Cluster Infomap Community Modularity for Core Node %d = %.3f\n",1+as.integer(core_node),modularity(g_infomap_comm)),file="../plots/Question_9.txt",append=T)
  png(sprintf("../plots/Question_9/Question9_infomap_node_%d.png",1+as.integer(core_node)))
  plot(g_sub, main=sprintf("Cluster Infomap Community Structure for Core Node %d",1+as.integer(core_node)), mark.groups=groups(g_infomap_comm), vertex.color=(g_infomap_comm$membership),
       vertex.color="gold", vertex.frame.color="black", vertex.size=4, vertex.label="", edge.arrow.size=.2, edge.color="grey50", edge.width=0.3, layout=layout.fruchterman.reingold)
  dev.off()
}

print("Question 10 =======================================================")

writeLines(sprintf("Community modularity scores: "),"../plots/Question_10/Question_10_deleteCoreNode.txt",sep="\n")
core_nodes_id <- c('0', '107', '348', '483', '1086')

for (core_node in core_nodes_id){
  # generate subgraph for the core nodes
  g_sub <- make_ego_graph(fb_network, order=1, nodes=core_node)[[1]]
  # remove the core node
  g_sub_removed <- delete_vertices(g_sub, core_node)
  
  # plot the community structure using the Fast-greedy method
  g_fastgreedy_comm <- cluster_fast_greedy(g_sub_removed)
  cat(sprintf("Fast-Greedy Community modularity for core node %d = %.3f\n",1+as.integer(core_node),modularity(g_fastgreedy_comm)),file="../plots/Question_10/Question_10_deleteCoreNode.txt",append=T)
  png(sprintf("../plots/Question_10/Question10_fastgreedy_node_%d.png",1+as.integer(core_node)))
  plot(g_sub_removed,main=sprintf("Fast-Greedy Community Structure for Core Node %d removed",1+as.integer(core_node)), mark.groups=groups(g_fastgreedy_comm), vertex.color=(g_fastgreedy_comm$membership),
       vertex.color="gold", vertex.frame.color="black", vertex.size=4, vertex.label="", edge.arrow.size=.2, edge.color="grey50", edge.width=0.3, layout=layout.fruchterman.reingold)
  dev.off()
  
  # plot the community structure using the Edge-betweenness method
  g_edgebetween_comm <- cluster_edge_betweenness(g_sub_removed)
  
  cat(sprintf("Edge-Betweenness Community Modularity for Core Node %d = %.3f\n",1+as.integer(core_node),modularity(g_edgebetween_comm)),file="../plots/Question_10/Question_10_deleteCoreNode.txt",append=T)
  png(sprintf("../plots/Question_10/Question10_edgebetween_node_%d.png",1+as.integer(core_node)))
  plot(g_sub_removed, main=sprintf("Edge-Betweenness Community Structure for Core Node %d removed",1+as.integer(core_node)), mark.groups=groups(g_edgebetween_comm), vertex.color=(g_edgebetween_comm$membership),
       vertex.color="gold", vertex.frame.color="black", vertex.size=4, vertex.label="", edge.arrow.size=.2, edge.color="grey50", edge.width=0.3, layout=layout.fruchterman.reingold)
  dev.off()
  
  # plot the community structure using the Custer Infomap method
  g_infomap_comm <- cluster_infomap(g_sub_removed)
  
  cat(sprintf("Cluster Infomap Community Modularity for Core Node %d = %.3f\n",1+as.integer(core_node),modularity(g_infomap_comm)),file="../plots/Question_10/Question_10_deleteCoreNode.txt",append=T)
  png(sprintf("../plots/Question_10/Question10_infomap_node_%d.png",1+as.integer(core_node)))
  plot(g_sub_removed, main=sprintf("Cluster Infomap Community Structure for Core Node %d removed",1+as.integer(core_node)), mark.groups=groups(g_infomap_comm), vertex.color=(g_infomap_comm$membership),
       vertex.color="gold", vertex.frame.color="black", vertex.size=4, vertex.label="", edge.arrow.size=.2, edge.color="grey50", edge.width=0.3, layout=layout.fruchterman.reingold)
  dev.off()
}

print("Question 11 =======================================================")
# embeddedness: the number of mutual friends a node shares with the core node.
# dispersion: the sum of distances between every pair of the mutual friends the node shares with the core node
# embeddedness(v_node, v_core) = deg(v_node) - 1

print("Question 12, 13, 14 ===============================================")
writeLines(sprintf("Question 13 and 14"),"../plots/Question_14/Question_14.txt",sep="\n")
# core nodes
core_nodes_id <- c('0', '107', '348', '483', '1086')

for (core_node in core_nodes_id){
  # Embeddedness:
  # Create a subgraph of nodes/neighbors that have at least one edge with the core node
  core_neighborhood <- make_ego_graph(fb_network,order=1,nodes=core_node)[[1]]
  
  # plot the neighborhood
  png(sprintf("../plots/Question_12/Question12_plot_cnode%d.png",1+as.integer(core_node)))
  plot(core_neighborhood,main=sprintf("Neighborhood for Core Node %d",1+as.integer(core_node)),
       vertex.color="gold", vertex.frame.color="black", vertex.size=4, vertex.label="", edge.arrow.size=.2, edge.color="grey50", edge.width=0.3, layout=layout.fruchterman.reingold)
  dev.off()
  
  # remove the core node
  coreless_neighborhood <- delete_vertices(core_neighborhood, v=core_node)
  
  # get the degrees of all the neighbors
  neighbor_degrees <- degree(coreless_neighborhood)
  
  # create data frame for ggplot
  neighbor_degrees_df <- data.frame()
  neighbor_degrees_df <- data.frame(prop=neighbor_degrees)
  
  # Embeddedness Plot - plot bar of the degree distribution
  png(sprintf("../plots/Question_12/Question12_embed_dist_cnode%d.png",1+as.integer(core_node)))
  print(ggplot(neighbor_degrees_df, aes(x=neighbor_degrees, y=after_stat(count)))+
          geom_histogram(fill = "royalblue",color="black")+
          scale_x_continuous(breaks = seq(0, ceiling(max(neighbor_degrees_df$prop)/(10^(nchar(as.character(round(max(neighbor_degrees_df$prop))))-1)))*(10^(nchar(as.character(round(max(neighbor_degrees_df$prop))))-1)), ceiling(max(neighbor_degrees_df$prop)/(10^(nchar(as.character(round(max(neighbor_degrees_df$prop))))-1)))*(10^(nchar(as.character(round(max(neighbor_degrees_df$prop))))-2)))) +
          stat_bin(aes(x=neighbor_degrees, y=after_stat(count), label=after_stat(ifelse(count == 0,"",count))), geom="text", vjust=-1, size=3) +
          labs(title=sprintf("Embeddedness Degree Distribution for Core Node %d",1+as.integer(core_node))) +
          theme(plot.title = element_text(hjust = 0.5)))
  dev.off()
  
  # Dispersion
  # find neighbors with no mutual friends
  singles <- which(degree(coreless_neighborhood)!=0)
  # get all neighbors in the neighborhood (excluding the core)
  neighbors <- (V(coreless_neighborhood)$name)[singles]
  # run through each of the neighbors
  dispersion.sum <- numeric()
  dispersion.id <- character()
  for (i in neighbors){
    # make modified graph of neighbors (mutual with core) for each neighbor
    clique <- make_ego_graph(coreless_neighborhood,order=1,nodes=i)[[1]]
    # remove the neighbor
    neighborless_clique <- delete_vertices(clique, v=i)
    # get the distance or shortest path between every pair of nodes (remaining mutual friends)
    distance <- distances(neighborless_clique)
    # filter out any infinite values that result from stray neighbors
    distance <- distance[is.finite(distance)]
    # keep running total of sum of distances for every clique of mutual friends
    dispersion.sum <- c(dispersion.sum, sum(distance)/2)
    dispersion.id <- c(dispersion.id, i)
  }
  dispersion.max <- max(dispersion.sum)
  dispersion.max_id <- dispersion.id[which(dispersion.sum==max(dispersion.sum))]
  
  # Dispersion Plot - create data frame for ggplot
  dispersion_df <- data.frame()
  dispersion_df <- data.frame(prop=dispersion.sum)
  # plot bar of the dispersion distribution
  png(sprintf("../plots/Question_12/Question12_dispersion_dist_cnode%d.png",1+as.integer(core_node)))
  print(ggplot(dispersion_df, aes(x=dispersion.sum, y=after_stat(count)))+
          geom_histogram(fill = "royalblue",color="black")+
          scale_x_continuous(breaks = seq(0, ceiling(max(dispersion_df$prop)/(10^(nchar(as.character(round(max(dispersion_df$prop))))-1)))*(10^(nchar(as.character(round(max(dispersion_df$prop))))-1)), ceiling(max(dispersion_df$prop)/(10^(nchar(as.character(round(max(dispersion_df$prop))))-1)))*(10^(nchar(as.character(round(max(dispersion_df$prop))))-2)))) +
          stat_bin(aes(x=dispersion.sum, y=after_stat(count), label=after_stat(ifelse(count == 0,"",count))), geom="text", vjust=-1, size=3) +
          labs(title=sprintf("Dispersion Distribution for Core Node %d",1+as.integer(core_node))) +
          theme(plot.title = element_text(hjust = 0.5)))
  dev.off()
  
  ########## Question 13 ##########
  cat(sprintf("\nCore Node: %d\n",1+as.integer(core_node)),file="../plots/Question_14/Question_14.txt",append=T)
  cat(sprintf("Max dispersion: %.3f\n",dispersion.max),file="../plots/Question_14/Question_14.txt",append=T)
  cat(sprintf("Max dispersion node: %d\n",as.integer(dispersion.max_id)),file="../plots/Question_14/Question_14.txt",append=T)
  
  core_neighborhood_community <- fastgreedy.community(core_neighborhood)
  # set aesthetics for question 13
  edge_color <- rep("grey50", length(E(core_neighborhood))) #set all edge colors grey
  edge_width <- rep(0.3, length(E(core_neighborhood))) #set all edge widths 0.5
  vertex_color <- rep(core_neighborhood_community$membership, times=length(V(core_neighborhood))) #set all vertices colors default
  vertex_size <- rep(4, length(V(core_neighborhood))) #set all vertices sizes 4
  vertex_label <- rep(NA, length(V(core_neighborhood))) #set all vertices labels NA
  
  # # set aesthetics of the core node
  # vertex_color[which(V(core_neighborhood)$name == core_node)] = "black"
  # vertex_size[which(V(core_neighborhood)$name == core_node)] = 10
  
  # special set the aesthetics of the max dispersion node
  edge_color[which(get.edgelist(core_neighborhood,name=TRUE)[,1] == dispersion.max_id | get.edgelist(core_neighborhood,name=TRUE)[,2] == dispersion.max_id)] = "blue"
  edge_width[which(get.edgelist(core_neighborhood,name=TRUE)[,1] == dispersion.max_id | get.edgelist(core_neighborhood,name=TRUE)[,2] == dispersion.max_id)] = 3
  vertex_color[which(V(core_neighborhood)$name == dispersion.max_id)] = "blue"
  vertex_size[which(V(core_neighborhood)$name == dispersion.max_id)] = 10
  vertex_label[which(V(core_neighborhood)$name == dispersion.max_id)] = dispersion.max_id
  
  # plot bar of the dispersion distribution
  png(sprintf("../plots/Question_13/Question13_highlight_maxDisp_cnode%d.png",1+as.integer(core_node)))
  plot(core_neighborhood_community, core_neighborhood, main = sprintf("Neighborhood for Core Node %d \n with Highlighted Max Dispersion",1+as.integer(core_node)),
       edge.color = edge_color,
       edge.width = edge_width,
       vertex.color = vertex_color,
       vertex.size = vertex_size,
       vertex.label= vertex_label,
       layout=layout.fruchterman.reingold)
  dev.off()
  
  ########## Question 14 ##########
  # find node with max embeddedness
  embeddedness.max <- max(neighbor_degrees)
  embeddedness.max_id <- names(neighbor_degrees)[which(neighbor_degrees==max(neighbor_degrees))]
  # calculate the dispersion/embeddedness
  disp_embed <- dispersion.sum/neighbor_degrees
  disp_embed.max <- max(disp_embed)
  disp_embed.max_id <- names(disp_embed)[which(disp_embed==max(disp_embed))]
  
  cat(sprintf("Max embeddedness: %.3f\n",embeddedness.max),file="../plots/Question_14/Question_14.txt",append=T)
  cat(sprintf("Max embeddedness node: %d\n",as.integer(embeddedness.max_id)),file="../plots/Question_14/Question_14.txt",append=T)
  cat(sprintf("Max dispersion/embeddedness: %.3f\n",disp_embed.max),file="../plots/Question_14/Question_14.txt",append=T)
  cat(sprintf("Max dispersion/embeddedness node: %d\n",as.integer(disp_embed.max_id)),file="../plots/Question_14/Question_14.txt",append=T)
  
  # set aesthetics for question 14
  edge_color <- rep("gray", length(E(core_neighborhood))) #set all edge colors grey
  edge_width <- rep(0.3, length(E(core_neighborhood))) #set all edge widths 0.3
  vertex_color <- rep(core_neighborhood_community$membership, times=length(V(core_neighborhood))) #set all vertices colors default
  vertex_size <- rep(4, length(V(core_neighborhood))) #set all vertices sizes 4
  vertex_label <- rep(NA, length(V(core_neighborhood))) #set all vertices labels NA
  
  # # set aesthetics of the core node
  # vertex_color[which(V(core_neighborhood)$name == core_node)] = "black"
  # vertex_size[which(V(core_neighborhood)$name == core_node)] = 10  
  
  # special set the aesthetics of the max dispersion node
  edge_color[which(get.edgelist(core_neighborhood,name=TRUE)[,1] == embeddedness.max_id | get.edgelist(core_neighborhood,name=TRUE)[,2] == embeddedness.max_id)] = "deeppink"
  edge_color[which(get.edgelist(core_neighborhood,name=TRUE)[,1] == disp_embed.max_id | get.edgelist(core_neighborhood,name=TRUE)[,2] == disp_embed.max_id)] = "blueviolet"
  edge_width[which(get.edgelist(core_neighborhood,name=TRUE)[,1] == embeddedness.max_id | get.edgelist(core_neighborhood,name=TRUE)[,2] == embeddedness.max_id)] = 3
  edge_width[which(get.edgelist(core_neighborhood,name=TRUE)[,1] == disp_embed.max_id | get.edgelist(core_neighborhood,name=TRUE)[,2] == disp_embed.max_id)] = 3
  vertex_color[which(V(core_neighborhood)$name == embeddedness.max_id)] = "deeppink"
  vertex_color[which(V(core_neighborhood)$name == disp_embed.max_id)] = "blueviolet"
  vertex_size[which(V(core_neighborhood)$name == embeddedness.max_id)] = 10
  vertex_size[which(V(core_neighborhood)$name == disp_embed.max_id)] = 10
  vertex_label[which(V(core_neighborhood)$name == embeddedness.max_id)] = embeddedness.max_id
  vertex_label[which(V(core_neighborhood)$name == disp_embed.max_id)] = disp_embed.max_id
  
  png(sprintf("../plots/Question_14/Question14_highlight_maxembed&dispEmbed_cnode%d.png",1+as.integer(core_node)))
  plot(core_neighborhood_community, core_neighborhood, main = sprintf("Neighborhood for Core Node %d \n with Highlighted Max Embeddedness & \n Dispersion/Embeddededness",1+as.integer(core_node)),
       edge.color = edge_color,
       edge.width = edge_width,
       vertex.color = vertex_color,
       vertex.size = vertex_size,
       vertex.label= vertex_label)
  dev.off()
}


print("Question 16 =======================================================")
fb_network <- read_graph("facebook_combined.txt", format='ncol', directed = F)
n415 <- c('414')
neighborhood <- make_ego_graph(fb_network, nodes=n415)[[1]]
neighborhood_degrees <- degree(neighborhood)
Nr <- which(neighborhood_degrees==24)
# writeLines(sprintf("Length of N_r with neighboring nodes of degree 24: ",length(Nr_deg24)),"../plots/Question_16/Question_16_Nr_length.txt",sep="\n")

print("Question 17 =======================================================")
# Computing algorithm accuracy:
# Step 1: compute average accuracy for each user in list Nr
#   Step 1a: Remove each edge of node i randomly with probability 0.25. Denote list of deleted friends as Ri
#   Step 1b: Use one of the three neighborhood based measures to recommend |Ri| new friends to user i. Denote list of recommended friends as Pi
#     Step1b_i: to recommend |Ri| new friends to user i, take each node in the neighborhood that is not a neighbor of node i, 
#               and compute the measure between node i and the node not in the neighborhood of it
#     Step1b_ii: Then pick |Ri| nodes that have the highest measure with node i, and recommend those nodes as friends to node i.
#   Step 1c: Calculate accuracy for the user i by |Pi intersect Ri|/|Ri|
#   Step 1d: Iterate steps a-c 10 times and take their average
# Step 2: Compute average accuracy of algorithm by averaging across the accuracies of the users in list Nr


Create_Recommendation <- function(scores, top_number, node_j_ids){
  # sort scores in decreasing and return their indices
  scores_index <- sort(scores, decreasing=TRUE, index.return=T)$ix
  # grab the names/ids of the top |Ri| recommended friends
  Pi <- character()
  Pi <- node_j_ids[scores_index[1:top_number]]
  return(Pi)
}


Score_Friend_Recommendation <- function(Pi,Ri){
  if (isempty(Pi) | (isempty(Ri))){
    score <- 0
  }else{
    score <- length(intersect(Pi,Ri))/length(Ri)
  }
  return(score)
}

### Common Neighbor Algorithm
Common_Neighbor <- function(neighborhood_deleted_edges,node_i_friends_remaining, node_j){
  
  # within the modified neighbors with deleted edges, find the friends of each potential new friend, node J
  node_j_friends <- as_ids(neighbors(neighborhood_deleted_edges,node_j))
  
  # find intersection of node j friends with node i friends
  node_j_score <- numeric()
  node_j_score <- length(intersect(node_i_friends_remaining,node_j_friends))
  if (isempty(node_j_score)){
    node_j_score <- 0
  }
  return(node_j_score)
}

### Jaccard Algorithm
Jaccard <- function(neighborhood_deleted_edges,node_i_friends_remaining, node_j){
  
  # within the modified neighbors with deleted edges, find the friends of each potential new friend, node J
  node_j_friends <- as_ids(neighbors(neighborhood_deleted_edges,node_j))
  
  # find intersection of node j friends with node i friends
  node_j_score <- numeric()
  node_j_intersect <- length(intersect(node_i_friends_remaining,node_j_friends))
  node_j_union <- length(union(node_i_friends_remaining,node_j_friends))
  node_j_score <- node_j_intersect/node_j_union
  if (isempty(node_j_score)){
    node_j_score <- 0
  }
  return(node_j_score)
}

### Common Neighbor Algorithm
Adamic_Adar <- function(neighborhood_deleted_edges,node_i_friends_remaining, node_j){
  
  # within the modified neighbors with deleted edges, find the friends of each potential new friend, node J
  node_j_friends <- as_ids(neighbors(neighborhood_deleted_edges,node_j))
  
  # find intersection of node j friends with node i friends
  node_j_score <- 0
  node_k <- intersect(node_i_friends_remaining,node_j_friends)
  
  for (k in node_k){
    k_size <- length(neighbors(neighborhood_deleted_edges, k))
    node_j_score <- node_j_score + 1/log10(k_size)
  }
  
  if (isempty(node_j_score)){
    node_j_score <- 0
  }
  return(node_j_score)
}


Pi_accuracy_common_neighbor_alg <- numeric()
Pi_accuracy_Jaccard_alg <- numeric()
Pi_accuracy_Adamic_Adar_alg <- numeric()

Pi_accuracy_common_neighbor <- numeric()
Pi_accuracy_Jaccard <- numeric()
Pi_accuracy_Adamic_Adar <- numeric()

iterations <- 10
node_i <- names(Nr)
writeLines(sprintf("Average accuracies for three different friend recommendation algorithms: "),"../plots/Question_17/Question_17.txt",sep="\n")
# run through each node in neighbor_set
for (i in 1:length(node_i)){
  cat(sprintf("Node %d \n", as.integer(node_i[i])),file="../plots/Question_17/Question_17.txt",append=T)
  # get all friends of node i
  node_i_friends <- as_ids(neighbors(neighborhood, node_i[i]))
  
  # remove edges between node_i and node_i_friends with probability 0.25
  edge_ids = which(get.edgelist(neighborhood)[,1] == node_i[i] | get.edgelist(neighborhood)[,2] == node_i[i])
  
  # iterate 10 times to find the average accuracy of Nr_i
  Pi_scores_common_neighbor <- numeric(length(iterations))
  Pi_scores_Jaccard <- numeric(length(iterations))
  Pi_scores_Adamic_Adar <- numeric(length(iterations)) 
  for (k in 1:iterations){
    # randomly pick with 0.25 probability which neighbor gets unfriended
    mask <- runif(length(edge_ids),0,1)
    unfriendship <- edge_ids[mask < 0.25]
    
    # create the community graph around node_i after removing edges
    neighborhood_deleted_edges <- delete.edges(neighborhood, unfriendship)
    
    # find node i friends remaining after removing edges
    node_i_friends_remaining <- as_ids(neighbors(neighborhood_deleted_edges,node_i[i]))
    
    # find remaining friends that are still connected by an edge
    Ri <- setdiff(node_i_friends, node_i_friends_remaining)
    # Ri <- ends(neighborhood, unfriendship)[which(ends(neighborhood, unfriendship) != names(node_i[i]))]
    
    # find neighbors in the neighborhood that are not already friends of node_i: potential_neighborhood_friends
    # node_i_not_friends <- setdiff(V(neighborhood)$name, node_i_friends_remaining)
    # node_j_ids <- setdiff(node_i_not_friends, Ri)
    node_j_ids <- setdiff(V(neighborhood)$name, node_i_friends_remaining)
    # initialize to hold the measures for friend recommendation score
    node_j_scores_common_neighbor <- numeric(length(node_j_ids))
    node_j_scores_Jaccard <- numeric(length(node_j_ids))
    node_j_scores_Adamic_Adar <- numeric(length(node_j_ids))
    
    for (j in 1:length(node_j_ids)){
      # Use Common Neighbor to give score to node j as a potential recommended friend
      node_j_scores_common_neighbor[j] <- Common_Neighbor(neighborhood_deleted_edges,node_i_friends_remaining, node_j_ids[j])
      node_j_scores_Jaccard[j] <- Jaccard(neighborhood_deleted_edges,node_i_friends_remaining, node_j_ids[j])
      node_j_scores_Adamic_Adar[j] <- Adamic_Adar(neighborhood_deleted_edges,node_i_friends_remaining, node_j_ids[j])
    }
    
    # Sort and create list of top |Ri| recommended friends
    Pi_common_neighbor <- Create_Recommendation(node_j_scores_common_neighbor, length(Ri), node_j_ids)
    Pi_Jaccard <- Create_Recommendation(node_j_scores_Jaccard, length(Ri), node_j_ids)
    Pi_Adamic_Adar <- Create_Recommendation(node_j_scores_Adamic_Adar, length(Ri), node_j_ids)
    
    #Score the list of recommended friends
    Pi_scores_common_neighbor[k] <- Score_Friend_Recommendation(Pi_common_neighbor,Ri)
    Pi_scores_Jaccard[k] <- Score_Friend_Recommendation(Pi_Jaccard,Ri)
    Pi_scores_Adamic_Adar[k] <- Score_Friend_Recommendation(Pi_Adamic_Adar,Ri)
    
  }
  # compute average accuracy for each user in the list Nr
  Pi_accuracy_common_neighbor[i] <- sum(Pi_scores_common_neighbor)/length(Pi_scores_common_neighbor)
  Pi_accuracy_Jaccard[i] <- sum(Pi_scores_Jaccard)/length(Pi_scores_Jaccard)
  Pi_accuracy_Adamic_Adar[i] <- sum(Pi_scores_Adamic_Adar)/length(Pi_scores_Adamic_Adar)
  
  cat(sprintf("Common Neighbor Average Accuracy: %.3f \n",Pi_accuracy_common_neighbor[i]),file="../plots/Question_17/Question_17.txt",append=T)
  cat(sprintf("Jaccard Average Accuracy: %.3f \n",Pi_accuracy_Jaccard[i]),file="../plots/Question_17/Question_17.txt",append=T)
  cat(sprintf("Adamic Adar Average Accuracy: %.3f \n\n",Pi_accuracy_Adamic_Adar[i]),file="../plots/Question_17/Question_17.txt",append=T)
}

Pi_accuracy_common_neighbor_alg <- sum(Pi_accuracy_common_neighbor)/length(Pi_accuracy_common_neighbor)
Pi_accuracy_Jaccard_alg <- sum(Pi_accuracy_Jaccard)/length(Pi_accuracy_Jaccard)
Pi_accuracy_Adamic_Adar_alg <- sum(Pi_accuracy_Adamic_Adar)/length(Pi_accuracy_Adamic_Adar)

cat(sprintf("Average Algorithm Accuracy: \n"),file="../plots/Question_17/Question_17.txt",append=T)
cat(sprintf("Common Neighbor Algorithm Accuracy: %.3f \n",Pi_accuracy_common_neighbor_alg),file="../plots/Question_17/Question_17.txt",append=T)
cat(sprintf("Jaccard Average Algorithm: %.3f \n",Pi_accuracy_Jaccard_alg),file="../plots/Question_17/Question_17.txt",append=T)
cat(sprintf("Adamic Adar Average Algorithm: %.3f \n",Pi_accuracy_Adamic_Adar_alg),file="../plots/Question_17/Question_17.txt",append=T)