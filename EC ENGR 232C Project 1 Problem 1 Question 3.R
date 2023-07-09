library(igraph)
library(pracma)
library(Matrix)
library(ggplot2)

rm(list=ls())
# Part a ############################################################
#####################################################################
print("Question 3 Part a ==================================================")

# Create a preferential attachment model that penalizes the age of a node
# calculate probability according to p~(c*k^alpha + a)*(d*l^beta + b)
a <- 1
c <- 1
d <- 1
b <- 0
beta <- -1
alpha <- 1
n <- 1050

# use sample_pa_age() to simulate evolution of a random graph
g <- sample_pa_age(n=n, pa.exp=alpha, aging.exp=beta, zero.deg.appeal=a, 
                   zero.age.appeal=b, deg.coef=c, age.coef=d, directed=F)

# plot the graph
png(sprintf("Question3_parta_plot_n=%d.png",n))
plot(g, main=sprintf("Modified Preferential Attachment Model \nUndirected Graph of n=%d and age penalty",n), 
     vertex.size=4, vertex.label=NA, edge.arrow.size=0.2)
dev.off()

# plot the degree distribution
g_distribution <- degree.distribution(g)
g_distribution_ind <- c(1:length(g_distribution))
g_distribution_df <- data.frame(degree=g_distribution_ind, probability=g_distribution)
# plot the degree distribution
png(sprintf("Question3_parta_dist_n=%d.png",n))
print(ggplot(data = g_distribution_df, aes(x=g_distribution_ind, y=g_distribution))+
        geom_bar(stat="identity", fill="royalblue",color="black", width=.7)+
        labs(title=sprintf("Degree Probability Distribution for Modified \nPreferential Attachment Model with n=%d",n), x="Degree", y="Probability")+
        theme(plot.title = element_text(hjust = 0.5)))
dev.off()

# plot the log-log to find the power law exponent or slope of the linear regression line
# need to remove the zeros from the degree distribution
ind <- which(g_distribution != 0)
log_x <- log(seq(1,length(g_distribution),1))[ind]
log_y <- log(g_distribution)[ind]
g_dist_df <- data.frame(logx=log_x, logy=log_y)

# estimate the slope using linear regression
model <- lm(log_y ~ log_x)
intercept <- coef(model)[1]
slope <- coef(model)[2]
print(sprintf("n=%d: Slope of linear regression for log-log degree distribution = %.3f",n,slope))
print(sprintf("n=%d: Intercept of linear regression for log-log degree distribution = %.3f",n,intercept))

# plot the Log-Log of the degree distribution
png(sprintf("Question3_parta_loglogdist_n=%d.png",n))
print(ggplot(g_dist_df, aes(x=log_x, y=log_y))+
        geom_point()+
        geom_abline(intercept=intercept, slope=slope, col="red")+
        labs(title=sprintf("Log-Log Degree Distribution for \nModified Preferential Attachment Model with n=%d",n), x="Log(Degree)", y="Log(Probability)")+
        theme(plot.title = element_text(hjust = 0.5)))
dev.off()

# Part b ############################################################
#####################################################################
print("Question 3 Part b ==================================================")

g_communities <- cluster_fast_greedy(g)
g_communities_modularity <- modularity(g_communities)
print(sprintf("Community modularity: %.3f", g_communities_modularity))

# plot community structure
png("Question3_partb_communities_plot.png")
plot(g, main=sprintf("Modified Preferential Attachment Model \nCommunities in an Undirected Graph with n=%d",n), 
     mark.groups=groups(g_communities), vertex.color="gold", vertex.frame.color="black", 
     vertex.size=2, vertex.label="", edge.arrow.size=.2)
dev.off()


