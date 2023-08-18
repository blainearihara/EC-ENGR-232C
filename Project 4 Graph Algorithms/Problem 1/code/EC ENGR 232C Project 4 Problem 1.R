library(igraph)
library(pracma)
library(Matrix)
library(ggplot2)
library(clevr)

################################################################################
## daily data
daily_data <- function(data_path){
  all_file_names = list.files(paste(wd,datapath,sep="/"), pattern="*.csv")
  
  return <- matrix(data=0, nrow=764, ncol=length(all_file_names))
  log_return <- matrix(data=0, nrow=nrow(return), ncol=ncol(return))
  excluded_stocks <- character()
  stock_names <- character()
  # Calculate stock returns over all time for all stocks
  i <- 0
  for (s in 1:length(all_file_names)){
    # read in stock data
    stock <- read.csv(paste(wd,data_path,all_file_names[s],sep="/"))
    if (dim(stock)[1]==765){
      i <- i+1
      stock_names <- c(stock_names,all_file_names[s])
      for (t in 1:length(stock$Close)-1){
        # Calculate the stock returns
        return[t,i] <- (stock$Close[t+1] - stock$Close[t])/(stock$Close[t])
        # Calculate the log-normalized stock returns
        log_return[t,i] <- log(1 + return[t,i])
      }
    }else{
      excluded_stocks <- c(excluded_stocks, gsub("\\.csv$","",all_file_names[s]))
    }
  }
  # calculate the log return means
  return <- return[, -seq(i+1,s,1)]
  log_return <- log_return[, -seq(i+1,s,1)]
  return(list(return=return, log_return=log_return, stock_names=stock_names, excluded_stocks=excluded_stocks))
}

## weekly data
weekly_data <- function(data_path){
  all_file_names = list.files(paste(wd,datapath,sep="/"), pattern="*.csv")
  
  return <- matrix(data=0, nrow=764, ncol=length(all_file_names))
  log_return <- matrix(data=0, nrow=nrow(return), ncol=ncol(return))
  excluded_stocks <- character()
  stock_names <- character()
  # Calculate stock returns over all time for all stocks
  i <- 0
  for (s in 1:length(all_file_names)){
    # read in stock data
    stock_week <- read.csv(paste(wd,data_path,all_file_names[s],sep="/"))
    # take only mondays
    stock_week["Day"] <- weekdays(as.Date(stock_week[,1]))
    stock_week <- subset(stock_week, Day=="Monday")
    if (dim(stock_week)[1]==143){
      i <- i+1
      stock_names <- c(stock_names,all_file_names[s])
      for (t in 1:length(stock_week$Close)-1){
        # Calculate the stock returns
        return[t,i] <- (stock_week$Close[t+1] - stock_week$Close[t])/(stock_week$Close[t])
        # Calculate the log-normalized stock returns
        log_return[t,i] <- log(1 + return[t,i])
      }
    }else{
      excluded_stocks <- c(excluded_stocks, gsub("\\.csv$","",all_file_names[s]))
    }
  }
  # calculate the log return means
  return <- return[, -seq(i+1,s,1)]
  log_return <- log_return[, -seq(i+1,s,1)]
  return(list(return=return, log_return=log_return, stock_names=stock_names, excluded_stocks=excluded_stocks))
}

## monthly data
monthly_data <- function(data_path){
  all_file_names = list.files(paste(wd,datapath,sep="/"), pattern="*.csv")
  
  return <- matrix(data=0, nrow=764, ncol=length(all_file_names))
  log_return <- matrix(data=0, nrow=nrow(return), ncol=ncol(return))
  excluded_stocks <- character()
  stock_names <- character()
  # Calculate stock returns over all time for all stocks
  i <- 0
  for (s in 1:length(all_file_names)){
    # read in stock data
    stock_month <- read.csv(paste(wd,data_path,all_file_names[s],sep="/"))
    # take only the days on the 15th of each month
    stock_month <- subset(stock_month, substr(stock_month[,1], nchar(stock_month[,1])-1, nchar(stock_month[,1]))=="15")
    if (dim(stock_month)[1]==25){
      i <- i+1
      stock_names <- c(stock_names,all_file_names[s])
      for (t in 1:length(stock_month$Close)-1){
        # Calculate the stock returns
        return[t,i] <- (stock_month$Close[t+1] - stock_month$Close[t])/(stock_month$Close[t])
        # Calculate the log-normalized stock returns
        log_return[t,i] <- log(1 + return[t,i])
      }
    }else{
      excluded_stocks <- c(excluded_stocks, gsub("\\.csv$","",all_file_names[s]))
    }
  }
  # calculate the log return means
  return <- return[, -seq(i+1,s,1)]
  log_return <- log_return[, -seq(i+1,s,1)]
  return(list(return=return, log_return=log_return, stock_names=stock_names, excluded_stocks=excluded_stocks))
}

################################################################################
wd <- getwd()
data_path <- c('finance_data/finance_data/data')
# daily data
output <- daily_data(data_path)

# weekly data
# output <- weekly_data(data_path)

# monthly data
# output <- monthly_data(data_path)


## Question 2
return <- output$return
log_return <- output$log_return
stock_names <- output$stock_names
excluded_stocks <- output$excluded_stocks

# calculate the correlation coefficient matrix
rho_matrix <- matrix(data=0, nrow=length(stock_names), ncol=length(stock_names))
weights <- matrix(data=0, nrow=nrow(rho_matrix), ncol=ncol(rho_matrix))
for (i in 1:length(stock_names)){
  for (j in i:length(stock_names)){
    if (i != j){
      r_i <- mean(log_return[,i])
      r_j <- mean(log_return[,j])
      r_ij <- mean(log_return[,i] * log_return[,j])
      r_ii <- mean(log_return[,i] * log_return[,i])
      r_jj <- mean(log_return[,j] * log_return[,j])
      rho_matrix[i,j] <- (r_ij - r_i*r_j)/sqrt((r_ii - r_i^2)*(r_jj - r_j^2))
      # Calculate the weights
      weights[i,j] <- sqrt(2*(1-rho_matrix[i,j]))
    }
  }
}

# hist(upper_diag_weights, breaks=50, main='Histogram of weights')

# create data frame for ggplot
upper_diag_weights <- weights[upper.tri(weights)]
upper_diag_weights_df <- data.frame()
upper_diag_weights_df <- data.frame(w=upper_diag_weights)


png(sprintf("../plots/Question 2/hist.png"),width=1000, height=600)
print(ggplot(upper_diag_weights_df, aes(x=upper_diag_weights, y=after_stat(count)))+
        geom_histogram(fill = "royalblue",color="black")+
        scale_x_continuous(breaks = seq(0,1.6,0.2)) +
        stat_bin(aes(x=upper_diag_weights, y=after_stat(count), label=after_stat(ifelse(count == 0,"",count))), geom="text", vjust=-1, size=3) +
        labs(title=sprintf("Histogram of Correlation Coefficient Edge Weights for Daily Data")) +
        theme(plot.title = element_text(hjust = 0.5)))
dev.off()


## Question 3
# read in the sector information for the stocks
sector_set <- read.csv(paste(wd,data_path,'../Name_sector.csv',sep="/"))
remove_excluded <- !sector_set$Symbol%in%excluded_stocks
sector_set <- sector_set[remove_excluded,]
sector_index <- integer(length=length(sector_set$Sector))
sectors <- unique(sector_set$Sector)


# create a graph and the minimum spanning tree of the stock information from the computed weights
g <- graph_from_adjacency_matrix(weights, weighted=TRUE, mode="undirected")
min_span_tree <- mst(g, weights=E(g)$weight)

# set colors for the different sectors
colors <- rainbow(length(sectors))
names(colors) <- sectors

for (i in 1:vcount(min_span_tree)){
  V(min_span_tree)[i]$name <- sector_set$Symbol[i]
  V(min_span_tree)[i]$sector <- sector_set$Sector[i]
  ind <- which(names(colors)==V(min_span_tree)[i]$sector)
  sector_index[i] <- ind
  V(min_span_tree)[i]$color <- colors[ind]
}

png(sprintf("../plots/Question 3/mst.png"), width=1000, height=600)
plot(min_span_tree, main=sprintf("Minimum Spanning Tree of Monthly Stock Data"), vertex.color=V(min_span_tree)$color,
     vertex.frame.color="black", vertex.size=7, vertex.label="", edge.arrow.size=.2, edge.color="black", edge.width=0.5)
legend(legend=sectors,pch=21, pt.bg=colors, cex=1., x=1.2, y=1)
dev.off()

## Question 4
# Run walktrap community detection
wc <- walktrap.community(min_span_tree)
png(sprintf("../plots/Question 4/community.png"), width=1000, height=600)
plot(wc, min_span_tree, main=sprintf("Minimum Spanning Tree of Daily Stock Communities"), vertex.color=V(min_span_tree)$color,
     vertex.frame.color="black", vertex.size=7, vertex.label="", edge.arrow.size=.2, edge.color="black", edge.width=0.5)
legend(legend=sectors,pch=21, pt.bg=colors, cex=1, x=1.2, y=1)
dev.off()

true <- c(sector_index)
pred <- c(wc$membership)
writeLines(sprintf("Question 4"),"../plots/Question 4/Question 4.txt",sep="\n")
cat(sprintf("Homogeneity: %.3f\n",homogeneity(true, pred)),file="../plots/Question 4/Question 4.txt",append=T)
cat(sprintf("Completeness: %.3f\n",completeness(true,pred)),file="../plots/Question 4/Question 4.txt",append=T)
cat(sprintf("Modularity: %.3f\n",modularity(wc)),file="../plots/Question 4/Question 4.txt",append=T)
homogeneity(true, pred)
completeness(true,pred)

## Question 5
# find number of stocks in each sector
S_i = table(sector_set$Sector)
q=vector(mode='numeric', length=vcount(min_span_tree))
# find alpha for 
p_method_1 = c()
p_method_2 = c()
for(v in c(1:vcount(min_span_tree))){
  # number of neighbors of stock v
  N_i <- length(neighbors(min_span_tree,v))
  Q_i <- 0
  for (N in as_ids(neighbors(min_span_tree,v))){
    
    N_ind <- which(sector_set$Symbol==N)
    # if stock v and stock N belong to the same sector
    if (sector_set$Sector[v] == sector_set$Sector[N_ind]){
      Q_i = Q_i + 1
    }
  }
  q[v] <- Q_i
  p_method_1[v] <-  Q_i / N_i
  p_method_2[v] <-  S_i[sector_set$Sector[v]]/vcount(min_span_tree)
}

writeLines(sprintf("Question 5"), "../plots/Question 5/Question 5.txt", sep="\n")
alpha_1 = sum(p_method_1)/vcount(min_span_tree)
cat(sprintf("alpha 1: %.5f\n", alpha_1), file="../plots/Question 5/Question 5.txt", append=T)
alpha_2 =  sum(p_method_2)/vcount(min_span_tree)
cat(sprintf("alpha 1: %.5f\n", alpha_2), file="../plots/Question 5/Question 5.txt", append=T)

