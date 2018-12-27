install.packages("igraph")
install.packages("dplyr")
library(igraph)
library(dplyr)

setwd("C:/Users/Algomoninc/Documents/UCI/Customer and Social Analytics")

products <- read.csv("products.csv", header=T)
co_purchase <- read.csv("copurchase.csv", header=T)

# slicing the products table to get the subset that are just Books
bookproducts <- products[products$group=="Book",]

# reducing the subset to include only those with salesranks within 0-150000, inclusive
bookproducts <- bookproducts[bookproducts$salesrank<=150000 & bookproducts$salesrank>=0,]

# slicing the copurchase table so that it only includes copurchases that involved books
copurchase_books <- co_purchase[co_purchase$Source %in% bookproducts$id & co_purchase$Target %in% bookproducts$id,]

# converting all ids to characters
bookproducts$id <- as.character(bookproducts$id)
copurchase_books$Source <- as.character(copurchase_books$Source)
copurchase_books$Target <- as.character(copurchase_books$Target)

# examining new structures and dataframes
str(bookproducts)
str(copurchase_books)
nrow(bookproducts)

# looking to see if there are duplicate copurchases which there are not
nrow(copurchase_books); nrow(unique(copurchase_books[,c("Source", "Target")]))

# organizing the copurchase dataframe by Source, then Target
copurchase_books <- copurchase_books[order(copurchase_books$Source, copurchase_books$Target),]

# creating the network using the bookproducts as nodes and copurchase relationships as edges
net <- graph_from_data_frame(d=copurchase_books, vertices=bookproducts, directed=T)

#net <- simplify(net, remove.multiple = F, remove.loops = T) # not necessary because no dups

# examining the nodes and edges
V(net)
E(net)

#plotting the network which looks like a giant sphere with concentration around a donut ring
plot(net, edge.arrow.size=.01, edge.curved=0, edge.arrow.width=.02, vertex.size=.01,
     vertex.label=NA,
     vertex.color="orange", vertex.frame.color="#555555")

# examinging information about the nodes in order to determine a subcomponent with most links
in_degree = degree(net, mode = 'in')
out_degree = degree(net, mode = 'out')
tot_degree = degree(net, mode = 'all')
max(in_degree)
max(out_degree)
max(tot_degree)

# finding the node with highest total degrees
maxdegreenode <- V(net)$name[degree(net)==max(tot_degree)]
# creating a subcomponent of the first node with highest degrees
sub <- subcomponent(net, maxdegreenode[1], mode = "all")

# generating a subnetwork based on subcomponent
subg <- induced_subgraph(net, sub)
#saving the node names and putting them into a vector
#subid<- V(subg)$name
V(subg)$label <- V(subg)$name
subid <- V(subg)$label
#examining the new nodes and edges
V(subg)
E(subg)

# graphing the new subcomponent
set.seed(123)

plot(subg, edge.arrow.size=.1, edge.curved=0, edge.arrow.width=.2, vertex.size=2,
     vertex.label=NA,
     vertex.color="skyblue", vertex.frame.color="#555555",
     layout = layout.kamada.kawai)

# identifying the diameter, finding and coloring its path so that it will be visible in graph
diameter(subg, directed=T, weights=NA)
nodes.diameter <- get.diameter(subg)
# setting colors for default nodes & edges of graph and then the diameter size & color as different
V(subg)$color<-"skyblue"
V(subg)$size <- 2
V(subg)[nodes.diameter]$color<-"darkgreen"
V(subg)[nodes.diameter]$size<-4

E(subg)$color<-"lightgrey"
E(subg)$width <- .4
E(subg,path=nodes.diameter)$color<-"darkgreen"
E(subg,path=nodes.diameter)$width<-2

# investigating different standard layouts to determine which is best
# by assiging the layout to a variable, stabilizes the graph for comparison
l <- layout_with_kk(subg)
#l <- layout_with_lgl(subg)
#l<- layout_with_fr(subg)

# setting norms to spread out graph
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax = 1)

# graph showing the diameter
plot(subg, edge.arrow.size=.15,  
     rescale=F, layout=l*1.5,
     vertex.label=NA,
     layout=l)

# looking at the degrees of the subcomponent
indeg <- degree(subg, mode ='in')
outdeg <- degree(subg, mode = 'out')
deg <- degree(subg, mode = "all")

# below are duplicate methods of generating the degree for subcomponent
#centr_degree(subg, mode='in', normalized = T)
#centr_degree(subg, mode = 'all', normalized = T)

# generating a dataframe for the subcompoent of the books with the ids and degrees for comparison
subbookproducts <- data.frame(subid, indeg, outdeg, deg)
# generating a subcomponent copurchase dataframe in case it is needed
subbokcopurchase <- as_edgelist(subg, names = T)

# graphing with size as a multiple of the total degrees
plot(subg, 
     vertex.size=deg*.6,
     vertex.label=NA,
     edge.arrow.size=.2,
     rescale=F, layout=l*2,
     layout=l
     )

# examining the histogram and degree distribution of the degrees of the subcomponent
hist(deg, breaks = 1:55, main="Histogram of node dgree")

deg.dist <- degree_distribution(subg, cumulative = T, mode="all")
plot(x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col='orange',
     xlab="Degree", ylab="Cumulative Frequency")

# determining the edge density of the subcomponent in two different manners
edge_density(subg, loops = F)
ecount(subg)/(vcount(subg)*(vcount(subg)-1))

# examining the reciprocity of the subcomponent
reciprocity(subg)

# examining the centrality and placing values in subcomponent table
subbookproducts$clos <- closeness(subg, mode = 'all', weights = NA)
centr_clo(subg, mode = 'all', normalized = T)

#subbookproducts$eigen <- eigen(subg, directed=T, normalized=T)
centr_eigen(subg, directed=T, normalized = T)

subbookproducts$btwn <- betweenness(subg, directed=T, weights=NA)
#edge_betweenness(subg, directed = T, weights = NA) #could add to subcopurchasebooks table
centr_betw(subg, directed = T, normalized = T)

#Below only works for undirected graphs -- not in our situation
#csubg <- cluster_edge_betweenness(subg)
#plot(csubg,
#     net,
#     vertex.size = 10,
#     vertex.label.cex = 0.8)

hs <- hub.score(subg, weights=NA)$vector
as <- authority_score(subg, weights = NA)$vector
subbookproducts$hs <- hs
subbookproducts$as <- as

par(mfrow=c(1,2))
plot(subg, vertex.size=hs*4, vertex.label=NA, edge.arrow.size=0.2, rescale=F, layout=l*2,layout=l, main="Hubs")
plot(subg, vertex.size=as*10, vertex.label=NA, edge.arrow.size=0.2, rescale=F, layout=l*2,layout=l, main='Authorities')

par(mfrow=c(1,1))

# building on to the subbookproducts table by adding in the mean sales rank, review count, and
# rating of each of the subcomponent nodes and in degree neighbors by creating a vector of the 
# incoming neighbor nodes and then using this to build a small slice of the original book products
# dataframe from which the means can be generated for the requested columns
for (i in 1:904) {
  subbookproducts$salesrank[V(subg)[i]] <- bookproducts$salesrank[V(subg)[i]]
  subbookproducts$review_cnt[V(subg)[i]] <- bookproducts$review_cnt[V(subg)[i]]
  subbookproducts$downloads[V(subg)[i]] <- bookproducts$downloads[V(subg)[i]]
  subbookproducts$rating[V(subg)[i]] <- bookproducts$rating[V(subg)[i]]
  ngh_nodes <- neighbors(subg, V(subg)[i], mode='in' )
  tempnodes <- as_ids(ngh_nodes)
  tempdf <- filter(bookproducts,  bookproducts$id %in% tempnodes)
  subbookproducts$ngh_mn_salesrank[V(subg)[i]]<- mean(tempdf$salesrank)
  subbookproducts$ngh_mn_review_cnt[V(subg)[i]]<- mean(tempdf$review_cnt)
  subbookproducts$ngh_mn_rating[V(subg)[i]]<- mean(tempdf$rating)
}
# Now we have a table of information about all the nodes within the subcomponent
View(subbookproducts)
summary(subbookproducts)
str(subbookproducts)

# Running Poisson logistic regression to determine which variables have biggest impact on sales rank
fit.salesrank <- glm(salesrank ~ review_cnt + downloads + rating + ngh_mn_salesrank + ngh_mn_review_cnt + ngh_mn_rating
                     + indeg + outdeg + deg + clos + btwn + hs + as, data = subbookproducts, family = poisson(link = log))
summary(fit.salesrank)
# running the correlation to identify collinearity
cor(subbookproducts[,-1])

# Removing total degrees (deg) because it is highly correlated to indegree  It didn't compute in the first regression because it is 
# 100% defined by the two other variables in the regression (indeg + outdeg)
fit.salesrank_final <- glm(salesrank ~ review_cnt + downloads + rating + ngh_mn_salesrank + ngh_mn_review_cnt + ngh_mn_rating + indeg + outdeg + clos + btwn + hs + as, data = subbookproducts, family = poisson())
summary(fit.salesrank_final)

# By taking the exp of each coefficient, we can say that salesrank will increase by that amount, exp(coefficient), when the variable increases by 1 unit
# This means that the actual sales will decrease based on the increased sales rank
Increase <- data.frame(exp(fit.salesrank_final$coefficients))
Increase
