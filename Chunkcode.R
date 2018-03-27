#Article Network ver 2
#============================
asdfas
# This program will scrape all citing articles surrounding one main article and 
# create: 1) a list of most cited articles, 2) lists of main authors, 3) network of articles


#Background and references
#1. https://cran.r-project.org/web/packages/rentrez/vignettes/rentrez_tutorial.html#advanced-counting
#2. https://ropensci.org/tutorials/rentrez_tutorial.html
#3. http://www.kateto.net/polnet2015
#4. http://www.shizukalab.com/toolkits/sna
#5. http://stackoverflow.com/questions/16390221/how-to-make-grouped-layout-in-igraph/31976350#31976350
#6. http://michael.hahsler.net/SMU/LearnROnYourOwn/code/igraph.html

#SETUP

#Load libraries
if (!require('XML')) install.packages('XML'); library('XML')
if (!require('rentrez')) install.packages('rentrez'); library('rentrez')
if (!require('igraph')) install.packages('igraph'); library('igraph')

#Set working directory
setwd("~/Dropbox/Projekter/2015 - Article Network")

#RETRIEVE DATA - this will take some time, depending on number of levels to iterate through

#Pubmed ID to start from - retrieve data and setup data frames

startnode <- c(21876761)
num_of_levels <- 1 #number of article "levels" from the start node 

#Get initial data from startnode - this will form the spring/center node
startnode_links <- entrez_link(dbfrom='pubmed', id=startnode, db='pubmed')
startnode_citations <- startnode_links$links$pubmed_pubmed_citedin
#startnode_links$links #shows all the link-out info available 

startnode_data <- entrez_search(db="pubmed", id=startnode)
args(entrez_search())
#Creates node list - will contain data on all nodes
nodes <- data.frame(PubmedID = startnode_data$uid, 
                    Title = startnode_data$title, 
                    Last_Author = startnode_data$lastauthor, 
                    Year =  substr(startnode_data$pubdate, 0, 4), 
                    Pubtype = startnode_data$pubtype, 
                    Citations = startnode_data$pmcrefcount,
                    Level = 0)

#Create Edges list - will contain to and from data
edges <- data.frame(From = startnode_citations, To = rep(startnode, times = length(startnode_citations)))

#Loop through pubmedIDs to generate list

for(l in 1:num_of_levels) {
  tofind_list <- c() #reset list
  
  cat("Level:", l, ".")
  cat("IDs:")
  for(i in 1:length(startnode_citations)) {
    cat(i, ", ") #output id, to check program is still running
    
    #Get node data - but only if not already in node-list
    if(startnode_citations[i] %in% nodes$PubmedID == FALSE) {
      
      node_data <- entrez_summary(db="pubmed", id=startnode_citations[i])
      nodes <- rbind(nodes, data.frame(PubmedID = node_data$uid, 
                                       Title = node_data$title, 
                                       Last_Author = node_data$lastauthor, 
                                       Year =  substr(node_data$pubdate, 0, 4),
                                       Pubtype = node_data$pubtype,
                                       Citations = node_data$pmcrefcount,
                                       Level = l))  
      #Get edge data
      node_links <- entrez_link(dbfrom='pubmed', id=startnode_citations[i], db='all')
      node_citations <- node_links$links$pubmed_pubmed_citedin
      if (length(node_citations > 0)) {  
        edges <- rbind(edges, data.frame(From = node_citations, To = rep(startnode_citations[i], times = length(startnode_citations[i]))))
        tofind_list <- append(tofind_list, node_links$links$pubmed_pubmed_citedin) #Add citations from this article to next list to find
      }
    }
  }
  startnode_citations <- unique(tofind_list) #Make new citation list from all edges found at previous level (but only once, hence the unique function)
}

#Find missing nodes
missing_nodes_IDs <- as.vector(edges[!(edges$From %in% nodes$PubmedID), ][1])
head(missing_nodes_IDs)
nrow(missing_nodes_IDs)

#Remove duplicate nodes
nrow(nodes[duplicated(nodes$PubmedID), ]) #count number of duplicates
nrow(nodes[!duplicated(nodes$PubmedID), ])
nodes <- nodes[!duplicated(nodes$PubmedID), ]

#Loop through missing list 
for(i in 1:nrow(missing_nodes_IDs)) {
  cat(missing_nodes_IDs$From[i]) #output id, to check program is still running
  
  node_data <- entrez_summary(db="pubmed", id=missing_nodes_IDs$From[i])
  
  #Check that data exists
  if (length(node_data$pubtype) > 0) {
    node_data_pubtype = node_data$pubtype
  } else {
    node_data_pubtype = "NA"
  }
  
  #Get node data
  nodes <- rbind(nodes, data.frame(PubmedID = node_data$uid, 
                                   Title = node_data$title, 
                                   Last_Author = node_data$lastauthor, 
                                   Year =  substr(node_data$pubdate, 0, 4),
                                   Pubtype = node_data_pubtype,
                                   Citations = node_data$pmcrefcount,
                                   Level = num_of_levels+1))
}

#Check that there are no missing nodes left - should give NULL result
missing_nodes_IDs <- as.vector(edges[!(edges$From %in% nodes$PubmedID), ][1])
missing_nodes_IDs

#Remove duplicate nodes
nrow(nodes[duplicated(nodes$PubmedID), ]) #count number of duplicates
nrow(nodes[!duplicated(nodes$PubmedID), ])
nodes <- nodes[!duplicated(nodes$PubmedID), ]

#SAVE DATA

write.table(nodes, file="nodes.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.table(edges, file="edges.csv", sep = ",", col.names = TRUE, row.names = FALSE)     

#LOAD DATA
nodes <- read.csv2("nodes.csv", as.is = TRUE, header = TRUE, sep = ",",
                   #nrows = 100,
                   row.names = NULL				
)
edges <- read.csv2("edges.csv", as.is = TRUE, header = TRUE, sep = ",",
                   #nrows = 100,
                   row.names = NULL				
)

#SETUP NETWORK

net <- graph_from_data_frame(edges, directed=TRUE, vertices=nodes)
cutoff <- 30

#VISUALIZE

#Degree distribution

dd <- degree.distribution(net, cumulative=T, mode="all")
plot(dd, pch=19, cex=0.5, col="orange", xlab="Degree", ylab="Cumulative Frequency")

#Whole network with highly citated nodes outlined in red

l <- layout_with_lgl(net) #very large graph layout
l <- layout.norm(l, ymin=-1, ymax=1, xmin=-1, xmax=1) #Sets up graph area
in_degree <- degree(net, mode="in") #mode can be all, out or in

plot(net, 
     vertex.size = log(in_degree+1), #Size of node determained by connections into it
     vertex.label.color = "black",
     #vertex.label = ifelse(degree(net, mode = "in", loops = FALSE) > cutoff, V(net)$Last_Author, NA),
     vertex.label = NA,
     vertex.color = ifelse(degree(net, mode = "in", loops = FALSE) > cutoff, "red", "white"), #Color nodes differently based on their pubtype
     edge.width = .4,
     edge.curved = 0,
     edge.arrow.size = 0,
     rescale = FALSE,
     main = "Network",
     layout = l*1.4 #For playing around with the size of the graph
)
legend(x=-2, y=-1, # position
       legend = c(paste("Nodes >", cutoff, "degrees"), paste("Nodes <", cutoff, " degrees")), 
       pch = 21, #open point
       pt.bg = c("Red","White"),
       pt.cex=2, 
       cex=.8, bty="n", 
       ncol=1
)
legend(x=1, y=-1, # position
       legend = c(paste("Nodes :", vcount(net)), paste("Edges :", ecount(net))), 
       pt.bg = c("black"),
       pt.cex=2, 
       cex=.8, bty="n", 
       ncol=1
)

#EXTRACT DATA

#Show data for articles with a minimum number of citations
net.vs <- V(net)[degree(net) < cutoff] #identify those vertices part of less than cutoff value
net.cutoff <- delete.vertices(net, net.vs) #exclude them from object

#create dataframe
list_of_articles <- data.frame(PubmedID = V(net.cutoff)$name, Last_Author = V(net.cutoff)$Last_Author, Title = V(net.cutoff)$Title, Pubtype = V(net.cutoff)$Pubtype)
list_of_articles
nrow(list_of_articles)

#export
write.table(list_of_articles, file="significant-articles.csv", sep = ",", col.names = TRUE, row.names = FALSE)

#Show top 10 of last authors have published most
top_authors <- as.data.frame(table(nodes$Last_Author))
top_authors[order(top_authors$Freq, decreasing = TRUE)[1:10], ]



##### Timeline Documentos#####
library(ggplot2)
library(rentrez)
search_year <- function(year, term){
  query <- paste(term, "AND (", year, "[PDAT])")
  entrez_search(db="pubmed", term=query, retmax=0)$count
}

year <- 1990:2018
papers <- sapply(year, search_year, term="microbiome", USE.NAMES=FALSE)

plot(year, papers, type='b', main="The Rise of the Microbiome")

plotting <- ggplot(data,aes( year,papers)) + geom_point() + theme_()

data <- data.frame(year = year, papers = papers)

publicaciones <- entrez_search(db = "pubmed", term = "Breast")[['ids']]
publicaciones <-append(publicaciones, publicaciones)

data <- entrez_summary(db = 'pubmed' , id = publicaciones[1:360])

startnode_links <- entrez_link(dbfrom='pubmed', id=publicaciones, db='pubmed')
startnode_citations <- startnode_links$links$pubmed_pubmed_citedin
