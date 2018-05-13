

# a <- entrez_search(db="pubmed",
#               term="breast cancer",
#               retmax=1000,)
# 
# 
# nucleotide_data <- entrez_link(id = 24129001, dbfrom ="pubmed")
# 
# 
# search_year <- function(year, term){
#   query <- paste(term, "AND (", year, "[PDAT])")
#   entrez_search(db="pubmed", term=query, retmax=0)$count
# }
# 
# year <- 2008:2016
# papers <- sapply(year, search_year, term="microbiome", USE.NAMES=FALSE)
# 
# plot(year, papers, type='b', main="The Rise of the Connectome")
# 
# entrez_search(db = 'pubmed',term = 'breast cancer', retmode = 100)
# startnode <- c(21876761, 29234320)
# 
# r_search <- entrez_search(db="pubmed", term="breast cancer")
# 
# a <- as.vector(r_search[["ids"]])
# a <- node_citations
# b <- entrez_summary(db="pubmed", id=a)
# d <- entrez_link(dbfrom='pubmed', id=a, db='pubmed', by_id = TRUE)
# 
# prueba1 <- c(d[["links"]][["pubmed_pubmed_citedin"]])
  
  ### 


list_adjacency <- edges
list_adjacency <- list_adjacency[1:500,]
df_adjacency <- as_tbl_graph(list_adjacency)
#df_adjacency <- play_islands(5, 10, 0.8, 3)
ggraph(df_adjacency,layout="linear")+
  geom_edge_link(colour="blue",alpha=0.5)+
  geom_node_point()+
  geom_node_text(aes(label=name),size=2,repel=T) +
  theme_graph()


transitivity(df_adjacency, type = 'average')

## comprobar para dinamico
df_adjacency <- list_adjacency
df_adjacency$From <- as.integer(df_adjacency$From)
df_adjacency$To <- as.integer(df_adjacency$To)

c1 <- c(df_adjacency[,1])
c2 <- c(df_adjacency[,2])
c3 <- c(c1, c2)
c_unique <- unique(c3)
leftjoiner <- data.frame(From = c_unique, id = seq(0,length(c_unique)-1,1))

df_adjacency2 <- left_join(df_adjacency, leftjoiner , by = 'From')
df_adjacency2 <- left_join(df_adjacency2, leftjoiner, by = c('To' = 'From' ))
df_adjacency2 <- df_adjacency2[,-c(1,2)]
df_adjacency <- as_tbl_graph(df_adjacency2)

# d3 = forceNetwork(Links = relations, Nodes = vertices,
#                   Source = "source.index", Target = "target.index",
#                   NodeID = "name",
#                   Group = "group", # color nodes by betweeness calculated earlier
#                   charge = -70, # node repulsion
#                   linkDistance = 25,
#                   zoom = T)


# Plot
simpleNetwork(df_adjacency2)

