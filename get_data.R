
#Sciencenet
#Info retrieval

#Load libraries
if (!require('XML')) install.packages('XML'); library('XML')
if (!require('rentrez')) install.packages('rentrez'); library('rentrez')
if (!require('igraph')) install.packages('igraph'); library('igraph')

startnode <- c("25630211")
num_of_levels <- 3


#Find relations within the article
#Poner is.null?? creo que no hace falta. en todos los levels si hay hacemos un next para continuar con el siguiente. 

startnode_links <- entrez_link(dbfrom='pubmed', id=startnode, db='pubmed')
startnode_citations <- startnode_links$links$pubmed_pubmed_citedin

if (length(startnode_citations) != 0){
  edges <- data.frame(From = startnode_citations, To = rep(startnode, times = length(startnode_citations)))
  print("hola")
  for(i in (1:num_of_levels)){
    print("Level", i)
    nodes_obj <- entrez_link(dbfrom='pubmed', id=startnode_citations, db='pubmed', by_id = TRUE)
    
    edges_temp <- data.frame(From = c(), To = c())
    for (j in (1:length(nodes_obj))){
      print(j)
      nodes_cit <- nodes_obj[[j]][["links"]][["pubmed_pubmed_citedin"]]
      edges_temp <- rbind(edges_temp, data.frame(From = nodes_cit, To = rep(startnode_citations[j],times=length(nodes_cit))))}
      
    edges <- rbind(edges,edges_temp)
    startnode_citations <- edges_temp$From
    print(startnode_citations)
                
      # names(matrix_temp) <- c("From", "To")
      # edges <- rbind(edges, matrix_temp)
    }
}



#####Get node data####

#List preparation

node_temp1 <- as.vector(unique(edges[,1]))
node_temp2 <- as.vector(unique(edges[,2]))

nodelist <- unique(c(node_temp1, node_temp2))


# nodelisttemp <- nodelist[1:-5]
# nodelisttemp <- na.exclude(nodelisttemp)


#Data splicing due to entrez 300 keys limitations
i <- 1
if (length(nodelist) > 300){
  #if the split doesnt finishes lenght add the last index.
  secu <- seq(1,length(nodelist),300)
  if (tail(secu,1) != length(nodelist)) {secu = c(secu,length(nodelist))}
  print(secu)

    for (i in 1:length(secu)){
      node_data <- entrez_summary(db="pubmed", id=nodelist[secu[i]:secu[i+1])
      
      nodes <- rbind(nodes, data.frame(PubmedID = node_data[[i]]$uid, 
                                       Title = node_data[[i]]$title, 
                                       Last_Author = node_data[[i]]$lastauthor, 
                                       Year =  substr(node_data[[i]]$pubdate, 0, 4),
                                       Pubtype = node_data[[i]]$pubtype,
                                       Citations = node_data[[i]]$pmcrefcount,
                                       Level = l)) 
      }
  }
  
  else{
  node_data <- entrez_summary(db="pubmed", id=nodelist)
  }
  
  nodes <-data.frame()
  node <- c()
  for (node in node_data){
    nodes <- rbind(nodes, data.frame(PubmedID = node$uid, 
                                     Title = node$title, 
                                     Last_Author = node$lastauthor, 
                                     Year =  substr(node$pubdate, 0, 4),
                                     #Pubtype = node$pubtype,
                                     Citations = as.integer(node$pmcrefcount),
                                     Level = num_of_levels+1, 
                                     stringsAsFactors = FALSE))
  }
  

