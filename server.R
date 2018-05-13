library(shiny)
library(shinydashboard)
library(shinycustomloader)
library(dplyr)
shinyServer(function(input, output) {
  

  
  output$map <- renderPlot({
    
    
    ggraph(df_adjacency,layout= input$layout_input)+
             geom_edge_link(colour="blue",alpha=0.5)+
             geom_node_point()+
             geom_node_text(aes(label='prueba'),size=2,repel=T) +
             theme_graph()
    
  })
  
  
  # output$clustering_plot <- renderPlot({
  # 
  # 
  #   df_adjacency2 <- df_adjacency %>% mutate(community = as.factor(group_infomap()))
  #   ggraph(df_adjacency2,layout= 'kk') +
  #     geom_edge_link(aes(alpha = ..index..), show.legend = FALSE) +
  #     geom_node_point(aes(colour = community), size = 7) +
  #     theme_graph()
  # })
  
  
  output$distribution_degree2 <- renderPlot({

    # quitar esto en el caso de que tarde mucho
    a <- degree_distribution(df_adjacency)[-1]
    distribution_edges <- data.frame(degrees = 1:length(a), frequency = a)

    ggplot(distribution_edges, aes(degrees, frequency)) +
      geom_point(fill = 'steelblue') +
      geom_path() +
      ggtitle('Degree Distribution with log10 transformations') +
      ylab('Frequency') +
      scale_x_log10() +
      scale_y_log10() +
      xlab('Number of degrees')


  })
  
  output$interactive_plot <- renderForceNetwork({
    
    data(MisLinks)
    data(MisNodes)
    edges2 = df_adjacency2
    edges2$value <- sample(c(seq(1,10,1)), nrow(edges2), T)
   
    colnames(edges2)[c(1,2)] <- c('source', 'target')
    nodes2 = data.frame(name = rep('Alejandro', 95), group = sample(c(1, 2),95, T),
                       size = sample(c(1,2,50), 95, T))
    
    forceNetwork(Links = edges2, Nodes = nodes2, Source = "source",
                 Target = "target", Value = "value", NodeID = 'name',
                 Group = 'group', 
                 #Nodesize = 'size',
                 radiusCalculation = "Math.sqrt(d.nodesize)+6",
                 zoom = TRUE,
                 bounded = TRUE,
                # colourScale = ValjeanCols,
                 legend = TRUE, arrows = TRUE)
    
    

    
  })
  
  output$interactive_plot1 <- renderSimpleNetwork({
    
    simpleNetwork(edges2, opacity = 1, zoom = TRUE)
    
    
  })
  
  output$distribution_degree <- renderPlot({
    
  
  a <- degree_distribution(df_adjacency)
  distribution_edges <- data.frame(degrees = 1:length(a), frequency = a)
  
  ggplot(distribution_edges, aes(degrees, frequency)) +
    geom_col(fill = 'steelblue') +
    ggtitle('Degree Distribution ') +
    ylab('Frequency') +
    xlab('Number of degrees')
  
  })
  
  
  output$transitivity <- renderValueBox({
    trans_value <- round(transitivity(df_adjacency, type = 'average'),2)
    valueBox(
      trans_value, "Transitivity", icon = icon("stats", lib = "glyphicon"),
      color = "yellow")
  })
  
  output$mean_distance <- renderValueBox({
    mean_distance_value <- round(mean_distance(df_adjacency),2)
    valueBox(
      mean_distance_value, "Mean distance", icon = icon("stats", lib = "glyphicon"),
      color = "green")
  })
  
  output$size <- renderValueBox({
    size <- round(gsize(df_adjacency),2)
    valueBox(
      size, "Size of graph", icon = icon("stats", lib = "glyphicon"),
      color = "green")
  })
  
  output$order <- renderValueBox({
    size <- round(gorder(df_adjacency),2)
    valueBox(
      size, "Order of graph", icon = icon("stats", lib = "glyphicon"),
      color = "green")
  })
  
  output$betweenness <- renderValueBox({
    betw <- round(betweenness(df_adjacency),4)
    valueBox(
      betw, "Betweenness", icon = icon("stats", lib = "glyphicon"),
      color = "green")
  })
  
  output$closeness <- renderValueBox({
    betw <- round(closeness(df_adjacency),4)
    valueBox(
      betw, "Closeness", icon = icon("stats", lib = "glyphicon"),
      color = "green")
  })
  
  output$density <- renderValueBox({
    dens <- round(edge_density(df_adjacency),2)
    valueBox(
      dens, "Density", icon = icon("stats", lib = "glyphicon"),
      color = "green")
  })
  
  output$degree_average <- renderValueBox({
    # DAMOS POR HECHO DE QUE EL GRAFO ES NO DIRIGIDO (DE MOMENTO)
    average_degree <- round(mean(degree(df_adjacency)),2)
    valueBox(
      average_degree, "Average degree", icon = icon("stats", lib = "glyphicon"),
      color = "green")
  })
  
  
  
  
  
  
})