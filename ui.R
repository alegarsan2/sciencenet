library(shiny)
library(shinydashboard)

header <- dashboardHeader(
  title = "ScienceNet - A Webtool for network analysis of scientific literature",
  titleWidth = 650
 
  )


sidebar <- dashboardSidebar(
  
  width = 250,
  sidebarMenu(id = 'tab',
              menuItem("Data Exploration", tabName = "data", icon = icon("flask")),
              menuItem("Interactive network", tabName = "interactive", icon = icon("flask")),
              downloadButton('downloadData', 'Download data'),

  
  br(),
  br(),
  br(),

  # email sharing link
  menuItem("Feedback & suggestion", icon = icon("envelope-o"),
           href = "##")))
body <-  dashboardBody(
  tabItems(
  tabItem(tabName = "data",
  fluidRow(
    column(width = 2,
           # necesitamos un boton de accion para que empiece a buscar...
           box(title = "Enter your Article's PMID", width = 200, solidHeader = TRUE, status = "primary",
               textInput("id_input", label = '', value = "Enter ID...")),
           
           box(title = "Enter your Article's PMID", width = 200, solidHeader = TRUE, status = "primary",
               selectInput("layout_input", label = '', 
                           choices = list("Linear" = 'linear', "Star" = 'star', "Circle" = 'circle',
                                          'Grid' = 'grid', 'Graphopt' = 'graphopt'), 
                           selected = 1))),
    
    column(width = 5, solidHeader = TRUE,
           box(width = NULL, solidHeader = TRUE,
               tabsetPanel(type = "tabs", 
                           tabPanel("European map",  
                                   # plotOutput("map") %>% withSpinner(type = 'gif', loader = 'prueba'),
                                    withLoader(plotOutput("map"), type="image", loader="prueba.gif")
                           ))
           )),
    column(width = 5, solidHeader = TRUE,
           box(width = NULL, solidHeader = TRUE,
               tabsetPanel(type = "tabs", 
                           tabPanel("Interactive boxplot graphic",  
                                    plotOutput("clustering_plot") %>% withSpinner()))
           )),
    column(width = 5, solidHeader = TRUE,
           box(width = NULL, solidHeader = TRUE,
               tabsetPanel(type = "tabs", 
                           tabPanel("Distribution degree",  plotOutput("distribution_degree") %>% withSpinner()),
                           tabPanel("Distribution degree2",  plotOutput("distribution_degree2") %>% withSpinner())))

                                    # Dynamic valueBoxes
                                    
           ),
    
    column(width = 5, solidHeader = TRUE,
           box(width = NULL, solidHeader = TRUE,
               tabsetPanel(type = "tabs", 
                           tabPanel("Sample size", valueBoxOutput("transitivity"),
                           valueBoxOutput("mean_distance"),
                           valueBoxOutput("size"),
                           valueBoxOutput("order"),
                           valueBoxOutput("betweenness"),
                           valueBoxOutput("closeness"),
                           valueBoxOutput("density"),
                           valueBoxOutput('degree_average')
    
  )
))))),
  tabItem(tabName = "interactive",
          fluidRow(
          
            column(width = 12, solidHeader = TRUE,
                   box(width = NULL, solidHeader = TRUE,
                       tabsetPanel(type = "tabs", 
                                   tabPanel("Simple Network",  
                                            simpleNetworkOutput("interactive_plot1") %>% withSpinner()),
                                   
                                   tabPanel("Force Network",  
                                            forceNetworkOutput("interactive_plot") %>% withSpinner()))
                   ))
          ))))

dashboardPage(skin = 'green',
  header,
  sidebar,
  body
)

