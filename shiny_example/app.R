library(shiny)
library(shinyjs)
library(tidyverse)
library(writexl)

# Define UI for application that draws a spider plot type of graph
ui <- fluidPage(

  useShinyjs(),
  
  # Application title
  titlePanel("My First Shiny Application"),
  tags$head(tags$style('h4 {color:cornflowerblue}')),
  
  # Options to Import the Data and Prepare the Plot 
  sidebarLayout(
    sidebarPanel(
      
      h4("Data Importation"),
      fileInput("datafile", "Upload your Sensory Profile Data (.xlsx)", accept=".xlsx"),
      
      br(),
      h4("Graphical Settings"),
      radioButtons("plottype", "Type of Plot to Draw:", choices=c("Spider Plot"="line", "Circular Barplot"="bar"), selected="line", inline=TRUE),
      uiOutput("attribute"),
      uiOutput("product")
      
    ),
    
    # Showing the Plot
    mainPanel(
      
      tabsetPanel(id="The Tabs",
                  tabPanel("Mean Table", 
                           tableOutput("meantable")),
                  tabPanel("Visual Rep.",
                           downloadButton("plotDL", "Download Plot"),
                           br(),
                           plotOutput("finalplot", height=600, width=600)))
    )
  )
)

# Define server logic required to draw the plot
server <- function(input, output) {
  
  # Reading and Storing the Data File 
  mydata <- reactive({
    
    req(input$datafile)
    data <- readxl::read_xlsx(input$datafile$datapath, sheet=1) %>% 
      pivot_longer(-c(Judge, Product), names_to="Attribute", values_to="Score") %>% 
      mutate(Attribute = fct_inorder(Attribute), Score = as.numeric(Score)) %>% 
      group_by(Product, Attribute) %>% 
      summarize(Score = mean(Score)) %>% 
      ungroup()
    
    return(data)
    
  })
  
  # Setting up the Options
  output$attribute <- renderUI({
    
    req(mydata())
    
    items <- mydata() %>% 
      pull(Attribute) %>% 
      as.character() %>% 
      unique()
    
    selectInput("attribute", "Select the Attributes (in order) ", choices=items, selected=items, multiple=TRUE)
    
  })
  output$product <- renderUI({
    
    req(mydata())
    items <- mydata() %>% 
      pull(Product) %>% 
      unique()
    
    checkboxGroupInput("product", "Select the Products to Display:", choices=items, selected=items)
    
  })
  
  # Format and Export the Mean Table
  mymean <- reactive({
    
    req(mydata(), input$attribute, input$product)
    mymean <- mydata() %>% 
      mutate(across(c("Product", "Attribute"), as.character)) %>%
      filter(Attribute %in% input$attribute) %>% 
      mutate(Product = factor(Product, input$product),
             Attribute = factor(Attribute, input$attribute),
             Score = format(round(Score, 2), nsmall=2)) %>% 
      pivot_wider(names_from=Product, values_from=Score)
    
    return(mymean)
  })
  output$meantable <- renderTable({
    
    req(mymean())
    mymean()
    
  })
  
  # Building the Plot
  myplot <- reactive({
    
    req(mydata(), input$attribute, input$product)
    
    data <- mydata() %>% 
      mutate(across(c("Product", "Attribute"), as.character)) %>% 
      filter(Attribute %in% input$attribute, Product %in% input$product)
    
    max_score <- max(data$Score)
    limit = c(0,10 * ceiling(max_score/10))
    
    if (input$plottype == "line"){
      
      desc <- tibble(Num = 0:length(input$attribute),
                     Attribute = c(input$attribute[length(input$attribute)], input$attribute))
      data_plot <- full_join(data, desc, by="Attribute") %>% 
        arrange(Product, Num)
      
      p <- ggplot(data_plot, aes(x=Num, y=Score, group=Product, colour=Product))+
        geom_line(lwd=1)+
        scale_x_continuous(name="", breaks=1:length(input$attribute), labels=input$attribute, limits=c(0,length(input$attribute)))+
        scale_y_continuous(name="", limits=limit)+
        theme_minimal()+
        theme(panel.grid.major=element_line(colour="grey90"), panel.grid.minor=element_blank(),
              axis.text.y = element_blank(), 
              legend.position="bottom", legend.title=element_blank())+
        coord_polar()+
        guides(linetype = "none", colour = guide_legend(nrow=1))+
        ggtitle("Spider Plot")
      
    } else {
      
      p <- ggplot(data, aes(x=Attribute, y=Score, group=Product, fill=Product))+
        geom_col(position="dodge")+
        theme_minimal()+
        theme(panel.grid.major=element_line(colour="grey90"), panel.grid.minor=element_blank(),
              axis.text.y = element_blank(), 
              legend.position="bottom", legend.title=element_blank())+
        xlab("")+
        scale_y_continuous(name="", limits=limit)+
        coord_polar()+
        guides(fill = guide_legend(nrow=1))+
        ggtitle("Circular Barplot")
      
    }
    
    return(p)
    
  })
  
  # Exporting the Plot
  output$finalplot <- renderPlot({
    
    req(myplot())
    myplot()
    
  })
  output$plotDL <- downloadHandler(
    filename="my plot.png",
    content = function(file){
      myplot()
      ggsave(file, device="png", width=8, height=8, unit="in", dpi=320)
    },
    contentType="image/png")
}

# Run the application 
shinyApp(ui = ui, server = server)
