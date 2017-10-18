# Load packages ----
library(shiny)
library(limma)
library(scatterplot3d)
library(Vennerable)
library(xlsx)
library(gplots)

# User interface ----
ui <- fluidPage(
  titlePanel("Microarray Analisys"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a target file. Remember that the target file
               MUST have these columns: FileName Cy3 Cy5"),
      # Input: Select a file ----
      
      fileInput('target', 'Choose the target file'),
      
      # Raw data selection
      
      
      helpText('Now upload here all the raw data files'),
      
      fileInput('raw_files', 'Upload Raw files', multiple = T),
      
      # Scanning sofware
      
      helpText('Select the scanning software used to generate raw files'),
      
      selectInput('source', 'Scanning program',
                  choices = list('Bluefuse' = 'bluefuse',
                                 'Others' = 'other')),
      # Reference channel
      
      helpText('Which channel is the reference? 
               This information is at the targets file'),
      
      selectInput('ref', 'Reference Channel', 
                  choices = list('Cy3' = 'Cy3',
                                 'Cy5' = 'Cy5')),
      
      # Statistical significance
      
      helpText('Select the Statistical significance threshold'),
      
      selectInput('fdr_pvalue', 'Select FDR
                  or p-value ',
                  choices = list('FDR (Recomended)' = 'fdr',
                                 'P Value (Strongly not recomended)' = 'pvalue')),
      
      sliderInput('alpha', 'Select the significance threshold',
                  min = 0.0001, max = 0.99, value = 0.05, step = 0.001)
    
      
      
      
    
    ),
    
    mainPanel(h3('Raw Data exploration'),
              
              tableOutput("contents")
              
              
              
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # raw data exploration ----
  
  target <- reactive({})
  
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$target)
    
    df <- readTargets(input$target$datapath)

    
  })

  
  
}

# Run the app
shinyApp(ui, server)
