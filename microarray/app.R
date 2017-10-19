# Load packages ----
library(shiny)
library(limma)
library(scatterplot3d)
library(Vennerable)
library(xlsx)
library(gplots)

# Demo data loading

targets_demo <- readTargets('data/Targets.txt')

rg_demo <- read.maimages(targets_demo, source = 'bluefuse',path = 'data/')

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
              
              plotOutput('raw_box'),
              verbatimTextOutput('rg'),
              tableOutput('probes')
              
              
              
    )
  )
)

# Server logic

server <- function(input, output) {
  
  
  # Making posible to load other files than the demo.
  # The reactive function allows to use the demo files or the others
  
  rg <- reactive({
    
    if(is.null(input$target)){
      print('using demo')
    return(rg_demo)
      
  } else {
    
    targets <- read.delim(input$target$name,
                          path = gsub('[0123456789].txt', '',
                                      input$target$datapath[1],
                                      perl = T))
    
    rg <- read.maimages(targets,
                        source = input$source,
                        path = gsub('[0123456789].xls', '',
                                    input$raw_files$datapath[1],
                                    perl = T) )
    print('using data')
    return(rg)
  }
    
  })
  
  output$raw_box <- renderPlot({

    boxplot(cbind(log2(rg()$G), log2(rg()$R)), main = 'Raw data boxplot',
            ylab = 'log2(Intensity)', xaxt='n',
            col = c(rep('green', dim(rg()$G)[2]), rep('red', dim(rg()$G)[2])))
    })
  output$rg <- renderPrint({
    input$target
    
  })
  
  output$probes <- renderTable({
    
      # ' Class Target: \n', class(input$target),
          # 'path:', input$target,
          # ' Class Files: \n', class(input$raw_files),
          # head(input$raw_files, n = nrow(input$raw_files))
    head(readTargets(input$target$name,
                     path = gsub('[0123456789].txt', '',
                                 input$target$datapath[1],
                                 perl = T)))
  })
  


  
  
}

# Run the app
shinyApp(ui, server)
