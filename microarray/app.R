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
    
    mainPanel(tabsetPanel(
      
      tabPanel("Raw data",
      
      h3('Raw Data exploration'),
              
              plotOutput('raw_box'),
              verbatimTextOutput('rg'),
              tableOutput('probes'),
              plotOutput('MA_raw'),
              plotOutput('density_raw')
              
          ),
      tabPanel('Normalized data',
               h3('Normalized data'),
               plotOutput('MA_norm'),
               plotOutput('density_norm'))
  )
)
)
)

# Server logic

server <- function(input, output) {
  
  
  # Making posible to load other files than the demo.
  # The reactive function allows to use the demo files or the others
  
  rg <- reactive({
    
    # If there is no data input we will show only a few samples
    
    if(is.null(input$target)){
      
    return(rg_demo)
      
  } else {
    
    # but if the input is defined, we will use the input files
    
    targets <- readTargets(input$target$datapath)
    
    rg <- read.maimages(input$raw_files$datapath,
                        source = input$source )
    print('using data')
    
    rg$targets <- targets
    
    colnames(rg$G) <- rownames(targets)
    colnames(rg$R) <- rownames(targets)
    
    return(rg)
  }
    
  })
  
  # definition of normalized data ------
  
  norm <- reactive({
    
    # Background correction
    
    back <- backgroundCorrect(rg(), method="normexp", offset=50)  
    
    # within array normalization
    
    within <- normalizeWithinArrays(back, method="loess")
    
    # between arrays normalization
    
    norm <- normalizeBetweenArrays(within, method="Aquantile")
    
    return(norm)
    
    
  })
  
  
  # Definition of the outputs ----
  
  # Raw boxplot
  
  output$raw_box <- renderPlot({

    boxplot(cbind(log2(rg()$G), log2(rg()$R)), main = 'Raw data boxplot',
            ylab = 'log2(Intensity)', xaxt='n',
            col = c(rep('green', dim(rg()$G)[2]), rep('red', dim(rg()$G)[2])))
    })
  
  # Raw density plot
  
  output$density_raw <- renderPlot({
    
    plotDensities(rg(), main = 'Raw density plot')
  })
  
  # How raw files looks like. ELIMINATE WHEN ENDED
  output$rg <- renderPrint({
    input$raw_files
    
  })
  
  # ANOTHER OUTPUT TO MAKE PROBES
  
  output$probes <- renderTable({
    
      # ' Class Target: \n', class(input$target),
          # 'path:', input$target,
          # ' Class Files: \n', class(input$raw_files),
          # head(input$raw_files, n = nrow(input$raw_files))
    head(read.delim(input$target$datapath))
    
  })
  
  # raw MA plot
  
  output$MA_raw <- renderPlot({
    limma::plotMA(rg(), main = 'MAplot of raw data')
  })
  
  # Normalized MA plot
  
  output$MA_norm <- renderPlot({
    
    limma::plotMA(norm(), main = 'MAplot of normalized data')
    
  })
  
  # normalized density plot
  
  output$density_norm <- renderPlot({
    
    plotDensities(norm(), main = 'Normalized density plot')
    
  })
  
  
  
}

# Run the app
shinyApp(ui, server)
