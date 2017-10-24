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
      
      # Raw data selection ----
      
      
      helpText('Now upload here all the raw data files'),
      
      fileInput('raw_files', 'Upload Raw files', multiple = T),
      
      # Scanning sofware ---- 
      
      helpText('Select the scanning software used to generate raw files'),
      
      selectInput('source', 'Scanning program',
                  choices = list('Bluefuse' = 'bluefuse',
                                 'Others' = 'other')),
      # Reference channel ----
      
      helpText('Which channel is the reference? 
               This information is at the targets file'),
      
      selectInput('ref', 'Reference Channel', 
                  choices = list('Cy3' = 'Cy3',
                                 'Cy5' = 'Cy5')),
      
      # Statistical significance ----
      
      helpText('Select the Statistical significance threshold'),
      
      selectInput('fdr_pvalue', 'Select FDR
                  or p-value ',
                  choices = list('FDR (Recomended)' = 'fdr',
                                 'P Value (Strongly not recomended)' = 'pvalue')),
      
      sliderInput('alpha', 'Select the significance threshold',
                  min = 0.001, max = 0.5, value = 0.05, step = 0.01),
      
      sliderInput('fc', ' Select the |log2(FC)| threshold',
                  
                  min = 0, max = 5, value = 1, step = 0.25 )
    

    
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
               plotOutput('density_norm'),
               
               selectInput('pca_shape', 'PCA dimensions', 
                           choices = list('3D' = '3D',
                                          '2D' = '2D')),
               
               plotOutput('pca')
               ),
      
      tabPanel('Differential Expression Analysis',
               
               h3('Differential Expression Analysis'),
               p('First of all, select the comparisons that you want to do.
                 The comparissons can be only be done if the group is defined
                 in the Target file. You can also select if you want a single
                 comparison or a multiple comparison. If this is the case,
                 you must write the comparison this way: A-B,C-B. Remember,
                 DONT use spaces. You can see which groups you can
                 compare below:'),
               tableOutput('groups'),
               
               selectInput('mul_comp', 'Is a multiple comparison?', 
                           choices = list('Yes' = T,
                                          'No' = F)),
               
               textInput('contrasts', 'Comparison(s),
                         remember to not use spaces:'),
               
               p('Now you can see some results according to the FDR and 
                 |log2(FC)| selected. Type below the comparison you want to 
                 see (the 20 most significants genes). For example you can type
                 AC-Cntrl if you are running the demo data'),
               
               textInput('top_contrast', 'Comparison of interest:'),
               
               tableOutput('top_table'),
               
               p('Here you can dowload the tables in a single xlsx file. If
                 there are multiple comparisons, they will be located in
                 different sheets of the file.'),
               textInput('f_tittle', 'File name:'),
               
               downloadButton("downloadData", "Download the table in xlsx")
               
               
               )
  )
)
)
)

# Server logic -----

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
    rg()$targets$Cy5
    
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
  
  # PCA analysis
  
  # First we store the PCa information in a variable
  
  pca.filt <- reactive({
    
    prcomp(t(norm()$A), scale = TRUE )
    
    })
  
  output$pca <- renderPlot({
    
    # First we choose the colors of the plot based on the targets argument
    
    col_targets <- colnames(rg()$targets)
    
    
    
    no_select <- c(input$ref, 'FileName')
    
    
    
    sel_colunm <- col_targets[!(col_targets %in% no_select)]
    
   
    
    groups <- as.factor(rg()$targets[,sel_colunm])
    
    
    
    # we choose as many colors as categories are
    
    colors <- rainbow(length(levels(groups)))
    
    
    
    group_color <- rep(NA, length(groups))
    
    
    
    for(i in 1:length(levels(groups))){
      
      
      group_color[groups == levels(groups)[i]] <- colors[i] 

      
    }
    
    # We plot the PCA depending on the type of PCA plot that we want
    
    
    if(input$pca_shape == '3D'){
      
    pca3d<-scatterplot3d(x=pca.filt()$x[,1],y=pca.filt()$x[,2],z=pca.filt()$x[,3],
                         xlab='PC1', ylab='PC2', zlab='PC3',
                         main='PCA Analysis',
                         pch=16,col.grid="lightblue",
                         color = group_color)
    
    }else{
    
    plot(x=pca.filt()$x[,1],y=pca.filt()$x[,2],
         xlab='PC1', ylab='PC2',
         main='PCA Analysis',
         pch=16,
         col = group_color)
      }
    
    # And finally we add a legend in order to know which color correspond with
    # each  group.
    
    legend('topright', legend = levels(groups),
           pch = 16, col = colors)
    
    
  })
  
  # plot a table with the groups that the user can use to make the comparison.
  
  output$groups <- renderTable({
    
    col_targets <- colnames(rg()$targets)
    
    
    
    no_select <- c(input$ref, 'FileName')
    
    
    
    sel_colunm <- col_targets[!(col_targets %in% no_select)]
    
    groups <- factor(rg()$targets[,sel_colunm])
    
    df <- data.frame(levels(groups))
    
    colnames(df) <- 'Groups'
    
    df
  })
  
  # Definition of the contrast matrix regarding if we have
  # a multiple comparison or a single comparison.
  
  DE_fit <- reactive({
    
    design <- modelMatrix(rg()$targets, ref = 'Ref')
    
    fit <- lmFit(norm(), design)
    
    if(input$mul_comp){
      
      comparisons <- as.character(unlist(strsplit(input$contrasts,
                                                   split = ',')))
      
    
    }else{
      
      comparisons <- input$contrasts
      
      
    }
    
    contrast.matrix <- makeContrasts(contrasts = comparisons,
                                     levels = design)
    
    fit2 <- contrasts.fit(fit, contrast.matrix)
    
    fit2 <- eBayes(fit2)
    
    return(fit2)
    
  })
  
  output$top_table <- renderTable({
    
    top_table <- toptable(DE_fit(), number = Inf, coef = input$top_contrast)
    
    if(input$fdr_pvalue == 'fdr' ){
      
      interest <- top_table[ abs(top_table$logFC) > input$fc &
                               top_table$adj.P.Value < input$alpha,]
    } else {
      
      interest <- top_table[ abs(top_table$logFC) > input$fc &
                               top_table$P.Value < input$alpha,]
    }
    
    return(head(interest, n= 20))
    
  })
  
  output$downloadData <- downloadHandler(
    
    filename = reactive({
      
      paste(input$f_tittle, '.xlsx', sep = "")
      
      print(input$f_tittle)
      }),

    
    content = function(file){
      
      top_table <- toptable(DE_fit(), number = Inf, coef = input$top_contrast)
      
      interest <- top_table[ abs(top_table$logFC) > input$fc &
                                top_table$P.Value < input$alpha,]
      
      write.xlsx(interest, file, row.names = FALSE,
                 sheetName = input$top_contrast)
      
    }
    
    
    
    
    
    
    
    
    
    
  )
}

# Run the app
shinyApp(ui, server)
