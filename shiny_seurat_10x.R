b <- c(
  'HORVU6Hr1G078330',
  'HORVU6Hr1G078420',
  'HORVU6Hr1G080790')
# 分析通用包
library(dplyr)
library(tidyr)
library(stringr)
library(patchwork)
library(ggplot2)
library(cowplot)
library(shiny)
library(Seurat)
options(shiny.maxRequestSize = 100*1024^3) # 设置为 100GB
# options(device.ask.default = FALSE)
# options(device.geometry = "width=600,height=600")

ui <- fluidPage(
  fileInput("file1", label = "选择seurat_obj文件"),        # plotOutput("DimPlot"),
  textInput("genelist", label = "输入要显示的基因列表（回车或英文空格分隔）"),
  plotOutput("SpatialFeaturePlot")
)




server <- function(input, output) {
  
  seurat_obj <- reactive({
    req(input$file1)
    readRDS(input$file1$datapath)
  })
  
  observeEvent(input$genelist, {
    req(seurat_obj())
    genelist <- strsplit(input$genelist, " ")[[1]]
    # print(input$genelist)
    # print(class(genelist))
    # print(genelist)
    # print(as.vector(genelist) )
    plots <- lapply(genelist, function(gene) {
      plot <- SpatialFeaturePlot(seurat_obj(), features = gene, ncol = 1, alpha = c(0.5, 1), max.cutoff = "q95", pt.size.factor = 2) +
        theme(legend.position = "none") +
        labs(title = gene) +
        theme(plot.title = element_text(hjust = 0.5))+
        theme(aspect.ratio = 1.2)
      return(plot)
    })
    
    output$SpatialFeaturePlot <- renderPlot({
      wrap_plots(plots, ncol  = 8)
    })
  })
  
  

}

shinyApp(ui, server)




