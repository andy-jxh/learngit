library(shiny)
library(Seurat)
options(shiny.maxRequestSize = 100*1024^3) # 设置为 100GB
# options(device.ask.default = FALSE)
# options(device.geometry = "width=600,height=600")

ui <- fluidPage(
  fileInput("file1", label = "选择seurat_obj文件"),        # plotOutput("DimPlot"),
  textInput("genelist", label = "输入要显示的基因列表（回车或英文空格分隔）"),
  plotOutput("vlnplot"),     # plotOutput("SpatialDimPlot")
  plotOutput("FeaturePlot"),
  plotOutput("DoHeatmap")
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
    output$vlnplot <- renderPlot({
      VlnPlot(seurat_obj(), features = genelist,pt.size = 0)
    })
  })
  

  observeEvent(input$genelist, {
    req(seurat_obj())
    genelist <- strsplit(input$genelist, " ")[[1]]
    output$FeaturePlot <- renderPlot({
      FeaturePlot(seurat_obj(), features = genelist,ncol = 5,pt.size = 0.1  )
    })
  })

  observeEvent(input$genelist, {
    req(seurat_obj())
    genelist <- strsplit(input$genelist, " ")[[1]]
    output$DoHeatmap <- renderPlot({
      DoHeatmap(seurat_obj(), features = genelist) + NoLegend()
    })
  })
}

shinyApp(ui, server)
