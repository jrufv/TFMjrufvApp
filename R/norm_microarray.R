#' Normalización de datos de microarray (UI)
#' 
#' @param id Identificador.
#' @export
#' @import shiny
#' @import htmltools

norm_microarrayUI <- function(id) {
  
  sidebarLayout(
    sidebarPanel(width = 2,
      h6("NORMALIZACIÓN"),
      h6("Filtrado"),
      br(),
      checkboxInput(NS(id, "varFilter"),
                    "Varianza"
      ),
      checkboxInput(NS(id, "featureFilter"),
                    "Características"
      ),
      textInput(NS(id, "annot_pack"),
                h6("Paquete de anotaciones:"),
                placeholder = "annotpack.db"
      ),
      actionButton(NS(id, "submit"),
                   "Aceptar",
                   class = "btn-sm btn-block btn-success"
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Info",
          icon = icon("info"),
          strong(h3("NORMALIZACIÓN DE DATOS DE MICROARRAY", align = "center")),
          br(),
          h5("En esta sección se normalizaran los datos cargados. Además tiene la opción
            de filtrar los datos según dos parámetros: según la varianza de los datos, o
            un filtrado basado en anotaciones y en la eliminación de duplicados. Ambos
            filtrados son compatibles."),
          h5("También puede añadir un paquete de anotaciones. Puede encontrar los paquetes
            de anotaciones en la",
             a("web de Bioconductor.",
               href = "https://www.bioconductor.org/packages/release/BiocViews.html#___AnnotationData")),
          h5("Una vez haya seleccionado los parámetros correspondientes presione ACEPTAR
             para realizar el procesamiento de los datos."),
          h5("En las pestañas contiguas podrá ver la tabla con los datos procesados, un
             resumen estadísitco de los mismos y diferentes gráficos"),
          br(),
          h3(div(textOutput(NS(id, "success")), align = "center", style = "color:green"))
        ),
        tabPanel(
          "Data",
          icon = icon("table"),
          DT::DTOutput(NS(id, "Data")),
          downloadButton(NS(id, "downlData"),
                         class = "btn-sm btn-primary")
        ),
        tabPanel(
          "Summary",
          icon = icon("terminal"),
          verbatimTextOutput(NS(id, "summ_Data"))
        ),
        navbarMenu(
          "Grafics",
          icon = icon("chart-bar"),
          tabPanel(
            "PCA",
            icon = icon("chart-bar"),
            plotOutput(NS(id, "pca")),
            downloadButton(NS(id, "downlpca"),
                           class = "btn-sm btn-primary")
          ),
          tabPanel(
            "Boxplot",
            icon = icon("chart-bar"),
            plotOutput(NS(id, "boxplot")), 
            downloadButton(NS(id, "downlboxplot"),
                           class = "btn-sm btn-primary")
          ),
          tabPanel(
            "Density",
            icon = icon("chart-bar"),
            plotOutput(NS(id, "density")),
            downloadButton(NS(id, "downldensity"),
                           class = "btn-sm btn-primary")
          ),
          tabPanel(
            "Heatmap",
            icon = icon("chart-bar"),
            plotOutput(NS(id, "heatmap")),
            downloadButton(NS(id, "downlheatmap"),
                           class = "btn-sm btn-primary")
          ),
          tabPanel(
            "Heatmap Samples",
            icon = icon("chart-bar"),
            plotOutput(NS(id, "heatmaps")),
            downloadButton(NS(id, "downlheatmaps"),
                           class = "btn-sm btn-primary")
          )
        )
      )
    )
  )
}

#' Normalización de datos de microarray (Server)
#' 
#' @param id Identificador.
#' @param data Datos cargados en el módulo \code{read_data}.
#' @export

norm_microarrayServer <- function(id, data) {
  stopifnot(is.reactive(data))
  
  moduleServer(id, function(input, output, session) {
    
    norm_data <- eventReactive(input$submit, {
      id <- showNotification("Procesing data...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      TFMjrufv::ins_pack(annot_pack = input$annot_pack)
      
      TFMjrufv::prep_norm_microarray(data(),
                                     varFilter = input$varFilter,
                                     featureFilter = input$featureFilter,
                                     annot_pack = input$annot_pack)
    })
    
    output$success <- renderText({
      req(norm_data())
      "El procesamiento de los datos se ha completado!"
    })
    
    output$Data <- DT::renderDT({
      req(input$submit)
      Biobase::exprs(norm_data())
    })
    
    output$summ_Data <- renderPrint({
      req(input$submit)
      summary(Biobase::exprs(norm_data()))
    })
    
    output$pca <- renderPlot({
      id <- showNotification("Drawing plot...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      TFMjrufv::plotTFM(norm_data(), plot = "PCA")
    }, res = 96)
    
    output$boxplot <- renderPlot({
      id <- showNotification("Drawing plot...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      TFMjrufv::plotTFM(norm_data(), plot = "boxplot")
    }, res = 96)
    
    output$density <- renderPlot({
      id <- showNotification("Drawing plot...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      TFMjrufv::plotTFM(norm_data(), plot = "density")
    }, res = 96)
    
    output$heatmap <- renderPlot({
      id <- showNotification("Drawing plot...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      TFMjrufv::plotTFM(norm_data(), plot = "heatmap")
    }, res = 96)
    
    output$heatmaps <- renderPlot({
      id <- showNotification("Drawing plot...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      TFMjrufv::plotTFM(norm_data(), plot = "heatmapS")
    }, res = 96)
    
    output$barplot <- renderPlot({
      id <- showNotification("Drawing plot...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      TFMjrufv::plotTFM(norm_data(), plot = "barplot")
    }, res = 96)
    
    output$bpc <- renderPlot({
      id <- showNotification("Drawing plot...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      TFMjrufv::plotTFM(norm_data(), plot = "BPC")
    }, res = 96)
    
    output$downlData <- downloadHandler(
      filename = function() {
        "norm_data.csv"
      },
      content = function(file) {
        write.csv(Biobase::exprs(norm_data()), file)
      }
    )
    
    output$downlpca <- downloadHandler(
      filename = function() {
        "norm_pca.png"
      },
      content = function(file) {
        grDevices::png(file)
        print(TFMjrufv::plotTFM(norm_data(), plot = "PCA"))
        grDevices::dev.off()
      }
    )
    
    output$downlboxplot <- downloadHandler(
      filename = function() {
        "norm_boxplot.png"
      },
      content = function(file) {
        grDevices::png(file)
        print(TFMjrufv::plotTFM(norm_data(), plot = "boxplot"))
        grDevices::dev.off()
      }
    )
    
    output$downldensity <- downloadHandler(
      filename = function() {
        "norm_density.png"
      },
      content = function(file) {
        grDevices::png(file)
        print(TFMjrufv::plotTFM(norm_data(), plot = "density"))
        grDevices::dev.off()
      }
    )
    
    output$downlheatmap <- downloadHandler(
      filename = function() {
        "norm_heatmap.png"
      },
      content = function(file) {
        grDevices::png(file)
        print(TFMjrufv::plotTFM(norm_data(), plot = "heatmap"))
        grDevices::dev.off()
      }
    )
    
    output$downlheatmaps <- downloadHandler(
      filename = function() {
        "norm_heatmapS.png"
      },
      content = function(file) {
        grDevices::png(file)
        print(TFMjrufv::plotTFM(norm_data(), plot = "heatmapS"))
        grDevices::dev.off()
      }
    )
    
    output$downlbarplot <- downloadHandler(
      filename = function() {
        "norm_barplot.png"
      },
      content = function(file) {
        grDevices::png(file)
        print(TFMjrufv::plotTFM(norm_data(), plot = "barplot"))
        grDevices::dev.off()
      }
    )
    
    output$downlbpc <- downloadHandler(
      filename = function() {
        "norm_bpc.png"
      },
      content = function(file) {
        grDevices::png(file)
        print(TFMjrufv::plotTFM(norm_data(), plot = "BPC"))
        grDevices::dev.off()
      }
    )
    
    list(
      data = norm_data,
      pack = reactive(input$annot_pack)
    )
  })
}