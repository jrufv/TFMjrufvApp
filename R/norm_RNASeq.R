#' Normalización de datos de RNA-Seq (UI)
#' 
#' @param id Identificador.
#' @export
#' @import shiny
#' @import htmltools

norm_RNASeqUI <- function(id) {
  
  sidebarLayout(
    sidebarPanel(width = 2,
      h6("NORMALIZACIÓN"),
      h6("Filtrado"),
      br(),
      checkboxInput(NS(id, "usFilter"),
                    "Inespecífico"
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
          strong(h3("NORMALIZACIÓN DE DATOS DE RNA-SEQ", align = "center")),
          br(),
          h5("En esta sección se normalizaran los datos cargados. Además tiene la opción
            de filtrar los datos para eliminar filas en las que la mitad de las muestras
            tengan un recuento por millón menor a 1."),
          h5("Una vez haya seleccionado los parámetros correspondientes presione ACEPTAR
             para realizar el procesamiento de los datos."),
          h5("En las pestañas contiguas podrá ver la tabla con los datos procesados, un
             resumen estadísitco de los mismos y diferentes gráficos"),
          br(),
          h3(div(textOutput(NS(id, "success")), align = "center", style = "color:green"))
        ),
        navbarMenu(
          "Data",
          icon = icon("table"),
          tabPanel(
            "Data",
            icon = icon("table"),
            DT::DTOutput(NS(id, "Data"))
          ),
          tabPanel(
            "pData",
            icon = icon("table"),
            DT::DTOutput(NS(id, "pData"))
          )
        ),
        navbarMenu(
          "Summary",
          icon = icon("terminal"),
          tabPanel(
            "Summary Data",
            icon = icon("terminal"),
            verbatimTextOutput(NS(id, "summ_Data"))
          ),
          tabPanel(
            "Summary pData",
            icon = icon("terminal"),
            verbatimTextOutput(NS(id, "summ_pData"))
          )
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

#' Normalización de datos de RNA-Seq (Server)
#' 
#' @param id Identificador.
#' @param data Datos cargados en el módulo \code{read_data}.
#' @export

norm_RNASeqServer <- function(id, data) {
  stopifnot(is.reactive(data))
  
  moduleServer(id, function(input, output, session) {
    
    norm_data <- eventReactive(input$submit, {
      id <- showNotification("Procesing data...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      TFMjrufv::prep_norm_rna(data(),
                              usFilter = input$usFilter)
    })
    
    
    output$success <- renderText({
      req(norm_data())
      "El procesamiento de los datos se ha completado!"
    })
    
    output$Data <- DT::renderDT({
      req(input$submit)
      Biobase::exprs(norm_data())
    })
    
    output$pData <- DT::renderDT({
      req(input$submit)
      Biobase::pData(norm_data())
    })
    
    output$summ_Data <- renderPrint({
      req(input$submit)
      summary(Biobase::exprs(norm_data()))
    })
    
    output$summ_pData <- renderPrint({
      req(input$submit)
      summary(Biobase::pData(norm_data()))
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
    
    norm_data
  })
}