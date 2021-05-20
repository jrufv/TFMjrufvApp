#' Normalización de datos de concentraciones de metabolitos (UI)
#' 
#' @param id Identificador.
#' @export
#' @import shiny
#' @import htmltools

norm_MetabMCUI <- function(id) {
  
  sidebarLayout(
    sidebarPanel(width = 2,
      h6("NORMALIZACIÓN"),
      h6("Valores perdidos"),
      checkboxInput(NS(id, "impute"),
                    "Imputar"
      ),
      numericInput(NS(id, "coff"),
                   "Valor de corte",
                   value = NULL,
                   min = 0,
                   max = 100
      ),
      selectInput(NS(id, "immethod"),
                  "Método",
                  choices = NULL
      ),
      h6("Normalización"),
      selectInput(NS(id, "nomethod"),
                  "Método",
                  choices = list("Ninguno " = "none",
                                 "Autoescalado"= "auto_scaling",
                                 "Escalado de nivel" = "level_scaling",
                                 "Escalado logarítmico" = "log_scaling",
                                 "Transformación logarítmica" = "log_transformation",
                                 "Gran escalado" = "vast_scaling",
                                 "Escalado de Pareto" = "log_pareto")
      ),
      h6("Outliers"),
      checkboxInput(NS(id, "routliers"),
                    "Eliminar"
      ),
      selectInput(NS(id, "oumethod"),
                  "Método",
                  choices = NULL
      ),
      selectInput(NS(id, "ditype"),
                  "Distancia",
                  choices = NULL
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
          p("Instrucciones de uso"),
          strong(h3("NORMALIZACIÓN DE DATOS DE CONCENTRACIONES DE METABOLITOS",
                    align = "center")),
          br(),
          h5("En esta sección podrá normalizar los datos cargados."),
          h5("La NORMALIZACIÓN presenta diferentes opciones:"),
          h5("1. 'Valores perdidos': puede 'Imputar' los valores perdidos seleccionando
             la casilla. Además deberá seleccionar un 'Valor de corte' que indica el
             porcentaje de valores perdidos permitido en cada grupo, y el 'Método' de
             imputación"),
          h5("2. 'Normalización': debe seleccionar un 'Método' de normalización."),
          h5("3. 'Outliers': puede 'Eliminar' los datos extremos seleccionando la
             casilla. Además deberá seleccionar un 'Método' de detección y el tipo
             de medida de la 'Distancia'."),
          h5("Una vez haya seleccionado los parámetros correspondientes presione ACEPTAR
             para realizar el procesamiento de los datos."),
          h5("En las pestañas contiguas podrá ver la tabla con los datos procesados, un
             resumen estadísitco de los mismos y diferentes gráficos"),
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

#' Normalización de datos de concentraciones de metabolitos (Server)
#' 
#' @param id Identificador.
#' @param data Datos cargados en el módulo \code{read_data}.
#' @export

norm_MetabMCServer <- function(id, data) {
  stopifnot(is.reactive(data))
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$impute, {
      if(input$impute == TRUE) {
        updateNumericInput(inputId = "coff",
                           value = 20)
        choices = list("Cero" = "none",
                       "Media/2" = "half_min",
                       "Mediana" = "median",
                       "Media" = "mean",
                       "Mínimo" = "min",
                       "KNN" = "knn",
                       "Random Forest" = "rf")
        updateSelectInput(inputId = "immethod",
                          choices = choices)
      }
    })
    
    observeEvent(input$routliers, {
      if(input$routliers == TRUE) {
        choices <- list("Mediana" = "median",
                        "Centroide" = "centroid")
        updateSelectInput(inputId = "oumethod",
                          choices = choices)
        choices2 <- list("Euclídea" = "euclidean",
                         "Máxima" = "maximum",
                         "Manhattan" = "manhattan",
                         "Canberra" = "canberra",
                         "Minkiowski" = "minkiowski")
        updateSelectInput(inputId = "ditype",
                          choices = choices2)
      }
    })
    
    norm_data <- eventReactive(input$submit, {
      id <- showNotification("Normalizing data...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      TFMjrufv::met_imp_norm(data(),
                             impute = input$impute,
                             coff = input$coff,
                             immethod = input$immethod,
                             nomethod = input$nomethod,
                             routliers = input$routliers,
                             oumethod = input$oumethod,
                             ditype = input$ditype)
    })
    
    
    output$success <- renderText({
      req(norm_data())
      "La normalización de los datos se ha completado!"
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
    
    norm_data
    
  })
}