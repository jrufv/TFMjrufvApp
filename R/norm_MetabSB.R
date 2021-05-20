#' Preprocesamiento y normalización de datos de contenedores de espectros de MS (UI)
#' 
#' @param id Identificador.
#' @export
#' @import shiny
#' @import htmltools

norm_MetabSBUI <- function(id) {
  
  sidebarLayout(
    sidebarPanel(width = 2,
                 h6("PREPROCESAMIENTO"),
                 h6("Filtrado"),
                 br(),
                 checkboxInput(NS(id, "filterMV"),
                               "Valores perdidos"
                 ),
                 numericInput(NS(id, "mpmv"),
                              "Umbral",
                              value = NULL,
                              min = 0,
                              max = 1,
                              step = 0.01
                 ),
                 br(),
                 checkboxInput(NS(id, "filterF"),
                               "Valores no perdidos"
                 ),
                 numericInput(NS(id, "mf"),
                              "Umbral",
                              value = NULL,
                              min = 0,
                              max = 1,
                              step = 0.01
                 ),
                 selectInput(NS(id, "fFmethod"),
                             "Método",
                             choices = NULL
                 ),
                 selectInput(NS(id, "QClab"),
                             "Etiqueta CC",
                             choices = NULL,
                             multiple = TRUE
                 ),
                 br(),
                 checkboxInput(NS(id, "filterRSD"),
                               "Control de calidad"
                 ),
                 selectInput(NS(id, "QClab2"),
                             "Etiquetas CC",
                             choices = NULL,
                             multiple = TRUE
                 ),
                 numericInput(NS(id, "mrsd"),
                              "Umbral",
                              value = NULL,
                              min = 0,
                              max = 1,
                              step = 0.01
                 ),
                 br(),
                 checkboxInput(NS(id, "filterB"),
                               "Blanco"
                 ),
                 numericInput(NS(id, "mfc"),
                              "Fold change",
                              value = NULL,
                              min = 0,
                              max = 3,
                              step = 0.1
                 ),
                 selectInput(NS(id, "Blab"),
                             "Etiquetas Blanco",
                             choices = NULL,
                             multiple = TRUE
                 ),
                 selectInput(NS(id, "remB"),
                             "Blancos",
                             choices = NULL,
                 ),
                 numericInput(NS(id, "fib"),
                              "Fracción",
                              value = NULL,
                              min = 0,
                              max = 1,
                              step = 0.01
                 ),
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
          strong(h3("PREPROCESAMIENTO Y NORMALIZACIÓN DE DATOS DE CONTENEDORES DE
                    ESPECTROS", align = "center")),
          br(),
          h5("En esta sección podrá preprocesar y normalizar los datos cargados."),
          h5("El PREPROCESAMIENTO tiene diferentes opciones de filtrado:"),
          h5("1. 'Valores perdidos': si selecciona la casilla se filtrarán muestras
             en función del 'Umbral' de porcentaje de valores perdidos."),
          h5("2. 'Valores no perdidos': si selecciona la casilla se filtrarán
             caracteristicas en funcion del 'Umbral' de porcentaje de muestras que
             contengan valores no perdidos. También deberá seleccionar un método de
             filtraje de entre los siguientes:"),
          h5("2.1. 'Control de calidad': entre las muestras de control de calidad. En
             este caso deberá especificar las 'Etiquetas CC'."),
          h5("2.2. 'Por clase': entre las muestras de la misma clase."),
          h5("2.3. 'Todo': en todas las muestras."),
          h5("3. 'Control de calidad': si selecciona la casilla se filtrarán 
             características en función del 'Umbral' de la desviación estándar relativa
             de las muestras de CC."),
          h5("4. 'Blanco': si selecciona la casilla se filtrarán características de
             orignen no biológico mediatne muestras en blanco. En este caso debera
             seleccionar un 'Fold change' mínimo entre muestras analíticas y en blanco,
             las muestras que hacen de blanco ('Etiquetas blanco'), si se deben eliminar
             las muestras blanco ('Blancos'), y la 'Fracción' de picos en blanco en las
             que deben estar presentes."),
          h5("La NORMALIZACIÓN también presenta diferentes opciones:"),
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

#' Preprocesamiento y normalización de datos de contenedores de espectros de MS (Server)
#' 
#' @param id Identificador.
#' @param data Datos cargados en el módulo \code{read_data}.
#' @export

norm_MetabSBServer <- function(id, data) {
  stopifnot(is.reactive(data))
  
  moduleServer(id, function(input, output, session) {
    
    choices <- reactive({
      colnames(data())
    })
    
    observeEvent(input$filterMV, {
      if(input$filterMV == TRUE) {
        updateNumericInput(inputId = "mpmv",
                           value = 0.1)
      } 
    })
    
    observeEvent(input$filterF, {
      if(input$filterF == TRUE) {
        updateNumericInput(inputId = "mf",
                           value = 0.9)
        choices = list("Control de calidad" = "QC",
                       "Por clase" = "within",
                       "Todo" = "across")
        updateSelectInput(inputId = "fFmethod",
                          choices = choices,
                          selected = "across")
      }
    })
    
    observeEvent(input$fFmethod, {
      if(input$fFmethod == "QC") {
        choices = choices()
        updateSelectInput(inputId = "QClab",
                          choices = choices)
      }
    })
    
    observeEvent(input$filterRSD, {
      if(input$filterRSD == TRUE) {
        if(is.null(input$QClab)) {
          choices = choices()
          updateSelectInput(inputId = "QClab2",
                            choices = choices)
        } else {
          choices = input$QClab
          updateSelectInput(inputId = "QClab2",
                            choices = choices,
                            selected = choices)
        }
        updateNumericInput(inputId = "mrsd",
                           value = 0.1)
      }
    })
    
    observeEvent(input$filterB, {
      if(input$filterB == TRUE) {
        updateNumericInput(inputId = "mfc",
                           value = 1)
        choices = choices()
        updateSelectInput(inputId = "Blab",
                          choices = choices)
        choices2 = list("Eliminar" = TRUE,
                        "Mantener" = FALSE)
        updateSelectInput(inputId = "remB",
                          choices = choices2)
        updateNumericInput(inputId = "fib",
                           value = 0.1)
      }
    })
    
    observeEvent(input$impute, {
      if(input$impute == TRUE) {
        updateNumericInput(inputId = "coff",
                           value = 20)
        choices = list("Cero" = "none",
                       "Media/2" = "half_min",
                       "Mediana" = "median",
                       "Media" = "mean",
                       "Mínimo" = "min",
                       "KNN" = "knn")
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
    
    prep_data <- eventReactive(input$submit, {
      id <- showNotification("Preprocessing data...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      if(!is.null(input$QClab)) {
        QClab <- input$QClab
      } else {
        QClab <- input$QClab2
      }
      
      TFMjrufv::prep_metSB(data(),
                           filterMV = input$filterMV,
                           filterF = input$filterF,
                           filterRSD = input$filterRSD,
                           filterB = input$filterB,
                           mpmv = input$mpmv,
                           mf = input$mf,
                           fFmethod = input$fFmethod,
                           QClab = QClab,
                           mrsd = input$mrsd,
                           mfc = input$mfc,
                           Blab = input$Blab,
                           remB = input$remB,
                           fib = input$fib)
    })
    
    norm_data <- reactive({
      req(prep_data())
      id <- showNotification("Normalizing data...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      TFMjrufv::met_imp_norm(prep_data(),
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
      "El procesamiento y la normalización de los datos se ha completado!"
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