#' Preprocesamiento y normalización de datos de espectros brutos de GC/LC-MS (UI)
#' 
#' @param id Identificador.
#' @export
#' @import shiny
#' @import htmltools

norm_MetabRSUI <- function(id) {
  
  sidebarLayout(
    sidebarPanel(width = 2,
      h6("PREPROCESAMIENTO"),
      h6("Detección de picos"),
      selectInput(NS(id, "fCPmethod"),
                  "Método",
                  choices = list("Cent Wave" = "centWave",
                                 "Cent Wave de dos pasos" = "centWaveWpi",
                                 "Matched Filter" = "matchedFilter",
                                 "Massifquant" = "massifquant",
                                 "Wavelet" = "MSW")
      ),
      h6("Filtrado"),
      br(),
      checkboxInput(NS(id, "refineRT"),
                    "Tiempo de retención"
      ),
      checkboxInput(NS(id, "refineIn"),
                    "Intensidad"
      ),
      checkboxInput(NS(id, "refineMN"),
                    "Fusión de picos"
      ),
      h6("Alineamiento de picos"),
      selectInput(NS(id, "aRtmethod"),
                  "Método",
                  choices = list("Grupos de picos" = "peakGroups",
                                 "Datos completos" = "obiwarp")
      ),
      h6("Correspondencia"),
      selectInput(NS(id, "gCPmethod"),
                  "Método",
                  choices = list("Densidades de picos" = "density",
                                 "Infusión directa" = "mzClust",
                                 "Proximidad" = "nearest")
      ),
      h6("Valores ausentes"),
      checkboxInput(NS(id, "fillCP"),
                    "Rellenar"
      ),
      selectInput(NS(id, "fillmethod"),
                  "Método",
                  choices = NULL
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
          strong(h3("PREPROCESAMIENTO Y NORMALIZACIÓN DE DATOS DE ESPECTROS BRUTOS DE
                    GC/LC-MS", align = "center")),
          br(),
          h5("En esta sección podrá preprocesar y normalizar los datos cargados."),
          h5("El PREPROCESAMIENTO tiene diferentes opciones:"),
          h5("1. 'Detección de picos': deberá seleccionar un 'Método' de entre los
             siguientes:"),
          h5("1.1. El algoritmo 'Cent Wave' realiza la detección de picos cromatográficos
             basada en la densidad de picos y ondículas para datos LC/MS de alta
             resolución en modo centroide"),
          h5("1.2. El algoritmo 'Cent Wave de dos pasos' realiza una detección de picos
             cromatográfica basada en Cent Wave en dos pasos: en una primera ejecución de
             Cent Wave, se identifican los picos para los que luego se predice la
             ubicación de sus isótopos potenciales en el tiempo de retención de mz.
             A continuación, se realiza una segunda ejecución de Cent Wave en estas
             regiones de interés (ROI). La lista final de picos cromatográficos comprende
             todos los picos que no se superponen de ambos ciclos de Cent Wave."),
          h5("1.3. El algoritmo 'Matched Filter' identifica picos en el dominio del tiempo
             cromatográfico como se describe en",
             a("[Smith 2006].",
               href = "https://pubmed.ncbi.nlm.nih.gov/16448051/")),
          h5("1.4. 'Massifquant' es una detección de picos cromatográfica basada en el
             filtro Kalman (KF) para datos XC-MS en modo centroide."),
          h5("1.5. El algoritmo 'Wavelet' realiza la detección de picos en el espectro de
             inyección directa de espectrometría de masas basado en ondículas."),
          h5("2. 'Filtrado: puede seleccionar la combinación de filtrados que desee."),
          h5("3. 'Alineamiento de picos': deberá seleccionar un 'Método' de entre los
             siguientes:"),
          h5("3.1. 'Grupos de picos': corrección del tiempo de retención basada en la
          alineación de características presentes en la mayoría/todas las muestras."),
          h5("3.2. 'Datos completos': alineación basada en los datos completos de mz-rt."),
          h5("4. 'Correspondencia': deberá seleccionar un 'Método' de entre los
             siguientes:"),
          h5("4.1. 'Densidad de picos': agrupación de picos basada en densidades de picos
             de dimensión temporal."),
          h5("4.2. 'Infusión directa': agrupación de picos de alta resolución para datos
             de MS de espectros únicos (infusión directa)."),
          h5("4.3. 'Proximidad': agrupación de picos cromatográficos en función de su
             proximidad en el espacio mz-rt."),
          h5("5. 'Valores ausentes': deberá seleccionarlo para realizar un rellenado
             de los valores ausentes. En caso de seleccionarlo deberá seleccionar un
             'Método' de entre los siguientes:"),
          h5("5.1. 'm/z': la señal se integra desde el intervalo de tiempo de retención
             m/z, es decir, desde 'rtmin', 'rtmax', 'mzmin' y 'mzmax'."),
          h5("5.2. 'Área': el área desde la cual se integra la señal para una
             característica se define en función de las áreas de picos cromatográficos
             de la característica. El rango m/z se define por defecto como el cuartil
             inferior del valor 'mzmin' de los picos cromatográficos al cuartil superior
             de los valores 'mzmax' de los picos cromatográficos. El intervalo de tiempo
             de retención para el área se define de forma análoga. A diferencia del
             enfoque anterior, este método utiliza los picos cromatográficos
             identificados reales de una característica para definir el área desde la
             cual se debe integrar la señal."),
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
          br(),
          h3(div(textOutput(NS(id, "success")), align = "center", style = "color:green"))
        ),
        tabPanel(
          "Data",
          icon = icon("table"),
          DT::DTOutput(NS(id, "Data"))
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

#' Preprocesamiento y normalización de datos de espectros brutos de GC/LC-MS (server)
#' 
#' @param id Identificador.
#' @param data Datos cargados en el módulo \code{read_data}.
#' @export

norm_MetabRSServer <- function(id, data) {
  stopifnot(is.reactive(data))
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$fillCP, {
      if(input$fillCP == TRUE) {
        choices = list("m/z" = "fill",
                       "Área" = "area")
        updateSelectInput(inputId = "fillmethod",
                          choices = choices)
      }
    })
    
    observeEvent(input$impute, {
      if(input$impute == TRUE) {
        updateNumericInput(inputId = "coff",
                           value = 20)
        choices = list("Cero" = "none",
                       "Media/2" = "half-min",
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
    
    prep_data <- eventReactive(input$submit, {
      id <- showNotification("Preprocessing data...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      TFMjrufv::prep_metRS(data(),
                           fCPmethod = input$fCPmethod,
                           refineRT = input$refineRT,
                           refineIn = input$refineIn,
                           refineMN = input$refineMN,
                           aRtmethod = input$aRtmethod,
                           gCPmethod = input$gCPmethod,
                           fillCP = input$fillCP,
                           fillmethod = input$fillmethod)
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