#' Lectura de datos
#' 
#' @param id Identificador
#' @import shiny
#' @import htmltools

read_dataUI <- function(id) {
  
  sidebarLayout(
    sidebarPanel(width = 2,
      h6("DATOS BRUTOS"),
      fileInput(NS(id, "file"),
                multiple = TRUE,
                "",
                placeholder = "Empty"
      ),
      selectInput(NS(id, "sep_rd"),
                  h6("Separador"),
                  choices = NULL
      ),
      selectInput(NS(id, "file_type"),
                  h6("Extensión de archivo"),
                  choices = NULL
      ),
      h6("DATOS DE MUESTRAS"),
      fileInput(NS(id, "targets"),
                "",
                placeholder = "Empty"
      ),
      selectInput(NS(id, "sep_targ"),
                  h6("Separador"),
                  choices = list(",",
                                 ";",
                                 "Tab" = "\t")
      ),
      selectInput(NS(id, "mode"),
                  h6("MODE"),
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
          strong(h3("CARGA DE DATOS", align = "center")),
          br(),
          h5("Deberá cargar dos tipos de archivos:"),
          h5(textOutput(NS(id, "instrucciones"))),
          h5("Una vez haya cargado todos los datos y seleccionado los parámetros
            correspondientes presione ACEPTAR para realizar la lectura de datos."),
          h5("En las pestañas contiguas podrá ver las tablas con los datos, un resumen
            estadísitco de los mismos y diferentes gráficos"),
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
          ),
          tabPanel(
            "fData",
            icon = icon("table"),
            DT::DTOutput(NS(id, "fData"))
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
          ),
          tabPanel(
            "Summary fData",
            icon = icon("terminal"),
            verbatimTextOutput(NS(id, "summ_fData"))
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
          ),
          tabPanel(
            "Barplot",
            icon = icon("chart-bar"),
            plotOutput(NS(id, "barplot")),
            downloadButton(NS(id, "downlbarplot"),
                           class = "btn-sm btn-primary")
          ),
          tabPanel(
            "BPC",
            icon = icon("chart-bar"),
            plotOutput(NS(id, "bpc")),
            downloadButton(NS(id, "downlbpc"),
                           class = "btn-sm btn-primary")
          )
        )
      )
    )
  )
}

read_dataServer <- function(id, data_type) {
  stopifnot(is.reactive(data_type))
  
  moduleServer(id, function(input, output, session) {
    
    options(shiny.maxRequestSize = 20 * 1024 ^ 2)
    
    inst_microarray <- reactive({
      "Los 'DATOS BRUTOS' se refieren a los archivos .CEL que contienen los datos de cada
      muestra. En este caso no es necesario especificar un 'separador', y la 'Extensión
      de archivo' está predeterminada. Los 'DATOS DE MUESTRAS' se refieren a los datos
      identificativos de cada muestra almacenados en un archivo .csv con cabecera, que
      deberá tener tres columnas como mínimo: la primera con el nombre de los archivos
      .CEL, la segunda con un identificador único para cada muestra y la tercera con el
      grupo experimental de las muestras. Además se aceptan más columnas para covariables
      adicionales. Finalmente debe seleccionar el 'separador' de columna de su archivo
      .csv. Para este tipo de datos el parámetro 'MODE' no es necesario."
    })
    inst_RNASeq <- eventReactive(data_type(), {
      "Los 'DATOS BRUTOS' se refieren a un archivo .csv que contenga la 'counts table'.
      Este archivo debe tener cabecera, cada fila debe corresponder a un gen y cada
      columna a una muestra. Debe seleccionar el 'separador' de columna del archivo .csv.
      En este caso la 'Extensión de archivo' está predeterminada. Los 'DATOS DE MUESTRAS'
      se refieren a los datos identificativos de cada muestra almacenados en un archivo
      .csv con cabecera, que deberá tener dos columnas como mínimo, la primera con un
      identificador único para cada muestra y la segunda con el grupo experimental de las
      muestras. Además se aceptan más columnas para covariables adicionales. Finalmente
      debe seleccionar el 'separador' de columna de su archivo .csv. Para este tipo de
      datos el parámetro 'MODE' no es necesario."
    })
    inst_MetabRS <- eventReactive(data_type(), {
      "Los 'DATOS BRUTOS' se refieren a los archivos que contienen los datos de cada
      muestra. En este caso se aceptan diferentes 'Extensiones de archivos', como puede
      comprobar en el desplegable correspondiente, donde deberá especificar el que
      corresponda. En todos los casos cada archivo debe contener los datos de una muestra.
      En este caso no es necesario especificar un 'separador'. Los 'DATOS DE MUESTRAS' se
      refieren a los datos identificativos de cada muestra almacenados en un archivo .csv
      con cabecera, que deberá tener tres columnas como mínimo: la primera con el nombre
      de los archivos de muestras, la segunda con un identificador único para cada muestra
      y la tercera con el grupo experimental de las muestras. Además se aceptan más
      columnas para covariables adicionales. A continuación debe seleccionar el
      'separador' de columna de su archivo .csv. Finalmente deberá especificar en el
      desplegable 'MODE' si desea cargar los datos sin porocesar en la memoria (inMemory)
      o generar un objeto con los datos y sólo acceder a ellos cuando sea necesario
      (onDisk). Para una mayor fluidez del análisis se recomienda seleccionar 'onDisk'."
    })
    inst_MetabSB <- eventReactive(data_type(), {
      "Los 'DATOS BRUTOS' se refieren a un archivo .csv que contenga las intensidades de
      señal para cada contenedor de espectro. Este archivo debe tener cabecera, cada fila
      debe corresponder a un contenedor y cada columna a una muestra. Debe seleccionar el
      'separador' de columna del archivo .csv. En este caso la 'Extensión de archivo'
      está predeterminada. Los 'DATOS DE MUESTRAS' se refieren a los datos
      identificativos de cada muestra almacenados en un archivo .csv con cabecera, que
      deberá tener dos columnas como mínimo, la primera con un identificador único para 
      cada muestra y la segunda con el grupo experimental de las muestras. Además se
      aceptan más columnas para covariables adicionales. Finalmente debe seleccionar el
      'separador' de columna de su archivo .csv. Para este tipo de datos el parámetro 'MODE'
      no es necesario."
    })
    inst_MetabMC <- eventReactive(data_type(), {
      "Los 'DATOS BRUTOS' se refieren a un archivo .csv que contenga las concentraciones
      de metabolitos. Este archivo debe tener cabecera, cada fila debe corresponder a un
      metabolito y cada columna a una muestra. Debe seleccionar el 'separador' de columna
      del archivo .csv. En este caso la 'Extensión de archivo' está predeterminada. Los
      'DATOS DE MUESTRAS' se refieren a los datos identificativos de cada muestra
      almacenados en un archivo .csv con cabecera, que deberá tener dos columnas como
      mínimo, la primera con un identificador único para cada muestra y la segunda con el
      grupo experimental de las muestras. Además se aceptan más columnas para covariables
      adicionales. Finalmente debe seleccionar el 'separador' de columna de su archivo
      .csv. Para este tipo de datos el parámetro 'MODE' no es necesario."
    })
    
    output$instrucciones <- renderText({
      if(data_type() == "microarray") {
        inst_microarray()
      } else if(data_type() == "RNA-Seq") {
        inst_RNASeq()
      } else if(data_type() == "MetabRS") {
        inst_MetabRS()
      } else if(data_type() == "MetabSB") {
        inst_MetabSB()
      } else if(data_type() == "MetabMC") {
        inst_MetabMC()
      } 
    })
    
    datatype <- reactive({
      dplyr::filter(sysdata$confi, sysdata$confi$TYPE == data_type())
    })
    observeEvent(datatype(), {
      choices <- unique(datatype()$SEPrd)
      updateSelectInput(inputId = "sep_rd",
                        choices = choices)
    })
    
    seprd <- reactive({
      req(input$sep_rd)
      dplyr::filter(datatype(), datatype()$SEPrd == input$sep_rd)
    })
    observeEvent(seprd(), {
      choices <- unique(seprd()$EXT)
      updateSelectInput(inputId = "file_type",
                        choices = choices)
    })
    
    filetype <- reactive({
      req(input$file_type)
      dplyr::filter(seprd(), seprd()$EXT == input$file_type)
    })
    observeEvent(filetype(), {
      choices <- unique(filetype()$MODE)
      updateSelectInput(inputId = "mode",
                        choices = choices)
    })
    
    files_path <- reactive({
      substr(input$file$datapath[1], 1, 40)
    })
    
    readata <- reactive({
      id <- showNotification("Reading data...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      if(data_type() == "microarray") {
        TFMjrufv::read_data(data_type = data_type(),
                            path = files_path(),
                            gz_file = FALSE,
                            targets = input$targets$datapath,
                            sep_targ = input$sep_targ)
      } else if(data_type() == "MetabRS") {
        TFMjrufv::read_data(data_type = data_type(),
                            path = files_path(),
                            file_type = input$file_type,
                            targets = input$targets$datapath,
                            sep_targ = input$sep_targ,
                            mode = input$mode)
      } else {
        if(input$sep_rd == "Tab") {
          TFMjrufv::read_data(data_type = data_type(),
                              raw_data = input$file$datapath,
                              sep_rd = "\t",
                              targets = input$targets$datapath,
                              sep_targ = input$sep_targ)
        } else {
          TFMjrufv::read_data(data_type = data_type(),
                              raw_data = input$file$datapath,
                              sep_rd = input$sep_rd,
                              targets = input$targets$datapath,
                              sep_targ = input$sep_targ)
        }
      }
    })
    
    data <- eventReactive(input$submit, {
      req(input$file)
      ext <- tools::file_ext(input$file$name[1])
      switch(ext, readata())
    })
    
    mat_rs <- reactive({
      res <- matrix(MSnbase::tic(data()), ncol = nrow(Biobase::pData(data())))
      colnames(res) <- rownames(Biobase::pData(data()))
      rownames(res) <- rownames(res, do.NULL = FALSE, prefix = "S")
      res
    })
    
    output$success <- renderText({
      req(data())
      "La carga de datos se ha completado!"
    })
    
    output$Data <- DT::renderDT({
      req(input$file, input$targets, input$submit)
      if(data_type() == "RNA-Seq") {
        data()[[1]]
      } else if(data_type() == "MetabRS") {
        mat_rs()
      } else if(data_type() == "MetabSB") {
        SummarizedExperiment::assay(data())
      } else {
        Biobase::exprs(data())
      }
    })
    
    output$pData <- DT::renderDT({
      req(input$file, input$targets, input$submit)
      if(data_type() == "RNA-Seq") {
        data()[[2]]
      } else if(data_type() == "MetabSB") {
        as.data.frame(SummarizedExperiment::colData(data()))
      } else {
        Biobase::pData(data())
      }
    })
    
    output$fData <- DT::renderDT({
      req(input$file, input$targets, input$submit)
      if(data_type() == "RNA-Seq" | data_type() == "MetabSB") {
      } else {
        Biobase::fData(data())
      }
    })
    
    output$summ_Data <- renderPrint({
      req(input$file, input$targets, input$submit)
      if(data_type() == "RNA-Seq") {
        summary(data()[[1]])
      } else if(data_type() == "MetabRS") {
        summary(mat_rs())
      } else if(data_type() == "MetabSB") {
        summary(SummarizedExperiment::assay(data()))
      } else {
        summary(Biobase::exprs(data()))
      }
    })
    
    output$summ_pData <- renderPrint({
      req(input$file, input$targets, input$submit)
      if(data_type() == "RNA-Seq") {
        summary(data()[[2]])
      } else if(data_type() == "MetabSB") {
        summary(as.data.frame(SummarizedExperiment::colData(data())))
      } else {
        summary(Biobase::pData(data()))
      }
    })
    
    output$summ_fData <- renderPrint({
      req(input$file, input$targets, input$submit)
      if(data_type() == "microarray" | data_type() == "MetabRS" | data_type == "MetabMC") {
        summary(Biobase::fData(data()))
      } 
    })
    
    output$pca <- renderPlot({
      id <- showNotification("Drawing plot...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      TFMjrufv::plotTFM(data(), plot = "PCA")
    }, res = 96)
    
    output$boxplot <- renderPlot({
      id <- showNotification("Drawing plot...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      TFMjrufv::plotTFM(data(), plot = "boxplot")
    }, res = 96)
    
    output$density <- renderPlot({
      id <- showNotification("Drawing plot...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      TFMjrufv::plotTFM(data(), plot = "density")
    }, res = 96)
    
    output$heatmap <- renderPlot({
      id <- showNotification("Drawing plot...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      TFMjrufv::plotTFM(data(), plot = "heatmap")
    }, res = 96)
    
    output$heatmaps <- renderPlot({
      id <- showNotification("Drawing plot...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      TFMjrufv::plotTFM(data(), plot = "heatmapS")
    }, res = 96)
    
    output$barplot <- renderPlot({
      id <- showNotification("Drawing plot...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      if(data_type() == "RNA-Seq") {
        TFMjrufv::plotTFM(data(), plot = "barplot")
      }
    }, res = 96)
    
    output$bpc <- renderPlot({
      id <- showNotification("Drawing plot...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      if(data_type() == "MetabRS") {
        TFMjrufv::plotTFM(data(), plot = "BPC")
      }
    }, res = 96)
    
    output$downlpca <- downloadHandler(
      filename = function() {
        "pca.png"
      },
      content = function(file) {
        grDevices::png(file)
        print(TFMjrufv::plotTFM(data(), plot = "PCA"))
        grDevices::dev.off()
      }
    )
    
    output$downlboxplot <- downloadHandler(
      filename = function() {
        "boxplot.png"
      },
      content = function(file) {
        grDevices::png(file)
        print(TFMjrufv::plotTFM(data(), plot = "boxplot"))
        grDevices::dev.off()
      }
    )
    
    output$downldensity <- downloadHandler(
      filename = function() {
        "density.png"
      },
      content = function(file) {
        grDevices::png(file)
        print(TFMjrufv::plotTFM(data(), plot = "density"))
        grDevices::dev.off()
      }
    )
    
    output$downlheatmap <- downloadHandler(
      filename = function() {
        "heatmap.png"
      },
      content = function(file) {
        grDevices::png(file)
        print(TFMjrufv::plotTFM(data(), plot = "heatmap"))
        grDevices::dev.off()
      }
    )
    
    output$downlheatmaps <- downloadHandler(
      filename = function() {
        "heatmapS.png"
      },
      content = function(file) {
        grDevices::png(file)
        print(TFMjrufv::plotTFM(data(), plot = "heatmapS"))
        grDevices::dev.off()
      }
    )
    
    output$downlbarplot <- downloadHandler(
      filename = function() {
        "barplot.png"
      },
      content = function(file) {
        grDevices::png(file)
        print(TFMjrufv::plotTFM(data(), plot = "barplot"))
        grDevices::dev.off()
      }
    )
    
    output$downlbpc <- downloadHandler(
      filename = function() {
        "bpc.png"
      },
      content = function(file) {
        grDevices::png(file)
        print(TFMjrufv::plotTFM(data(), plot = "BPC"))
        grDevices::dev.off()
      }
    )
    
    data
  })
}