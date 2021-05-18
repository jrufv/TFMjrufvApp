#' Análisis de significación biológica (UI)
#' 
#' @param id Identificador.
#' @export
#' @import shiny
#' @import htmltools

bsaUI <- function(id) {
  
  sidebarLayout(
    sidebarPanel(width = 2,
                 selectInput(NS(id, "num"),
                             h6("Contraste"),
                             choices = NULL
                 ),
                 numericInput(NS(id, "category"),
                              h6("Vías a mostrar"),
                              value = 15,
                              min = 1,
                              max = 50
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
          strong(h3("ANÁLISIS DE SIGNIFICACIÓN BIOLÓGICA", align = "center")),
          br(),
          h5("En esta sección puede ver los resultados del análisis de significación
             biológica. En este caso no es necesario seleccionar ningún parámetro,
             únicamente seleccionar un contraste y presione el botón 'ACEPTAR' para
             llevar a cabo el análisis del contraste seleccionado."),
          h5("En las pestañas contiguas puede ver una tabla con el resultado del
             análisis de enriquecimiento, y un gráfico con una red de las vías
             enriquecidas. En este último caso tiene disponible el parámetro
             'Vías a mostrar' para modificar la salida gráfica."),
          h5("Esta opción sólo está disponible para datos de Microarrays y RNA-Seq."),
          br(),
          h3(div(textOutput(NS(id, "error")), align = "center", style = "color:red")),
          h3(div(textOutput(NS(id, "success")), align = "center", style = "color:green"))
        ),
        tabPanel(
          "Resultados",
          icon = icon("table"),
          DT::DTOutput(NS(id, "results"))
        ),
        tabPanel(
          "Gráfico",
          icon = icon("chart-bar"),
          plotOutput(NS(id, "cneplot")),
          downloadButton(NS(id, "downlcne"),
                         class = "btn-sm btn-primary")
        )
      )
    )
  )
}

#' Análisis de significación biológica (Server)
#' 
#' @param id Identificador.
#' @export

bsaServer <- function(id, data_type, data, org, pvcoff, padmethod, struct) {
  stopifnot(is.reactive(data_type))
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(org))
  stopifnot(is.reactive(pvcoff))
  stopifnot(is.reactive(padmethod))
  stopifnot(is.reactive(struct))
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(struct(), {
      choices = struct()$Etiqueta
      updateSelectInput(inputId = "num",
                        choices = choices)
    })
    
    bsa_data <- eventReactive(input$submit, {
      id <- showNotification("Performing the analysis...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      if(data_type() == "microarray" | data_type() == "RNA-Seq") {
        for(i in 1:nrow(sysdata$speci)) {
          if(sysdata$speci[i,1] == org()) {
            org_package <- sysdata$speci[i,3]
            org_package <- as.character(org_package)
            organism <- sysdata$speci[i,2]
            organism <- as.character(organism)
          }
        }
        root_package <- substr(org_package, start = 1, stop = (nchar(org_package) - 3))
        
        GOpackage <- BiocGenerics::eval(parse(text = paste0(org_package, "::",
                                                            root_package, "GO")))
        PATHpackage <- BiocGenerics::eval(parse(text = paste0(org_package, "::",
                                                              root_package, "PATH")))
        
        TFMjrufv::bsa(data(),
                      GOPackage = GOpackage,
                      PATHPackage = PATHpackage,
                      organism = organism,
                      pvcoff = pvcoff(),
                      padmethod = padmethod())
      } else {
        "Análisis no disponible para los datos seleccionados"
      }
    })
    
    pos <- reactive({
      logvec <- struct()$Etiqueta == input$num
      pos <- which(logvec)
    })
    
    table <- reactive({
      table <- as.data.frame(bsa_data()[[pos()]])
      table <- table[1:nrow(table), 1:9]
      rownames(table) <- seq(1:nrow(table))
      colnames(table) <- c("ID", "Description", "GeneRatio", "BgRatio", "pvalue",
                           "p.adjust", "q.value", "geneID", "Count")
      table$pvalue <- round(table$pvalue, 7)
      table$p.adjust <- round(table$p.adjust, 7)
      table$q.value <- round(table$q.value, 7)
      table
    })
    
    output$error <- renderText({
      if(data_type() == "MetabRS" | data_type() == "MetabSB" | data_type() == "MetabMC") {
        "El presnte análisis no esta disponible para los datos seleccionados"
      }
    })
    
    output$success <- renderText({
      req(input$submit, bsa_data)
        "El análisis de significación biológica se ha completado!"
    })
    
    output$results <- DT::renderDT({
      req(input$submit)
      table()
    })
    
    output$cneplot <- renderPlot({
      id <- showNotification("Drawing plot...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      enrichplot::cnetplot(bsa_data()[[pos()]], categorySize = "geneNum",
                           schowCategory = input$category, vertex.label.cex = 0.75)
    }, res = 96)
    
    output$downlcne <- downloadHandler(
      filename = function() {
        "cne_plot.png"
      },
      content = function(file) {
        grDevices::png(file)
        print(enrichplot::cnetplot(bsa_data()[[pos()]], categorySize = "geneNum",
                                   schowCategory = input$category,
                                   vertex.label.cex = 0.75))
        grDevices::dev.off()
      }
    )
      
      # Salida
  })
}