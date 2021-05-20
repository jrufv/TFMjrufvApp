#' Análisis de expresión diferencial (UI)
#' 
#' @param id Identificador.
#' @export
#' @import shiny
#' @import htmltools

deaUI <- function(id) {
  
  sidebarLayout(
    sidebarPanel(width = 2,
      h6("ESTRUCTURA"),
      selectInput(NS(id, "cont"),
                  h6("Contrastes"),
                  choices = NULL,
                  multiple = TRUE
      ),
      selectInput(NS(id, "int"),
                  h6("Interacción"),
                  choices = NULL,
                  multiple = TRUE
      ),
      textInput(NS(id, "name"),
                h6("Etiquetas")
      ),
      h6("SELECCIÓN DE GENES"),
      selectInput(NS(id, "adjmethod"),
                  h6("Ajuste p-val"),
                  choices = list("Bonferroni" = "bonferroni",
                                 "Holm" = "holm",
                                 "Benjamini & Hochberg" = "BH",
                                 "Benjamini & Yekutieli" = "BY",
                                 "Ninguno" = "none")
      ),
      numericInput(NS(id, "pvalcoff"),
                   h6("Umbral p-val"),
                   value = 0.05,
                   min = 0,
                   max = 0.2,
                   step = 0.01
      ),
      numericInput(NS(id, "maxanal"),
                   h6("Filas máximas"),
                   value = NULL,
                   min = 0,
                   max = 100
      ),
      selectInput(NS(id, "dtmethod"),
                  h6("Método"),
                  choices = list("Independiente" = "separate",
                                 "Global" = "global",
                                 "Jerárquico" = "hierarchical",
                                 "Anidado" = "nestedF")
      ),
      h6("ANOTACIÓN"),
      checkboxInput(NS(id, "annot"),
                    "Anotar"
      ),
      selectInput(NS(id, "organism"),
                  h6("Organismo"),
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
          strong(h3("ANÁLISIS DE EXPRESIÓN DIFERENCIAL", align = "center")),
          br(),
          h5("En esta sección puede realizar el análisis de expresión diferencial y 
             la anotación de los resultados."),
          h5("Para realizar el ánalisis de expresión diferencial debe definir la
             'ESTRUCTURA' del análisis, definiendo los 'Contrastes' a realizar y la
             'Interacción' entre contrastes, si la hubiera. Además permite personalizar
             el nombre de los contrastes mediante el apartado 'Etiquetas'. Este apartado
             debe contener tantos nombres como contrastes, separados por coma y espacio.
             Más abajo puede ver la tabla de la estuctura seleccionada."),
          h5("A continuación se deben seleccionar los parámetros de 'SELECCIÓN DE GENES'.
             Estos son el 'Ajuste p-val', que incluye los métodos de 'Bonferroni' en el
             que los p-valores se multiplican por el número de comparaciones,",
             a("'Holm',",
               href = "https://www.jstor.org/stable/4615733"),
             a("'Benjamini & Hochberg',",
               href = "https://www.jstor.org/stable/2346101"),
             a("'Benjamini & Yakutieli',",
               href = "https://projecteuclid.org/journals/annals-of-statistics/volume-29/issue-4/The-control-of-the-false-discovery-rate-in-multiple-testing/10.1214/aos/1013699998.full"),
             "o ninguno; el 'Umbral p-val' de corte; las 'Filas máximas' a mostrar en
             los resultados; y el 'Método' de selección de genes."),
          h5("Finalmente puede realizar la 'ANOTACIÓN', unicamente debe marcar la casilla
             correspondiente y seleccionar el organismo de su estuio. No obstante este
             apartado sólo está disponible para datos de Microarray y RNA-Seq"),
          h5("Una vez haya seleccionado los parámetros correspondientes presione ACEPTAR
             para realizar el análisis de los datos."),
          h5("En las pestañas contiguas podrá ver diferentes tablas con los resultados
             del análisis. La pestaña de 'Results' permite visualizar los resultados
             de cada contraste seleccionado por separado."),
          br(),
          h5("ESTRUCTURA"),
          tableOutput(NS(id, "structure")),
          br(),
          h3(div(textOutput(NS(id, "success")), align = "center", style = "color:green"))
        ),
        tabPanel(
          "Results",
          icon = icon("table"),
          selectInput(NS(id, "num"),
                      h6("Contraste"),
                      choices = NULL,
                      width = "20%"
          ),
          DT::DTOutput(NS(id, "toptab")),
          downloadButton(NS(id, "downltoptab"),
                         class = "btn-sm btn-primary")
        ),
        tabPanel(
          "Expression",
          icon = icon("table"),
          DT::DTOutput(NS(id, "expression"))
        ),
        tabPanel(
          "Test Results",
          icon = icon("table"),
          DT::DTOutput(NS(id, "testres"))
        )
      )
    )
  )
}

#' Análisis de expresión diferencial (Server)
#'
#' @param id Identificador.
#' @param norm_data Datos normalizados en uno de los módulos anteriores.
#' @param data_type Tipo de datos, seleccionado en la aplicación inicial.
#' @param pack Paquete de anotaciones introducido en el módulo
#'   \code{norm_microarray}.
#' @export

deaServer <- function(id, norm_data, data_type, pack) {
  stopifnot(is.reactive(norm_data))
  stopifnot(is.reactive(data_type))
  stopifnot(is.reactive(pack))
  
  moduleServer(id, function(input, output, session) {
    
    groups <- reactive({
      req(norm_data())
      levels(as.factor(Biobase::pData(norm_data())[[1]]))
    })
    cont <- reactive({
      combina(groups())
    })
    observeEvent(cont(), {
      choices = cont()
      updateSelectInput(inputId = "cont",
                        choices = choices)
    })
    
    int <- eventReactive(input$cont, {
      combina_int(input$cont)
    })
    observeEvent(int(), {
      if(length(input$cont) < 2) {
        choices = NULL
      } else {
        choices = int()
      }
      updateSelectInput(inputId = "int",
                        choices = choices)
    })
    
    cont_int <- reactive({
      c(input$cont, input$int)
    })
    
    name <- reactive({
      list <- strsplit(input$name, split = ", ")
      list[[1]]
    })
    
    structure <- reactive({
      if(length(cont_int()) != length(name())) {
        data.frame(Etiqueta = cont_int(), Contraste = cont_int())
      } else {
        data.frame(Etiqueta = name(), Contraste = cont_int())
      }
    })
    
    output$structure <- renderTable({
      req(cont_int())
      structure()
    })
    
    observeEvent(norm_data(), {
      updateNumericInput(inputId = "maxanal",
                         value = round(nrow(Biobase::exprs(norm_data())) / 2, 0),
                         max = nrow(Biobase::exprs(norm_data())))
    })
    
    observeEvent(structure(), {
      choices = structure()$Etiqueta
      updateSelectInput(inputId = "num",
                        choices = choices)
    })
    
    observeEvent(input$annot, {
      if(input$annot == TRUE) {
        choices = base::unique(sysdata$speci$Organismo)
        updateSelectInput(inputId = "organism",
                          choices = choices)
      } 
    })
    
    dea_data <- eventReactive(input$submit, {
      id <- showNotification("Performing the analysis...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      if(input$name == "") {
        names = NULL
      } else {
        names = name()
      }
      
      TFMjrufv::dea(norm_data(),
                    cont = cont_int(),
                    name = names,
                    maxanal = input$maxanal,
                    adjmethod = input$adjmethod,
                    pvalcoff = input$pvalcoff,
                    dtmethod = input$dtmethod)
    })
   
    annot_dea_data <- reactive({
      req(input$submit, dea_data())
      
      id <- showNotification("Annotating results...",
                             duration = NULL,
                             closeButton = FALSE,
                             type = "message")
      on.exit(removeNotification(id), add = TRUE)
      
      if(input$annot == TRUE) {
        for(i in 1:nrow(sysdata$speci)) {
          if(sysdata$speci[i,1] == input$organism) {
            org_package <- sysdata$speci[i,3]
            org_package <- as.character(org_package)
          }
        }
        TFMjrufv::ins_pack(annot_pack = org_package)
        
        if(data_type() == "microarray") {
          TFMjrufv::annotated(dea_data(),
                              maPackage = pack(),
                              ID = "PROBEID")
        } else if(data_type() == "RNA-Seq"){
          package <- BiocGenerics::eval(parse(text = paste0(org_package,
                                                            "::", org_package)))
          TFMjrufv::annotated(dea_data(),
                              annotPackage = package,
                              ID = "ENSEMBL")
        }
      } else {
        dea_data()
      }
    })
    
    pos <- reactive({
      logvec <- structure()$Etiqueta == input$num
      pos <- which(logvec)
    })
    
    output$success <- renderText({
      req(annot_dea_data())
      "El análisis de expresión diferencial se ha completado!"
    })
    
    output$toptab <- DT::renderDT({
      req(input$submit)
      annot_dea_data()[[pos()]] %>%
        dplyr::mutate_if(is.numeric, round, digits = 5)
    })
    
    output$expression <- DT::renderDT({
      req(input$submit)
      methods::as(annot_dea_data()$MultComp, "matrix")
    })
    
    output$testres <- DT::renderDT({
      req(input$submit)
      annot_dea_data()$TestResMat
    })
    
    output$downltoptab <- downloadHandler(
      filename = function() {
        paste0("toptab_", input$num, ".csv")
      },
      content = function(file) {
        write.csv(annot_dea_data()[[pos()]], file)
      }
    )
    
    datatype <- reactive(data_type())
    
    list(
      data = annot_dea_data,
      struct = structure,
      org = reactive(input$organism),
      pvalcoff = reactive(input$pvalcoff),
      adjmethod = reactive(input$adjmethod)
    )
  })
}