#' Framework para el análisis de datos ómicos
#'
#' Permite realizar un análisis completo de diferentes tipos de datos ómicos a
#' través de diferentes pestañas correspondientes a los pasos a dar para cargar
#' los datos, preprocesarlos y normalizarlos, realizar análisis estadísticos,
#' anotar los resultados y obtener análisis de significación biológica.
#' @export
#' @import shiny


TFMjrufvApp <- function() {

  ui <- fluidPage(
    
    theme = bslib::bs_theme(version = 4, bootswatch = "materia"),
    
    titlePanel(
      fluidRow(
        column(3,
               img(src = "logo.png", height = 51, width = 199)),
        column(9,
               (h1("FRAMEWORK PARA EL ANÁLISIS DE DATOS ÓMICOS")))
      )
    ),
    
    navbarPage(
      title = "",
      
      tabPanel(
        h4("HOME"),
        icon = icon("home"),
        sidebarLayout(
          sidebarPanel(width = 2,
            selectInput("data_type",
                        h6("TIPO DE DATOS"),
                        choices = base::unique(sysdata$confi$TYPE),
                        selected = "CHOOSE"
            ),
            actionButton("accept",
                         "Aceptar",
                         class = "btn-sm btn-block btn-success"
            )
          ),
          mainPanel(
            strong(h3("BIENVENIDO A LA APP", align = "center")),
            br(),
            h5("Esta aplicación le permitirá realizar un análisis de expresión
               diferenical a partir de diferentes tipos de datos ómicos."),
            h5("Puede escoger entre datos de Microarray, de RNA-Seq o diferentes tipos de
               datos metabolómicos, como son espectros brutos de GC/LC-MS (MetabRS),
               contenedores de espectros de MS (MetabSB), o concentraciones de
               metabolitos (MetabMC)."),
            h5("Para el ańalisis de Microarrays puede utilizar los datos almacenados en
               diferentes archivos .CEL que correspondan a cada una de las muestras."),
            h5("Para el análisis de espectros brutos puede utilizar los datos almacenados
               en diferentes archivos que correspondan a cada una de las muestras. Las
               extensiones de archivos aceptadas son .NetCDF, .mzML, .mzXML y .mzData."),
            h5("Para el resto de análisis puede utilizar los datos almacenados en un
               archivo .csv."),
            strong(h4("Para iniciar el análisis seleccione el TIPO DE DATOS a analizar y
                      presione el boton ACEPTAR", align = "center")),
            br(),
            h3(div(textOutput("error"), align = "center", style = "color:red")),
            h3(div(textOutput("success"), align = "center", style = "color:green")),
            br(),
            h5("Una vez haya confirmado la selección puede acceder a la siguiente pestaña
               para realizar la carga de datos."),
            h5("Deberá presionar el botón ACEPTAR en cada una de las pestañas para poder
               continuar avanzando por las diferentes pestañas.")
          )
        )
      ),
      
      tabPanel(
        h4("CARGA DE DATOS"),
        icon = icon("file-import"),
        read_dataUI("mod1")
      ),
      
      navbarMenu(
        h4("PREPROCESAMIENTO"),
        icon = icon("filter"),
        tabPanel(
          h5("Microarrays"),
          norm_microarrayUI("mod2a")
        ),
        tabPanel(
          h5("RNA-Seq"),
          norm_RNASeqUI("mod2b")
        ),
        tabPanel(
          h5("GC/LC-MS Raw Spectra"),
          norm_MetabRSUI("mod2c")
        ),
        tabPanel(
          h5("MS Spectra Bins"),
          norm_MetabSBUI("mod2d")
        ),
        tabPanel(
          h5("Metabolite Concentration"),
          norm_MetabMCUI("mod2e")
        )
      ),
      
      tabPanel(
        h4("EXPRESIÓN DIFERENICAL"),
        icon = icon("calculator"),
        deaUI("mod3")
      ),
      
      tabPanel(
        h4("SIGNIFICACIÓN BIOLÓGICA"),
        icon = icon("microscope"),
        bsaUI("mod4")
      )
    )
  )
  
  server <- function(input, output) {
    
    reactive({
      TFMjrufv::ins_pack()
    })
    
    error <- eventReactive(input$accept, {
      if(input$data_type == "CHOOSE") {
        "Debe seleccionar el tipo de datos"
      }
    })
    message <- eventReactive(input$accept, {
      if(input$data_type != "CHOOSE") {
        base::paste0("Ha seleccionado datos de: ", input$data_type)
      }
    })
    
    output$error <- renderText({
      error()
    })
    
    output$success <- renderText({
      message()      
    })
    
    read_dataServer("mod1", reactive(input$data_type))
    
    data <- read_dataServer("mod1", reactive(input$data_type))
    
    norm_microarrayServer("mod2a", data)
    norm_RNASeqServer("mod2b", data)
    norm_MetabRSServer("mod2c", data)
    norm_MetabSBServer("mod2d", data)
    norm_MetabMCServer("mod2e", data)
    
    norm_microarray <- norm_microarrayServer("mod2a", data)
    norm_data_RNASeq <- norm_RNASeqServer("mod2b", data)
    norm_data_MetabRS <- norm_MetabRSServer("mod2c", data)
    norm_data_MetabSB <- norm_MetabSBServer("mod2d", data)
    norm_data_MetabMC <- norm_MetabMCServer("mod2e", data)
    
    norm_data <- reactive({
      if(input$data_type == "microarray") {
        norm_microarray$data()
      } else if(input$data_type == "RNA-Seq") {
        norm_data_RNASeq()
      } else if(input$data_type == "MetabRS") {
        norm_data_MetabRS()
      } else if(input$data_type == "MetabSB") {
        norm_data_MetabSB()
      } else if(input$data_type == "MetabMC") {
        norm_data_MetabMC()
      }
    })
    
    deaServer("mod3", norm_data, reactive(input$data_type), norm_microarray$pack)
    
    dea <- deaServer("mod3", norm_data, reactive(input$data_type), norm_microarray$pack)
    
    bsaServer("mod4", reactive(input$data_type), dea$data, dea$org, dea$pvalcoff,
              dea$adjmethod, dea$struct)
  }

  shinyApp(ui, server) 
}