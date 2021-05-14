#' Framework para el análisis de datos ómicos
#'
#' Permite realizar un análisis completo de diferentes tipos de datos ómicos a
#' través de diferentes pestañas correspondientes a los pasos a dar para cargar
#' los datos, preprocesarlos y normalizarlos, realizar análisis estadísticos,
#' anotar los resultados y obtener análisis de significación biológica.
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
                                   choices = base::unique(confi$TYPE),
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
            p("Instrucciones..."),
            br(),
            textOutput("instrucciones"),
            br(),
            h3(div(textOutput("error"), align = "center", style = "color:red")),
            h3(div(textOutput("success"), align = "center", style = "color:green")),
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
        h4("ANOTACIÓN"),
        icon = icon("pencil-alt")
      ),
      
      navbarMenu(
        h4("ANÁLISIS ESTADÍSTICO"),
        icon = icon("calculator"),
        tabPanel(
          h5("Análisis de Expresión Diferencial")
        ),
        tabPanel(
          h5("Clasificación")
        ),
        tabPanel(
          h5("Clustering")
        ),
        tabPanel(
          h5("Selección de características")
        )
      ),
      tabPanel("Prueba", verbatimTextOutput("prueba"))
    )
  )
  
  server <- function(input, output) {
    
    reactive({
      TFMjrufv::ins_pack()
    })
    
    inst_microarray <- eventReactive(input$data_type, {
      "inst_microarray"
    })
    inst_RNASeq <- eventReactive(input$data_type, {
      "inst_RNASeq"
    })
    inst_MetabRS <- eventReactive(input$data_type, {
      "inst_MetabRS"
    })
    inst_MetabSB <- eventReactive(input$data_type, {
      "inst_MetabSB"
    })
    inst_MetabMC <- eventReactive(input$data_type, {
      "inst_MetabMC"
    })
    
    output$instrucciones <- renderText({
      if(input$data_type == "microarray") {
        inst_microarray()
      } else if(input$data_type == "RNA-Seq") {
        inst_RNASeq()
      } else if(input$data_type == "MetabRS") {
        inst_MetabRS()
      } else if(input$data_type == "MetabSB") {
        inst_MetabSB()
      } else if(input$data_type == "MetabMC") {
        inst_MetabMC()
      } else if(input$data_type == "CHOOSE") {
        "Seleccione el tipo de datos"
      }
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
    
    output$prueba <- renderPrint({
      norm_data()
    })
    
    read_dataServer("mod1", reactive(input$data_type))
    
    data <- read_dataServer("mod1", reactive(input$data_type))
    
    norm_microarrayServer("mod2a", data)
    norm_RNASeqServer("mod2b", data)
    norm_MetabRSServer("mod2c", data)
    norm_MetabSBServer("mod2d", data)
    norm_MetabMCServer("mod2e", data)
    
    norm_data_microarray <- norm_microarrayServer("mod2a", data)
    norm_data_RNASeq <- norm_RNASeqServer("mod2b", data)
    norm_data_MetabRS <- norm_MetabRSServer("mod2c", data)
    norm_data_MetabSB <- norm_MetabSBServer("mod2d", data)
    norm_data_MetabMC <- norm_MetabMCServer("mod2e", data)
    
    norm_data <- reactive({
      if(input$data_type == "microarray") {
        norm_data_microarray()
      } else if(input$data_type == "RNA-Seq") {
        norm_data_RNASeq()
      } else if(input$data_type == "MetabRA") {
        norm_data_MetabSB()
      }
    })
    
    output$prueba <- renderText({
      confi
    })
    
  }

  shinyApp(ui, server) 
}