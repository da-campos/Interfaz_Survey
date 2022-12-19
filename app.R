library(shiny)
library(shinydashboardPlus)
library(shinyWidgets)
library(readxl)
library(shinysurveys)
library(DBI)
library(shinydashboard)



exceldata = read_excel(file.choose())
productos_ctei = data.frame(exceldata)
productos_ctei[productos_ctei == "NA"] <- NA

# Register a "check" input type
extendInputType("check", {
  shiny::checkboxGroupInput(
    inputId = surveyID(),
    label = surveyLabel(),
    choices = surveyOptions(), 
  )
})

# Register a date input to {shinysurveys}, limiting possible dates to a twenty-day period.
extendInputType("date", {
  shiny::dateInput(
    inputId = surveyID(),
    value = Sys.Date(),
    label = surveyLabel(),
    min = Sys.Date()-60,
    max = Sys.Date()+60
  )
})

title <- tags$a(href='https://www.car.gov.co/',
                tags$style(".topimg {
                            margin-left:-25px;
                            margin-right:-25px;
                            margin-top:-45px;
                          }"),
                tags$div(class="topimg",img(src="CAR1.jpeg", width='100%',height='100%'))) # an image downloaded from web (b.jpg under www folder)


ui <- dashboardPage(
  title = 'CAR',
  dashboardHeader(title = title,titleWidth='250',
                  # Set height of dashboardHeader
                  tags$li(class = "dropdown",
                          tags$style(".main-header {max-height: 200px}"),
                          tags$style(".main-header .logo {height: 200px}")
                  ),
                  dropdownMenu(headerText="Contato", type = "messages", badgeStatus = "success",
                               messageItem("E-mail", "google@gmail.br", icon = icon("envelope")),
                               messageItem("Site Principal",  uiOutput("site"), icon = icon("sitemap")),
                               messageItem("Server", uiOutput("server"), icon = icon("server")),
                               messageItem("Instagram", uiOutput("insta"), icon = icon("instagram-square")),
                               messageItem("Facebook", uiOutput("face"), icon = icon("facebook-square")),
                               messageItem("Youtube", uiOutput("youtube"), icon = icon("youtube-square"))
                  ),
                  
                  dropdownMenu(headerText="Aviso", type = "notifications", badgeStatus = "warning",
                               notificationItem(icon = icon("users"), status = "info", "Sobre Nós"),
                               notificationItem(icon = icon("cog"), status = "info", "Metodologia"))
                  ),
  dashboardSidebar(width = 250,
                   ### Adjust the sidebar
                   tags$style(".left-side, .main-sidebar {padding-top: 200px}"),
                   sidebarMenu(id="tabs", style = "white-space: normal;",
                               menuItem("Informacion General de Productos",tabName = 'Info_General'),
                               menuItem("Categoria de Productos",
                                        menuSubItem("Generación del Nuevo Conocimiento"),
                                        menuSubItem("Resultados de Actividades de Apropación Social
                                        del Conocimiento y Divulgación Pública de la Ciencia"),
                                        menuSubItem("Desarrollo Tecnológico e Innovación"),
                                        menuSubItem("Resultados de Actividades de Formación del
                                        Recurso Humano")),
                               menuItem("Base de Datos")
                   )),
  dashboardBody(tags$head( tags$style(HTML('
    /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                                background-image:url("https://media.giphy.com/media/3ohhwg3O1TGRXHQYh2/giphy.gif");
                                background-size: 1400px 200px;
                                background-repeat: no-repeat;
                              }
                              '))),
                
                fluidPage(tags$style('.container-fluid {
                             background-color: #006400;}'),
                          fluidRow(
                            column(12,style='padding-bottom:-50px;height:560px ;background-image:url("Plantas.jpg");
                                     background-repeat: no-repeat;background-size: 100% 100%;´',
                                   fluidRow( 
                                     column(6,style='
                                                      height:560px;overflow-y: scroll;)',
                                                      surveyOutput(df = productos_ctei, theme = "#63B8FF")),
                                     column(6, offset = 0,style ='height:280px;background-image:url("https://media.giphy.com/media/yLrLQPkyz7dLYshVhO/giphy.gif");
                                     background-repeat: no-repeat;background-size: 100% 100% ',
                                     fluidRow(tags$link(
                                       rel = "stylesheet", 
                                       href="https://fonts.googleapis.com/css?family=Monofett"
                                     ),
                                     tags$style("h1{font-family: 'Monofett', serif;
                                                    color:#000000;
                                                    font-weight: bold;
                                                    font-size: 3.5vh;
                                                    text-shadow: 4px 4px 4px #aaa;
                                                    text-align: right;
                                                    margin-top: 4px
                                                         }"),
                                              column(12,tags$h1("Información General de Productos de Ciencia,
                                              Tecnología E Innovación Ambiental DLIA-CAR")), style="height:280px;"),
                                     fluidRow(column(12,
                                     tags$h2("Definición de las categorías de los productos de ciencia y tecnología"),
                                     tags$style("h2{font-family: 'Tahoma', serif;
                                                    color:#FFFDFA;
                                                    font-weight: bold;
                                                    font-size: 1.3vw;
                                                    text-align: justify;
                                                         }"),
                                     

                                     fluidRow(
                                              column(6, align="center",
                                                     actionButton("Interfaz1", 
                                                     label = "Generación del Nuevo Conocimiento",
                                                     style='padding:4px; font-size:80%;
                                                     height:70px;
                                                     width:138px;
                                                     font-weight: bold;
                                                     white-space:normal;
                                                     color: white;
                                                     text-align: right;
                                                     background-color: blue;
                                                     ',icon = icon("fa-solid fa-lightbulb")),style="height:85px;"),
                                              column(6, align="center",
                                                     actionButton("Interfaz2", 
                                                                  label = "Resultados de Actividades 
                                                                  de Apropación Social del Conocimiento
                                                                  y Divulgación Pública de la Ciencia",
                                                                  style='padding:4px; font-size:80%;
                                                     height:70px;
                                                     width:138px;
                                                     font-weight: bold;
                                                     white-space:normal;
                                                     color: white;
                                                     text-align: right;
                                                     font-size: 0.65vw;
                                                     background-color: green;
                                                     ',icon = icon("fa-sharp fa-solid fa-users-between-lines")))
                                            ),
                                     fluidRow(
                                       column(6, align="center",
                                              actionButton("Interfaz3", 
                                                           label = "Desarrollo Tecnológico
                                                                    e Innovación",
                                                           style='padding:4px; font-size:80%;
                                                     height:70px;
                                                     width:138px;
                                                     font-weight: bold;
                                                     white-space:normal;
                                                     color: black;
                                                     text-align: right;
                                                     background-color: yellow;
                                                     ',icon = icon("atom"))),
                                       column(6, align="center",
                                              actionButton("Interfaz4", 
                                                           label = "Resultados de Actividades
                                                           de Formación del Recurso Humano",
                                                           style='padding:4px; font-size:80%;
                                                     height:70px;
                                                     width:138px;
                                                     font-weight: bold;
                                                     white-space:normal;
                                                     color: white;
                                                     text-align: right;
                                                     background-color: purple;
                                                     ',icon = icon("chalkboard-user")))
                                     )
                                     
                                     
                                     
                                     
                                     )))
                                     )
                                   )
                            )
                          )
                



                  ))


############################################################################################################
############################################################################################################
############################################################################################################

server <- function(input, output, session) {
  renderSurvey()
  values <- reactiveValues(df_data = NULL,myplot=NULL)
  observeEvent(input$submit, {
    response_data <- getSurveyData()
    values$df_data<-response_data
    print(response_data)
  })
  


  ##Update data in Rpostgresql table
  observeEvent(input$submit ,{
    
    ###Construct query to insert values into database table
    qry = paste0("INSERT INTO productos_ctei (nom_prod, celula,fecha_prod,nom_cread,cedula_cread)",
                 "VALUES ('",paste(input$Nom_Prod,"'",",","'",input$Celula_Prod,"'",",","'",input$
                                     Date_Prod,"'",",","'",input$Creador_Prod,"'",",","'",input$Cedula_Cread,"')"))
    ###Query to send to database
    dbSendQuery(conn = con, statement = qry)
    ###shinyModal to show to user when the update to the database table is successful
    showModal( modalDialog( title=paste0("RPostgreSQL table data Inserted"),
                            br(),
                            div(tags$b(paste0("You have Inserted the data into your RPostgresSQL table"), style = "color: green;"))
    ))
    
  })
  
  observeEvent(input$Interfaz1 ,{
    ###Boton de Informacion 1 Generacion del Nuevo Conocimiento
    showModal( modalDialog( title=paste0("Generación del Nuevo Conocimiento"),
                            br(),
                            div(tags$b(paste0("Se consideran productos resultados de actividades
                            de generacion de nuevo conocimiento aquellos aportes significativos
                            al estado del arte de un area de conocimiento, que han sido discutidos
                            y validados para llegar a ser incorporados a la discusion cientifica,
                            al desarrollo de las actividades de investigacion, al desarrollo 
                            tecnologico, y que pueden ser fuente de innovaciones. Este tipo de
                            producto se caracteriza por involucrar mecanismos de estandarizacion
                            que permiten corroborar la existencia de una evaluacion que
                            verifique la generacion de nuevo conocimiento."), style = "color: black;"))
    ))
                            
  })
  
  observeEvent(input$Interfaz2 ,{
    ###Boton de Informacion 1 Generacion del Nuevo Conocimiento
    showModal( modalDialog( title=paste0("Resultados de Actividades de Apropación Social del
                            Conocimiento y Divulgación Pública de la Ciencia"),
                            br(),
                            div(tags$b(paste0("Se consideran productos resultados de procesos
                            de apropiacion social del conocimiento, aquellos que implican que la
                            ciudadania intercambie saberes y conocimientos de ciencia, tecnologia e
                            innovacion para abordar situaciones de interes comun y proponer soluciones
                            o mejoramientos concertados, que respondan a sus realidades."), style = "color: black;"))
    ))
    
  })
  
  observeEvent(input$Interfaz3 ,{
    ###Boton de Informacion 1 Generacion del Nuevo Conocimiento
    showModal( modalDialog( title=paste0("Desarrollo Tecnológico e Innovación"),
                            br(),
                            div(tags$b(paste0("Estos productos dan cuenta de la generacion de ideas, 
                            metodos y herramientas que impactan el desarrollo economico y generan 
                            transformaciones en la sociedad. En el desarrollo de estos metodos y 
                            herramientas esta implicita la investigacion que genera el conocimiento 
                            enfocado en la solucion de problemas sociales, tecnicos y economicos. "), style = "color: black;"))
    ))
    
  })
  
  observeEvent(input$Interfaz4 ,{
    ###Boton de Informacion 1 Generacion del Nuevo Conocimiento
    showModal( modalDialog( title=paste0("Resultados de Actividades de Formación del Recurso Humano"),
                            br(),
                            div(tags$b(paste0("actividades relacionadas con la formacion de recurso 
                            humano para la CTeI, MINCIENCIAS reconoce los siguientes productos: la 
                            generacion de espacios para asesorar y desarrollar las actividades 
                            implicadas en la realizacion de una tesis o trabajo de grado que otorgo el
                            titulo de doctor(a), magister o profesional (respectivamente); la ejecucion 
                            de proyectos de I+D+I con formacion y apoyo a programas de formacion; y la
                            gestion de proyectos de investigacion que permiten la consecucion de los
                            recursos necesarios para el desarrollo de la investigacion o la innovacion."), style = "color: black;"))
    ))
    
  })
  
  
}

shinyApp(ui, server)
