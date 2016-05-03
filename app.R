# Copyright (c) 2016, Jose L. ALONSO <jl.alonso (at) upm.es>
# All rights reserved.

# Redistribution and use in source and binary forms, with or without 
# modification, are permitted provided that the following conditions are met:

# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.


library(shiny)
library(googleVis)


ui <- fluidPage(                        
  titlePanel("HELIOS - Simulador de autoconsumo SOLAR [beta]"),       
  sidebarLayout(                           
  sidebarPanel(                           
    fileInput('datafile', 'Archivo Mensual datos Smart-Meter',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    selectInput(inputId = "Provincia",
                label = "Provincia:",
                choices = c('Barcelona','Madrid','Malaga','Santander')),
    hr(),
    radioButtons(inputId = "PVTech", label = "Tecnología Panel FV", 
                   choices = c('SiMC', 'SiA', 'CIS')),
    sliderInput("MultFV", label = "Núm. Paneles FV", min = 0, max = 15, value = 1),
    #numericInput("MultFV", label = "Núm. Paneles FV", value = 1),
    #checkboxInput("batt", label = "Bateria", value = FALSE),
    hr(),
    actionButton("goButton", "Calcular"),
    hr(),
    helpText("Copyright © 2016")
  ),
    mainPanel(
      tabsetPanel(selected="Inicio", id = "tabs",
      tabPanel("Inicio",
               HTML("<p>Bienvenido/a a <strong>HELIOS App</strong>, esta aplicación web te permitirá conocer de manera sencilla el ahorro apróximado en tu consumo eléctrico mensual con una instalación solar fotovoltaica para autoconsumo.</p>"),
               HTML("<br>"),
               HTML("<p>Para empezar necesitas DESCARGAR un MES de datos horarios de tu Smart-Meter de la web de tu empresa distribuidora.</p>"),
               HTML("<ul><li><a href='https://zonaprivada.endesadistribucion.es/es-ES/Paginas/Index.aspx' target='_blank'>ENDESA</a></li><li><a href='https://www.iberdroladistribucionelectrica.com/01disd/wtgapp/www/inicio.html' target='_blank'>IBERDROLA</a></li><li><a href='https://areaprivada.unionfenosadistribucion.com/ovde-web/Login.gas?_ga=1.36827488.1595407201.1461920771' target='_blank'>UNION FENOSA</a></li></ul>"),
               HTML("<p>O descárgate este archivo de <a href='https://www.dropbox.com/s/5r13k1u3zc3sbbq/Consumo_01_12_2015-31_12_2015-R.csv' target='_blank'>EJEMPLO</a></p>"),
               HTML("<p>Introduce el archivo DESCARGADO, Después selecciona la PROVINCIA y presiona CALCULAR.</p>"),
               HTML("<p>Puedes modificar la TECNOLOGÍA y el NÚMERO de Paneles FV para lograr el MÁXIMO ahorro de la Instalación (Manteniendo el Grado de Autoconsumo FV, <em>porcentaje de energía FV consumida del total generado</em>, por encima del 70%).</p>"),
               HTML("<br>"),
               HTML("<p>¡Ahh! y NO te olvides de COMPARTIR tu AHORRO con tus mejores amigos ;-)</p>"),
	       hr(),
	       HTML("<p>Una idea desarrollada por <strong>José L. ALONSO</strong>, sígueme en <a href='https://twitter.com/alonsoxl/' target='_blank'>@AlonsoXL</a></p>")
               ),
      tabPanel("Gráficos",
      textOutput("textDisplay"),
      htmlOutput("Chart"), 
      htmlOutput("textDisplay2"),
      hr(),
      htmlOutput("textDisplay3"),
      tags$div(HTML("Comparte tu ahorro con tus mejores amigos...<div style='float:right'><a href='https://twitter.com/share' class='twitter-share-button' data-url='https://alonsoxl.shinyapps.io/HELIOSApp/' data-text='Descubre con HELIOSApp cuanto puedes ahorrar en tu factura eléctrica con #autoconsumo #solar ' data-size='large' data-hashtags='renovables' data-dnt='true'>Tweet</a></div>")),

      tags$script(HTML("!function(d,s,id){
            var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';
            if(!d.getElementById(id)){
                    js=d.createElement(s);
                    js.id=id;
                    js.src=p+'://platform.twitter.com/widgets.js';
                    fjs.parentNode.insertBefore(js,fjs);
            }
       } (document, 'script', 'twitter-wjs');"))
      )
      )
    )
  )
)

server <- function(input, output, session){
  
  observe({
    if (input$goButton > 0) {
    updateTabsetPanel(session, "tabs", "Gráficos")
    }
  })
  
   filedata <- reactive({
    	infile <- input$datafile
        if (is.null(infile)) return(NULL)
    
    	 read.csv(infile$datapath, header = TRUE, dec = ",", sep = ";")
   })   
    
   provincia <- reactive({
      switch(input$Provincia,
           	"Madrid" = Madrid,
           	"Barcelona" = Barcelona,
           	"Malaga" = Malaga,
                "Santander" = Santander)
   })
 
   SolarData <- reactive({
     if (input$Provincia =="Madrid") {
       SolarData <- read.csv('data/calc_pot_FV_Madrid.csv', sep=';', dec = ",", header=T)
     }
     if (input$Provincia =="Barcelona") {
       SolarData <- read.csv('data/calc_pot_FV_Barcelona.csv', sep=';', dec = ",", header=T)
     } 
     if (input$Provincia =="Malaga") {
       SolarData <- read.csv('data/calc_pot_FV_Malaga.csv', sep=';', dec = ",", header=T)
     } 
     if (input$Provincia =="Santander") {
       SolarData <- read.csv('data/calc_pot_FV_Santander.csv', sep=';', dec = ",", header=T)
     } 
     SolarData$Fecha <- as.Date(SolarData$Fecha, format="%Y-%m-%d")
     SolarData
     })
   
   output$textDisplay <- renderText({
     df <- filedata()
     if (is.null(df)) return(NULL)
     
     df <- df[,-c(1,5)]
     df$Fecha <- as.Date(df$Fecha, format="%d/%m/%Y")
     
	 paste0("Mes: ", format(df[1,1],"%B"))
   })

   output$Chart <- renderGvis({
   Solardf <- SolarData()
   df <- filedata()
   if (is.null(df)) return(NULL)

   df <- df[,-c(1,5)]
   df$Fecha <- as.Date(df$Fecha, format="%d/%m/%Y")
   mes <- format(df[1,1],"%m")
   Solardf <- subset(Solardf, format(Solardf$Fecha, "%m") == mes)
   
   df$PFV <- input$MultFV*Solardf$PFV/1000
   df$Neto <- ifelse((df$Consumo_kWh-df$PFV)>0,df$Consumo_kWh-df$PFV,0)
   df$IFV <- ifelse((df$Consumo_kWh-df$PFV)<0,df$Consumo_kWh-df$PFV,0)
   
   Chartdf <- aggregate(Consumo_kWh~Hora, data=df, FUN = "sum")
   colnames(Chartdf)[2] <- as.character("Consumo_sin_FV")
   Chartdf$Consumo_con_FV <- aggregate(Neto~Hora, data=df, FUN = "sum")[,2]
   Chartdf$FV_a_Red <- aggregate(IFV~Hora, data=df, FUN = "sum")[,2]
   
   gvisColumnChart(Chartdf, xvar="Hora", options=list(chartArea="{width:'100%'}",
                                                      colors="['#2171b5', '#6baed6', 'red']",
                                                      bar="{groupWidth:'90%'}",
                                                      legend="{position:'top'}"))
   })
   

   output$textDisplay2 <- renderUI({
	  Solardf <- SolarData()
    df <- filedata()
	  if (is.null(df)) return(NULL)
    
	  df <- df[,-c(1,5)]
	  df$Fecha <- as.Date(df$Fecha, format="%d/%m/%Y")
    mes <- format(df[1,1],"%m")
	  Solardf <- subset(Solardf, format(Solardf$Fecha, "%m") == mes)
	  
    df$PFV <- input$MultFV*Solardf$PFV/1000
	  df$Neto <- ifelse((df$Consumo_kWh-df$PFV)>0,df$Consumo_kWh-df$PFV,0)
	  df$IFV <- ifelse((df$Consumo_kWh-df$PFV)<0,df$Consumo_kWh-df$PFV,0)
	  
    Chartdf <- aggregate(Consumo_kWh~Hora, data=df, FUN = "sum")
	  Chartdf$Consumo_FV <- aggregate(Neto~Hora, data=df, FUN = "sum")[,2]
	  Chartdf$FV_Red <- aggregate(IFV~Hora, data=df, FUN = "sum")[,2]
	  Solardf <- aggregate(PFV~Hora, data=Solardf, FUN = "sum")
	  
    str1 <- paste("<strong>Consumo TOTAL: </strong>", round(sum(Chartdf$Consumo_kWh), digits = 2),"kWh")
	  str2 <- paste("<strong>Consumo en horas SOL: </strong>", round((sum(Chartdf[9:18, 2])/sum(Chartdf$Consumo_kWh))*100, digits = 0),"%")
    str3 <- paste("<strong>Energia FV Generada: </strong>", round(input$MultFV*sum(Solardf$PFV/1000), digits = 2),"kWh")
	  str4 <- paste("<strong>Grado de Autoconsumo: </strong>", round(((sum(df$PFV)+sum(df$IFV))/sum(df$PFV))*100, digits = 0),"% (Recom. > 70%)")
    str5 <- paste("<strong>Consumo TOTAL con Autoconsumo FV: </strong>", round(sum(Chartdf$Consumo_FV), digits = 2),"kWh") 
	  str6 <- paste("<strong>Grado Autosuficiencia (Ahorro): </strong>", round(((sum(Chartdf$Consumo_kWh)-sum(Chartdf$Consumo_FV))/sum(Chartdf$Consumo_kWh))*100, digits = 0),"% (Máx. ahorro Consumo en horas SOL)")
    str7 <- paste("<strong>Ahorro Emisiones CO2: </strong>", round(input$MultFV*sum(Solardf$PFV/1000)*0.331, digits = 2),"kgCO2")
	  HTML(paste(str1, str2, str3, str4, str5, str6, str7, sep = '<br/>'))
	  })

}

shinyApp(ui, server)
