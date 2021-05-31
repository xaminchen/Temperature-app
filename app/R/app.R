library(shiny)
library(lubridate)
library(recipes)
library(leaflet)
library(tidyverse)
library(modeltime)
library(tidymodels)
library(timetk)
library(modeltime.h2o)
library(modeltime.ensemble)
library(modeltime.resample)
library(modeltime.gluonts)
library(h2o)
library(dplyr)
library(feather)
library(earth)
library(vip)
library(lime)
library(plotly)
library(shinycssloaders)
library(bslib)



XGBOOST=readRDS(file="D:/Dropbox/2021/Shiny/App-2 Timeseries Forecasting/app/R/XGBOOST_model.rds")
RANGER=readRDS(file="D:/Dropbox/2021/Shiny/App-2 Timeseries Forecasting/app/R/RANGER_model.rds")
df_full=read_feather("D:/Dropbox/2021/Shiny/App-2 Timeseries Forecasting/app/R/Temperatures.feather")

df=df_full%>%filter(date>"2017-01-01")
df$id=ordered(df$id,levels=c("Bremen","Fehmarn","Brocken","Berlin","Frankfurt","Zugspitze"))


ui<-navbarPage(title="Machine Learning Temperaturvorhersage",
               theme=bs_theme(
                 bg="#202123",fg="#B8BCC2",primary="#EA80FC",
                 base_font=font_google("Fira Sans")
               ),
                   tabPanel("Vorhersage",
                            fluidRow(column(12,
                                            h1("Temperaturvorhersage"),
                                            p("Das Ziel dieser Webapplikation ist an einem simplen Beispiel zu  zeigen, wie mit R Shiny Analysen, Zeitreihenvorhersagen oder Machine Learning Algorithmen produktionalisiert werden koennen."),
                                            h4("Instruktionen"),
                                            p("Benutze die Eingabe um einen Ort und ein Datum fuer eine Tageshoechsttemperaturvorhersage auszuwaehlen."),
                                            hr())),
                            fluidRow(
                                sidebarPanel(width = 3,
                                    selectInput(inputId="id",
                                            label="Ort",
                                            choices=c("Berlin","Bremen","Brocken","Fehmarn","Frankfurt","Zugspitze"),
                                            selected = c("Bremen")),
                                    dateInput(inputId="date",
                                          label="Datum",
                                          value = lubridate::today(),
                                          min = "2019-01-01",
                                          max = "2021-12-31"),
                                    format = "yyyy-mm-dd"),
                                mainPanel(withSpinner(tableOutput("forecast")),
                                        hr(),
                                        br(),
                                        withSpinner(leafletOutput("leaflet",height = 550))))),
                   
#Historische Daten                   
                   tabPanel("Historische Daten",
                            fluidRow(column(width =12,
                                            h1("Historische Temperaturdaten von 2000 bis 2019"),
                                            p("Diese Seite zeigt die hisorischen Temperaturdaten des Deutschen Wetterdienst (DWD) fuer die Jahre 2000-2020. Die Vorhersage basiert auf den Jahren 2017-2019"),
                                            br(),
                                            h4("Instruktionen"),
                                            p("Benutze die Eingabe um den Ort auszuwaehlen."),
                                            hr())),
                            fluidRow(
                                sidebarPanel(width = 3,
                                             selectInput(inputId="loc",
                                                         label="Ort",
                                                         choices=c("Berlin","Bremen","Brocken","Fehmarn","Frankfurt","Zugspitze"),
                                                         selected = c("Bremen"))),
                                mainPanel(
                                    withSpinner(plotlyOutput("hist", height = 500))))),

  #Erklaerung zur Wettervorhersage                 
                   tabPanel("Daten",
                            fluidRow(column(width =12,
                                            h1("Zeitreihenvorhersage anhand von historischen Wetterdaten mit Hilfe von Machine Learning"),
                                            p("Die historische Wetterdaten stammen vom Deutschen Wetterdienst."),
                                            a("https://www.dwd.de/DE/leistungen/klimadatendeutschland/klarchivtagmonat.html?nn=16102"),
                                            br()
                                     ))))
                            
    




                                                         


server <- function(input, output) {
#Data Preparation-----------------    
    #Data Preperation
    FORECAST_HORIZON=365
    
    full_data_tbl<-df%>%
        select(id,date,temp)%>%
        
        #apply group wise time series manipulation
        group_by(id)%>%
        future_frame(
            .date_var =date,
            .length_out=FORECAST_HORIZON,
            .bind_data=T
        )%>%
        ungroup()
    
    #Split into Training and Test set
    data_prepared_tbl<-full_data_tbl%>%
        filter(!is.na(temp))
    
    
    
    future_tbl<-full_data_tbl%>%
        filter(is.na(temp))
    
    
    #SPLITTING
    splits<-data_prepared_tbl%>%
        time_series_split(
            date_var    = date, 
            assess      = FORECAST_HORIZON,
            cumulative  = TRUE
        )
    
    
    #Make Cross validation files
    temperature_tscv <- data_prepared_tbl %>%
        time_series_cv(
            date_var    = date, 
            assess      = "12 months",
            skip        = "5 months",
            cumulative  = TRUE,
            slice_limit = 3
        )
    
 #Outputs----------------------------------------------------------------------------   

    output$leaflet <- renderLeaflet({
        pos=data.frame(
            lon=c(8.7979,10.6183,11.0606,10.9848,8.5213,13.4021),
            lat=c(53.045,51.7986,54.5283,47.4210,50.0259,52.4675),
            label=c("Bremen","Brocken","Fehmarn","Zugspitze","Frankfurt","Berlin"))
        
        pos1=pos%>%filter(label!=input$id)
        pos2=pos%>%filter(label==input$id)
        
        
        leaflet(Map)%>% #create a leaflet file
            addProviderTiles(providers$Esri.WorldImagery)%>%#change the basemap
            addProviderTiles(providers$CartoDB.Positron,
                             options = providerTileOptions(opacity = 0.25))%>%
            setView(lng=mean(pos$lon), lat=mean(pos$lat),zoom=5)%>%
            addCircleMarkers(lng=pos1$lon,lat=pos1$lat,
                             color = "black",
                             weight = 2,
                             opacity = 1,
                             fillColor = "black",
                             radius=8,
                             label = pos1$label)%>%
            addCircleMarkers(lng=pos2$lon,
                             lat=pos2$lat,
                             color = "black",
                             weight = 2,
                             opacity = 1,
                             fillColor = "red",
                             radius=8,
                             label = pos2$label)
    })
    
    output$forecast <-  function() {
        library(dplyr)
        library(kableExtra)
        data=data.frame(id=input$id,date=ymd(input$date))
        
        
        forecast=modeltime_table(RANGER,
                                 XGBOOST)%>%
            modeltime_calibrate(testing(splits)%>%arrange(id,date))%>%
            modeltime_forecast(
                full_tbl,
                new_data = data,
                h=NULL,
                actual_data = NULL,
                conf_interval = 0.95,
                keep_data = FALSE,
                arrange_index = FALSE,
            )
        
        table=data.frame(Model=forecast$.model_desc,Datum=input$date, Ort=input$id, Temperaturvorhersage=round(forecast$.value,1), Min.=round(forecast$.conf_lo,1), Max.=round(forecast$.conf_hi,1))
        
        table%>%
            knitr::kable("html") %>%
            kable_styling("striped", full_width = T)%>%
            add_footnote(
                         "Vorraussage der Tageshoechsttemperatur auf Basis von historischen Temperaturdaten (2017-2019) des DWD")
    }
    output$hist <- renderPlotly({
        df_full%>%
            filter(id==input$loc)%>%
            plot_time_series(.date_var=date,
                             .value=temp,
                             .facet_vars=id,
                             .facet_ncol=2,
                             .smooth_period=365,
                             .title = "Temperaturentwicklung",
                             .x_lab = "",
                             .y_lab = "Temperatur")
    })
    output$cv <- renderPlot({
        #Plot Cross Validation Plan
        temperature_tscv %>%
            tk_time_series_cv_plan() %>%
            plot_time_series_cv_plan(date, temp, 
                                     .facet_ncol = 1,
                                     .interactive = F)
    }) 
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
