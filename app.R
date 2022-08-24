### Shiny app to analyze US Census Data ###
### Author: Kevin Zolea ###
### Date: 11/2018 ###
############################################################################
#if (!require(pacman)) {
##install.packages('pacman')

#}
############################################################################
#pacman::p_load("ggplot2","tidyr","plyr","dplyr","readxl","shinycssloaders",
#"readr","cowplot","lubridate","scales","shinydashboardPlus",
#"gridExtra","stringr","ggpmisc","data.table","rlang","purrr",
#"shiny","shinydashboard","DT","leaflet","rgdal","sf","rmapshaper",
#"rsconnect","shinyjs","tidycensus")
############################################################################
library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
library(readxl)
library(shinycssloaders)
library(readr)
library(shinydashboardPlus)
library(ggpmisc)
library(data.table)
library(shiny)
library(rlang)
library(purrr)
library(shinydashboard)
library(DT)
library(leaflet)
library(sf)
library(rmapshaper)
library(rsconnect)
library(shinyjs)
library(rgeos)
library(tidycensus)
library(stringr)
library(leaflet.extras)
############################################################################
readRenviron("~/.Renviron")
### Define Census API Key and set it with census_api_key() ###
#Sys.getenv("CENSUS_API_KEY")
#api_key<-"1c32b297e99b22d82fb12c683f56d95eb9e12168"
#census_api_key(api_key,install = TRUE,overwrite = T)
### Make vector of variables of interest to pull from API ###
variables_interest<-c('Population' ="B01003_001",'Median Income'="B19013_001",
                      'Median Home Value' = "B25077_001","Total Owner Occupied Housing by Tenure"= "B25003_002",
                      "Median Gross Rent" = "B25064_001","Median Monthly Housing Costs" = "B25105_001",
                      "Total Male Population" = "B01001_002","Total Female Population" = "B01001_026")

variables_interest<-sort(variables_interest)

states<-c("NJ","NY","CT","CO","FL","AL","AK",
          "AR","CA","DE","GA","HI","ID","IL","IN","IA",
          "KS","KY","LA","ME","MD","MA","MI","MN","MS",
          "MO","MT","NE","NV","NH","NM","NC","ND","OH",
          "OK","OR","PA","RI","SC","SD","TN","TX","UT",
          "VT","VA","WA","WV","WI","WY")

states<-sort(states)
### Creates header for app ###
header<- dashboardHeader(title = "US Census Data Explorer",titleWidth = 350,
                             dropdownMenu(
                               type = "notifications",
                               notificationItem(text = "Information about US Census Bureau Data",
                                                href = "https://www.census.gov/data.html")))
############################################################################ 
### Create Sidebar for app ###
sidebar<-dashboardSidebar(
  selectInput("variable","Select Variable:",choices = variables_interest),
  selectInput("state","Select State:",choices = states,selected = "NJ"),
  HTML("<h4>&nbsp; &nbsp; &nbsp; Author: Kevin Zolea </h4>")
  
)
############################################################################ 
### Create body of app ###
body<-dashboardBody(
            fluidRow(box(leafletOutput("census_map")%>%
                           withSpinner(type=5,color = "blue")),
            box(plotOutput("plot1")%>%
              withSpinner(type = 5, color = "blue"))),
            fluidRow(DT::dataTableOutput("census_table")%>%
              withSpinner(type = 5,color = "blue")))
### Create UI for app ###
ui<- dashboardPage(
  #shinyjs::useShinyjs(),
  header = header,
  sidebar = sidebar,
   body = body)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
### Create a reactive data frame to get census data based on user input ###
  df<-reactive({
    get_acs(geography = "county",
                       variables = input$variable,
                       state = input$state,
                       geometry = TRUE)

  })
  
  #df2<-reactive({
  #  df()%>%
  #  dplyr::select(NAME,estimate,moe)})
### Create reactive dataframe to get colors for leaflet map ###
  pal<- reactive({
    colorNumeric(palette = "plasma", 
                 domain = df()$estimate, n =10,reverse = T)
  })
   
### Output for datatable ###
  output$census_table<-DT::renderDataTable({
    datatable(df(),colnames = c('County'='NAME',"Estimate" = "estimate",'Margin of Error' = 'moe'),
              filter = "top",options = list(autoWidth = TRUE,columnDefs = list(list(className='dt-center',visible=FALSE, targets=c(1,3,6)))))
  })
  
### Output for leaflet map ###  
  output$census_map<- renderLeaflet({
        df()%>%
    leaflet(options = leafletOptions(minZoom = 7))%>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addResetMapButton()%>%
      addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                  stroke = FALSE,
                  smoothFactor = 0,
                  fillOpacity = 0.7,
                  color = ~ pal()(estimate),
                  layerId = ~unique(NAME))%>%
      addLegend("bottomright",
                pal = pal(),
                values = ~estimate,
                title = "",
                opacity = 1)

})
  
  ### Allows user to have map zoomed in when impaired HUC is clicked ###
  observe({
    click <- input$census_map_shape_click
    if(is.null(click))
      return()
    else
      leafletProxy("census_map")%>%
      setView(lng = click$lng , lat = click$lat, zoom=10)
  })
  
 ### Output for plot ###


  output$plot1<- renderPlot({
    if(input$variable == "B01003_001"|input$variable == "B01001_002"|input$variable == "B01001_026"){
    df()%>%
      ggplot(aes(x = NAME, y =estimate)) +
      geom_bar(stat= "identity",fill = "#0c439b") +
      labs(title = paste("Household population by county in ",input$state),
           subtitle = "2013-2017 American Community Survey",
           y = "",
           x = "County")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5,margin = margin(c(0, 20, 0, 0))))
      
    }
    
    else{
      options(scipen=10000)
      df()%>%
        ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
        geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
        geom_point(color = "red", size = 3)+
        labs(title = "2013-2017 American Community Survey",
             y = "",
             x = "ACS estimate (bars represent margin of error)")



    }
  })
  
   
}

# Run the application 
shinyApp(ui,server)

#rsconnect::setAccountInfo(name='kzolea695',
 #token='DC7A93A56CEAB9776CFADE6C8F5E1367',
 #secret='9DSUwsPxNtDEDx2BrHXG8lZ6MPZ6OukS8LM/UQeX')


#deployApp()


