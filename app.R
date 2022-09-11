### Shiny app to analyze US Census Data ###
### Author: Kevin Zolea ###
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
# Load libraries in
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
Sys.getenv("CENSUS_API_KEY")
Sys.getenv("api_key")
#api_key<-"1c32b297e99b22d82fb12c683f56d95eb9e12168"
census_api_key(api_key,install = TRUE,overwrite = T)
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
                                                href = "https://www.census.gov/data.html"),
                               notificationItem(text = "API used for App's Data",
                                                href = "https://walker-data.com/tidycensus/")))
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
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
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
  test<-reactive({
        get_acs(geography = "county",
          variables = input$variable,
          state = input$state,
          geometry = TRUE)})
  
  df<-reactive({
    test()%>%
      arrange(desc(estimate)) %>%
      slice(1:10) 
      
  })

  df2<-reactive({
    df()%>%
      mutate(variable= recode(variable,B01003_001='Population',B19013_001='Median Income',
                              B25077_001='Median Home Value',B25003_002="Total Owner Occupied Housing by Tenure",
                              B25064_001="Median Gross Rent",B25105_001="Median Monthly Housing Costs",
                              B01001_002="Total Male Population",B01001_026="Total Female Population"))})
  
  
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
  
  tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))
  
  title <- tags$div(
    tag.map.title, HTML(paste("Top 10", "Counties", sep="<br/>"))
  )  
  
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
                title = unique(df2()$variable),
                opacity = 1)%>%
      addControl(title, position = "topleft", className="map-title")
    

})
  
  ### Allows user to have map zoomed in when impaired HUC is clicked ###
  observe({
    click <- input$census_map_shape_click
    if(is.null(click))
      return()
    else
      leafletProxy("census_map")%>%
      setView(lng = click$lng , lat = click$lat, zoom=12)
  })
  
 ### Output for plot ###
  output$plot1<- renderPlot({
    if(input$variable == "B01003_001"|input$variable == "B01001_002"|input$variable == "B01001_026"){
      

      df()%>%
        ggplot(aes(x = NAME, y =estimate)) +
        geom_bar(stat= "identity",fill = "#0c439b") +
        #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        labs(title = paste0("Top 10 Counties for ", df2()$variable , " in ", input$state),
             subtitle = "2016-2020 American Community Survey",
             y = "",
             x = "County")+
        theme(axis.text.x = element_text(angle = 90))
      
    }
    
    else{
      options(scipen=10000)
      df()%>%
        ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
        geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
        geom_point(color = "red", size = 3)+
        scale_x_continuous(labels=scales::dollar_format())+
        labs(title = paste0("Top 10 Counties for ", df2()$variable , " in ", input$state),
             subtitle = "2016-2020 American Community Survey",
             y = "",
             x = "ACS estimate (bars represent margin of error)")



    }
  })
  
   
}

# Run the application 
shinyApp(ui,server)


