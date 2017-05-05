library(shiny)
library(leaflet)
library(shinythemes)
library(ggvis)

shinyUI(fluidPage(
  theme = shinytheme("simplex"),
  HTML('<h4 style="margin-top: 40px;">Seattle housing housing data by neighborhood.</h4>'),
  tags$head(
    tags$style(type='text/css', 
               ".nav-tabs a {color:black;} ")),
  tabsetPanel(
    tabPanel("Geospatial", 
             fluidRow(
               column(width=8,
                      div(leafletOutput("mapmain", height = 600)),style='margin-top: 20px;'),
               column(width=4,
                      div(radioButtons('map_color',
                                   label='Color By:',
                                   choiceNames = c("Median Days on Market","Median Sale Price","# of Homes Sold",
                                                   "Number of Homes on Market","Median Price per Sq Ft"),
                                   choiceValues = c("Days on Market","Median Sale Price","Homes Sold",
                                                    "Inventory","Median Ppsf")),
                          style='margin-top: 20px;'),
                      div(htmlOutput("neighborhood_detail"), style='margin-top: 20px;')
               )
             )
             ),
    tabPanel("Historical", 
             fluidRow(
               column(width=7,
                      div(ggvisOutput("timeseries"), style='margin-top: 20px;')),
               column(width=5,
                      div(leafletOutput("map_select_timeseries", height = 300), style='margin-top: 20px'),
                      radioButtons('timeseries_var',
                                   label='Plot historical data for:',
                                   choiceNames = c("Median Days on Market","Median Sale Price","# of Homes Sold",
                                                   "Number of Homes on Market","Median Price per Sq Ft"),
                                   choiceValues = c("Days on Market","Median Sale Price","Homes Sold",
                                                    "Inventory","Median Ppsf")),
                      sliderInput("dates",
                                  "Dates:",
                                  min = as.Date("2012-01-01","%Y-%m-%d"),
                                  max = as.Date("2017-12-02","%Y-%m-%d"),
                                  value=c(as.Date("2012-01-01"),as.Date("2017-12-02")),
                                  timeFormat="%Y-%m-%d")
               )
             )),
    tabPanel("Correlational", 
             fluidRow(
               column(width=7,
                      div(span(htmlOutput("date_shown")), style='text-align: center;'),
                      div(ggvisOutput("parcoord"))),
               column(width=5,
                      div(leafletOutput("map_select_parcoord", height = 300), style='margin-top: 20px'),
                      sliderInput("month",
                                  "Select Month:",
                                  min = as.Date("2012-01-01","%Y-%m-%d"),
                                  max = as.Date("2017-02-01","%Y-%m-%d"),
                                  value=as.Date("2017-02-01"),
                                  timeFormat="%Y-%m-%d")
               )
             )
    )
  ),
  HTML('<div>Data Sources:
          <ul>
       <li>Neighborhood historical data is from Redfin: https://www.redfin.com/blog/data-center</li>
       <li>Neighborhood map data (shapefile) is from the City of Seattle: https://data.seattle.gov/dataset/Neighborhoods/2mbt-aqqx</li>
       </ul>')
)
)