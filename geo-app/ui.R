library(shiny)
library(leaflet)
library(shinythemes)
library(ggvis)

shinyUI(fluidPage(
  theme = shinytheme("simplex"),
  fluidRow(
    h1('Geospatial Perspective ', style='padding: 20px;'),
    column(width=8,
           leafletOutput("mapmain", height = 600)),
    column(width=4,
           radioButtons('map_color',
                        label='Color By:',
                        choiceNames = c("Median Days on Market","Median Sale Price","# of Homes Sold",
                                        "Number of Homes on Market","Median Price per Sq Ft"),
                        choiceValues = c("Days on Market","Median Sale Price","Homes Sold",
                                         "Inventory","Median Ppsf")),
           div(htmlOutput("neighborhood_detail"), style='margin-top: 20px;')
           )
    )
  )
)