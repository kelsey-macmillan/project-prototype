library(shiny)
library(leaflet)
library(shinythemes)
library(ggvis)

shinyUI(fluidPage(
  theme = shinytheme("simplex"),
  fluidRow(h1('Relationships Perspective ', style='padding: 20px;')),
  fluidRow(
    column(width=7,
           div(span(htmlOutput("date_shown")), style='text-align: center;'),
           div(ggvisOutput("parcoord"))),
    column(width=5,
           div(leafletOutput("mapmain", height = 300), style='padding: 40px'),
           sliderInput("month",
                       "Select Month:",
                       min = as.Date("2012-01-01","%Y-%m-%d"),
                       max = as.Date("2017-02-01","%Y-%m-%d"),
                       value=as.Date("2017-02-01"),
                       timeFormat="%Y-%m-%d")
    )
    )
  )
)