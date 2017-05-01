library(shiny)
library(leaflet)
library(shinythemes)
library(ggvis)

shinyUI(fluidPage(
  theme = shinytheme("simplex"),
  fluidRow(h1('Historical Perspective ', style='padding: 20px;')),
  fluidRow(
    column(width=7,
           div(ggvisOutput("timeseries"), style='margin-top: 20px;')),
    column(width=5,
           div(leafletOutput("mapmain", height = 300), style='padding: 10px'),
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
    )
  )
)