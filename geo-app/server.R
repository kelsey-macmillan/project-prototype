library(shiny)
library(ggvis)
library(dplyr)
library(tidyr)
library(rgdal)
library(leaflet)
library(htmltools)

# Import data
# Read in the shapefile
shapefile <- readOGR("Neighborhoods/WGS84/", "Neighborhoods")

# Read in object ID mapping
id_mapping <- read.csv('object_id_mapping.csv', stringsAsFactors = FALSE) %>%
  mutate(object.ID = as.character(object.ID))

# Get neighborhood data
neighborhoods <- read.csv('seattle_overview.csv') %>%
  distinct() %>%
  spread(Measure.Names, Measure.Values) %>%
  rename(Year = Year.of.Period.End) %>%
  rename(Month = Month.of.Period.End) %>%
  arrange(Region, Year, Month) %>%
  separate(Region, c("City","Neighborhood"), sep=' - ') %>%
  mutate(common_name = tolower(Neighborhood)) %>%
  filter(common_name %in% unique(id_mapping$common_name)) %>%
  filter(is.na(Neighborhood)==FALSE)

# Map shape file ID to common ID
shapefile@data <- shapefile@data %>%
  mutate(OBJECTID = as.character(OBJECTID)) %>%
  left_join(id_mapping[c('object.ID','common_name')], by=c("OBJECTID"='object.ID')) %>%
  mutate(common_name=ifelse(common_name=="",'UNK',common_name)) %>%
  mutate(common_name=ifelse(is.na(common_name),'NA',common_name))

# Filter neighborhoods to only have Feb 2017 data (most recent) and join with shapefile
neighborhoods_2017 <- neighborhoods %>% 
  filter(Year==2017) %>%
  select(-one_of(c('Month','City'))) %>%
  group_by(common_name, Neighborhood) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))
shapefile@data <- shapefile@data %>%
  left_join(neighborhoods_2017, by='common_name')


shinyServer(function(input, output) {
  
  last_clicked <- reactiveValues(obj_id = 'none')
  
  cdata <- eventReactive(input$map_color,{
    shapefile@data[[input$map_color]]
  })
  
  cpal <- eventReactive(input$map_color,{
    colorQuantile("PuRd", domain = cdata())
  })
  
  output$mapmain <- renderLeaflet({
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data=shapefile, 
                  layerId = shapefile@data[['OBJECTID']],
                  label = ~htmlEscape(Neighborhood),
                  stroke = FALSE, 
                  weight = 1, 
                  smoothFactor = 0.5,
                  opacity = 1.0, 
                  fillOpacity = 0.5, 
                  color = ~cpal()(cdata()),
                  highlightOptions = highlightOptions(fillColor = "white", 
                                                      bringToFront = TRUE)) %>%
      addLegend(pal=cpal(), values=cdata())
  })
  
  observeEvent(input$mapmain_shape_click,{
    
    proxy <- leafletProxy("mapmain")
    
    # Get neighborhood clicked
    obj_id <- input$mapmain_shape_click$id
    neighborhood_name <- id_mapping$common_name[id_mapping$object.ID==obj_id]
    
    # If another polygon has been clicked, unhighlight it
    if (last_clicked$obj_id != 'none') {
      proxy %>% removeShape(layerId = last_clicked$obj_id) %>%
        addPolygons(data = shapefile[shapefile$OBJECTID==last_clicked$obj_id,],
                    layerId = last_clicked$obj_id,
                    label = ~htmlEscape(Neighborhood),
                    stroke = FALSE, 
                    weight = 1, 
                    smoothFactor = 0.5,
                    opacity = 1.0, 
                    fillOpacity = 0.5, 
                    color = ~cpal()(cdata()),
                    highlightOptions = highlightOptions(fillColor = "white", 
                                                        bringToFront = TRUE))
    } 
    
    # If the current click is different from the last click or there is no last click,
    # then highlight new click, display info, and update the last click
    # Else (if the current click is same as the last click),
    # clear info and set last click to 'none'
    if ((last_clicked$obj_id != obj_id)|(last_clicked$obj_id=='none')){
      proxy %>% removeShape(layerId = obj_id) %>%
        addPolygons(data = shapefile[shapefile$OBJECTID==obj_id,],
                    layerId = obj_id,
                    fillColor = "white",
                    label = ~htmlEscape(Neighborhood),
                    color = "black",
                    stroke = TRUE, 
                    weight = 1, 
                    smoothFactor = 0.5,
                    opacity = 1.0, 
                    fillOpacity = 0.5)
      
      # Update last clicked
      last_clicked$obj_id <- obj_id
      
      # Get data to display
      d <- neighborhoods_2017 %>%
        filter(common_name == neighborhood_name)
      
      
      if (neighborhood_name==""){
        
        output$neighborhood_detail <- renderText({
          HTML(paste("<h4>", d$Neighborhood, "</h4>",
                     "No data available for this area.",
                     sep=" "))
        })
        
      } else {
        
        output$neighborhood_detail <- renderText({
          HTML(paste("<h4>", d$Neighborhood, "</h4>",
                     "<b>Avg. Monthly # of Homes for Sale:</b> ", round(d[['Inventory']]), "<br/>",
                     "<b>Median Sale Price:</b> $", prettyNum(d[['Median Sale Price']],big.mark=",",scientific=FALSE), "<br/>",
                     "<b>Median Days on Market:</b> ", round(d[['Days on Market']]), "<br/>",
                     "<b>Median Price per Sq. Ft.:</b> ", round(d[['Median Ppsf']]), "<br/>",
                     sep=""))
          
        })
      }
      
    } else {
      last_clicked$obj_id <- 'none'
      output$neighborhood_detail <- renderText({HTML("")})
    }
    
    })
  
})