library(shiny)
library(ggvis)
library(dplyr)
library(tidyr)
library(rgdal)
library(leaflet)
library(htmltools)
library(lubridate)

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
  filter(is.na(Neighborhood)==FALSE) %>%
  mutate(ones = '1') %>%
  unite('date_string',one_of(c('Year','Month', 'ones')), sep=' ') %>%
  mutate(date = ymd(date_string)) 

# Map shape file ID to common ID
shapefile@data <- shapefile@data %>%
  mutate(OBJECTID = as.character(OBJECTID)) %>%
  left_join(id_mapping[c('object.ID','common_name')], by=c("OBJECTID"='object.ID')) %>%
  mutate(common_name=ifelse(common_name=="",'UNK',common_name)) %>%
  mutate(common_name=ifelse(is.na(common_name),'NA',common_name))

shapefile@data <- shapefile@data %>%
  left_join(unique(neighborhoods[c('common_name','Neighborhood')]), by='common_name')

# Define functions
tooltip_fun <- function(x) {
  if(is.null(x)) return(NULL)
  n <- neighborhoods[neighborhoods$common_name ==x$common_name,]$Neighborhood[1]
  paste(n)
}

normalize <- function(x){
  return((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))}


# Normalize data for par coords plot
neighborhoods_group <- neighborhoods %>%select(one_of(c('common_name','date','Neighborhood')))
neighborhoods_vars <- neighborhoods %>% select(one_of(c("Days on Market","Median Sale Price","Homes Sold",
                                       "Inventory","Median Ppsf"))) %>%
  mutate_each(funs(normalize))
neighborhoods_norm <- df_par <- bind_cols(neighborhoods_group, neighborhoods_vars)


shinyServer(function(input, output) {
  
  # Render map
  cpal <-  colorFactor(c('#d9d9d9','#bdbdbd','#969696','#737373','#525252','#252525'), 
                       shapefile@data[['OBJECTID']])
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
                  fillOpacity = 0.6, 
                  color = ~cpal(OBJECTID),
                  highlightOptions = highlightOptions(fillColor = "white", 
                                                      bringToFront = TRUE))
  })
  
  # Code to handle selecting on map
  obj_selected <- reactiveValues(ids = c(), names=c())
  
  observeEvent(input$mapmain_shape_click,{
    
    # Map proxy
    proxy <- leafletProxy("mapmain")
    
    # Get neighborhood clicked
    obj_id <- input$mapmain_shape_click$id
    neighborhood_name <- id_mapping$common_name[id_mapping$object.ID==obj_id]
    
    # If the click is not yet in the list, highlight it and add to selected list
    # else unhighlight and remove from selected list
    if (!(obj_id %in% obj_selected$ids)){
      proxy %>% removeShape(layerId = obj_id) %>%
        addPolygons(data = shapefile[shapefile$OBJECTID==obj_id,],
                    layerId = obj_id,
                    color = "#e7298a",
                    label = ~htmlEscape(Neighborhood),
                    stroke = TRUE, 
                    weight = 1, 
                    smoothFactor = 0.5,
                    opacity = 0.6, 
                    fillOpacity = 0.8)
      
      # add to list of clicked
      obj_selected$ids <- c(obj_id, obj_selected$ids)
      obj_selected$names <- c(neighborhood_name, obj_selected$names)
    } else {
      proxy %>% removeShape(layerId = obj_id) %>%
        addPolygons(data = shapefile[shapefile$OBJECTID==obj_id,],
                    layerId = obj_id,
                    label = ~htmlEscape(Neighborhood),
                    stroke = FALSE, 
                    weight = 1, 
                    smoothFactor = 0.5,
                    opacity = 1.0, 
                    fillOpacity = 0.5, 
                    color = ~cpal(OBJECTID),
                    highlightOptions = highlightOptions(fillColor = "white", 
                                                        bringToFront = TRUE))
      # remove from list of clicked
      obj_selected$ids <- obj_selected$ids[obj_selected$ids!=obj_id]
      obj_selected$names <- obj_selected$names[obj_selected$names!=neighborhood_name]
    }
    })
  
  # Handle clicking on plot
  click_fun <- function(data,...){
    
    name <- data$common_name
    ids <- id_mapping$object.ID[id_mapping$common_name==name]
    
    # Map proxy
    proxy <- leafletProxy("mapmain")
    
    # if this line has already been selected, unselect it, else select it
    isolate({
      if (name %in% obj_selected$names){
        obj_selected$names <- obj_selected$names[obj_selected$names!=name]
        obj_selected$ids <- obj_selected$ids[!(obj_selected$ids %in% ids)]
        
        proxy %>% removeShape(layerId = ids) %>%
          addPolygons(data = shapefile[shapefile$OBJECTID %in% ids,],
                      layerId = ids,
                      label = ~htmlEscape(Neighborhood),
                      stroke = FALSE, 
                      weight = 1, 
                      smoothFactor = 0.5,
                      opacity = 1.0, 
                      fillOpacity = 0.5, 
                      color = ~cpal(OBJECTID),
                      highlightOptions = highlightOptions(fillColor = "white", 
                                                          bringToFront = TRUE))
      } else {
        obj_selected$names <- c(name, obj_selected$names)
        obj_selected$ids <- c(ids, obj_selected$ids)
        
        # Highlight everything in the list
        proxy %>% removeShape(layerId = ids) %>%
          addPolygons(data = shapefile[shapefile$OBJECTID %in% ids,],
                      layerId = ids,
                      color = "#e7298a",
                      label = ~htmlEscape(Neighborhood),
                      stroke = TRUE, 
                      weight = 1, 
                      smoothFactor = 0.5,
                      opacity = 0.6, 
                      fillOpacity = 0.8)
      }
    })
    
    return(NULL)
  } 
  
    output$date_shown <- renderText({
      HTML(paste("<h4>", months(ymd(input$month)), year(input$month),"</h4>", sep=" "))
    })
  
  # Plot par coord
  v <- reactive({
    y_var <- reactive(input$timeseries_var) 
    gg <- neighborhoods_norm %>%
      mutate(line_color = ifelse(common_name %in% obj_selected$names,"#e7298a",'gray')) %>%
      mutate(line_opacity = ifelse(common_name %in% obj_selected$names, 1, 0.2)) %>%
      na.omit() %>%
      filter(date == ymd(paste(year(input$month),month(input$month),1,sep='-'))) %>%
      mutate(text = ifelse(common_name %in% obj_selected$names, Neighborhood,'')) %>%
      mutate_(text_y =as.name("Days on Market")) %>%
      select(c(one_of('common_name','date','Neighborhood','line_color','line_opacity','text','text_y',
                      "Days on Market","Median Sale Price","Homes Sold","Inventory","Median Ppsf"))) %>%
      gather('Var','Value',8:12) %>%
      ggvis(x=~Var, y=~Value) %>%
      group_by(common_name) %>%
      layer_paths(opacity :=~line_opacity,
                  opacity.hover := 0.8,
                  stroke :=~line_color,
                  stroke.hover := "#FFCC00",
                  strokeWidth := 1.5, 
                  strokeWidth.hover := 4) %>%
      layer_text(x:=20, y=~text_y, text:= ~text, fontWeight:='lighter', align:='right') %>%
      scale_numeric('y',domain=c(0,1)) %>%
      add_axis("x", 
               title = "",
               properties = axis_props(grid = list(strokeWidth = 2),
                                       axis = list(stroke = NULL),
                                       ticks = list(stroke = NULL),
                                       labels = list(angle = 45, align = "left",
                                                     baseline = "middle", fontSize=12))) %>%
      add_axis("y", 
               title = "",
               values=c(0,1),
               properties = axis_props(grid = list(stroke = NULL),
                                       axis = list(stroke = NULL),
                                       ticks = list(stroke = NULL),
                                       labels = list(text = c('')))) %>%
      add_tooltip(tooltip_fun, on='hover') %>%
      set_options(width = "auto", height = 600) %>%
      handle_click(click_fun)
    
    return(gg)
  })
  
  v %>% bind_shiny("parcoord")
  
})