library(shiny)
library(ggvis)
library(dplyr)
library(tidyr)
library(rgdal)
library(leaflet)
library(htmltools)
library(lubridate)

# Define functions
source('helper_functions.R',local = TRUE)

# Import data
source('import_data.R',local = TRUE)

shinyServer(function(input, output) {
  
  ######### CHLOROPLETH ############
  
  last_clicked <- reactiveValues(obj_id = 'none')
  
  cdata <- eventReactive(input$map_color,{
    shapefile@data[[input$map_color]]
  })
  
  cpal <- eventReactive(input$map_color,{
    colorQuantile("PuRd", domain = cdata())
  })
  
  # Plot main map
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
  
  # Change selection and print HTML on click
  
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
  
  ######### TIMESERIES ############
  
  # Define color palette for selector map
  cpal_select <-  colorFactor(c('#d9d9d9','#bdbdbd','#969696','#737373','#525252','#252525'), 
                       shapefile@data[['OBJECTID']])
  
  # Create select map
  output$map_select_timeseries <- renderLeaflet({
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
                  color = ~cpal_select(OBJECTID),
                  highlightOptions = highlightOptions(fillColor = "white", 
                                                      bringToFront = TRUE))
  })
  
  # Code to handle selecting on timeseries map
  obj_selected_timeseries <- reactiveValues(ids = c(), names=c())
  observeEvent(input$map_select_timeseries_shape_click,{
    handle_map_select_click(mapname='map_select_timeseries', 
                            click_event = input$map_select_timeseries_shape_click, 
                            obj_selected = obj_selected_timeseries,
                            cpal=cpal_select)
  })
  
  # Handle clicking on timeseries plot
  click_fun_timeseries <- function(data,...){
    handle_line_click(data=data, 
                      mapname='map_select_timeseries',
                      obj_selected = obj_selected_timeseries, 
                      cpal=cpal_select)
  }
  
  # Plot time series
  v_timeseries <- reactive({
    y_var <- reactive(input$timeseries_var) 
    gg_timeries <- neighborhoods %>%
      mutate(line_color = ifelse(common_name %in% obj_selected_timeseries$names,"#e7298a",'gray')) %>%
      mutate(line_opacity = ifelse(common_name %in% obj_selected_timeseries$names,1, 0.2)) %>%
      mutate(text = ifelse(common_name %in% obj_selected_timeseries$names, 'Neighborhoods','')) %>%
      na.omit() %>%
      filter((date > ymd(input$dates[1])) & (date < ymd(input$dates[2]))) %>% 
      ggvis(x=~date, prop("y", as.name(y_var()))) %>%
      group_by(common_name) %>%
      layer_lines(opacity :=~line_opacity,
                  opacity.hover := 0.8,
                  stroke :=~line_color,
                  stroke.hover := "#FFCC00",
                  strokeWidth := 1.5, 
                  strokeWidth.hover := 4) %>%
      add_tooltip(tooltip_fun, on='hover') %>%
      scale_datetime('x', nice = "month") %>%
      add_axis("x", title = "Time", subdivide = 12,  title_offset = 60,
               properties = axis_props(
                 labels = list(angle = 45, align = "left",baseline = "middle", fontSize=12),
                 title = list(fontSize=16))) %>%
      set_options(width = "auto", height = "auto") %>%
      handle_click(click_fun_timeseries)
    
    # Fix formatting for median
    if (y_var()=='Median Sale Price') {
      gg_timeries <- gg_timeries %>%
        add_axis("y", title = y_var(), title_offset = 90, format="$,",
                 properties = axis_props(labels = list(fontSize=12),
                                         title = list(fontSize=16),
                                         axis = list(stroke = NULL)))
    } else {
      gg_timeries <- gg_timeries %>%
        add_axis("y", title = y_var(), title_offset = 60,
                 properties = axis_props(labels = list(fontSize=12),
                                         title = list(fontSize=16),
                                         axis = list(stroke = NULL)))
    }
    return(gg_timeries)
  })
  
  v_timeseries %>% bind_shiny("timeseries")
  
  ######### PARACOORD ############
  
  # Create select map
  output$map_select_parcoord <- renderLeaflet({
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
                  color = ~cpal_select(OBJECTID),
                  highlightOptions = highlightOptions(fillColor = "white", 
                                                      bringToFront = TRUE))
  })
  
  # Code to handle selecting on parcoord map
  obj_selected_parcoord <- reactiveValues(ids = c(), names=c())
  observeEvent(input$map_select_parcoord_shape_click,{
    handle_map_select_click(mapname='map_select_parcoord', 
                            click_event = input$map_select_parcoord_shape_click, 
                            obj_selected = obj_selected_parcoord,
                            cpal=cpal_select)
  })
  
  # Handle clicking on parcoord plot
  click_fun_parcoord <- function(data,...){
    handle_line_click(data=data, 
                      mapname='map_select_parcoord',
                      obj_selected = obj_selected_parcoord, 
                      cpal=cpal_select)
    }
  
  # Show data selected
  output$date_shown <- renderText({
    HTML(paste("<h4>", months(ymd(input$month)), year(input$month),"</h4>", sep=" "))
  })
  
  # Plot par coord
  v_paracoord <- reactive({
    y_var <- reactive(input$timeseries_var) 
    gg_paracoord <- neighborhoods_norm %>%
      mutate(line_color = ifelse(common_name %in% obj_selected_parcoord$names,"#e7298a",'gray')) %>%
      mutate(line_opacity = ifelse(common_name %in% obj_selected_parcoord$names, 1, 0.2)) %>%
      na.omit() %>%
      filter(date == ymd(paste(year(input$month),month(input$month),1,sep='-'))) %>%
      mutate(text = ifelse(common_name %in% obj_selected_parcoord$names, Neighborhood,'')) %>%
      mutate_(text_y =as.name("Median Ppsf")) %>%
      select(c(one_of('common_name','date','Neighborhood','line_color','line_opacity','text','text_y',
                      "Median Ppsf","Median Sale Price","Homes Sold","Inventory","Days on Market"))) %>%
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
      handle_click(click_fun_parcoord)
    return(gg_paracoord)
  })
  
  v_paracoord %>% bind_shiny("parcoord")
  
})