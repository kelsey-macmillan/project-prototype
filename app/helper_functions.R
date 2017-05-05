tooltip_fun <- function(x) {
  if(is.null(x)) return(NULL)
  n <- neighborhoods[neighborhoods$common_name ==x$common_name,]$Neighborhood[1]
  paste(n)
}



normalize <- function(x){
  return((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))}



handle_map_select_click <- function(mapname, click_event, obj_selected, cpal){
  
  # Map proxy
  proxy <- leafletProxy(mapname)
  
  # Get neighborhood clicked
  obj_id <- click_event$id
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
}



handle_line_click <- function(data, mapname, obj_selected, cpal){
  
  name <- data$common_name
  ids <- id_mapping$object.ID[id_mapping$common_name==name]
  
  # Map proxy
  proxy <- leafletProxy(mapname)
  
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