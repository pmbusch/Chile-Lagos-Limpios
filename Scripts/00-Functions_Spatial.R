# Functions to create interactive maps using Leaflet
# 
# PBH March 2022


## Spatial Interactive Visualization function ----
# Create spatial layer map with labels, to quickly visualize it
# Inputs: 
# - sf object (spatial vector)
# - label vector: Label to display on the map
# - polygon form: "a" for polygon area (default), "p" for point and "l" for line
f.interactive.map <- function(map_object, label_vector, polygon_form="a",...){
  
  # Create labels for the vectors
  labels <- sprintf(
    "<strong>%s</strong><br/>",
    label_vector) %>% 
    lapply(HTML)
  
  polygon_form <- str_to_lower(polygon_form)
  
  # create interactive map object
  if (polygon_form=="a"){
    map_interactive <- leaflet(map_object) %>% 
      addTiles() %>% 
      addPolygons(label = labels,...)
  } else if (polygon_form=="p") {
    map_interactive <- leaflet(map_object) %>% 
      addTiles() %>% 
      addCircles(label = labels,...)
  } else {
    map_interactive <- leaflet(map_object) %>% 
      addTiles() %>% 
      addPolylines(label = labels,...)
  }
  # return map
  return(map_interactive)
}


# Function to create popup for the spatial layers ------
# Inputs:
# - Layer: sf object, to get the feature
# - Title of the layer
# - Features: Vector of character of features to print
# - Source: Source of the layer, to include in the map
f.create.labels <- function(layer,title, features, source=""){
  
  # Extract features to print from the layer
  features_data <- layer %>% as.data.frame() %>% dplyr::select(features)
  
  # Incorporate column name into the feature to print
  feat <- sapply(colnames(features_data), 
                 function(x) sprintf("<i>%s</i>: %s",
                                     x,features_data[,x])) %>%
    as.data.frame()
  # Merge all columns into a single text, separated by a line break
  feat2 <- unite(feat,"x",colnames(feat),sep="<br/>")
  
  # Add title
  feat2$x <- paste0("<strong>",title,"</strong><br/>",feat2$x)
  
  # Add source
  source <- ifelse(source=="","",
                   paste0("<br/> <i>Fuente: ",source,"</i>"))
  feat2$x <- paste0(feat2$x,source)
  
  # Convert to HTML to incorporate into the map
  label <- feat2$x %>% as.list() %>% lapply(HTML)
  
  # return
  return(label)
}

## Function to add layers with a specific format
# - layer: sf object (spatial vector)
# - Title: Title to display on the map
# - polygon form: "a" for polygon area (default), "p" for point and "l" for line


label_options <- labelOptions(
  style = list("font-weight" = "normal", padding = "3px 8px"),
  textsize = "12px",
  direction = "auto")

add.Layer <- function(map,layer,
                      title,features,
                      source="",
                      polygon_form="a",
                      group_l="default",
                      color="black",
                      pos_leg="bottomleft",
                      ...){
  
  group_l <- if (group_l=="default") title else group_l
  polygon_form <- str_to_lower(polygon_form)
  label_out <- f.create.labels(layer,title,features,source = source)
  
  # feature to return
  feat_return <- map
  
  if (polygon_form=="a"){
    feat_return <- addPolygons(map,data=layer,
                label=label_out,
                group=group_l,
                labelOptions = label_options,
                color = color,
                ...)
  } else if(polygon_form=="p"){
    feat_return <- addCircles(map,data=layer,
               label=label_out,
               group=group_l,
               labelOptions = label_options,
               color = color,
               ...)
  } else {
    feat_return <- addPolylines(map,data=layer,
                 label=label_out,
                 group=group_l,
                 labelOptions = label_options,
                 color = color,
                 ...)
  } 
  
  # return with legend
  feat_return  %>% 
    addLegend(values = 1, group = group_l,
              position = pos_leg, labels = group_l,
              colors= color)
}






# EoF