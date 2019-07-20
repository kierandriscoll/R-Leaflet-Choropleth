# Generate a Choropleth ---------------------------------

library(dplyr)
library(tools)
library(readxl)
library(readr)
library(sp)
library(leaflet)
library(geojsonio)
library(htmltools)


# PARAMETERS ---------------------------------

data_filepath <- "stats_by_la.csv"
code_column <- "lad16cd"  # The name of the column that contains the ONS area code
stat_column <- "values"   # The name of the column that contains the statistics to map

legend_title <- ""
bin_range <- c(500,1000,1500,2000,3000,4000,5000)
colour <- "Purples"  # or use "Greens" "Oranges" "Blues" "Reds" "PuBuGn" "YlOrRd"
format <- "?"        # or "%" or "?"

boundary <- "https://raw.githubusercontent.com/kierandriscoll/UK-Topojson/master/Local-Authorities/Local_Auths_Dec16_Gen_Clip_GB.json"
ons_code <- "lad16cd"   # The boundary data feature that contains the ONS area code
ons_name <- "lad16nm"   # The boundary data feature that contains the ONS area name



# Import Stat dataset ------------------------------------

import_data <- 
if (tools::file_ext(data_filepath) %in% c("csv", "txt") )  {
  
  read.csv(data_filepath, stringsAsFactors = FALSE)
  
} else if (tools::file_ext(data_filepath) %in% c("xls", "xlsx") )  {
  
  readxl::read_excel(data_filepath) %>% as.data.frame() 
  
} else if (tools::file_ext(data_filepath) %in% c("rds") )  {
  
  readRDS(data_filepath) 
  
} else {
  warning("File format not compatible - Requires CVS/TXT/XLS/XLSX/RDS")
}


# Combine boundary and stats ------------------------------------

# Load the boundary file
shp <- geojsonio::geojson_read(boundary, what = "sp") # Use if boundary is JSON
#shp <- readRDS(boundary) # Use if boundary is already a spdf 

# Standardise data field names
df <- import_data %>% dplyr::rename("onscode" = code_column, "mapstat" = stat_column)

# Merge extra stats into the sp
shp <- sp::merge(shp, df, by.x = ons_code, by.y = "onscode")


# Bins and colors ------------------------------------

  bin_color <- leaflet::colorBin(colour,
                                 bins = bin_range,
                                 domain = shp$mapstat)


# Hovertip information------------------------------------  

  label_content <- lapply(seq(nrow(shp@data)), function(i) {
    
    paste0(shp@data[i, ons_code], "<br>", 
           shp@data[i, ons_name], "<br>", 
           "Value : ", shp@data[i, "mapstat"] ) 
  })


# Label formatting ------------------------------------   

  lab_input <- if (format == "None") { c("", "") } else
               if (format == "%") { c("", "%") } else
               if (format == "?") { c("&pound;", "") } 


# Draw Map ------------------------------------  

  leaflet::leaflet(shp,
                   options = leaflet::leafletOptions(zoomControl = T, zoomsnap = 0.25)) %>%
  leaflet::setView(-3.589, 54.9, zoom = 6.25) %>%
  leaflet::addTiles(urlTemplate = "", attribution = "Contains National Statistics data. Crown copyright and database right 2019") %>%
  leaflet::addPolygons(stroke = TRUE,
                       color = "black",
                       weight = 1,
                       smoothFactor = 1,
                       fillOpacity = 1,
                       fillColor = ~bin_color(mapstat),
                       label = lapply(label_content, htmltools::HTML)
  ) %>% 
  leaflet::addLegend(pal = bin_color,
                     values = ~mapstat,
                     opacity = 0.9,
                     title = legend_title,
                     labFormat = leaflet::labelFormat(prefix = lab_input[1],
                                                      suffix = lab_input[2],
                                                      between = " to "),      # for - use " &ndash; "
                     position = "bottomleft"
  )
  