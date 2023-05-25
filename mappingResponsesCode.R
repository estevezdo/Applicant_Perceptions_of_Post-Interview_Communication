library(tidyverse)
library(sf)
library(mapview)

###Mapping Responses
sveyResponse <- read_csv("/codedDatabase.csv")

sveyResponse_no_na = sveyResponse[complete.cases(sveyResponse[, c("LocationLongitude", "LocationLatitude")]), ]

mapview(sveyResponse_no_na, xcol = "LocationLongitude", ycol = "LocationLatitude", crs = 4269, grid = FALSE)

#Alternative: Transform data to Spatial object
SurveyResponses <- st_as_sf(sveyResponse_no_na, coords = c("LocationLongitude", "LocationLatitude"),  crs = 4326)
#mapview(sbux_sf)
mapview(SurveyResponses, map.types = "Stamen.Toner") 
