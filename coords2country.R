# turn coords into a country code

# https://stackoverflow.com/a/14342127/513463
coords2country = function(points){  
  # countriesSP <- rworldmap::getMap(resolution='low')
  countriesSP <- rworldmap::getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  # convert our list of points to a SpatialPoints object
  # pointsSP = sp::SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  #setting CRS directly to that from rworldmap
  pointsSP = sp::SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = sp::over(pointsSP, countriesSP)
  # return the ADMIN names of each country
  # indices$ADMIN  
  indices$ISO3 # returns the ISO3 code
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}

