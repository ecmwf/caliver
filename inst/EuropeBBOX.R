## Workflow: visualise results for the European bounding box
# All the maps created with this package can be generated for a particular region.
# The example below shows how to plot fwi percentiles for the European bounding box

# Define the extent
e <- regionalBBOX(region = "Europe")
# Crop the global file to Europe's extent
croppedStack <- raster::crop(maskedMaps, e)

# In Europe the saturation is expected to be below 50, therefore we tweak the map to ignore higher values
# croppedStack[croppedStack > 50] <- NA
p <- rasterVis::levelplot(croppedStack, 
                          col.regions = rev(grDevices::heat.colors(20)),
                          colorkey = list(space = "right"), 
                          names.attr = ifiles)
# Define a background map
backgroundMap <- rworldmap::getMap(resolution = "low")
p + latticeExtra::layer(sp::sp.lines(backgroundMap, lwd=0.8, col='darkgray'))
