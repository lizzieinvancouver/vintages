Notes on extracting climate data from Daymet for Geoff's wine raitings analysis
-------------------------------------------------------------------------------
----------------------------------------------------------------------------

started by Faith Sep 24 2021


We decided to use Daymet because:
- there is precipitation data 
- the data runs from 1980 to now, so is the right years
- it's easier to download manageable amounts of data

Faith downloaded California data in Sep 2021

Climate Variables
--------------------
minimum temperature
maximum temperature
precipitation 


Choosing shapefiles
-----------------
Nacho sent us a shapefile with winegrowing regions for all of North America. Faith selected the one labeled Napa for the Napa data, although this region doesn't include sub AVAs that are often considered part of the greater Napa AVA. 

Processing maps
----------------

Read in shapefile and rasters
Change Raster protection to lat/long
Crop and then Mask the raster using the shapefile
Get mean daily min and max data
Calculate daily mean values for each raster shape
