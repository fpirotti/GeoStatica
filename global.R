if (!require("pacman")) install.packages("pacman")

# library()
packages <- c("leaflet", "shiny", "shinydashboard", "tippy",
              "mapview", "sf", "ggplot2","sjPlot", "rgl", "shinycssloaders",
              "sp", "car", "gstat","shinyWidgets","raster","spdep",
              "nabor","htmlwidgets", "spatstat","shinyjs")
allOk <- pacman::p_load(char=packages)

notOK <- which( !unname(allOk) )
if(length(notOK)>0){
  stop("Alcune librerie non sono presenti! Verifica che le seguenti librerie siano correttamente installate....",
       sprintf("\n - %s  ", names(allOk)[notOK] ))
}



mycolb <- rgb(0,0,255, alpha = 125,max=255)#blu
mycolr <- rgb(255,0,0, alpha = 125,max=255)#rosso
mycolg <- rgb(0,255,0, alpha = 125,max=255)#verde
mycolc <- rgb(255,165,0, alpha = 125,max=255)#magenta

TempModis <- terra::rast("data/VenetoCorrectedMODIS_LST_Avg2017.tif")
TempDEM <- terra::rast("data/VenetoDEM.tif")
veneto <- sf::read_sf("data/veneto.gpkg")

#
# if(file.exists("data/data.rda")){
#   load(file="data/data.rda")
# } else {
  map <-#   mapview::mapview(veneto, hide=T) +
    mapview::mapview( raster::raster(TempModis),   layer.name="Temperature", query.digits=0 ) +
    mapview::mapview( raster::raster(TempDEM),   hide=T,  layer.name="Elevation", query.digits=0)
  map.map <- map@map %>%
    htmlwidgets::onRender("
    function(el, x) {
      this.on('click', function(e) {
        var lat = e.latlng.lat.toFixed(6);
        var lng = e.latlng.lng.toFixed(6);

        // Show popup at click
        L.popup()
          .setLatLng(e.latlng)
          .setContent('Geographic coords (EPSG:4326):<br/>Lon: ' + lng + ', Lat: ' + lat)
          .openOn(this);

        // Here’s the fun: send coords back to Shiny or R console if you want transforms
        if (typeof Shiny !== 'undefined') {
          Shiny.setInputValue('map_click', {lat: lat, lng: lng}, {priority: 'event'});
        }
      });
    }
  ")
  # save(map.map, file="data/data.rda")
# }


logit <- function(text, type="message"){
  colo <- "black"
  if(type=="warning"){
    colo <- "orange"
  } else if(type=="error"){
    colo <- "red"
  }
  command <- sprintf('$("#logdiv").append("<b style=\\"color:%s\\">%s</b>: %s<br>");', colo,
                     format(Sys.time(), "%b %d - %X"),
                     text )

  shinyjs::runjs( command )

  shinyjs::runjs( "$('#logdiv').scrollTop($('#logdiv')[0].scrollHeight);" )
#


}

#l1<-list( sf::as_Spatial(veneto),pch=16,col=2)
# er <- tryCatch({ load(file="data/data.rda") },
#                 error = function(e){
#                   print("dd")
#                 },
#                warning = function(e){
#                 message("loading all")
#                  map <- mapview::mapview(veneto, hide=T) +
#                    mapview::mapview( raster::raster(TempModis),   layer.name="Temperature", query.digits=0 ) +
#                    mapview::mapview(raster::raster(TempDEM),   hide=T,  layer.name="Quota", query.digits=0)
#
#                  save(map, file="data/data.rda")
#
#                })



