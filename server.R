library(shinydashboard)
library(leaflet)
library(dplyr)
library(magrittr)
library(ggmap)
library(curl) # make the jsonlite suggested dependency explicit

function(input, output, session) {

  # load data
  # gares <- rgdal::readOGR("data/gares-du-reseau-ferre-dile-de-france/gares-du-reseau-ferre-dile-de-france.shp")
  # socio <- rgdal::readOGR("data/DATA/socioprofessionel.shp")
  # rural <- rgdal::readOGR("data/zonage/74_rural.shp")
  # urbain <- rgdal::readOGR("data/zonage/paris_urbain.shp")
  # zone <- sp::rbind.SpatialPolygonsDataFrame(rural, urbain)
  # zone$segment <- factor(x = c("rural", "urbain"))
  # bus <- rgdal::readOGR("data/depots_bus/depots_bus.shp")
  # bornes <- rgdal::readOGR("data/bornes_recharges_elec/IRVE-201605-1-_epsg3857.shp")
  # save(list = c("gares", "socio", "zone", "bus", "bornes"),
  #      file = "national_bin")
  load("national_bin")
  
  socio$segment <- factor(unlist(lapply(lapply(strsplit(x = as.character(socio$segment), split = "_"), tail, -1), paste, collapse = " ")))
  pal_socio <- colorFactor(heat.colors(5), socio$segment)

  pal_zone <- colorFactor(palette = c("darkgreen", "darkgrey"), domain = zone$segment)
  pariscenter <- list(lat = 48.822429, lng = 2.314383)
  savoiecenter <- list(lat = 46.076489, lng = 6.257019)

  ratpIcon <- makeIcon(
    iconUrl = "data/icons/Sigle-RATP.png",
    iconWidth = 28, iconHeight = 28,
    iconAnchorX = 22, iconAnchorY = 94
  )

  bornesIcon <- makeIcon(
    iconUrl = "data/icons/voiture-electrique-logo.png",
    iconWidth = 28, iconHeight = 14,
    iconAnchorX = 22, iconAnchorY = 94
  )
  
  garesIcon <- makeIcon(
    iconUrl = "data/icons/sncf_logo2005-940x510-383x208.png",
    iconWidth = 28, iconHeight = 15,
    iconAnchorX = 22, iconAnchorY = 94
  )
  
  # load urbain
  # bt_aerien_urbain <- rgdal::readOGR("data/urbain/bt_aerien_2.shp")
  # bt_sout_urbain <- rgdal::readOGR("data/urbain/bt_sout_2.shp")
  # # building_urbain <- rgdal::readOGR("data/urbain/building_urbain.shp")
  # conso_elect_urbain <- rgdal::readOGR("data/urbain/consoElec_urbain.shp")
  # grdf_cana_urbain <- rgdal::readOGR('data/urbain/GRDF_CANA_PARIS_final_2.shp')
  # hta_sout_urbain <- rgdal::readOGR("data/urbain/hta_sout_2.shp")
  # iris_urbain <- rgdal::readOGR("data/urbain/iris_urbain.shp")
  # parking_urbain <- rgdal::readOGR("data/urbain/parking_urbain.shp")
  # recens_urbain <- rgdal::readOGR("data/urbain/recens_urbain.shp")
  # # routes_paris <- rgdal::readOGR("data/urbain/routes_paris.shp")
  # 
  # save(list = c("bt_aerien_urbain",
  #               "bt_sout_urbain",
  #               "conso_elect_urbain",
  #               "grdf_cana_urbain",
  #               "hta_sout_urbain",
  #               "iris_urbain",
  #               "parking_urbain",
  #               "recens_urbain"),
  #     file = "urbain_bin")
  load("urbain_bin")

  # load rural
  # # building_rural <- rgdal::readOGR("data/rural/building_rural.shp")
  # iris_rural <- rgdal::readOGR("data/rural/iris_rural.shp")
  # recens_rural <- rgdal::readOGR("data/rural/recens_rural.shp")
  # routes_rural <- rgdal::readOGR("data/rural/routes_rural.shp")
  # 
  # save(list = c("iris_rural", "recens_rural", "routes_rural"),
  #      file = "rural_bin")
  load("rural_bin")
  
  # render map
  output$map <- renderLeaflet({
    token = "pk.eyJ1IjoiY2xlbWxhZmxlbW1lIiwiYSI6ImNqNGlqdGZmOTA5MWwyd283dG9pdDUxdzUifQ.3MfyxhnJO0oMIAq6TVkuLw"

    recens_urbain$jeune <- (as.numeric(as.character(recens_urbain$POP_18_24)) + as.numeric(as.character(recens_urbain$POP_25_39))) / as.numeric(as.character(recens_urbain$POP))
    conso_elect_urbain$conso <- as.numeric(as.character(conso_elect_urbain$energie_BT.1)) + as.numeric(as.character(conso_elect_urbain$energie_BT.2)) + as.numeric(as.character(conso_elect_urbain$energie_HT))
    
    leaflet() %>%
      addTiles(
        urlTemplate = paste0("//{s}.tiles.mapbox.com/v4/mapbox.light/{z}/{x}/{y}.png?access_token=", token),
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lat = 46.84, lng = 2.42, zoom = 6) %>%
      
      # add urbain polylines
      addPolylines(data = bt_aerien_urbain, group = "bt") %>%
      addPolylines(data = bt_sout_urbain, group = "bt") %>%
      addPolylines(data = grdf_cana_urbain, group = "grdf") %>%
      addPolylines(data = hta_sout_urbain, group = "hta") %>%

      # addPolygons(data = iris_urbain) %>%
      # add urban polygons
      addPolygons(data = parking_urbain, group = "parking") %>%
      addPolygons(data = recens_urbain, group = "recens",
                  fillColor = ~colorNumeric("Reds", domain = c(0, 1))(jeune),
                  stroke = FALSE, fillOpacity = 0.8,
                  popup = ~paste("Pourcentage de la population sous 40 ans :", 100*round(jeune,digits = 2))) %>%

      # add urban dots
      addCircles(data = conso_elect_urbain, group = "conso", 
                 popup = ~paste(conso, "kWh/an"),
                 color = ~colorNumeric("Blues", domain = NULL)(conso)) %>%

      # add national data
      addPolygons(data = zone,
                  fillOpacity = 0, stroke = TRUE, color = ~pal_zone(segment), fill = FALSE, group = "zone") %>%
      addPolygons(data = socio,
                  fillColor = ~pal_socio(segment), popup = socio$segment, stroke = FALSE, color = "white", group = "socio") %>%
      addMarkers(data = bus, group = "ratp",icon = ratpIcon) %>%
      addMarkers(data = bornes, group = "bornes", icon = bornesIcon) %>%
      addMarkers(data = gares, group = "gares", icon = garesIcon) %>%

      # add rural polylines
      addPolygons(data = recens_rural, group = "recens") %>%
      addPolylines(data = routes_rural, group = "route")
  })
  
  groups <- c("bt", "grdf", "hta", "parking", "recens", "conso", "zone", "socio", "ratp", "bornes", "gares", "route")
  # Observe update map
  observe({
    # toggle layers
    map_tmp <- leafletProxy('map') %>% hideGroup(group = groups)
    for(g in groups){
      if(g %in% input$selectLayer) map_tmp %<>% showGroup(g)
    }
    map_tmp
  })
  
  # observe slide plugs
  observeEvent(input$numberPlugs,{
    # add plugs
    N <- input$numberPlugs
    bound <- input$map_bounds
    # lat_range = abs(bound$south - bound$north)
    # lng_range <- abs(bound$west - bound$east)
    # 
    
    if(N > 0 && !is.null(bound)){
      # plugs <- DiceDesign::discrepESE_LHS(matrix(runif(2*N), 10, 2))$design
      # plugs <- t(plugs)*range + start
      clat <- runif(N, min = bound$south, max = bound$north)
      clng <- runif(N, min = bound$west, max = bound$east)
      hash <- sapply(clat*clng, digest::digest)
      leafletProxy('map') %>% clearGroup(group = "togglePlugs") %>%
        addMarkers(lat = clat,
                   lng = clng, group = "togglePlugs",
                   layerId = hash)
    } else {
      leafletProxy('map') %>% clearGroup(group = "togglePlugs")
    }
  })
  
  # observe the marker click
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    id <- click$id
    leafletProxy("map") %>% removeMarker(layerId = id)
  })
  
  # observe clear button
  observeEvent(input$removePlugs, {
    leafletProxy('map') %>% clearGroup(group = "togglePlugs") %>% clearGroup(group = "manualPlugs")
  })
  
  # Observe click to add marker
  observeEvent(input$map_click, {
    ## Get the click info like had been doing
    click <- input$map_click
    clat <- click$lat
    clng <- click$lng
    hash <- digest::digest(clng*clat)
    
    leafletProxy('map') %>% # use the proxy to save computation
      addMarkers(lng=clng, lat=clat, layerId = digest::digest(clng*clat), group = "manualPlugs")
  })
  
  # Observe toggle legend
  observeEvent(input$showLegend, {
    if(input$showLegend){
      leafletProxy('map') %>%
        addLegend(position = "bottomright", pal = pal_socio, values = socio$segment,
                  title = "Cat. socio-professionnelle",
                  opacity = 0.6) %>%
        addLegend(position = "bottomright", pal = pal_zone, values = zone$segment,
                  title = "Zone",
                  opacity = 0.6)
    } else {
      leafletProxy('map') %>% clearControls()
    }
  })
  
  # Observe zoom click
  observeEvent(input$showParis,{
    leafletProxy('map') %>% setView(lat = pariscenter$lat, lng = pariscenter$lng, zoom = 13)
  })
  observeEvent(input$showSavoie,{
    leafletProxy('map') %>% setView(lat = savoiecenter$lat, lng = savoiecenter$lng, zoom = 11)
  })
}
