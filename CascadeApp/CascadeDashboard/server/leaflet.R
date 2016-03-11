LoadCountryMapData <- function() {
    world <- rgdal::readOGR(dsn = "server/data/leaflet-countries/ne_50m_admin_0_countries.dbf", layer = 'ne_50m_admin_0_countries', encoding = 'UTF-8')
    relevantCountries <- subset(world, name %in% LeafletCountryList)
    relevantCountries
}

observeEvent(input$countryMap_shape_click, {
    updateSelectInput(session, inputId = "selectCountry", selected = input$countryMap_shape_click)
})

output$countryMap <- renderLeaflet({
    mapData <- LoadCountryMapData()

    leaflet() %>%
    addProviderTiles("Hydda.Base") %>%
    # addProviderTiles("MapQuestOpen.Aerial") %>%
    setView(lng = 0, lat = 30, zoom = 2) %>%
    addPolygons(
        data = mapData,
        layerId = mapData$name,
        weight = 3,
        stroke = TRUE,
        color = "green",
        popup = mapData$name
    )
})
