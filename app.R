library(shiny)
library(leaflet)        # For GIS
library(rgdal)          
library(plyr)           # For data processing 
library(reshape2)  
library(SPARQL)         # To return data from API

library(SPARQL)

endpoint <- 'http://statistics.gov.scot/sparql.csv'
query <- 'PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX dim: <http://statistics.gov.scot/def/dimension/>
PREFIX sdmx: <http://purl.org/linked-data/sdmx/2009/dimension#>

SELECT ?year ?age ?gender ?servicesperformance ?simd ?urbanrural ?area ?95lowerCI ?percent ?95upperCI

WHERE { graph <http://statistics.gov.scot/graph/local-authority-services-and-performance---shs> {
?s qb:dataSet <http://statistics.gov.scot/data/local-authority-services-and-performance---shs>;
dim:age ?ageURI;
sdmx:refPeriod ?yearURI;
dim:gender ?genderURI;
dim:localAuthorityServicesAndPerformance ?servicesperformanceURI;
dim:simdQuintiles ?simdURI;
dim:urbanRuralClassification ?urbanruralURI;
<http://statistics.gov.scot/def/measure-properties/95-lower-confidence-limit-percent> ?95lowerCI;
sdmx:refArea ?areaURI.

?t qb:dataSet <http://statistics.gov.scot/data/local-authority-services-and-performance---shs>;
dim:age ?ageURI;
sdmx:refPeriod ?yearURI;
dim:gender ?genderURI;
dim:localAuthorityServicesAndPerformance ?servicesperformanceURI;
dim:simdQuintiles ?simdURI;
dim:urbanRuralClassification ?urbanruralURI;
<http://statistics.gov.scot/def/measure-properties/95-upper-confidence-limit-percent> ?95upperCI;
sdmx:refArea ?areaURI.

?u qb:dataSet <http://statistics.gov.scot/data/local-authority-services-and-performance---shs>;
dim:age ?ageURI;
sdmx:refPeriod ?yearURI;
dim:gender ?genderURI;
dim:localAuthorityServicesAndPerformance ?servicesperformanceURI;
dim:simdQuintiles ?simdURI;
dim:urbanRuralClassification ?urbanruralURI;
<http://statistics.gov.scot/def/measure-properties/percent> ?percent;
sdmx:refArea ?areaURI.
}

?ageURI rdfs:label ?age.
?yearURI rdfs:label ?year.
?genderURI rdfs:label ?gender.
?servicesperformanceURI rdfs:label ?servicesperformance.
?simdURI rdfs:label ?simd.
?urbanruralURI rdfs:label ?urbanrural.
?areaURI rdfs:label ?area.
}'

rawData <- SPARQL::SPARQL(endpoint, query, format = "csv")
results <- rawData$results

selectList <-unique(results[4])

DateCodeUnsorted <- unique(Unfiltered["year"])
DateCode <- DateCodeUnsorted[order(unique(Unfiltered["year"])),]

FeatureCodeUnsorted <- unique(Unfiltered["area"])
FeatureCode <- FeatureCodeUnsorted[order(unique(Unfiltered["area"])),]

byYearColoursMap <- c("#000000", "#008080","#6cafe1",  "#ccccff")

ui <- fluidPage(
    
    titlePanel("SHS Map Test"),
    
    sidebarLayout(
        
        sidebarPanel(
            selectInput("DateCodeBC",
                        "Year:",
                        choices=sort(DateCode, decreasing = TRUE),
                        selected = max(DateCode)),
            
            selectInput("LocalAuthorityServicesAndPerformanceBC",
                        "Statement:",
                        choices=LocalAuthorityServicesAndPerformance)
        ),
        
        mainPanel(
            leafletOutput("map")
        )
    )
)

server <- function(input, output) {
    
    SHS_BC_Scotland <- reactive ({
        
        Filtered_Scotland <- Unfiltered %>% filter(age == "All"
                                                   & servicesperformance == input$"LocalAuthorityServicesAndPerformanceBC"
                                                   & gender == "All"
                                                   & area == "Scotland"
                                                   & urbanrural == "All" 
                                                   & simd == "All"
                                                   & year == input$"DateCodeBC")
        
        Filtered_Scotland$percent <- round(Filtered_Scotland$percent, digits = 0)
        Filtered_Scotland$X95upperCI <- round(Filtered_Scotland$X95upperCI, digits = 1)
        Filtered_Scotland$X95lowerCI <- round(Filtered_Scotland$X95lowerCI, digits = 1)
        
        Filtered_Scotland
    })
    
    SHS_BC <- reactive ({
        
        Filtered <- Unfiltered %>% filter(age == "All"
                                          & servicesperformance == input$"LocalAuthorityServicesAndPerformanceBC"
                                          & gender == "All"
                                          & area != "Scotland"
                                          & urbanrural == "All" 
                                          & simd == "All"
                                          & year == input$"DateCodeBC")
        
        Filtered$percent <- round(Filtered$percent, digits = 0)
        Filtered$X95upperCI <- round(Filtered$X95upperCI, digits = 1)
        Filtered$X95lowerCI <- round(Filtered$X95lowerCI, digits = 1)
        
        df <- data.frame(Filtered$year
                         , Filtered$servicesperformance
                         , Filtered$percent
                         , Filtered$X95upperCI
                         , Filtered$X95lowerCI
                         , Filtered$area
        )
        
        SHS_BC <- rename(df, c("Filtered.year" = "Year"
                               , "Filtered.age" = "Age"
                               , "Filtered.gender" = "Gender"
                               , "Filtered.percent" = "Percent"
                               , "Filtered.simd" = "SIMDQuintiles"
                               , "Filtered.urbanrural" = "UrbanRuralClassification"
                               , "Filtered.servicesperformance" = "Statement"
                               , "Filtered.area" = "Area"
                               , "Filtered.X95upperCI" = "UpperConfidenceLimit"
                               , "Filtered.X95lowerCI" = "LowerConfidenceLimit"))
        
        SHS_BC <- cbind(StatisticallySignificant = "None", SHS_BC)
        
        levels(SHS_BC$StatisticallySignificant) <-c("None", "Significantly Higher", "Significantly Lower", "No Significant Difference")
        
        SHS_BC[which(SHS_BC$LowerConfidenceLimit > SHS_BC_Scotland()$X95upperCI), "StatisticallySignificant"] <- "Significantly Higher"
        
        SHS_BC[which(SHS_BC$UpperConfidenceLimit < SHS_BC_Scotland()$X95lowerCI), "StatisticallySignificant"] <- "Significantly Lower"
        
        SHS_BC[which(SHS_BC$StatisticallySignificant == "None"), "StatisticallySignificant"] <- "No Significant Difference"
        
        SHS_BC
    })
    
    output$map <- renderLeaflet({
        
        mapData <- merge(scotLocAuth,
                         SHS_BC(),
                         by.x = "lad16nm",
                         by.y = "Area")
        
        factpal=colorFactor(palette = byYearColoursMap, domain=SHS_BC()$StatisticallySignificant)
        
        labels <- sprintf("<p>Value: %g&#37;<br/>
                    Upper Confidence Limit: %g&#37;<br/>
                    Lower Confidence Limit: %g&#37;<br/>
                    %s, %s<br/>
                    </p>",
                          mapData$Percent, 
                          mapData$UpperConfidenceLimit,
                          mapData$LowerConfidenceLimit,
                          mapData$lad16nm,
                          mapData$Year
        ) %>% lapply(htmltools::HTML)
        
        leaflet(mapData) %>% 
            
            setView(lng = -3.5, lat = 56.817, zoom = 4.5) %>% 
            
            addPolygons(color = "#000000",
                        weight = 0.4,
                        smoothFactor = 0.5,
                        opacity = 1,
                        fillOpacity = 1,
                        fillColor = ~factpal(StatisticallySignificant),
                        highlightOptions = highlightOptions(color = "#000000", weight = 1, bringToFront = TRUE),
                        label = labels,
                        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), 
                                                    textsize = "12px", direction = "auto")) %>%
            
            addLegend(pal=factpal,
                      values= ~StatisticallySignificant,
                      title = NULL,
                      opacity = 1)
    })
    
}

shinyApp(ui, server)