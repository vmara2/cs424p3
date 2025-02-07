library(shiny)
library(shinydashboard)
library(ggplot2)
library(mapview)
library(leaflet)
library(leafpop)
library(tigris)

mapviewOptions(default=TRUE)
# ---- Helper Functions ----

# when there's multiple buildings in one block,
# this function labels the type as multiple for 
# easier aggregation
multiples <- function(dataset){
    # creates table with blocks containing multiples
    dups <- data.frame(table(dataset$CENSUS.BLOCK))
    dups <- dups[dups$Freq > 1,]
    
    # for every block in the table, change
    # building type to multiple
    for(i in 1:nrow(dataset)) {
        if(dataset[i,2] %in% dups$Var1){
            dataset[i,3] = "Multiple"
            dataset[i, 31:34] <- dataset[i, 31:34] / (dups[dups$Var1 == dataset[i,2],2])
        }
    }
    
    return(dataset)
}

# prepared the data so it can be used on the map
prepareData <- function(dataset, community){
    dataset <- subset(chicago_data, chicago_data$COMMUNITY.AREA.NAME == community)
    dataset <- multiples(dataset)
    dataset <- aggregate(.~COMMUNITY.AREA.NAME+CENSUS.BLOCK+BUILDING.TYPE, data=dataset, sum, na.action = na.pass)
    
    return(dataset)
}
# here you pass already processed data in order to produce data ready 
# to show on the interface
interfaceData <- function(dataset) {
    electric <- data.frame(colSums(dataset[, 4:15], na.rm = TRUE))
    electric$Month <- month.abb
    colnames(electric)[1] <- c("KWh")
    electric <- electric[, c(2,1)]
    
    gas <- data.frame(colSums(dataset[, 17:28], na.rm = TRUE))
    gas$Month<- month.abb
    colnames(gas)[1] <- c("KWh")
    gas <- gas[, c(2,1)]

    p1 <- ggplot(electric, aes(x=Month, y=KWh, group=1)) + geom_line() +
        scale_x_discrete(limits = month.abb)
    
    p2 <- ggplot(gas, aes(x=Month, y=KWh, group=1)) + geom_line() +
        scale_x_discrete(limits = month.abb)
    
    return(list(p1, p2, electric, gas))
}

# ---- Data processing ----
chicago_data <- read.csv("energy-usage-2010-clean.csv")
chicago_data[2] <- lapply(chicago_data[2], as.character)


cook_county <- blocks(state="IL", county="Cook", year="2010")

merged_chicago <- merge(chicago_data, cook_county[,c(3,5)], by.y="GEOID10", by.x="CENSUS.BLOCK")
merged_chicago <- merged_chicago[, c(2,1,3:35)]

View(cook_county)
# below are lists to help with the input groups
selectionsList <- list("Electricity"="TOTAL.KWH", "Gas"="TOTAL.THERMS",
                       "Building Age"="AVERAGE.BUILDING.AGE", "Building Height"="AVERAGE.STORIES",
                       "Building Type"="BUILDING.TYPE", "Total Population"="TOTAL.POPULATION")

monthList <- month.name
names(monthList) <- month.abb

communities <- levels(factor(chicago_data$COMMUNITY.AREA.NAME))

chicago_tracts <- levels(factor(merged_chicago$TRACTCE10))
# ---- UI ----
ui <- dashboardPage(
    dashboardHeader(title="CS 424 Project 3"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     sidebarMenu(
                         menuItem("Near West Side View", tabName = "t1", icon=icon("far fa-arrow-circle-left")),
                         menuItem("Block View", tabName = "t2", icon=icon("far fa-columns")),
                         menuItem("Tract View", tabName = "t3", icon=icon("far fa-columns")),
                         menuItem("About", tabName = "t4", icon=icon("far fa-info"))
                     )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "t1",
                    fluidRow(
                        column(1, 
                               selectInput("select", "Select Category", selectionsList),
                               radioButtons("months", "Gas and Electric Month", c(monthList, "Total"),
                                            selected = "Total")),
                        column(7, box(title="Near West Side View", solidHeader=TRUE,
                                       width=12, mapviewOutput("chimap", height="50vh"))),
                        column(2, 
                               box(title = "Electric Data by Month", solidHeader=TRUE,
                                       width="100%", height="56vh", tableOutput("electric_data"))),
                        column(2, box(title="Gas Data by Month", solidHeader = TRUE,
                                       width="100%", height="56vh", tableOutput("gas_data")))
                    ), # end fluidRow
                    fluidRow(
                        column(6, box(title = "Electric Line Plot", solidHeader=TRUE, width="40vw",
                                      plotOutput("electric_line", height="20vh"))),
                        column(6, box(title="Gas Line Plot", solidHeader=TRUE, 
                                      width="40vw", plotOutput("gas_line", height="20vh")))
                    ) # end fluidRow
            ), # end tabItem1
            tabItem(tabName = "t2",
                    fluidRow(
                        column(1, 
                               selectInput("community1", "Select Community", communities, selected = "Near West Side"),
                               selectInput("select1", "Select Category", selectionsList),
                               radioButtons("months1", "Gas and Electric Month", c(monthList, "Total"),
                                            selected = "Total")),
                        column(5,
                               tabBox(title="Side 1 Comparison", width = 12,
                                      tabPanel("Map", mapviewOutput("comparison1", height = "70vh")),
                                      tabPanel("E. Plot", plotOutput("eplot1")),
                                      tabPanel("E. Table", tableOutput("etable1")),
                                      tabPanel("G. Plot", plotOutput("gplot1")),
                                      tabPanel("G. Table", tableOutput("gtable1")))
                        ),
                        column(5,
                               tabBox(title="Side 2 Comparison", width = 12,
                                      tabPanel("Map", mapviewOutput("comparison2", height = "70vh")),
                                      tabPanel("E. Plot", plotOutput("eplot2")),
                                      tabPanel("E. Table", tableOutput("etable2")),
                                      tabPanel("G. Plot", plotOutput("gplot2")),
                                      tabPanel("G. Table", tableOutput("gtable2")))
                        ),
                        column(1, 
                               selectInput("community2", "Select Community", communities, selected = "Loop"),
                               selectInput("select2", "Select Category", selectionsList),
                               radioButtons("months2", "Gas and Electric Month", c(monthList, "Total"),
                                            selected = "Total"))
                    ),
                    fluidRow(
                        column(12, align="center",
                               radioButtons("color", "Legend Palette", c("Red", "Blue", "Purple", "Default"),
                                            inline = TRUE, selected = "Default"))
                    )
            ), # end tabItem2
            tabItem(tabName = "t3",
                    fluidRow(
                        column(1, 
                               selectInput("tracts1", "Select Tract", chicago_tracts, selected = "833300"),
                               selectInput("select3", "Select Category", selectionsList),
                               radioButtons("months3", "Gas and Electric Month", c(monthList, "Total"),
                                            selected = "Total")),
                        column(5,
                               tabBox(title="Side 1 Comparison", width = 12,
                                      tabPanel("Map", mapviewOutput("comparison3", height = "70vh")),
                                      tabPanel("E. Plot", plotOutput("eplot3")),
                                      tabPanel("E. Table", tableOutput("etable3")),
                                      tabPanel("G. Plot", plotOutput("gplot3")),
                                      tabPanel("G. Table", tableOutput("gtable3")))
                        ),
                        column(5,
                               tabBox(title="Side 2 Comparison", width = 12,
                                      tabPanel("Map", mapviewOutput("comparison4", height = "70vh")),
                                      tabPanel("E. Plot", plotOutput("eplot4")),
                                      tabPanel("E. Table", tableOutput("etable4")),
                                      tabPanel("G. Plot", plotOutput("gplot4")),
                                      tabPanel("G. Table", tableOutput("gtable4")))
                        ),
                        column(1, 
                               selectInput("tracts2", "Select Tract", chicago_tracts),
                               selectInput("select4", "Select Category", selectionsList),
                               radioButtons("months4", "Gas and Electric Month", c(monthList, "Total"),
                                            selected = "Total"))
                    ),
                    fluidRow(
                        column(12, align="center",
                               radioButtons("color1", "Legend Palette", c("Red", "Blue", "Purple", "Default"),
                                            inline = TRUE, selected = "Default")))
            ), # end tabItem3
            tabItem(tabName = "t4",
                    h2("About this project"),
                    p("This project was made by Valo Mara for CS 424 Spring '21"),
                    p(),
                    tagList("All data available from ", a("kaggle", 
                                                          href="https://www.kaggle.com/chicago/chicago-energy-usage-2010")),
                    p(),
                    tagList("All code available on ", a("github", href = "https://github.com/vmara2/cs424p3"))
            ) # end tabItem4
        ) 
    )
)

# ---- SERVER ----
server <- function(input, output) {
    
    side1Reactive <- reactive({
        prepareData(chicago_data, input$community1)
    })
    
    side2Reactive <- reactive({
        prepareData(chicago_data, input$community2)
    })
    
    paletteReactive <- reactive({
        if(input$color == "Red") {
            legend_pallete <- c("#fee8c8","#fdbb84","#e34a33")
        }
        else if(input$color == "Blue"){
            legend_pallete <- c("#ece7f2","#a6bddb","#2b8cbe")
        }
        else if(input$color == "Purple"){
            legend_pallete <- c("#e0ecf4","#9ebcda","#8856a7")
        }
        else{
            legend_pallete <- viridis::viridis(3)
        }
    })
    
    paletteReactive1 <- reactive({
        if(input$color1 == "Red") {
            legend_pallete <- c("#fee8c8","#fdbb84","#e34a33")
        }
        else if(input$color1 == "Blue"){
            legend_pallete <- c("#ece7f2","#a6bddb","#2b8cbe")
        }
        else if(input$color1 == "Purple"){
            legend_pallete <- c("#e0ecf4","#9ebcda","#8856a7")
        }
        else{
            legend_pallete <- viridis::viridis(3)
        }
    })
    # ---- Near West Side View ----
    near_west_side <- prepareData(chicago_data, "Near West Side")
    
    output$chimap <- renderLeaflet({
        chicago <- subset(cook_county, GEOID10 %in% near_west_side$CENSUS.BLOCK)
        chicago <- merge(chicago, near_west_side, by.x = "GEOID10", by.y = "CENSUS.BLOCK")
        
        cat <- input$select
        if(cat == "TOTAL.KWH" | cat == "TOTAL.THERMS"){
            if(input$months != "Total" & cat == "TOTAL.KWH") {
                cat <- paste("KWH.",toupper(input$months),".2010", sep = "")
            }
            else if(input$months != "Total" & cat == "TOTAL.THERMS"){
                cat <- paste("THERM.",toupper(input$months),".2010", sep = "")
            }
        }
        
        mapviewOptions(default=TRUE)
        mapview(chicago, zcol=cat, popup=popupTable(chicago, zcol="BUILDING.TYPE"))@map
    })
    
    output$electric_data <- renderTable({
        interfaceData(near_west_side)[3]
    }, width = "100%", striped = TRUE, bordered = TRUE)
    
    output$gas_data <- renderTable({
        interfaceData(near_west_side)[4]
    }, width = "100%", striped = TRUE, bordered = TRUE)
    
    output$electric_line <- renderPlot({
        interfaceData(near_west_side)[1]
    })
    
    output$gas_line <- renderPlot({
        interfaceData(near_west_side)[2]
    })
    
    # ---- Compare View ----
    ## side 1
    output$comparison1 <- renderLeaflet({
        side1 <- side1Reactive()
        legend_palette <- paletteReactive()
        
        chicago <- subset(cook_county, GEOID10 %in% side1$CENSUS.BLOCK)
        chicago <- merge(chicago, side1, by.x = "GEOID10", by.y = "CENSUS.BLOCK")
        
        cat <- input$select1
        if(cat == "TOTAL.KWH" | cat == "TOTAL.THERMS"){
            if(input$months1 != "Total" & cat == "TOTAL.KWH") {
                cat <- paste("KWH.",toupper(input$months1),".2010", sep = "")
            }
            else if(input$months1 != "Total" & cat == "TOTAL.THERMS"){
                cat <- paste("THERM.",toupper(input$months1),".2010", sep = "")
            }
        }
        
        mapviewOptions(vector.palette = colorRampPalette(legend_palette))
        mapview(chicago, zcol=cat, popup=popupTable(chicago, 
                                                    zcol=c("BUILDING.TYPE","TRACTCE10")))@map
    })
    
    output$eplot1 <- renderPlot({
        side1 <- side1Reactive()
        interfaceData(side1)[1]
    })
    
    output$etable1 <- renderTable({
        side1 <- side1Reactive()
        interfaceData(side1)[3]
    }, width = "100%", striped = TRUE, bordered = TRUE)
    
    output$gplot1 <- renderPlot({
        side1 <- side1Reactive()
        interfaceData(side1)[2]
    })
    
    output$gtable1 <- renderTable({
        side1 <- side1Reactive()
        interfaceData(side1)[4]
    }, width = "100%", striped = TRUE, bordered = TRUE)
    
    ## side 2
    output$comparison2 <- renderLeaflet({
        side2 <- side2Reactive()
        legend_palette <- paletteReactive()
        
        chicago <- subset(cook_county, GEOID10 %in% side2$CENSUS.BLOCK)
        chicago <- merge(chicago, side2, by.x = "GEOID10", by.y = "CENSUS.BLOCK")
        
        cat <- input$select2
        if(cat == "TOTAL.KWH" | cat == "TOTAL.THERMS"){
            if(input$months2 != "Total" & cat == "TOTAL.KWH") {
                cat <- paste("KWH.",toupper(input$months2),".2010", sep = "")
            }
            else if(input$months2 != "Total" & cat == "TOTAL.THERMS"){
                cat <- paste("THERM.",toupper(input$months2),".2010", sep = "")
            }
        }
        
        mapviewOptions(vector.palette = colorRampPalette(legend_palette))
        mapview(chicago, zcol=cat, popup=popupTable(chicago, 
                                                    zcol=c("BUILDING.TYPE", "TRACTCE10")))@map
    })
    
    output$eplot2 <- renderPlot({
        side2 <- side2Reactive()
        interfaceData(side2)[1]
    })
    
    output$etable2 <- renderTable({
        side2 <- side2Reactive()
        interfaceData(side2)[3]
    }, width = "100%", striped = TRUE, bordered = TRUE)
    
    output$gplot2 <- renderPlot({
        side2 <- side2Reactive()
        interfaceData(side2)[2]
    })
    
    output$gtable2 <- renderTable({
        side2 <- side2Reactive()
        interfaceData(side2)[4]
    }, width = "100%", striped = TRUE, bordered = TRUE)
    
    # ---- Chicago View ----
    ## side 1
    output$comparison3 <- renderLeaflet({
        legend_palette <- paletteReactive1()
        
        chicago <- subset(merged_chicago, merged_chicago$TRACTCE10 == input$tracts1)
        chicago <- multiples(chicago)
        chicago <- aggregate(.~COMMUNITY.AREA.NAME+CENSUS.BLOCK+BUILDING.TYPE+TRACTCE10
                             , data=chicago, sum, na.action = na.pass)
        chicago <- merge(cook_county, chicago, by.x="GEOID10", by.y="CENSUS.BLOCK")
        
        cat <- input$select3
        if(cat == "TOTAL.KWH" | cat == "TOTAL.THERMS"){
            if(input$months3 != "Total" & cat == "TOTAL.KWH") {
                cat <- paste("KWH.",toupper(input$months3),".2010", sep = "")
            }
            else if(input$months3 != "Total" & cat == "TOTAL.THERMS"){
                cat <- paste("THERM.",toupper(input$months3),".2010", sep = "")
            }
        }
        
        mapviewOptions(vector.palette = colorRampPalette(legend_palette))
        mapView(chicago, zcol=cat, popup=popupTable(chicago, zcol="BUILDING.TYPE"))@map
    })
    
    output$eplot3 <- renderPlot({
        chicago <- subset(merged_chicago, merged_chicago$TRACTCE10 == input$tracts1)
        interfaceData(chicago)[1]
    })
    
    output$etable3 <- renderTable({
        chicago <- subset(merged_chicago, merged_chicago$TRACTCE10 == input$tracts1)
        interfaceData(chicago)[3]
    }, width = "100%", striped = TRUE, bordered = TRUE)
    
    output$gplot3 <- renderPlot({
        chicago <- subset(merged_chicago, merged_chicago$TRACTCE10 == input$tracts1)
        interfaceData(chicago)[2]
    })
    
    output$gtable3 <- renderTable({
        chicago <- subset(merged_chicago, merged_chicago$TRACTCE10 == input$tracts1)
        interfaceData(chicago)[4]
    }, width = "100%", striped = TRUE, bordered = TRUE)
    ## side 2
    output$comparison4 <- renderLeaflet({
        legend_palette <- paletteReactive1()
        
        chicago <- subset(merged_chicago, merged_chicago$TRACTCE10 == input$tracts2)
        chicago <- multiples(chicago)
        chicago <- aggregate(.~COMMUNITY.AREA.NAME+CENSUS.BLOCK+BUILDING.TYPE+TRACTCE10
                             , data=chicago, sum, na.action = na.pass)
        chicago <- merge(cook_county, chicago, by.x="GEOID10", by.y="CENSUS.BLOCK")
        
        cat <- input$select4
        if(cat == "TOTAL.KWH" | cat == "TOTAL.THERMS"){
            if(input$months4 != "Total" & cat == "TOTAL.KWH") {
                cat <- paste("KWH.",toupper(input$months4),".2010", sep = "")
            }
            else if(input$months4 != "Total" & cat == "TOTAL.THERMS"){
                cat <- paste("THERM.",toupper(input$months4),".2010", sep = "")
            }
        }
        
        mapviewOptions(vector.palette = colorRampPalette(legend_palette))
        mapView(chicago, zcol=cat, popup=popupTable(chicago, zcol="BUILDING.TYPE"))@map
    })
    
    output$eplot4 <- renderPlot({
        chicago <- subset(merged_chicago, merged_chicago$TRACTCE10 == input$tracts2)
        interfaceData(chicago)[1]
    })
    
    output$etable4 <- renderTable({
        chicago <- subset(merged_chicago, merged_chicago$TRACTCE10 == input$tracts2)
        interfaceData(chicago)[3]
    }, width = "100%", striped = TRUE, bordered = TRUE)
    
    output$gplot4 <- renderPlot({
        chicago <- subset(merged_chicago, merged_chicago$TRACTCE10 == input$tracts2)
        interfaceData(chicago)[2]
    })
    
    output$gtable4 <- renderTable({
        chicago <- subset(merged_chicago, merged_chicago$TRACTCE10 == input$tracts2)
        interfaceData(chicago)[4]
    }, width = "100%", striped = TRUE, bordered = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
