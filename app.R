library(shiny)
library(shinydashboard)
library(ggplot2)
library(mapview)
library(leaflet)
library(leafpop)
library(tigris)

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
            dataset[i, 30:34] <- dataset[i, 30:34] / (dups[dups$Var1 == dataset[i,2],2])
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
#chicago_data[is.na(chicago_data)] <- 0

cook_county <- blocks(state="IL", county="Cook", year="2010")

selectionsList <- list("Electricity"="TOTAL.KWH", "Gas"="TOTAL.THERMS",
                       "Building Age"="AVERAGE.BUILDING.AGE", "Building Height"="AVERAGE.STORIES",
                       "Building Type"="BUILDING.TYPE", "Total Population"="TOTAL.POPULATION")

monthList <- month.name
names(monthList) <- month.abb

communities <- levels(factor(chicago_data$COMMUNITY.AREA.NAME))
# ---- UI ----
ui <- dashboardPage(
    dashboardHeader(title="CS 424 Project 3"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     sidebarMenu(
                         menuItem("Near West Side View", tabName = "t1", icon=icon("far fa-arrow-circle-left")),
                         menuItem("Compare View", tabName = "t2", icon=icon("far fa-columns")),
                         menuItem("Chicago View", tabName = "t3", icon=icon("far fa-window-maximize")),
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
                    )
            ) # end tabItem2
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
        
        mapview(chicago, zcol=cat, popup=popupTable(chicago, zcol="BUILDING.TYPE"))@map
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
        
        mapview(chicago, zcol=cat, popup=popupTable(chicago, zcol="BUILDING.TYPE"))@map
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
}

# Run the application 
shinyApp(ui = ui, server = server)
