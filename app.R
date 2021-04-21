library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title="CS 424 Project 3"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     sidebarMenu(
                         menuItem("Tab 1", tabName = "t1"),
                         menuItem("Tab 2", tabName = "t2"),
                         menuItem("Tab 3", tabName = "t3")
                     )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "t1",
                    h1("Sample")
                    )
        ) # t1 end
    )
)

server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
