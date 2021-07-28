library(devtools)
devtools::install_github("JaseZiv/worldfootballR", ref="main")
library(worldfootballR)
library(mongolite)
library(reactable)
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


mongo_db_user <- "abhi"
mongo_db_password <- "dataCollectMDB"
mongo_database <- "FBRefData"
mongo_collection <- "playerdata"
mongo_clustername <- "clusterfbplayer.r4q2t.mongodb.net"

# the following is the connection string for an Atlas online MongoDB cluster
url_path = sprintf("mongodb+srv://%s:%s@%s/admin",
                   mongo_db_user, mongo_db_password, mongo_clustername)

mongo_db <- mongo(collection = mongo_collection, db = mongo_database, url = url_path, verbose = TRUE)
player_standard = mongo_db$find('{}')
library(shiny)
# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Standardized Metric Scores"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectizeInput("player", "Select a Player", choices = player_standard$Player,
                           options = list(maxItems = 1, placeholder = 'Type Player Name')),
            actionButton("show", label = "Show"),
            actionButton("update", label = "Update Dataset"),
            selectizeInput("year", "Select a Year", choices = c(2021,2020,2019,2018,2017),
                           options = list(maxItems = 1, placeholder = 'Type Year Name'))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            reactableOutput("playerData")
        )
    )
))
