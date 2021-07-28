devtools::install_github("JaseZiv/worldfootballR", ref="main")
library(worldfootballR)
library(tidyverse)
library(reactable)
library(mongolite)
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
mongo_db_user <- "abhi"
mongo_db_password <- "dataCollectMDB"
mongo_database <- "FBRefData"
mongo_collection <- "playerdata"
mongo_clustername <- "clusterfbplayer.r4q2t.mongodb.net"

# the following is the connection string for an Atlas online MongoDB cluster
url_path = sprintf("mongodb+srv://%s:%s@%s/admin",
                   mongo_db_user, mongo_db_password, mongo_clustername)

mongo_db <- mongo(collection = mongo_collection, db = mongo_database, url = url_path, verbose = TRUE)
player_general = mongo_db$find('{}')
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    # observeEvent(input$activate, {
    #     player_general = mongo_db$find('{}')
    #     showNotification(
    #         "Dataset has been activated! Please select a Player and click Show",
    #         type = "message",
    #         duration = 45,
    #         closeButton = TRUE
    #     )
    # })
    
    observeEvent(input$update, {
        if(mongo_db$count('{}') != 0){
            mongo_db$remove(('{}'))
            showNotification(
                "Previous dataset has been removed -> updated dataset to be scraped",
                type = "message",
                duration = 30,
                closeButton = TRUE
            )
        }
        
        showNotification(
            "Dataset will now start updating! Please hold tight and drink some coffee",
            type = "message",
            duration = 20,
            closeButton = TRUE
        )
        
        categories = c(
            "shooting",
            "passing",
            "passing_types",
            "gca",
            "defense",
            "possession"
        )
        
        showNotification(
            "Getting standard statistics",
            type = "message",
            duration = 35,
            closeButton = TRUE
        )
        player_standard <- 
            fb_big5_advanced_season_stats(season_end_year=input$year, stat_type="standard", team_or_player= "player")
        player_standard <- player_standard %>%
            arrange(Player)
        
        
        
        for(i in 1:length(categories)){
            
            msg = paste("Getting", categories[i])
            msg = paste(msg, "stats")
            showNotification(
                msg,
                type = "message",
                duration = 35,
                closeButton = TRUE
            )
            
            player_general <- 
                fb_big5_advanced_season_stats(season_end_year=input$year, stat_type=categories[i], team_or_player= "player")
           
            player_category <- player_general %>%
                arrange((Player)) %>%
                select(-c("Nation","Pos","Age",
                          "Born","Comp","Squad",
                          "Season_End_Year", "Mins_Per_90"))
            
            player_standard = merge(player_standard, player_category, by = "Player")
        }
        
        mongo_db$insert(player_standard)
        
        showNotification(
            "Dataset has been updated! Please activate the dataset",
            type = "message",
            duration = 45,
            closeButton = TRUE
        )
        
        player_general = mongo_db$find('{}')
        input$player = player_general$Player
        showNotification(
          "Names and data fully updated!",
          type = "message",
          duration = 45,
          closeButton = TRUE
        )
    })
    
    playerTable <- eventReactive(input$show,{
        #Categories
        player_index = which(player_general$Player == input$player)
        player_team = player_general$Squad[player_index]
       
        player_standard <- player_general %>%
            filter(Squad == player_team) %>%
            filter(Min_Playing >= 800) 
        
        player_standard <- player_standard %>%
            distinct(Player, .keep_all = TRUE)
        
        player_index = which(player_standard$Player == input$player)
        
        
        player_standard = player_standard %>%
            select(-c("Nation","Pos","Age",
                      "Born","Comp","Squad",
                      "Season_End_Year", "Player"))
        
        metrics = (colnames(player_standard))

        playerStd <- function(x){
            mean_metric = mean(x)
            std_metric = sd(x)
            player_metric = (x[player_index]-mean_metric)/std_metric
        }
        
        player_metric = lapply(player_standard, playerStd)
        
        player_data = data_frame(Metric = metrics,
                                 Statistic = as.numeric(player_metric))
        
        player_data$Statistic = round(player_data$Statistic, digits=3)
        
        graph_data <- player_data %>%
            filter(Metric %in% c('Sh_Standard','SoT_Standard',
                                 "npxG_Expected","TotDist_Total",
                                 "PrgDist_Total", "KP",
                                 "Final_Third","PPA","CrsPA",
                                 "Prog","TB_Pass","Sw_Pass",
                                 "Press_Pass", "Crs_Pass", "SCA90_SCA",
                                 "PassLive_SCA", "Drib_SCA",
                                 "Sh_SCA", "GCA90_GCA", "PassLive_GCA",
                                 "Sh_GCA", "TklW_Tackles", "Def 3rd_Tackles",
                                 "Mid 3rd_Tackles", "Att 3rd_Tackles",
                                 "Succ_Pressures", "Def 3rd_Pressures",
                                 "Mid 3rd_Pressures", "Att 3rd_Pressures",
                                 "Blocks_Blocks", "Int", "Carries_Carries",
                                 "TotDist_Carries", "PrgDist_Carries", "Prog_Carries",
                                 "Final_Third_Carries", "CPA_Carries", "Rec_Receiving"))
        
        
        graph_data <- graph_data %>%
            mutate(Categories = ifelse(
                Metric %in% c('Sh_Standard','SoT_Standard',
                              "npxG_Expected","SCA90_SCA","Drib_SCA",
                              "Sh_SCA","GCA90_GCA","Sh_GCA","PPA","CrsPA",
                              "CPA_Carries"), "Attack",
                ifelse(
                    Metric %in% c("TotDist_Total",
                                  "PrgDist_Total", "KP",
                                  "Final_Third", "Prog","TB_Pass","Sw_Pass",
                                  "Press_Pass", "Crs_Pass",
                                  "PassLive_SCA", "PassLive_GCA"), "Passing",
                    ifelse(
                        Metric %in% c("Succ_Pressures", "Def 3rd_Pressures",
                                      "Mid 3rd_Pressures", "Att 3rd_Pressures",
                                      "Carries_Carries",
                                      "TotDist_Carries", "PrgDist_Carries", "Prog_Carries",
                                      "Final_Third_Carries","Rec_Receiving"), "Resistance", "Defence"
                    )
                )
            ))
  
        
        red_pal <- function(x) rgb(colorRamp(c("#fc4f30", "white", "#30a2da"))(x), maxColorValue = 255)
        
        reactable(graph_data, filterable = TRUE,
                  defaultPageSize = 20,
                  columns = list(
                      Statistic = colDef(
                          style = function(value) {
                              normalized <- (value - min(graph_data$Statistic)) / (max(graph_data$Statistic) - min(graph_data$Statistic))
                              color <- red_pal(normalized)
                              list(background = color)
                          }
                      )
                  ),
                  theme = reactableTheme(
                      borderColor = "#dfe2e5",
                      stripedColor = "#f6f8fa",
                      highlightColor = "#f0f5f9",
                      cellPadding = "8px 12px",
                      style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                      searchInputStyle = list(width = "100%")
                  )
        )
    })
    
    output$playerData <- renderReactable({
        playerTable()
    })
})
