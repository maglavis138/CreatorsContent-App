# LIBRARIES ========================================================================================================================

library(ggplot2)
library(reshape)
library(scales)
library(shiny)
library(shinydashboard)
library(plyr)
library(dplyr)
library(tidyr)
library(viridis)
library(ggthemes)
library(RColorBrewer)
library(highcharter)
library(quantmod)
library(dtplyr)
library(data.table)
library(readr)
library(jsonlite)
library(DT)
library(RMySQL)


# DATA LOAD ========================================================================================================================

# mydb = dbConnect(MySQL(), host = "104.198.210.36", user = "root", password = "tacozombies54992", db = "analytics")
# 
# rs <- dbSendQuery(mydb, "select FEED_DATA.*, EDITORIAL_AUTHOR.author, POST_DATA.* 
# from EDITORIAL_AUTHOR 
# inner join FEED_DATA on EDITORIAL_AUTHOR.status_id = FEED_DATA.status_id
# inner join POST_DATA on EDITORIAL_AUTHOR.status_id = POST_DATA.status_id")
# DataArticles <- fetch(rs, -1)
# 
# dbClearResult(rs)
# dbDisconnect(mydb)
# 
# DataArticles <- DataArticles[, !duplicated(colnames(DataArticles))]
# DataArticles$date <- as.Date(DataArticles$date)
# 
# DataArticles$author_status <- ifelse(!(DataArticles$author %in% c("Jorge Rodriguez-Jimenez", "Omar Villegas", "Lucas Molandes", "Jessica Garcia", "Andrew Santiago", "Jason Marcus")), "Contributor", DataArticles$author)
# 
# DataArticles$author_status <- ifelse(DataArticles$author %in% c("mitÃº Staff", "Adriana Venegas", "Fidel Martinez", "Alex Alvarez", "Wendy Barba"), "Old Staff", DataArticles$author_status)

load("data/DataArticles.Rda")
load("data/DataArticlesBH.Rda")
load("data/DataArticlesFC.Rda")

DataArticles <- DataArticles[!is.na(DataArticles$author),]
DataArticles$article_click_rank <- ifelse(DataArticles$link_clicks <= 5000, "< 5K", ifelse(DataArticles$link_clicks <= 10000, "5K - 10K", ifelse(DataArticles$link_clicks <= 25000, "10K - 25K", "> 25K")))


hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)


# UI ================================================================================================================================


ui <- dashboardPage(skin = "blue",
                    
                    # Dashboard Header ------------------------------------------------------------------------------------------------------------------     
                    
                    dashboardHeader(title = "Creators Content"),
                    
                    # Dashboard Sidebar -----------------------------------------------------------------------------------------------------------------
                    
                    dashboardSidebar(
                      
                      tags$head(tags$style(HTML(".sidebar { height: 200vh; }"))),
                      
                      sidebarMenu(
                        
                        menuItem("Articles WAM", tabName = "articles_wam", icon = icon("fa fa-file-text-o")),
                        menuItem("Videos", tabName = "videos_reposts", icon = icon("fa fa-film")),
                        menuItem("Memes", tabName = "memes_reposts", icon = icon("fa fa-picture-o"))
                        
                      )
                    ),
                    
                    # Dashboard Body --------------------------------------------------------------------------------------------------------------------               
                    dashboardBody(fluidRow(
                      
                      tabItems(
                        
                        #  1. Articles WAM -----------------------------------------------------------------------------------------------------------   
                        
                        tabItem(tabName = "articles_wam", fluidRow(
                          
                          tabBox( title = "",
                                  
                                  
                                  #  1.1 Articles Repost Suggestions ------------------------------------------------------------------------------------
                                  tabPanel("Overview",
                                           
                                           box(title = "", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                               column(2, selectizeInput(inputId = "chart_time_overview_wam", label = "Timeline:", choices = c("Day", "Week", "Month"), selected = "Week")),
                                               column(2, selectizeInput(inputId = "chart_general_detail", label = "View:", choices = c("General", "Detail"), selected = "General")),
                                               column(2, selectizeInput(inputId = "chart_avg_total", label = "Method:", choices = c("Average", "Sum"), selected = "Average")),
                                               column(2, selectizeInput(inputId = "chart_stack", label = "Stack:", choices = c("Normal", "Percent"), selected = "Normal")),
                                               column(2,selectizeInput(inputId = "chart_type", label = "Type:", choices = c("Area", "Column"), selected = "Column")),
                                               column(2, selectizeInput(inputId = "article_overview_repost_wam", label = "Posts:", choices = c("Originals","Reposts"), selected = c("Originals","Reposts"), multiple = TRUE)),
                                               column(8, selectizeInput(inputId = "article_overview_categories", label = "Categories:", choices = as.character(unique(DataArticles$category)[order(unique(DataArticles$category))]), selected = as.character(unique(DataArticles$category)), multiple = TRUE)),
                                               column(4, actionButton(inputId = "plot_article_overview_wam", label = "Plot", width = "100%", style = "height:80px")),
                                               
                                               
                                               column(6, highchartOutput("PlotOverviewAuthorsWAM", height = 500)),
                                               column(6, highchartOutput("PlotOverviewAuthorsWAM1", height = 500)),
                                               column(6, highchartOutput("PlotOverviewAuthorsWAM2", height = 500)),
                                               column(6, highchartOutput("PlotOverviewAuthorsWAM3", height = 500))
                                               
                                           )
                                           
                                  ),
                                  
                                  
                                  #  1.2 Articles Repost Candidates -------------------------------------------------------------------------------------
                                  tabPanel("Leaderboard",
                                           
                                           box(title = "Authors", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                               tags$head(
                                                 tags$style(type = "text/css",
                                                            HTML("th { text-align: center; }")
                                                 )
                                               ),
                                               
                                               column(3, dateRangeInput('leaderboard_date_range',label = "Date:", start = range(DataArticles$date)[1], end = range(DataArticles$date)[2], min = range(DataArticles$date)[1], max = range(DataArticles$date)[2])),
                                               column(3, selectizeInput(inputId = "leaderboard_variable_select", label = "Variable:", choices = c("Link Clicks", "Reach", "Engagements"), "Link Clicks", multiple = FALSE)),
                                               column(3, selectizeInput(inputId = "leaderboard_staff_select", label = "Staff Status:", choices = c("Current Staff", "Old Staff", "Contributor"), c("Current Staff", "Old Staff", "Contributor"), multiple = TRUE)),
                                               column(3, actionButton(inputId = "leaderboard_apply_button", label = "Apply", width = "100%", style = "height:60px")),
                                               column(9, selectizeInput(inputId = "leaderboard_categories_select", label = "Categories:", choices = as.character(unique(DataArticles$category)[order(unique(DataArticles$category))]), selected = as.character(unique(DataArticles$category)), multiple = TRUE)),
                                               column(3,selectizeInput(inputId = "chart_link_clicks_group_wam", label = "Link Clicks:", choices = c("< 5K", "5K - 10K", "10K - 25K", "> 25K"), selected = c("< 5K", "5K - 10K", "10K - 25K", "> 25K"), multiple = TRUE)),
                                               
                                               column(12, highchartOutput("LeaderboardAuthorsGraph", height = 500)),
                                               
                                               
                                               column(12, DT::dataTableOutput("LeaderboardTable"))
                                           ),
                                           
                                           box(title = "Posts", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                               fluidRow(
                                               column(width = 12, 
                                                      
                                                      DT::dataTableOutput("LeaderboardPostsTable", width = "100%"))
                                               )
                                               
                                           )
                                           
                                  ),
                                  
                                  
                                  #  1.3 Articles Repost Groups -----------------------------------------------------------------------------------------                    
                                  tabPanel("Categories",
                                           
                                           box(title = "Summary", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                               # column(1, ),
                                               column(12, 
                                                      checkboxInput(inputId = "category_author_cs_select_checkbox", label = "", value = TRUE, width = "10px"),
                                                      
                                                      selectizeInput(inputId = "category_author_cs_select", label = "Current Staff:", choices = as.character(unique(DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor"),]$author)[order(unique(DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor"),]$author))]), selected = as.character(unique(DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor"),]$author)), multiple = TRUE)),
                                               column(12, selectizeInput(inputId = "category_author_os_select", label = "Old Staff:", choices = as.character(unique(DataArticles[which(DataArticles$author_status == "Old Staff"),]$author)[order(unique(DataArticles[which(DataArticles$author_status == "Old Staff"),]$author))]), selected = as.character(unique(DataArticles[which(DataArticles$author_status == "Old Staff"),]$author)), multiple = TRUE)),
                                               column(12, selectizeInput(inputId = "category_author_c_select", label = "Contributor", choices = as.character(unique(DataArticles[which(DataArticles$author_status == "Contributor"),]$author)[order(unique(DataArticles[which(DataArticles$author_status == "Contributor"),]$author))]), selected = as.character(unique(DataArticles[which(DataArticles$author_status == "Contributor"),]$author)), multiple = TRUE)),
                                               
                                               column(12, actionButton(inputId = "category_author_select_button", label = "Apply", width = "100%", style = "height:60px")),
                                               br(),
                                               column(6, highchartOutput("LeaderboardCategoriesGraph", height = 500)), 
                                               column(6, highchartOutput("CategoriesReachGraph", height = 500)),
                                               column(6, highchartOutput("CategoriesLinkClicksGraph", height = 500)),
                                               column(6, highchartOutput("CategoriesEngagementsGraph", height = 500))
                                               )
                                           
                                  )
                                  
                                  , width = 12))),
                        
                        
                        #  2. Videos Repost -----------------------------------------------------------------------------------------------------------   
                        
                        tabItem(tabName = "videos_reposts", fluidRow(
                          
                          tabBox( title = "",
                                  
                                  
                                  #  2.1 Videos Repost Suggestions ------------------------------------------------------------------------------------
                                  tabPanel("Repost Suggestions"
                                           
                                  ),
                                  
                                  
                                  #  2.2 Videos Repost Candidates -------------------------------------------------------------------------------------
                                  tabPanel("Repost Candidates"
                                           
                                  ),
                                  
                                  
                                  #  2.3 Videos Repost Groups -----------------------------------------------------------------------------------------                
                                  
                                  tabPanel("Repost Groups"
                                           
                                  )
                                  
                                  , width = 12))),
                        
                        
                        #  3. Memes Repost -----------------------------------------------------------------------------------------------------------   
                        tabItem(tabName = "memes_reposts", fluidRow(
                          
                          tabBox( title = "",
                                  
                                  
                                  #  3.1 Memes Repost Suggestions ------------------------------------------------------------------------------------
                                  tabPanel("Repost Suggestions"
                                           
                                  ),
                                  
                                  
                                  #  3.2 Memes Repost Candidates -------------------------------------------------------------------------------------
                                  tabPanel("Repost Candidates"
                                           
                                  ),
                                  
                                  
                                  #  3.3 Memes Repost Groups -----------------------------------------------------------------------------------------                
                                  
                                  tabPanel("Repost Groups"
                                           
                                  )
                                  
                                  , width = 12)))
                        
                        
                        
                        
                        
                      )))
)




# SERVER  ===========================================================================================================================

server <- function(input, output, session){
  
  # Overview --------------------------------------------------------------------------------------------------------------
  
  Plot_Authors_Output_Function <- function (contributors, old_staff, current_staff, input_chart_time, input_chart_stack, input_chart_type = tolower(input$chart_type)){
    
    colors <- c(brewer.pal(3, "Paired"))
    
    hc <-highchart(type = "stock") %>%
      hc_title(text = "Content Output", align = "left", style = list(fontSize = "25px")) %>%
      hc_colors(colors) %>%
      hc_yAxis(offset = 30) %>%
      hc_add_series(contributors, name = "Contributors", type = input_chart_type) %>%
      hc_add_series(old_staff, name = "Old Staff", type = input_chart_type) %>%
      hc_add_series(current_staff, name = "Current Staff", type = input_chart_type) %>%
      hc_scrollbar(enabled = FALSE) %>%
      hc_rangeSelector(selected = ifelse(input_chart_time == "day", 0, 2)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = input_chart_stack, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input_chart_time, list(1)))))) %>%
      hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  }
  
  Plot_Authors_Output_Function_Detail <- function (contributors, old_staff, jorge_rodriguez, omar_villegas, lucas_molandes, jessica_garcia, andrew_santiago, jason_marcus, input_chart_time, input_chart_stack, input_chart_type = tolower(input$chart_type)){
   
    colors <- c(brewer.pal(8, "Paired"))
      
    hc <-highchart(type = "stock") %>%
      hc_title(text = "Content Output", align = "left", style = list(fontSize = "25px")) %>%
      hc_colors(colors) %>%
      hc_yAxis(offset = 30) %>% 
      hc_add_series(contributors, name = "Contributors", type = input_chart_type) %>%
      hc_add_series(old_staff, name = "Old Staff", type = input_chart_type) %>%
      hc_add_series(jorge_rodriguez, name = "Jorge Rodriguez-Jimenez", type = input_chart_type) %>%
      hc_add_series(omar_villegas, name = "Omar Villegas", type = input_chart_type) %>%
      hc_add_series(lucas_molandes, name = "Lucas Molandes", type = input_chart_type) %>%
      hc_add_series(jessica_garcia, name = "Jessica Garcia", type = input_chart_type) %>%
      hc_add_series(andrew_santiago, name = "Andrew Santiago", type = input_chart_type) %>%
      hc_add_series(jason_marcus, name = "Jason Marcus", type = input_chart_type) %>%
      hc_scrollbar(enabled = FALSE) %>%
      hc_rangeSelector(selected = ifelse(input_chart_time == "day", 0, 2)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = input_chart_stack, dataGrouping = list(approximation = "sum", enabled = TRUE, forced = TRUE, units = list(list(input_chart_time, list(1)))))) %>%
      hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  }
  
  Plot_Authors_Reach_Function <- function (contributors, old_staff, current_staff, input_chart_time, input_chart_method, input_chart_stack, input_chart_type = tolower(input$chart_type)){
    
    colors <- c(brewer.pal(3, "Paired"))
    
    hc <-highchart(type = "stock") %>%
      hc_title(text = "Reach", align = "left", style = list(fontSize = "25px")) %>%
      hc_colors(colors) %>%
      hc_yAxis(offset = 30) %>%
      hc_add_series(contributors, name = "Contributors", type = input_chart_type) %>%
      hc_add_series(old_staff, name = "Old Staff", type = input_chart_type) %>%
      hc_add_series(current_staff, name = "Current Staff", type = input_chart_type) %>%
      hc_scrollbar(enabled = FALSE) %>%
      hc_rangeSelector(selected = ifelse(input_chart_time == "day", 0, 2)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = input_chart_stack, dataGrouping = list(approximation = input_chart_method, enabled = TRUE, forced = TRUE, units = list(list(input_chart_time, list(1)))))) %>%
      hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
      hc_tooltip(valueDecimals = ifelse(input_chart_method == "average", 2, 0)) %>%
      hc_add_theme(hc_theme_smpl())
    hc
   
  }
  
  Plot_Authors_Reach_Function_Detail <- function (contributors, old_staff, jorge_rodriguez, omar_villegas, lucas_molandes, jessica_garcia, andrew_santiago, jason_marcus, input_chart_time, input_chart_method, input_chart_stack, input_chart_type = tolower(input$chart_type)){
    
    colors <- c(brewer.pal(8, "Paired"))
    
    hc <-highchart(type = "stock") %>%
      hc_title(text = "Reach", align = "left", style = list(fontSize = "25px")) %>%
      hc_colors(colors) %>%
      hc_yAxis(offset = 30) %>% 
      hc_add_series(contributors, name = "Contributors", type = input_chart_type) %>%
      hc_add_series(old_staff, name = "Old Staff", type = input_chart_type) %>%
      hc_add_series(jorge_rodriguez, name = "Jorge Rodriguez-Jimenez", type = input_chart_type) %>%
      hc_add_series(omar_villegas, name = "Omar Villegas", type = input_chart_type) %>%
      hc_add_series(lucas_molandes, name = "Lucas Molandes", type = input_chart_type) %>%
      hc_add_series(jessica_garcia, name = "Jessica Garcia", type = input_chart_type) %>%
      hc_add_series(andrew_santiago, name = "Andrew Santiago", type = input_chart_type) %>%
      hc_add_series(jason_marcus, name = "Jason Marcus", type = input_chart_type) %>%
      hc_scrollbar(enabled = FALSE) %>%
      hc_rangeSelector(selected = ifelse(input_chart_time == "day", 0, 2)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = input_chart_stack, dataGrouping = list(approximation = input_chart_method, enabled = TRUE, forced = TRUE, units = list(list(input_chart_time, list(1)))))) %>%
      hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
      hc_tooltip(valueDecimals = ifelse(input_chart_method == "average", 2, 0)) %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  }
  
  Plot_Authors_Clicks_Function <- function (contributors, old_staff, current_staff, input_chart_time, input_chart_method, input_chart_stack, input_chart_type = tolower(input$chart_type)){
    
    colors <- c(brewer.pal(3, "Paired"))
    
    hc <-highchart(type = "stock") %>%
      hc_title(text = "Link Clicks", align = "left", style = list(fontSize = "25px")) %>%
      hc_colors(colors) %>%
      hc_yAxis(offset = 30) %>% 
      hc_add_series(contributors, name = "Contributors", type = input_chart_type) %>%
      hc_add_series(old_staff, name = "Old Staff", type = input_chart_type) %>%
      hc_add_series(current_staff, name = "Current Staff", type = input_chart_type) %>%
      hc_scrollbar(enabled = FALSE) %>%
      hc_rangeSelector(selected = ifelse(input_chart_time == "day", 0, 2)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = input_chart_stack, dataGrouping = list(approximation = input_chart_method, enabled = TRUE, forced = TRUE, units = list(list(input_chart_time, list(1)))))) %>%
      hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
      hc_tooltip(valueDecimals = ifelse(input_chart_method == "average", 2, 0)) %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  }
  
  Plot_Authors_Clicks_Function_Detail <- function (contributors, old_staff, jorge_rodriguez, omar_villegas, lucas_molandes, jessica_garcia, andrew_santiago, jason_marcus, input_chart_time, input_chart_method, input_chart_stack, input_chart_type = tolower(input$chart_type)){
    
    colors <- c(brewer.pal(8, "Paired"))
    
    hc <-highchart(type = "stock") %>%
      hc_title(text = "Link Clicks", align = "left", style = list(fontSize = "25px")) %>%
      hc_colors(colors) %>%
      hc_yAxis(offset = 30) %>% 
      hc_add_series(contributors, name = "Contributors", type = input_chart_type) %>%
      hc_add_series(old_staff, name = "Old Staff", type = input_chart_type) %>%
      hc_add_series(jorge_rodriguez, name = "Jorge Rodriguez-Jimenez", type = input_chart_type) %>%
      hc_add_series(omar_villegas, name = "Omar Villegas", type = input_chart_type) %>%
      hc_add_series(lucas_molandes, name = "Lucas Molandes", type = input_chart_type) %>%
      hc_add_series(jessica_garcia, name = "Jessica Garcia", type = input_chart_type) %>%
      hc_add_series(andrew_santiago, name = "Andrew Santiago", type = input_chart_type) %>%
      hc_add_series(jason_marcus, name = "Jason Marcus", type = input_chart_type) %>%
      hc_scrollbar(enabled = FALSE) %>%
      hc_rangeSelector(selected = ifelse(input_chart_time == "day", 0, 2)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = input_chart_stack, dataGrouping = list(approximation = input_chart_method, enabled = TRUE, forced = TRUE, units = list(list(input_chart_time, list(1)))))) %>%
      hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
      hc_tooltip(valueDecimals = ifelse(input_chart_method == "average", 2, 0)) %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  }
  
  Plot_Authors_Interactions_Function <- function (contributors, old_staff, current_staff, input_chart_time, input_chart_method, input_chart_stack, input_chart_type = tolower(input$chart_type)){
    
    colors <- c(brewer.pal(3, "Paired"))
    
    hc <-highchart(type = "stock") %>%
      hc_title(text = "Interactions", align = "left", style = list(fontSize = "25px")) %>%
      hc_colors(colors) %>%
      hc_yAxis(offset = 30) %>% 
      hc_add_series(contributors, name = "Contributors", type = input_chart_type) %>%
      hc_add_series(old_staff, name = "Old Staff", type = input_chart_type) %>%
      hc_add_series(current_staff, name = "Current Staff", type = input_chart_type) %>%
      hc_scrollbar(enabled = FALSE) %>%
      hc_rangeSelector(selected = ifelse(input_chart_time == "day", 0, 2)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = input_chart_stack, dataGrouping = list(approximation = input_chart_method, enabled = TRUE, forced = TRUE, units = list(list(input_chart_time, list(1)))))) %>%
      hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
      hc_tooltip(valueDecimals = ifelse(input_chart_method == "average", 2, 0)) %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  }
  
  Plot_Authors_Interactions_Function_Detail <- function (contributors, old_staff, jorge_rodriguez, omar_villegas, lucas_molandes, jessica_garcia, andrew_santiago, jason_marcus, input_chart_time, input_chart_method, input_chart_stack, input_chart_type = tolower(input$chart_type)){
    
    colors <- c(brewer.pal(8, "Paired"))
    
    hc <-highchart(type = "stock") %>%
      hc_title(text = "Interactions", align = "left", style = list(fontSize = "25px")) %>%
      hc_colors(colors) %>%
      hc_yAxis(offset = 30) %>% 
      hc_add_series(contributors, name = "Contributors", type = input_chart_type) %>%
      hc_add_series(old_staff, name = "Old Staff", type = input_chart_type) %>%
      hc_add_series(jorge_rodriguez, name = "Jorge Rodriguez-Jimenez", type = input_chart_type) %>%
      hc_add_series(omar_villegas, name = "Omar Villegas", type = input_chart_type) %>%
      hc_add_series(lucas_molandes, name = "Lucas Molandes", type = input_chart_type) %>%
      hc_add_series(jessica_garcia, name = "Jessica Garcia", type = input_chart_type) %>%
      hc_add_series(andrew_santiago, name = "Andrew Santiago", type = input_chart_type) %>%
      hc_add_series(jason_marcus, name = "Jason Marcus", type = input_chart_type) %>%
      hc_scrollbar(enabled = FALSE) %>%
      hc_rangeSelector(selected = ifelse(input_chart_time == "day", 0, 2)) %>%
      hc_legend(enabled = TRUE) %>%
      hc_plotOptions(series = list(animation = FALSE, borderColor = "black", stacking = input_chart_stack, dataGrouping = list(approximation = input_chart_method, enabled = TRUE, forced = TRUE, units = list(list(input_chart_time, list(1)))))) %>%
      hc_tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
      hc_tooltip(valueDecimals = ifelse(input_chart_method == "average", 2, 0)) %>%
      hc_add_theme(hc_theme_smpl())
    hc
    
  }
  
  
  output$PlotOverviewAuthorsWAM <- renderHighchart({
    
    input$plot_article_overview_wam
    isolate({
      
      DataArticles <- DataArticles[which(DataArticles$category %in% input$article_overview_categories),]
      DataArticles[which(DataArticles$repost == 0),]$repost <- "Originals"
      DataArticles[which(DataArticles$repost == 1),]$repost <- "Reposts"
      
      input_chart_time <- tolower(input$chart_time_overview_wam)
      input_chart_stack <- tolower(input$chart_stack)
      
      dates <- data.frame(created_time = DataArticles$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      # contributors <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status == "Contributor"& DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = rep(1,nrow(DataArticles[which(DataArticles$author_status == "Contributor" & DataArticles$repost %in% input$article_overview_repost_wam),]))), by = "created_time", all = TRUE)
      # contributors <- as.xts(contributors[,"num"], order.by = contributors[,"created_time"])
      # 
      # old_staff <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status == "Old Staff" & DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = rep(1,nrow(DataArticles[which(DataArticles$author_status == "Old Staff" & DataArticles$repost %in% input$article_overview_repost_wam),]))), by = "created_time", all = TRUE)
      # old_staff <- as.xts(old_staff[,"num"], order.by = old_staff[,"created_time"])
      # 
      # current_staff <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor" & DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = rep(1,nrow(DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor" & DataArticles$repost %in% input$article_overview_repost_wam),]))), by = "created_time", all = TRUE)
      # current_staff <- as.xts(current_staff[,"num"], order.by = current_staff[,"created_time"])
      # 
      # jorge_rodriguez <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status == "Jorge Rodriguez-Jimenez" & DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = rep(1,nrow(DataArticles[which(DataArticles$author_status == "Jorge Rodriguez-Jimenez" & DataArticles$repost %in% input$article_overview_repost_wam),]))), by = "created_time", all = TRUE)
      # jorge_rodriguez <- as.xts(jorge_rodriguez[,"num"], order.by = jorge_rodriguez[,"created_time"])
      # 
      # omar_villegas <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status == "Omar Villegas" & DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = rep(1,nrow(DataArticles[which(DataArticles$author_status == "Omar Villegas" & DataArticles$repost %in% input$article_overview_repost_wam),]))), by = "created_time", all = TRUE)
      # omar_villegas <- as.xts(omar_villegas[,"num"], order.by = omar_villegas[,"created_time"])
      # 
      # lucas_molandes <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status == "Lucas Molandes" & DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = rep(1,nrow(DataArticles[which(DataArticles$author_status == "Lucas Molandes" & DataArticles$repost %in% input$article_overview_repost_wam),]))), by = "created_time", all = TRUE)
      # lucas_molandes <- as.xts(lucas_molandes[,"num"], order.by = lucas_molandes[,"created_time"])
      # 
      # jessica_garcia <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status == "Jessica Garcia" & DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = rep(1,nrow(DataArticles[which(DataArticles$author_status == "Jessica Garcia" & DataArticles$repost %in% input$article_overview_repost_wam),]))), by = "created_time", all = TRUE)
      # jessica_garcia <- as.xts(jessica_garcia[,"num"], order.by = jessica_garcia[,"created_time"])
      # 
      # andrew_santiago <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status == "Andrew Santiago" & DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = rep(1,nrow(DataArticles[which(DataArticles$author_status == "Andrew Santiago" & DataArticles$repost %in% input$article_overview_repost_wam),]))), by = "created_time", all = TRUE)
      # andrew_santiago <- as.xts(andrew_santiago[,"num"], order.by = andrew_santiago[,"created_time"])
      # 
      # jason_marcus <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status == "Jason Marcus" & DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = rep(1,nrow(DataArticles[which(DataArticles$author_status == "Jason Marcus" & DataArticles$repost %in% input$article_overview_repost_wam),]))), by = "created_time", all = TRUE)
      # jason_marcus <- as.xts(jason_marcus[,"num"], order.by = jason_marcus[,"created_time"])
      
      
      contributors <- as.xts(rep(1,nrow(DataArticles[which(DataArticles$author_status == "Contributor" & DataArticles$repost %in% input$article_overview_repost_wam),])), order.by = DataArticles[which(DataArticles$author_status == "Contributor"& DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      old_staff <- as.xts(rep(1,nrow(DataArticles[which(DataArticles$author_status == "Old Staff" & DataArticles$repost %in% input$article_overview_repost_wam),])), order.by = DataArticles[which(DataArticles$author_status == "Old Staff"& DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      current_staff <- as.xts(rep(1,nrow(DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor" & DataArticles$repost %in% input$article_overview_repost_wam),])), order.by = DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      jorge_rodriguez <- as.xts(rep(1,nrow(DataArticles[which(DataArticles$author_status == "Jorge Rodriguez-Jimenez" & DataArticles$repost %in% input$article_overview_repost_wam),])), order.by = DataArticles[which(DataArticles$author_status == "Jorge Rodriguez-Jimenez" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)

      omar_villegas <- as.xts(rep(1,nrow(DataArticles[which(DataArticles$author_status == "Omar Villegas" & DataArticles$repost %in% input$article_overview_repost_wam),])), order.by = DataArticles[which(DataArticles$author_status == "Omar Villegas" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)

      lucas_molandes <- as.xts(rep(1,nrow(DataArticles[which(DataArticles$author_status == "Lucas Molandes" & DataArticles$repost %in% input$article_overview_repost_wam),])), order.by = DataArticles[which(DataArticles$author_status == "Lucas Molandes" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)

      jessica_garcia <- as.xts(rep(1,nrow(DataArticles[which(DataArticles$author_status == "Jessica Garcia" & DataArticles$repost %in% input$article_overview_repost_wam),])), order.by = DataArticles[which(DataArticles$author_status == "Jessica Garcia" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)

      andrew_santiago <- as.xts(rep(1,nrow(DataArticles[which(DataArticles$author_status == "Andrew Santiago" & DataArticles$repost %in% input$article_overview_repost_wam),])), order.by = DataArticles[which(DataArticles$author_status == "Andrew Santiago" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)

      jason_marcus <- as.xts(rep(1,nrow(DataArticles[which(DataArticles$author_status == "Jason Marcus" & DataArticles$repost %in% input$article_overview_repost_wam),])), order.by = DataArticles[which(DataArticles$author_status == "Jason Marcus" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)

      # colores<- c('#D55200', '#2580B9')
      
      if(input$chart_general_detail == "General"){
        
        Plot_Authors_Output_Function(contributors, old_staff, current_staff, input_chart_time, input_chart_stack)
        
      }
      
      else {
        
        Plot_Authors_Output_Function_Detail(contributors, old_staff, jorge_rodriguez, omar_villegas, lucas_molandes, jessica_garcia, andrew_santiago, jason_marcus, input_chart_time, input_chart_stack)
          
      }
      
    })
  })
  
  output$PlotOverviewAuthorsWAM1 <- renderHighchart({
    
    input$plot_article_overview_wam
    isolate({
      
      dates <- data.frame(created_time = DataArticles$created_time)
      dates$created_time <- as.POSIXct(dates$created_time)
      
      DataArticles <- DataArticles[which(DataArticles$category %in% input$article_overview_categories),]
      DataArticles[which(DataArticles$repost == 0),]$repost <- "Originals"
      DataArticles[which(DataArticles$repost == 1),]$repost <- "Reposts"
      
      input_chart_time <- tolower(input$chart_time_overview_wam)
      input_chart_method <- tolower(input$chart_avg_total)
      input_chart_stack <- tolower(input$chart_stack)
      
      # contributors <- as.xts(DataArticles[which(DataArticles$author_status == "Contributor"& DataArticles$repost %in% input$article_overview_repost_wam),]$post_reach, order.by = DataArticles[which(DataArticles$author_status == "Contributor"& DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      # 
      # old_staff <- as.xts(DataArticles[which(DataArticles$author_status == "Old Staff"& DataArticles$repost %in% input$article_overview_repost_wam),]$post_reach, order.by = DataArticles[which(DataArticles$author_status == "Old Staff"& DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      # 
      # current_staff <- as.xts(DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor" & DataArticles$repost %in% input$article_overview_repost_wam),]$post_reach, order.by = DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      # 
      # jorge_rodriguez <- as.xts(DataArticles[which(DataArticles$author_status == "Jorge Rodriguez-Jimenez" & DataArticles$repost %in% input$article_overview_repost_wam),]$post_reach, order.by = DataArticles[which(DataArticles$author_status == "Jorge Rodriguez-Jimenez" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      # 
      # omar_villegas <- as.xts(DataArticles[which(DataArticles$author_status == "Omar Villegas" & DataArticles$repost %in% input$article_overview_repost_wam),]$post_reach, order.by = DataArticles[which(DataArticles$author_status == "Omar Villegas" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      # 
      # lucas_molandes <- as.xts(DataArticles[which(DataArticles$author_status == "Lucas Molandes" & DataArticles$repost %in% input$article_overview_repost_wam),]$post_reach, order.by = DataArticles[which(DataArticles$author_status == "Lucas Molandes" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      # 
      # jessica_garcia <- as.xts(DataArticles[which(DataArticles$author_status == "Jessica Garcia" & DataArticles$repost %in% input$article_overview_repost_wam),]$post_reach, order.by = DataArticles[which(DataArticles$author_status == "Jessica Garcia" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      # 
      # andrew_santiago <- as.xts(DataArticles[which(DataArticles$author_status == "Andrew Santiago" & DataArticles$repost %in% input$article_overview_repost_wam),]$post_reach, order.by = DataArticles[which(DataArticles$author_status == "Andrew Santiago" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      # 
      # jason_marcus <- as.xts(DataArticles[which(DataArticles$author_status == "Jason Marcus" & DataArticles$repost %in% input$article_overview_repost_wam),]$post_reach, order.by = DataArticles[which(DataArticles$author_status == "Jason Marcus" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)

      
      contributors <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status == "Contributor"& DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = DataArticles[which(DataArticles$author_status == "Contributor"& DataArticles$repost %in% input$article_overview_repost_wam),"post_reach"]), by = "created_time", all = TRUE)
      contributors <- as.xts(contributors[,"num"], order.by = contributors[,"created_time"])

      old_staff <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status == "Old Staff" & DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = DataArticles[which(DataArticles$author_status == "Old Staff"& DataArticles$repost %in% input$article_overview_repost_wam),"post_reach"]), by = "created_time", all = TRUE)
      old_staff <- as.xts(old_staff[,"num"], order.by = old_staff[,"created_time"])

      current_staff <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor" & DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor" & DataArticles$repost %in% input$article_overview_repost_wam),"post_reach"]), by = "created_time", all = TRUE)
      current_staff <- as.xts(current_staff[,"num"], order.by = current_staff[,"created_time"])

      jorge_rodriguez <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status == "Jorge Rodriguez-Jimenez" & DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = DataArticles[which(DataArticles$author_status == "Jorge Rodriguez-Jimenez"& DataArticles$repost %in% input$article_overview_repost_wam),"post_reach"]), by = "created_time", all = TRUE)
      jorge_rodriguez <- as.xts(jorge_rodriguez[,"num"], order.by = jorge_rodriguez[,"created_time"])

      omar_villegas <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status == "Omar Villegas" & DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = DataArticles[which(DataArticles$author_status == "Omar Villegas" & DataArticles$repost %in% input$article_overview_repost_wam),"post_reach"]), by = "created_time", all = TRUE)
      omar_villegas <- as.xts(omar_villegas[,"num"], order.by = omar_villegas[,"created_time"])

      lucas_molandes <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status == "Lucas Molandes" & DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = DataArticles[which(DataArticles$author_status == "Lucas Molandes" & DataArticles$repost %in% input$article_overview_repost_wam),"post_reach"]), by = "created_time", all = TRUE)
      lucas_molandes <- as.xts(lucas_molandes[,"num"], order.by = lucas_molandes[,"created_time"])

      jessica_garcia <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status == "Jessica Garcia" & DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = DataArticles[which(DataArticles$author_status == "Jessica Garcia" & DataArticles$repost %in% input$article_overview_repost_wam),"post_reach"]), by = "created_time", all = TRUE)
      jessica_garcia <- as.xts(jessica_garcia[,"num"], order.by = jessica_garcia[,"created_time"])

      andrew_santiago <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status == "Andrew Santiago" & DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = DataArticles[which(DataArticles$author_status == "Andrew Santiago" & DataArticles$repost %in% input$article_overview_repost_wam),"post_reach"]), by = "created_time", all = TRUE)
      andrew_santiago <- as.xts(andrew_santiago[,"num"], order.by = andrew_santiago[,"created_time"])

      jason_marcus <- merge(dates, data.frame(created_time = DataArticles[which(DataArticles$author_status == "Jason Marcus" & DataArticles$repost %in% input$article_overview_repost_wam),"created_time"], num = DataArticles[which(DataArticles$author_status == "Jason Marcus" & DataArticles$repost %in% input$article_overview_repost_wam),"post_reach"]), by = "created_time", all = TRUE)
      jason_marcus <- as.xts(jason_marcus[,"num"], order.by = jason_marcus[,"created_time"])

      
      if(input$chart_general_detail == "General"){
        
        Plot_Authors_Reach_Function(contributors, old_staff, current_staff, input_chart_time, input_chart_method, input_chart_stack)
        
      }
      
      else {
        
        Plot_Authors_Reach_Function_Detail(contributors, old_staff, jorge_rodriguez, omar_villegas, lucas_molandes, jessica_garcia, andrew_santiago, jason_marcus, input_chart_time, input_chart_method, input_chart_stack)
        
      }

    })
  })
  
  output$PlotOverviewAuthorsWAM2 <- renderHighchart({
    
    input$plot_article_overview_wam
    isolate({
      
      DataArticles <- DataArticles[which(DataArticles$category %in% input$article_overview_categories),]
      DataArticles[which(DataArticles$repost == 0),]$repost <- "Originals"
      DataArticles[which(DataArticles$repost == 1),]$repost <- "Reposts"
      
      input_chart_time <- tolower(input$chart_time_overview_wam)
      input_chart_method <- tolower(input$chart_avg_total) 
      input_chart_stack <- tolower(input$chart_stack)
      
      contributors <- as.xts(DataArticles[which(DataArticles$author_status == "Contributor"& DataArticles$repost %in% input$article_overview_repost_wam),]$link_clicks, order.by = DataArticles[which(DataArticles$author_status == "Contributor"& DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      old_staff <- as.xts(DataArticles[which(DataArticles$author_status == "Old Staff"& DataArticles$repost %in% input$article_overview_repost_wam),]$link_clicks, order.by = DataArticles[which(DataArticles$author_status == "Old Staff"& DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      current_staff <- as.xts(DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor" & DataArticles$repost %in% input$article_overview_repost_wam),]$link_clicks, order.by = DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      jorge_rodriguez <- as.xts(DataArticles[which(DataArticles$author_status == "Jorge Rodriguez-Jimenez" & DataArticles$repost %in% input$article_overview_repost_wam),]$link_clicks, order.by = DataArticles[which(DataArticles$author_status == "Jorge Rodriguez-Jimenez" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      omar_villegas <- as.xts(DataArticles[which(DataArticles$author_status == "Omar Villegas" & DataArticles$repost %in% input$article_overview_repost_wam),]$link_clicks, order.by = DataArticles[which(DataArticles$author_status == "Omar Villegas" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      lucas_molandes <- as.xts(DataArticles[which(DataArticles$author_status == "Lucas Molandes" & DataArticles$repost %in% input$article_overview_repost_wam),]$link_clicks, order.by = DataArticles[which(DataArticles$author_status == "Lucas Molandes" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      jessica_garcia <- as.xts(DataArticles[which(DataArticles$author_status == "Jessica Garcia" & DataArticles$repost %in% input$article_overview_repost_wam),]$link_clicks, order.by = DataArticles[which(DataArticles$author_status == "Jessica Garcia" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      andrew_santiago <- as.xts(DataArticles[which(DataArticles$author_status == "Andrew Santiago" & DataArticles$repost %in% input$article_overview_repost_wam),]$link_clicks, order.by = DataArticles[which(DataArticles$author_status == "Andrew Santiago" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      jason_marcus <- as.xts(DataArticles[which(DataArticles$author_status == "Jason Marcus" & DataArticles$repost %in% input$article_overview_repost_wam),]$link_clicks, order.by = DataArticles[which(DataArticles$author_status == "Jason Marcus" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
     
     
      if(input$chart_general_detail == "General"){
        
        Plot_Authors_Clicks_Function(contributors, old_staff, current_staff, input_chart_time, input_chart_method, input_chart_stack)
        
      }
      
      else {
        
        Plot_Authors_Clicks_Function_Detail(contributors, old_staff, jorge_rodriguez, omar_villegas, lucas_molandes, jessica_garcia, andrew_santiago, jason_marcus, input_chart_time, input_chart_method, input_chart_stack)
        
      }
      
    })
  })

  output$PlotOverviewAuthorsWAM3 <- renderHighchart({
    
    input$plot_article_overview_wam
    isolate({
      
      DataArticles <- DataArticles[which(DataArticles$category %in% input$article_overview_categories),]
      DataArticles[which(DataArticles$repost == 0),]$repost <- "Originals"
      DataArticles[which(DataArticles$repost == 1),]$repost <- "Reposts"
      
      input_chart_time <- tolower(input$chart_time_overview_wam)
      input_chart_method <- tolower(input$chart_avg_total)
      input_chart_stack <- tolower(input$chart_stack)
      
      contributors <- as.xts(DataArticles[which(DataArticles$author_status == "Contributor"& DataArticles$repost %in% input$article_overview_repost_wam),]$total_interactions, order.by = DataArticles[which(DataArticles$author_status == "Contributor"& DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      old_staff <- as.xts(DataArticles[which(DataArticles$author_status == "Old Staff"& DataArticles$repost %in% input$article_overview_repost_wam),]$total_interactions, order.by = DataArticles[which(DataArticles$author_status == "Old Staff"& DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      current_staff <- as.xts(DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor" & DataArticles$repost %in% input$article_overview_repost_wam),]$total_interactions, order.by = DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      jorge_rodriguez <- as.xts(DataArticles[which(DataArticles$author_status == "Jorge Rodriguez-Jimenez" & DataArticles$repost %in% input$article_overview_repost_wam),]$total_interactions, order.by = DataArticles[which(DataArticles$author_status == "Jorge Rodriguez-Jimenez" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      omar_villegas <- as.xts(DataArticles[which(DataArticles$author_status == "Omar Villegas" & DataArticles$repost %in% input$article_overview_repost_wam),]$total_interactions, order.by = DataArticles[which(DataArticles$author_status == "Omar Villegas" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      lucas_molandes <- as.xts(DataArticles[which(DataArticles$author_status == "Lucas Molandes" & DataArticles$repost %in% input$article_overview_repost_wam),]$total_interactions, order.by = DataArticles[which(DataArticles$author_status == "Lucas Molandes" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      jessica_garcia <- as.xts(DataArticles[which(DataArticles$author_status == "Jessica Garcia" & DataArticles$repost %in% input$article_overview_repost_wam),]$total_interactions, order.by = DataArticles[which(DataArticles$author_status == "Jessica Garcia" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      andrew_santiago <- as.xts(DataArticles[which(DataArticles$author_status == "Andrew Santiago" & DataArticles$repost %in% input$article_overview_repost_wam),]$total_interactions, order.by = DataArticles[which(DataArticles$author_status == "Andrew Santiago" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      jason_marcus <- as.xts(DataArticles[which(DataArticles$author_status == "Jason Marcus" & DataArticles$repost %in% input$article_overview_repost_wam),]$total_interactions, order.by = DataArticles[which(DataArticles$author_status == "Jason Marcus" & DataArticles$repost %in% input$article_overview_repost_wam),]$created_time)
      
      # colores<- c('#D55200', '#2580B9')
      
      if(input$chart_general_detail == "General"){
        
        Plot_Authors_Interactions_Function(contributors, old_staff, current_staff, input_chart_time, input_chart_method, input_chart_stack)
        
      }
      
      else {
        
        Plot_Authors_Interactions_Function_Detail(contributors, old_staff, jorge_rodriguez, omar_villegas, lucas_molandes, jessica_garcia, andrew_santiago, jason_marcus, input_chart_time, input_chart_method, input_chart_stack)
        
      }
      
    })
  })
  
  # -----------------------------------------------------------------------------------------------------------------------
  
  # Leaderboard -----------------------------------------------------------------------------------------------------------
  
  output$LeaderboardTable = DT::renderDataTable({
    
    input$leaderboard_apply_button
    isolate({ 
      
      sketch = htmltools::withTags(table(
        
        class = 'display',
        thead(
          
          tr(
            th(rowspan = 2, 'Author'),
            th(colspan = 5, 'Originals'),
            th(colspan = 5, 'Reposts'),
            th(colspan = 5, 'Total'),
            th(rowspan = 2, 'Status')
          ),
          
          tr(
            lapply(rep(c('Num.', '%', 'Tot.', '%', 'Avg.'), 3), th)
            
          )
        )
      ))
      
      
      DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor"),]$author_status <- "Current Staff"
      
      DataArticles <- DataArticles[which(DataArticles$author_status %in% input$leaderboard_staff_select & DataArticles$category %in% input$leaderboard_categories_select & DataArticles$date >= input$leaderboard_date_range[1] & DataArticles$date<= input$leaderboard_date_range[2] & DataArticles$article_click_rank %in% input$chart_link_clicks_group_wam),]
      
      
      leaderboard_table <- ddply(DataArticles, "author", summarize, original_posts = length(status_id[repost == 0]), original_perc = original_posts/nrow(DataArticles[which(DataArticles$repost == 0),]), original_lc = ifelse(input$leaderboard_variable_select == "Link Clicks", round(sum(link_clicks[repost == 0]),0), ifelse(input$leaderboard_variable_select == "Reach",  sum(post_reach[repost == 0]),  round(sum(total_interactions[repost == 0]),0))), original_avg_lc = ifelse(input$leaderboard_variable_select == "Link Clicks", round(mean(link_clicks[repost == 0]),0), ifelse(input$leaderboard_variable_select == "Reach",  round(mean(post_reach[repost == 0]),0),  round(mean(total_interactions[repost == 0]),0))), repost_posts = length(status_id[repost == 1]), repost_perc = repost_posts/nrow(DataArticles[which(DataArticles$repost == 1),]), repost_lc = ifelse(input$leaderboard_variable_select == "Link Clicks", round(sum(link_clicks[repost == 1]),0), ifelse(input$leaderboard_variable_select == "Reach",  round(sum(post_reach[repost == 1]),0),  round(sum(total_interactions[repost == 1]),0))), repost_avg_lc = ifelse(input$leaderboard_variable_select == "Link Clicks", round(mean(link_clicks[repost == 1]),0), ifelse(input$leaderboard_variable_select == "Reach",  round(mean(post_reach[repost == 1]),0),  round(mean(total_interactions[repost == 1]),0))), total_posts = length(status_id), total_perc = total_posts/nrow(DataArticles), total_lc =  ifelse(input$leaderboard_variable_select == "Link Clicks", sum(link_clicks), ifelse(input$leaderboard_variable_select == "Reach",  sum(post_reach),  sum(total_interactions))), total_avg_lc =  ifelse(input$leaderboard_variable_select == "Link Clicks", round(mean(link_clicks),0), ifelse(input$leaderboard_variable_select == "Reach",  round(mean(post_reach),0),  round(mean(total_interactions),0))), status = ifelse(unique(author_status) %in% c("Old Staff", "Contributor"), unique(author_status), "Current Staff"))
      
      leaderboard_table$original_lc_perc <- leaderboard_table$original_lc/sum(leaderboard_table$original_lc)
      leaderboard_table$repost_lc_perc <- leaderboard_table$repost_lc/sum(leaderboard_table$repost_lc)
      leaderboard_table$total_lc_perc <- leaderboard_table$total_lc/sum(leaderboard_table$total_lc)
      
      leaderboard_table$original_lc <- format(leaderboard_table$original_lc, big.mark = ",")
      leaderboard_table$repost_lc <- format(leaderboard_table$repost_lc, big.mark = ",")
      leaderboard_table$total_lc <- format(leaderboard_table$total_lc, big.mark = ",")
      
      leaderboard_table$original_avg_lc <- format(leaderboard_table$original_avg_lc, big.mark = ",", digits = 1)
      leaderboard_table$repost_avg_lc <- format(leaderboard_table$repost_avg_lc, big.mark = ",", digits = 1)
      leaderboard_table$total_avg_lc <- format(leaderboard_table$total_avg_lc, big.mark = ",", digits = 1)
      
      
      leaderboard_table <- leaderboard_table[,c("author", "original_posts", "original_perc", "original_lc", "original_lc_perc", "original_avg_lc", "repost_posts", "repost_perc", "repost_lc", "repost_lc_perc", "repost_avg_lc", "total_posts", "total_perc", "total_lc", "total_lc_perc", "total_avg_lc", "status")]
      
      leaderboard_table <- leaderboard_table[order(leaderboard_table$total_lc, decreasing = TRUE), ]
      colors <- c(brewer.pal(3, "Paired"))
      
      # stateSave=TRUE, 
      datatable(leaderboard_table, container = sketch, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(pageLength = 25, columnDefs = list(list(className = 'dt-center', targets = c(0:16))), dom = "tip")) %>% 
        # formatStyle("author", "status", backgroundColor = styleEqual(c("Contributor", "Old Staff", "Current Staff"), colors)) %>%
      formatPercentage(c("original_perc", "repost_perc","total_perc","original_lc_perc","repost_lc_perc","total_lc_perc"), 1) %>%
      formatStyle(
        c('original_perc', 'repost_perc', 'total_perc', 'original_lc_perc', 'repost_lc_perc','total_lc_perc'),
        background = styleColorBar(leaderboard_table$original_perc, '#A6CEE3'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
      # formatStyle(0:16, fontWeight = 'bold')
    })
    
    
  })
  
  output$LeaderboardPostsTable = DT::renderDataTable({
    
    input$leaderboard_apply_button
    isolate({ 
      
      sketch = htmltools::withTags(table(

        class = 'display',
        thead(

          tr(
            th(colspan = 5, 'Post'),
            th(colspan = 11, 'Post Results'),
            th(colspan = 2, 'Post Author')
          ),

          tr(
            lapply(c(" ", "Image", "Sharetext + Headline", "Date", "Category", "Clicks", "C.T.R", "Reach", "% Fan", "% Viral", "Comments", "Likes", "Shares", "Interactions", "Intr. Rate", "Times Repost", "Author", "Status"), th)

          )
        )
      ))


      DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor"),]$author_status <- "Current Staff"
      
      DataArticles <- DataArticles[which(DataArticles$author_status %in% input$leaderboard_staff_select & DataArticles$category %in% input$leaderboard_categories_select & DataArticles$date >= input$leaderboard_date_range[1] & DataArticles$date<= input$leaderboard_date_range[2] & DataArticles$article_click_rank %in% input$chart_link_clicks_group_wam),]
      
      # DataArticles <- ddply(DataArticles, "mitu_link", summarize, status_id = status_id[created_time == min(created_time)], sharetext = sharetext[created_time == min(created_time)], headline = headline[created_time == min(created_time)], full_picture = full_picture[created_time == min(created_time)], first_date = min(date), last_date = max(date), link_clicks = sum(link_clicks), post_reach = sum(post_reach), total_comments = sum(total_comments),  total_likes = sum(total_likes),  total_shares = sum(total_shares), total_interactions = sum(total_interactions), author = author[created_time == min(created_time)], author_status = author_status[created_time == min(created_time)], times_repost = length(mitu_link)-1)
      
      DataArticles$full_picture <- paste("<img src ='", DataArticles$full_picture,"'",'title=""', 'alt="" border="0" height="120" width="120">')
      
      DataArticles$headline <- paste0("<br><a href='", DataArticles$permalink,"' target='_blank'>", DataArticles$headline,"</a>")
      
      DataArticles$share_head <- paste(DataArticles$sharetext, DataArticles$headline)
      DataArticles$perc_fan <- round(DataArticles$post_reach_fan_unique/(DataArticles$post_reach_fan_unique+DataArticles$post_reach_viral_unique), digits = 4)
      DataArticles$perc_viral <- round(DataArticles$post_reach_viral_unique/(DataArticles$post_reach_fan_unique+DataArticles$post_reach_viral_unique), digits = 4)
      DataArticles$category <- as.factor(DataArticles$category)
      DataArticles$ctr <- round(DataArticles$ctr, digits = 4)
      DataArticles$interaction_rate <- round(DataArticles$interaction_rate, digits = 4)
      
      leaderboard_posts_table <- DataArticles[,c("full_picture", "share_head", "date", "category", "link_clicks", "ctr", "post_reach", "perc_fan","perc_viral", "total_comments", "total_likes", "total_shares", "total_interactions", "interaction_rate", "times_repost", "author", "author_status")]
      
      leaderboard_posts_table <- leaderboard_posts_table[order(leaderboard_posts_table$link_clicks, decreasing = TRUE), ]
      
      colors <- c(brewer.pal(3, "Paired"))
      
      datatable(cbind(' ' = '<font size="6"> &#43;</font>', leaderboard_posts_table), extensions = "Scroller", filter = 'top',  container = sketch, escape = FALSE, rownames = FALSE, selection = "single", class = "compact", options = list(deferRender = TRUE, scrollY = 1000, scrollX = FALSE, autoWidth = FALSE, pageLength = 100, columnDefs = list(list(visible = FALSE, targets = c(3, 4, 15, 16, 17)), list(width = "10px", orderable = FALSE, className = 'details-control', targets = 0), list(className = 'dt-center', targets = "_all")), dom = "tip"), callback = JS("
                table.column(1).nodes().to$().css({cursor: 'pointer'});
                              var format = function(d) {
                              return '<table width = \"100%\" cellpadding=\"5\" cellspacing=\"0\" border=\"0\" style=\"padding-left:50px;\">'+
                              '<tr>'+
                              '<th>Date Posted</th>'+
                              '<th>Category</th>'+
                              '<th>Times Reposted</th>'+
                              '<th>Author</th>'+
                              '<th>Author Status</th>'+
                              '</tr>'+
                              '<tr>'+
                              '<td align = \"center\">'+d[3]+'</td>'+
                              '<td align = \"center\">'+d[4]+'</td>'+
                              '<td align = \"center\">'+d[15]+'</td>'+
                              '<td align = \"center\">'+d[16]+'</td>'+
                              '<td align = \"center\">'+d[17]+'</td>'+
                              '</tr>'+
                              '</table>';
                              };
                              table.on('click', 'td.details-control', function() {
                              var td = $(this), row = table.row(td.closest('tr'));
                              if (row.child.isShown()) {
                              row.child.hide();
                              td.html('<font size=\"6\"> &#43;</font>');
                              } else {
                              row.child(format(row.data())).show();
                              td.html('<font size=\"6\">&#8722;</font>');
                              }
                              });"
)) %>%
        # formatStyle(columns = "author_status", target = "row", background = styleEqual(c("Contributor", "Old Staff", "Current Staff"), colors))%>%
        # formatStyle(0:16, fontWeight = 'bold') %>%
        # formatDate(columns = "created_time", method = "toDateString") %>%
        formatPercentage(c("perc_fan", "perc_viral", "ctr", "interaction_rate"), 2) %>%
        formatCurrency(c("link_clicks", "post_reach", "total_comments", "total_likes", "total_shares", "total_interactions"), '', digits = 0)
      
    })
  })
  
  # output$LeaderboardAuthorsGraph <- renderHighchart({
  #   
  #   input$leaderboard_apply_button
  #   isolate({
  #     
  #     DataArticles[which(DataArticles$author_status != "Old Staff" & DataArticles$author_status != "Contributor"),]$author_status <- "Current Staff"
  #     
  #     DataArticles <- DataArticles[which(DataArticles$author_status %in% input$leaderboard_staff_select & DataArticles$category %in% input$leaderboard_categories_select & DataArticles$date >= input$leaderboard_date_range[1] & DataArticles$date<= input$leaderboard_date_range[2] & DataArticles$article_click_rank %in% input$chart_link_clicks_group_wam),]
  #     
  #     
  #     leaderboard_table <- ddply(DataArticles, "author", summarize, original_posts = length(status_id[repost == 0]), original_perc = original_posts/nrow(DataArticles[which(DataArticles$repost == 0),]), original_lc = ifelse(input$leaderboard_variable_select == "Link Clicks", sum(link_clicks[repost == 0]), ifelse(input$leaderboard_variable_select == "Reach",  sum(post_reach[repost == 0]),  sum(total_interactions[repost == 0]))), repost_posts = length(status_id[repost == 1]), repost_perc = repost_posts/nrow(DataArticles[which(DataArticles$repost == 1),]), repost_lc = ifelse(input$leaderboard_variable_select == "Link Clicks", sum(link_clicks[repost == 1]), ifelse(input$leaderboard_variable_select == "Reach",  sum(post_reach[repost == 1]),  sum(total_interactions[repost == 1]))), total_posts = length(status_id), total_perc = total_posts/nrow(DataArticles), total_lc =  ifelse(input$leaderboard_variable_select == "Link Clicks", sum(link_clicks), ifelse(input$leaderboard_variable_select == "Reach",  sum(post_reach),  sum(total_interactions))), status = ifelse(unique(author_status) %in% c("Old Staff", "Contributor"), unique(author_status), "Current Staff"))
  #     
  #     leaderboard_table <- leaderboard_table[order(leaderboard_table$total_lc, decreasing = TRUE), ]
  #     
  #     leaderboard_table <- leaderboard_table[1:min(40,nrow(leaderboard_table)), ]
  #     
  #     ds_num_originals <- lapply(1:nrow(leaderboard_table), function(x){
  #       list(name = leaderboard_table[x,]$author, y = leaderboard_table[x,]$original_posts)
  #     })
  #     
  #     ds_num_reposts <- lapply(1:nrow(leaderboard_table), function(x){
  #       list(name = leaderboard_table[x,]$author, y = leaderboard_table[x,]$repost_posts)
  #     })
  #     
  #     ds_lc_originals <- lapply(1:nrow(leaderboard_table), function(x){
  #       list(name = leaderboard_table[x,]$author, y = leaderboard_table[x,]$original_lc)
  #     })
  #     
  #     ds_lc_reposts <- lapply(1:nrow(leaderboard_table), function(x){
  #       list(name = leaderboard_table[x,]$author, y = leaderboard_table[x,]$repost_lc)
  #     })
  #     
  #     colors <- c(brewer.pal(4, "Paired"))
  #     
  #     hc <-highchart() %>%
  #       hc_chart(type = "column") %>%
  #       hc_colors(colors) %>%
  #       # hc_title(text = "Content Output - Monthly", align = "center") %>%
  #       hc_xAxis(type = "category") %>%
  #       hc_legend(enabled = TRUE) %>%
  #       hc_yAxis_multiples(
  #         list(title = list(text = "Num. of Posts"),  lineWidth = 3),
  #         list(title = list(text = input$leaderboard_variable_select), opposite = TRUE)) %>% 
  #       hc_add_series(data = ds_num_originals, name = "Num. Originals", type = "column") %>%
  #       hc_add_series(data = ds_num_reposts, name = "Num. Reposts", type = "column") %>%
  #       hc_add_series(data = ds_lc_originals, name = "Var. Originals", type = "line", yAxis = 1) %>%
  #       hc_add_series(data = ds_lc_reposts, name = "Var. Reposts", type = "line", yAxis = 1) %>%
  #       hc_plotOptions(
  #         line = list(stacking = "normal", marker = list(enabled = TRUE)), 
  #         column = list(stacking = "normal", grouping = TRUE, shadow = FALSE, borderColor = "black")) %>%
  #       hc_tooltip(
  #         shared = TRUE, 
  #         pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
  #       hc_add_theme(hc_theme_smpl())
  #     hc
  #    
  #    
  #   })
  # })
  
  output$LeaderboardAuthorsGraph <- renderHighchart({

    author_data <- DataArticles %>%
      group_by(author) %>%
      summarise(n = n(), link_clicks = sum(link_clicks)) %>%
      arrange(-n, -link_clicks)
    
    ds_author_data <- lapply(1:nrow(author_data), function(x){
      list(name = author_data[x,]$author, value = author_data[x,]$n, colorValue = author_data[x,]$link_clicks)
    })
    
    
    hc <-highchart() %>%
      # hc_chart(zoomType = "xy") %>%
      hc_colorAxis(minColor = '#FFFFFF', maxColor = JS("Highcharts.getOptions().colors[0]")) %>%
      hc_add_series(data = ds_author_data, type = "treemap") %>%
      hc_plotOptions(treemap = list(borderColor = "black")) %>%
      # hc_tooltip(pointFormat = "{series.name}: <b>{point.colorValue}</b><br/>")%>% 
      # hc_tooltip(formatter = JS("function(){ return '<div><b>' + this.point.name + '</b><br/>' + 'Num. of Posts: <b>' + this.point.value + '</b><br/>' + 'Link Clicks: <b>' + this.point.colorValue + '</b></div>'}"))%>% 
      hc_add_theme(hc_theme_smpl())
    hc
    
    # "function(){ return '<div><b>' + this.point.name + '</b>' + '<b>' this.point.colorValue '</b></div>'}"
    
    # fntltp <- JS(paste("function(){
    #                    return", '"',"<div style='width: 250px; white-space:normal;'>", '"', "+ 'Share text: <b>' + this.point.share_text + '</b>' + '<br/>' + '<img src ='", "+ this.point.photo_url +","'",'title=""', 'alt="" border="0" height="250" width="250">', "'+ '<br/><b>' + this.point.name + ' | Times Reposted: </b>' + this.point.times_repo + '<br/>Reach: <b>' + this.point.reach + '</b> | Rank: <b>' + this.point.rank_reach + '</b> | Gen. Rank: <b>' + this.point.general_rank_reach + '</b><br/>Interactions: <b>' + this.point.interactions + '</b> | Rank: <b>' + this.point.rank_interactions + '</b> | Gen. Rank: <b>' + this.point.general_rank_interactions + '</b></div>'",
    #                    "}"))


  })
  
  
  
   
  # -----------------------------------------------------------------------------------------------------------------------
  
  # Categories ------------------------------------------------------------------------------------------------------------
  
  output$LeaderboardCategoriesGraph <- renderHighchart({
    
    input$category_author_select_button
    isolate({
      
      DataArticles <- DataArticles[which(DataArticles$author %in% input$category_author_cs_select | DataArticles$author %in% input$category_author_os_select | DataArticles$author %in% input$category_author_c_select),]
      
      categories_data <- ddply(DataArticles, "category", summarize, avg_viral = round(mean(post_reach_viral_unique), 2), avg_fan = round(mean(post_reach_fan_unique), 2), avg_reach = round(mean(post_reach), 2))
      
      categories_data <- categories_data[order(categories_data$avg_reach, decreasing = TRUE), ]
      
      ds_viral <- lapply(1:nrow(categories_data), function(x){
        list(name = categories_data[x,]$category, y = categories_data[x,]$avg_viral)
      })
      
      ds_fan <- lapply(1:nrow(categories_data), function(x){
        list(name = categories_data[x,]$category, y = categories_data[x,]$avg_fan)
      })
      
      ds_reach <- lapply(1:nrow(categories_data), function(x){
        list(name = categories_data[x,]$category, y = categories_data[x,]$avg_reach)
      })
      
      colors <- c(brewer.pal(3, "Paired"))
      
      hc <-highchart() %>%
        hc_chart(type = "column") %>%
        hc_colors(colors) %>%
        hc_title(text = "Reach", align = "left", style = list(fontSize = "25px")) %>%
        hc_xAxis(type = "category") %>%
        hc_legend(enabled = TRUE) %>%
        hc_yAxis_multiples(
          list(title = list(text = "%"),  lineWidth = 3),
          list(title = list(text = "Reach"), opposite = TRUE)) %>% 
        hc_add_series(data = ds_viral, name = "Avg. Viral", type = "column") %>%
        hc_add_series(data = ds_fan, name = "Avg. Fan", type = "column") %>%
        hc_add_series(data = ds_reach, name = "Avg. Reach", type = "line", yAxis = 1) %>%
        hc_plotOptions(
          line = list(stacking = "none", marker = list(enabled = TRUE)), 
          column = list(stacking = "percent", grouping = TRUE, shadow = FALSE, borderColor = "black")) %>%
        hc_tooltip(
          shared = TRUE, 
          pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        hc_add_theme(hc_theme_smpl())
      hc
    })
  })
  
  output$CategoriesReachGraph <- renderHighchart({
    
    input$category_author_select_button
    isolate({
      
      DataArticles <- DataArticles[which(DataArticles$author %in% input$category_author_cs_select | DataArticles$author %in% input$category_author_os_select | DataArticles$author %in% input$category_author_c_select),]
      
      categories_data <- ddply(DataArticles, "category", summarize, original_posts = length(status_id[repost == 0]), repost_posts = length(status_id[repost == 1]), original_lc = sum(post_reach[repost == 0]), repost_lc = sum(post_reach[repost == 1]))
      
      categories_data <- categories_data[order(categories_data$original_lc + categories_data$repost_lc, decreasing = TRUE), ]
      
      ds_num_originals <- lapply(1:nrow(categories_data), function(x){
        list(name = categories_data[x,]$category, y = categories_data[x,]$original_posts)
      })
      
      ds_num_reposts <- lapply(1:nrow(categories_data), function(x){
        list(name = categories_data[x,]$category, y = categories_data[x,]$repost_posts)
      })
      
      ds_lc_originals <- lapply(1:nrow(categories_data), function(x){
        list(name = categories_data[x,]$category, y = categories_data[x,]$original_lc)
      })
      
      ds_lc_reposts <- lapply(1:nrow(categories_data), function(x){
        list(name = categories_data[x,]$category, y = categories_data[x,]$repost_lc)
      })
      
      colors <- c(brewer.pal(4, "Paired"))
      
      hc <-highchart() %>%
        hc_chart(type = "column") %>%
        hc_colors(colors) %>%
        hc_title(text = "Reach", align = "left", style = list(fontSize = "25px")) %>%
        hc_xAxis(type = "category") %>%
        hc_legend(enabled = TRUE) %>%
        hc_yAxis_multiples(
          list(title = list(text = "Num. of Posts"),  lineWidth = 3),
          list(title = list(text = "Reach"), opposite = TRUE)) %>% 
        hc_add_series(data = ds_num_originals, name = "Num. Originals", type = "column") %>%
        hc_add_series(data = ds_num_reposts, name = "Num. Reposts", type = "column") %>%
        hc_add_series(data = ds_lc_originals, name = "Var. Originals", type = "line", yAxis = 1) %>%
        hc_add_series(data = ds_lc_reposts, name = "Var. Reposts", type = "line", yAxis = 1) %>%
        hc_plotOptions(
          line = list(stacking = "normal", marker = list(enabled = TRUE)), 
          column = list(stacking = "normal", grouping = TRUE, shadow = FALSE, borderColor = "black")) %>%
        hc_tooltip(
          shared = TRUE, 
          pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  output$CategoriesLinkClicksGraph <- renderHighchart({
    
    input$category_author_select_button
    isolate({
      
      DataArticles <- DataArticles[which(DataArticles$author %in% input$category_author_cs_select | DataArticles$author %in% input$category_author_os_select | DataArticles$author %in% input$category_author_c_select),]
      
      categories_data <- ddply(DataArticles, "category", summarize, original_posts = length(status_id[repost == 0]), repost_posts = length(status_id[repost == 1]), original_lc = sum(link_clicks[repost == 0]), repost_lc = sum(link_clicks[repost == 1]))
      
      categories_data <- categories_data[order(categories_data$original_lc + categories_data$repost_lc, decreasing = TRUE), ]
      
      ds_num_originals <- lapply(1:nrow(categories_data), function(x){
        list(name = categories_data[x,]$category, y = categories_data[x,]$original_posts)
      })
      
      ds_num_reposts <- lapply(1:nrow(categories_data), function(x){
        list(name = categories_data[x,]$category, y = categories_data[x,]$repost_posts)
      })
      
      ds_lc_originals <- lapply(1:nrow(categories_data), function(x){
        list(name = categories_data[x,]$category, y = categories_data[x,]$original_lc)
      })
      
      ds_lc_reposts <- lapply(1:nrow(categories_data), function(x){
        list(name = categories_data[x,]$category, y = categories_data[x,]$repost_lc)
      })
      
      colors <- c(brewer.pal(4, "Paired"))
      
      hc <-highchart() %>%
        hc_chart(type = "column") %>%
        hc_colors(colors) %>%
        hc_title(text = "Link Clicks", align = "left", style = list(fontSize = "25px")) %>%
        hc_xAxis(type = "category") %>%
        hc_legend(enabled = TRUE) %>%
        hc_yAxis_multiples(
          list(title = list(text = "Num. of Posts"),  lineWidth = 3),
          list(title = list(text = "Link Clicks"), opposite = TRUE)) %>% 
        hc_add_series(data = ds_num_originals, name = "Num. Originals", type = "column") %>%
        hc_add_series(data = ds_num_reposts, name = "Num. Reposts", type = "column") %>%
        hc_add_series(data = ds_lc_originals, name = "Var. Originals", type = "line", yAxis = 1) %>%
        hc_add_series(data = ds_lc_reposts, name = "Var. Reposts", type = "line", yAxis = 1) %>%
        hc_plotOptions(
          line = list(stacking = "none", marker = list(enabled = TRUE)), 
          column = list(stacking = "normal", grouping = TRUE, shadow = FALSE, borderColor = "black")) %>%
        hc_tooltip(
          shared = TRUE, 
          pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
  output$CategoriesEngagementsGraph <- renderHighchart({
    
    input$category_author_select_button
    isolate({
      
      DataArticles <- DataArticles[which(DataArticles$author %in% input$category_author_cs_select | DataArticles$author %in% input$category_author_os_select | DataArticles$author %in% input$category_author_c_select),]
      
      categories_data <- ddply(DataArticles, "category", summarize, original_posts = length(status_id[repost == 0]), repost_posts = length(status_id[repost == 1]), original_lc = sum(total_interactions[repost == 0]), repost_lc = sum(total_interactions[repost == 1]))
      
      categories_data <- categories_data[order(categories_data$original_lc + categories_data$repost_lc, decreasing = TRUE), ]
      
      ds_num_originals <- lapply(1:nrow(categories_data), function(x){
        list(name = categories_data[x,]$category, y = categories_data[x,]$original_posts)
      })
      
      ds_num_reposts <- lapply(1:nrow(categories_data), function(x){
        list(name = categories_data[x,]$category, y = categories_data[x,]$repost_posts)
      })
      
      ds_lc_originals <- lapply(1:nrow(categories_data), function(x){
        list(name = categories_data[x,]$category, y = categories_data[x,]$original_lc)
      })
      
      ds_lc_reposts <- lapply(1:nrow(categories_data), function(x){
        list(name = categories_data[x,]$category, y = categories_data[x,]$repost_lc)
      })
      
      colors <- c(brewer.pal(4, "Paired"))
      
      hc <-highchart() %>%
        hc_chart(type = "column") %>%
        hc_colors(colors) %>%
        hc_title(text = "Engagements", align = "left", style = list(fontSize = "25px")) %>%
        hc_xAxis(type = "category") %>%
        hc_legend(enabled = TRUE) %>%
        hc_yAxis_multiples(
          list(title = list(text = "Num. of Posts"),  lineWidth = 3),
          list(title = list(text = "Engagements"), opposite = TRUE)) %>% 
        hc_add_series(data = ds_num_originals, name = "Num. Originals", type = "column") %>%
        hc_add_series(data = ds_num_reposts, name = "Num. Reposts", type = "column") %>%
        hc_add_series(data = ds_lc_originals, name = "Var. Originals", type = "line", yAxis = 1) %>%
        hc_add_series(data = ds_lc_reposts, name = "Var. Reposts", type = "line", yAxis = 1) %>%
        hc_plotOptions(
          line = list(stacking = "none", marker = list(enabled = TRUE)), 
          column = list(stacking = "normal", grouping = TRUE, shadow = FALSE, borderColor = "black")) %>%
        hc_tooltip(
          shared = TRUE, 
          pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.percentage:.0f}%)<br/>')%>%
        hc_add_theme(hc_theme_smpl())
      hc
      
    })
  })
  
}


# SHINY APP =========================================================================================================================

shinyApp(ui = ui, server = server)