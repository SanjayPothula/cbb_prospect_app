load("data/draft_prospects.RData")
library(gridExtra)
library(grid)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(shinycustomloader)
playerNames <- as.character(college_stats$cbb_url)
names(playerNames) <- college_stats$player_name

library(shiny)

# Define UI for application that draws a histogram
ui<-navbarPage(title="CBB Prospects",inverse = TRUE,
               tabPanel("Player Performance",
                        sidebarPanel(id="sidebar",
                                     selectInput(
                                       "playerID", "Player:",
                                       choices = playerNames
                                     ),
                                     actionButton("select_player", "Send It"), width = 2
                        ),
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Stats",withLoader(DT::dataTableOutput("stats"), type = "html"),
                                      tabsetPanel(type = "tabs", 
                                                  tabPanel("General", withLoader(plotOutput("general_graph", height = '700px'), type = "html")), 
                                                  tabPanel("Offense", withLoader(plotOutput("offense_graph", height = '700px'), type = "html")), 
                                                  tabPanel("Defense", withLoader(plotOutput("defense_graph", height = '550px'), type = "html"))
                                                  )
                                      )
                 ))
               ), 
               tabPanel("LeaderBoard", fluidRow(column(3, uiOutput("conf")), column(3, uiOutput("pos"))), withLoader(DT::dataTableOutput("stats_leaders"), type = "html"))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  player_id <- reactiveVal(playerNames[1])
  
  observeEvent(input$select_player, {
    new_player= input$playerID
    player_id(new_player)
  })
  
  observe({
    updateSelectInput(session, 'playerID', "Player:", choices = playerNames, player_id())
  })
  output$stats<-DT::renderDataTable({
    player_df<-college_stats %>% filter(cbb_url == player_id()) %>% ungroup() %>%
      select(school_name, conf, pos, N,  g_yr, pts_m, 
             fg3a_per_fga_pct, ts_pct, ast_m, trb_m, tov_m, 
             stl_m, blk_m, sos, Xwar_season) %>% mutate(
               pts_m = format(round(pts_m, 1), nsmall = 1), 
               fg3a_per_fga_pct = format(round(fg3a_per_fga_pct, 3), nsmall = 3), 
               ts_pct = format(round(ts_pct, 3), nsmall = 3), 
               ast_m = format(round(ast_m, 1), nsmall = 1), 
               trb_m = format(round(trb_m, 1), nsmall = 1), 
               tov_m = format(round(tov_m, 1), nsmall = 1), 
               stl_m = format(round(stl_m, 1), nsmall = 1), 
               blk_m = format(round(blk_m, 1), nsmall = 1), 
               sos = format(round(sos, 2), nsmall = 2), 
               Xwar_season = format(round(Xwar_season, 2), nsmall = 2)
             )
    DT::datatable(player_df, 
                  rownames = FALSE, 
                  colnames = c("School", "Conf", "Pos", "N", "G", "PTS", "FG3R", "TS%", "AST", "TRB", "TOV", "STL", "BLK", "SOS", 'xWAR'), 
                  options = list(dom = 't',ordering = F, pageLength = 25, columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  })
  output$general_graph <- renderPlot({
    single_player<-college_stats %>% group_by(pos) %>% mutate(
      sos = round(cume_dist(sos)*100),
      ts_pct= round(cume_dist(ts_pct)*100),
      pts_m= round(cume_dist(pts_m)*100),
      ast_m= round(cume_dist(ast_m)*100),
      trb_m= round(cume_dist(trb_m)*100),
      tov_m= round(cume_dist(tov_m)*100),
      pf_m= 100 - round(cume_dist(pf_m)*100),
      blk_m= round(cume_dist(blk_m)*100),
      stl_m= round(cume_dist(stl_m)*100),
      fg3a_per_fga_pct= round(cume_dist(fg3a_per_fga_pct)*100),
      usg_prx= round(cume_dist(usg_prx)*100),
      Xwar_season= round(cume_dist(Xwar_season)*100)
    ) %>% filter(cbb_url == player_id())
    
    Xwar_season<-ggplot(data = single_player, aes(x= player_name, y = Xwar_season)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("Expected WAR") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                       axis.text.y=element_blank(),
                                                                                                       axis.title.y=element_blank(),
                                                                                                       axis.title.x = element_blank())
    
    sos<-ggplot(data = single_player, aes(x= player_name, y = sos)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("SOS") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                              axis.text.y=element_blank(),
                                                                                              axis.title.y=element_blank(),
                                                                                              axis.title.x = element_blank())
    ts_pct<-ggplot(data = single_player, aes(x= player_name, y = ts_pct)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("TS%") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                              axis.text.y=element_blank(),
                                                                                              axis.title.y=element_blank(),
                                                                                              axis.title.x = element_blank())
    
    pts_m<-ggplot(data = single_player, aes(x= player_name, y = pts_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("PTS Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    ast_m<-ggplot(data = single_player, aes(x= player_name, y = ast_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("AST Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    trb_m<-ggplot(data = single_player, aes(x= player_name, y = trb_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("TRB Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    tov_m<-ggplot(data = single_player, aes(x= player_name, y = tov_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("TOV Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    blk_m<-ggplot(data = single_player, aes(x= player_name, y = blk_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("BLK Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    stl_m<-ggplot(data = single_player, aes(x= player_name, y = stl_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("STL Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    fg3a_per_fga_pct<-ggplot(data = single_player, aes(x= player_name, y = fg3a_per_fga_pct)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("3-Point Rate Per FG") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                              axis.text.y=element_blank(),
                                                                                                              axis.title.y=element_blank(),
                                                                                                              axis.title.x = element_blank())
    
    usg_prx<-ggplot(data = single_player, aes(x= player_name, y = usg_prx)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("Usage") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                axis.text.y=element_blank(),
                                                                                                axis.title.y=element_blank(),
                                                                                                axis.title.x = element_blank())
    grid.arrange(Xwar_season, sos, usg_prx, ncol = 1, top=textGrob("General",gp=gpar(fontsize=20,font=2)))
  })
  output$offense_graph <- renderPlot({
    single_player<-college_stats %>% group_by(pos) %>% mutate(
      sos = round(cume_dist(sos)*100),
      ts_pct= round(cume_dist(ts_pct)*100),
      pts_m= round(cume_dist(pts_m)*100),
      ast_m= round(cume_dist(ast_m)*100),
      trb_m= round(cume_dist(trb_m)*100),
      tov_m= 100 - round(cume_dist(tov_m)*100),
      pf_m= 100 - round(cume_dist(pf_m)*100),
      blk_m= round(cume_dist(blk_m)*100),
      stl_m= round(cume_dist(stl_m)*100),
      fg3a_per_fga_pct= round(cume_dist(fg3a_per_fga_pct)*100),
      usg_prx= round(cume_dist(usg_prx)*100),
      Xwar_season= round(cume_dist(Xwar_season)*100)
    ) %>% filter(cbb_url == player_id())
    
    Xwar_season<-ggplot(data = single_player, aes(x= player_name, y = Xwar_season)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("Expected WAR") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                       axis.text.y=element_blank(),
                                                                                                       axis.title.y=element_blank(),
                                                                                                       axis.title.x = element_blank())
    
    sos<-ggplot(data = single_player, aes(x= player_name, y = sos)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("SOS") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                              axis.text.y=element_blank(),
                                                                                              axis.title.y=element_blank(),
                                                                                              axis.title.x = element_blank())
    ts_pct<-ggplot(data = single_player, aes(x= player_name, y = ts_pct)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("TS%") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                              axis.text.y=element_blank(),
                                                                                              axis.title.y=element_blank(),
                                                                                              axis.title.x = element_blank())
    
    pts_m<-ggplot(data = single_player, aes(x= player_name, y = pts_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("PTS Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    ast_m<-ggplot(data = single_player, aes(x= player_name, y = ast_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("AST Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    trb_m<-ggplot(data = single_player, aes(x= player_name, y = trb_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("TRB Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    tov_m<-ggplot(data = single_player, aes(x= player_name, y = tov_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("TOV Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    blk_m<-ggplot(data = single_player, aes(x= player_name, y = blk_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("BLK Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    stl_m<-ggplot(data = single_player, aes(x= player_name, y = stl_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("STL Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    fg3a_per_fga_pct<-ggplot(data = single_player, aes(x= player_name, y = fg3a_per_fga_pct)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("3-Point Rate Per FG") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                              axis.text.y=element_blank(),
                                                                                                              axis.title.y=element_blank(),
                                                                                                              axis.title.x = element_blank())
    
    usg_prx<-ggplot(data = single_player, aes(x= player_name, y = usg_prx)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("Usage") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                axis.text.y=element_blank(),
                                                                                                axis.title.y=element_blank(),
                                                                                                axis.title.x = element_blank())
    grid.arrange(pts_m, fg3a_per_fga_pct, ts_pct, ast_m, trb_m, tov_m, ncol = 2, top=textGrob("Offense",gp=gpar(fontsize=20,font=2)))
  })
  output$defense_graph <- renderPlot({
    single_player<-college_stats %>% group_by(pos) %>% mutate(
      sos = round(cume_dist(sos)*100),
      ts_pct= round(cume_dist(ts_pct)*100),
      pts_m= round(cume_dist(pts_m)*100),
      ast_m= round(cume_dist(ast_m)*100),
      trb_m= round(cume_dist(trb_m)*100),
      tov_m= 100 - round(cume_dist(tov_m)*100),
      pf_m= 100 - round(cume_dist(pf_m)*100),
      blk_m= round(cume_dist(blk_m)*100),
      stl_m= round(cume_dist(stl_m)*100),
      fg3a_per_fga_pct= round(cume_dist(fg3a_per_fga_pct)*100),
      usg_prx= round(cume_dist(usg_prx)*100),
      Xwar_season= round(cume_dist(Xwar_season)*100)
    ) %>% filter(cbb_url == player_id())
    
    Xwar_season<-ggplot(data = single_player, aes(x= player_name, y = Xwar_season)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("Expected WAR") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                       axis.text.y=element_blank(),
                                                                                                       axis.title.y=element_blank(),
                                                                                                       axis.title.x = element_blank())
    
    sos<-ggplot(data = single_player, aes(x= player_name, y = sos)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("SOS") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                              axis.text.y=element_blank(),
                                                                                              axis.title.y=element_blank(),
                                                                                              axis.title.x = element_blank())
    ts_pct<-ggplot(data = single_player, aes(x= player_name, y = ts_pct)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("TS%") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                              axis.text.y=element_blank(),
                                                                                              axis.title.y=element_blank(),
                                                                                              axis.title.x = element_blank())
    
    pts_m<-ggplot(data = single_player, aes(x= player_name, y = pts_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("PTS Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    ast_m<-ggplot(data = single_player, aes(x= player_name, y = ast_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("AST Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    trb_m<-ggplot(data = single_player, aes(x= player_name, y = trb_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("TRB Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    tov_m<-ggplot(data = single_player, aes(x= player_name, y = tov_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("TOV Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    blk_m<-ggplot(data = single_player, aes(x= player_name, y = blk_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("BLK Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    stl_m<-ggplot(data = single_player, aes(x= player_name, y = stl_m)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("STL Per 36") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.title.y=element_blank(),
                                                                                                     axis.title.x = element_blank())
    
    fg3a_per_fga_pct<-ggplot(data = single_player, aes(x= player_name, y = fg3a_per_fga_pct)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("3-Point Rate Per FG") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                              axis.text.y=element_blank(),
                                                                                                              axis.title.y=element_blank(),
                                                                                                              axis.title.x = element_blank())
    
    usg_prx<-ggplot(data = single_player, aes(x= player_name, y = usg_prx)) + 
      geom_bar(stat = "identity", width = 0.3, color = 'black', fill = 'red') + 
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) + coord_flip() + 
      geom_hline(aes(yintercept = 25), color = 'black')+ geom_hline(aes(yintercept = 50), color = 'black')+ 
      geom_hline(aes(yintercept = 75), color = 'black') + theme_bw() + ggtitle("Usage") + theme(plot.margin = unit(c(2.5,1,2.5,1),"cm"),
                                                                                                axis.text.y=element_blank(),
                                                                                                axis.title.y=element_blank(),
                                                                                                axis.title.x = element_blank())
    grid.arrange(stl_m, blk_m, ncol = 1, top=textGrob("Defense",gp=gpar(fontsize=20,font=2)))
  })
  output$conf <- renderUI({
    selectInput("conf_input", "Conference:", choices = c(unique(as.character(college_stats$conf)), 'All'), selected = 'All')
  })
  output$pos <- renderUI({
    selectInput("pos_input", "Position:", choices = c(unique(as.character(college_stats$pos)), 'All'), selected = 'All')
  })
  output$stats_leaders<-DT::renderDataTable({
    if(input$conf_input != 'All'){
      college_stats <- college_stats %>% filter(as.character(conf) == input$conf_input)
    }
    if(input$pos_input != 'All'){
      college_stats <- college_stats %>% filter(as.character(pos) == input$pos_input)
    }
    player_df<-college_stats %>% ungroup() %>%
      select(player_name, school_name, conf, pos, N,  g_yr, pts_m, 
             fg3a_per_fga_pct, ts_pct, ast_m, trb_m, tov_m, 
             stl_m, blk_m, sos, Xwar_season) %>% mutate(
               pts_m = format(round(pts_m, 1), nsmall = 1), 
               fg3a_per_fga_pct = format(round(fg3a_per_fga_pct, 3), nsmall = 3), 
               ts_pct = format(round(ts_pct, 3), nsmall = 3), 
               ast_m = format(round(ast_m, 1), nsmall = 1), 
               trb_m = format(round(trb_m, 1), nsmall = 1), 
               tov_m = format(round(tov_m, 1), nsmall = 1), 
               stl_m = format(round(stl_m, 1), nsmall = 1), 
               blk_m = format(round(blk_m, 1), nsmall = 1), 
               sos = format(round(sos, 2), nsmall = 2), 
               Xwar_season = format(round(Xwar_season, 2), nsmall = 2)
             )
    DT::datatable(player_df, 
                  rownames = FALSE, 
                  colnames = c("Name", "School", "Conf", "Pos", "N", "G", "PTS", "FG3R", "TS%", "AST", "TRB", "TOV", "STL", "BLK", "SOS", 'xWAR'), 
                  options = list(ordering = T, pageLength = 25, columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  })
  session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)

