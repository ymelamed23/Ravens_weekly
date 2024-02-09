library(nflfastR)
library(tidyverse)
library(reactable)
library(knitr)
library(htmltools)
library(kableExtra)
library(reactablefmtr)
#load pbp
pbp=load_pbp(season=2023)
#load roster data
roster=fast_scraper_roster(2023)
#choose week
current_week=13
#make pass plays dataset with roster data
pass_plays = pbp %>% 
  filter(pass==1, !is.na(receiver_id), aborted_play==0) %>% 
  select(defteam, posteam, pass,epa, complete_pass, aborted_play, receiver_id) %>% 
  left_join(roster, by = c("receiver_id" = "gsis_id"))
#find defensive epa by for TEs
te_pass = pass_plays %>% 
  filter(position=="TE") %>% 
  group_by(defteam) %>% 
  summarise(epa=sum(epa), plays=n())
#find defensive epa for RBs
rb_pass = pass_plays %>% 
  filter(position=="RB") %>% 
  group_by(defteam) %>% 
  summarise(epa=sum(epa), plays=n())

########## add explosive games
#explosive plays
pbp = pbp %>% 
  mutate(explosive_play = ifelse((!is.na(rushing_yards) & rushing_yards>10)|(!is.na(passing_yards) & passing_yards>20), 1, 0)) 
#clean data no aborted plays
clean = pbp %>% 
  filter(play_type=="pass"|play_type=="run", aborted_play==0) 
#explosive offense rate
explosive_o = clean %>% 
  group_by(posteam) %>% 
  summarise(explosive_plays_o = sum(explosive_play), plays_o=n(), explosive_rate_o=explosive_plays_o/plays_o)
#explosive defense rate
explosive_d = clean %>% 
  group_by(defteam) %>% 
  summarise(explosive_plays_d = sum(explosive_play), plays_d=n(), explosive_rate_d=explosive_plays_d/plays_d)
#load games
games=fast_scraper_schedules(2023)
games = games %>% filter(week==current_week)
#combine with explosive data
explosive_games_home = games %>% select(season, week, home_team, away_team, spread_line, roof) %>% 
  left_join(explosive_o, by=c("home_team"="posteam")) %>% 
  left_join(explosive_d, by = c("away_team"="defteam")) %>% 
  mutate(posteam=home_team, defteam=away_team)
explosive_games_away  =  games %>% select(season, week, home_team, away_team, spread_line, roof) %>% 
  left_join(explosive_o, by=c("away_team"="posteam")) %>% 
  left_join(explosive_d, by = c("home_team"="defteam")) %>% 
  mutate(posteam=away_team, defteam=home_team)
#combine two datasets
explosive_games = rbind(explosive_games_home, explosive_games_away) 
#find differential 
explosive_games = explosive_games %>% 
  mutate(explosive_diff=explosive_rate_o + explosive_rate_d)
#scripted
script_o=pbp %>% 
  group_by(posteam) %>% 
  filter(!is.na(epa), drive==1|drive==2) %>% 
  summarise(scripted_o_epa=mean(epa)) %>% 
  arrange(-scripted_o_epa)
script_d=pbp %>% 
  group_by(defteam) %>% 
  filter(!is.na(epa), drive==1|drive==2) %>% 
  summarise(scripted_d_epa=mean(epa))
#first half
first_half_o=pbp %>% 
  group_by(posteam) %>% 
  filter(!is.na(epa), game_half=="Half1") %>% 
  summarise(first_half_o_epa=mean(epa)) 
first_half_d=pbp %>% 
  group_by(defteam) %>% 
  filter(!is.na(epa), game_half=="Half1") %>% 
  summarise(first_half_d_epa=mean(epa)) 
#ADD LOGOS
logos=teams_colors_logos
logos=logos %>% select(team_abbr, team_logo_espn)
explosive_games = explosive_games %>% 
  left_join(logos, by = c("posteam"="team_abbr")) %>% 
  rename(pos_logo=team_logo_espn) %>% 
  left_join(logos, by = c("defteam"="team_abbr")) %>%
  rename(def_logo=team_logo_espn)
explosive_games=explosive_games %>% 
  mutate(logo_pos=sprintf('![](%s)', pos_logo),
         logo_def=sprintf('![](%s)', def_logo))

######### combine
betting_angles = explosive_games %>% 
  left_join(te_pass, by = c("defteam")) %>% 
  rename(te_epa=epa) %>% 
  left_join(rb_pass, by = c("defteam")) %>% 
  rename(rb_epa=epa) %>% 
  left_join(script_o, by =c("posteam")) %>% 
  left_join(script_d, by = c("defteam")) %>% 
  left_join(first_half_o, by = c("posteam")) %>% 
  left_join(first_half_d, by = c("defteam"))
#add differential for first half and script
betting_angles = betting_angles %>% 
  mutate(script_diff= (scripted_o_epa+scripted_d_epa),
         first_half_diff= (first_half_o_epa+first_half_d_epa))
  
#prepare for table
betting_angles_table=betting_angles %>% 
  select(week, pos_logo, def_logo,explosive_diff, explosive_rate_o, explosive_rate_d, explosive_plays_o, explosive_plays_d,
         te_epa, rb_epa, scripted_o_epa, scripted_d_epa, script_diff, first_half_o_epa, first_half_d_epa, first_half_diff)
########### make chart #####################################################


#make chart
color_set <- c("#f7c844","#429460","#2e6d9e")
betting_angles_table[, 4:16 ] <- round(betting_angles_table[,4:16], digits = 3)
betting_angles_table %>% reactable(
  columns = list(
    week=colDef(name="Week"),
    pos_logo = colDef(cell = embed_img(height = 60, width = 60), name = "Offense"),
    def_logo = colDef(cell = embed_img(height = 60, width = 60), name = "Defense"),
    explosive_diff=colDef(name="Explosive Rate Advantage", format = colFormat(digits = 3), cell = color_tiles(betting_angles_table[], colors = color_set), align = "center", maxWidth = 130),
    explosive_rate_o=colDef(name="Explosive Rate Offense", format = colFormat(digits = 3), cell = color_tiles(., colors = color_set),align = "center", maxWidth = 130),
    explosive_rate_d=colDef(name = "Explosive Rate Defense",format = colFormat(digits = 3), cell = color_tiles(., colors = color_set), align = "center", maxWidth = 130),
    explosive_plays_o=colDef(name="Explosive Plays Offense",format = colFormat(digits = 0), cell = color_tiles(., colors = color_set), align = "center", maxWidth = 130),
    explosive_plays_d=colDef(name="Explosive Plays Defense",format = colFormat(digits = 0), cell = color_tiles(., colors = color_set), align = "center", maxWidth = 130),
    te_epa=colDef(name="Defensive TE EPA",format = colFormat(digits = 2),cell = color_tiles(., colors = color_set), align = "center", maxWidth = 130),
    rb_epa=colDef(name="Defensive RB EPA", format = colFormat(digits = 2),cell = color_tiles(., colors = color_set), align = "center", maxWidth = 130),
    script_diff=colDef(name = "Script Plays Advantage", format = colFormat(digits=2),cell = color_tiles(., colors = color_set), align = "center", maxWidth = 130),
    scripted_o_epa = colDef(name = "Scripted EPA Offense", format = colFormat(digits = 2), cell = color_tiles(., colors = color_set), align = "center", maxWidth = 130),
    scripted_d_epa = colDef(name = "Scripted EPA Defense", format = colFormat(digits=2), cell = color_tiles(., colors = color_set), align = "center", maxWidth = 130),
    first_half_diff= colDef(name="1st Half Advantage", format = colFormat(digits=2),cell = color_tiles(., colors = color_set), align = "center", maxWidth = 130),
    first_half_o_epa = colDef(name = "1st Half Avg EPA Offense", format = colFormat(digits = 2), cell = color_tiles(., colors = color_set), align = "center", maxWidth = 130),
    first_half_d_epa  = colDef(name = "1st Half Avg EPA Defense", format = colFormat(digits = 2), cell = color_tiles(., colors = color_set), align = "center", maxWidth = 130)),
  columnGroups = list(
    # colGroup(name = "Game", columns = c("week", "posteam", "defteam")),
    colGroup(name="Explosive Rate", columns = c("explosive_diff", "explosive_rate_o",
                                                "explosive_rate_d", "explosive_plays_o",
                                                "explosive_plays_d")),
    colGroup(name="Positional Total EPA", columns = c("te_epa", "rb_epa")),
    colGroup(name = "Scripted Plays", columns = c("script_diff", "scripted_o_epa", "scripted_d_epa")),
    colGroup(name = "First Half", columns = c("first_half_diff", "first_half_o_epa", "first_half_d_epa"))),
  theme=espn(),
  highlight = TRUE,
  striped = TRUE,
  pagination = FALSE)



#add
#explosive pass
#yac
#pass location
#pass length
#player with most explosive plays on offense
























########OLD 
#add colors
exdiff_normalized <- (betting_angles_table$explosive_diff - min(betting_angles_table$explosive_diff)) / (max(betting_angles_table$explosive_diff) - min(betting_angles_table$explosive_diff))
exdiff_colors <-  rgb(colorRamp(c("#FFFFFF", "#10e642"))(exdiff_normalized), maxColorValue = 255)
te_normalized <- (betting_angles_table$te_epa - min(betting_angles_table$te_epa)) / (max(betting_angles_table$te_epa) - min(betting_angles_table$te_epa))
te_colors <-  rgb(colorRamp(c("#FFFFFF", "#10e642"))(te_normalized), maxColorValue = 255)
rb_normalized <- (betting_angles_table$rb_epa - min(betting_angles_table$rb_epa)) / (max(betting_angles_table$rb_epa) - min(betting_angles_table$rb_epa))
rb_colors <-  rgb(colorRamp(c("#FFFFFF", "#10e642"))(rb_normalized), maxColorValue = 255)
scripted_normalized <- (betting_angles_table$script_diff - min(betting_angles_table$script_diff)) / (max(betting_angles_table$script_diff) - min(betting_angles_table$script_diff))
script_colors <-  rgb(colorRamp(c("#FFFFFF", "#10e642"))(scripted_normalized), maxColorValue = 255)
firsthalf_normalized <- (betting_angles_table$first_half_diff - min(betting_angles_table$first_half_diff)) / (max(betting_angles_table$first_half_diff) - min(betting_angles_table$first_half_diff))
firsthalf_colors <-  rgb(colorRamp(c("#FFFFFF", "#10e642"))(firsthalf_normalized), maxColorValue = 255)


reactable(betting_angles_table[], 
          columns = list(
            week=colDef(name="Week"),
            pos_logo = colDef(cell = embed_img(height = 60, width = 60), name = "Offense"),
            def_logo = colDef(cell = embed_img(height = 60, width = 60), name = "Defense"),
            explosive_diff=colDef(name="Explosive Rate Advantage", format = colFormat(digits = 3),
                                  style = JS("function(rowInfo, column, state) {
        const { showColors, exdiffColors } = state.meta
        if (showColors) {
          return { backgroundColor: exdiffColors[rowInfo.index] }
        }
      }")),
            explosive_rate_o=colDef(name="Explosive Rate Offense", format = colFormat(digits = 3)),
            explosive_rate_d=colDef(name = "Explosive Rate Defense",format = colFormat(digits = 3)),
            explosive_plays_o=colDef(name="Explosive Plays Offense",format = colFormat(digits = 0)),
            explosive_plays_d=colDef(name="Explosive Plays Defense",format = colFormat(digits = 0)),
            te_epa=colDef(name="Defensive TE EPA",format = colFormat(digits = 2),
                          style = JS("function(rowInfo, column, state) {
        const { showColors, teColors } = state.meta
        if (showColors) {
          return { backgroundColor: teColors[rowInfo.index] }
        }
      }")),
            rb_epa=colDef(name="Defensive RB EPA", format = colFormat(digits = 2),
                          style = JS("function(rowInfo, column, state) {
        const { showColors, rbColors } = state.meta
        if (showColors) {
          return { backgroundColor: rbColors[rowInfo.index] }
        }
      }")),
            script_diff=colDef(name = "Script Plays Advantage", format = colFormat(digits=2),
          style = JS("function(rowInfo, column, state) {
        const { showColors, scriptColors } = state.meta
        if (showColors) {
          return { backgroundColor: scriptColors[rowInfo.index] }
        }
      }")),
          scripted_o_epa = colDef(name = "Scripted EPA Offense", format = colFormat(digits = 2)),
          scripted_d_epa = colDef(name = "Scripted EPA Defense", format = colFormat(digits=2)),
          first_half_diff= colDef(name="1st Half Advantage", format = colFormat(digits=2),
                                  style = JS("function(rowInfo, column, state) {
        const { showColors, firsthalfColors } = state.meta
        if (showColors) {
          return { backgroundColor: firsthalfColors[rowInfo.index] }
        }
      }")),
          first_half_o_epa = colDef(name = "1st Half EPA Offense", format = colFormat(digits = 2)),
          first_half_d_epa  = colDef(name = "1st Half EPA Defense", format = colFormat(digits = 2))),
          columnGroups = list(
           # colGroup(name = "Game", columns = c("week", "posteam", "defteam")),
            colGroup(name="Explosive Rate", columns = c("explosive_diff", "explosive_rate_o",
                                                        "explosive_rate_d", "explosive_plays_o",
                                                        "explosive_plays_d")),
            colGroup(name = "Scripted Plays", columns = c("script_diff", "scripted_o_epa", "scripted_d_epa")),
            colGroup(name = "First Half", columns = c("first_half_diff", "first_half_o_epa", "first_half_d_epa"))),
          meta = list(
            exdiffColors = exdiff_colors,
            teColors= te_colors,
            rbColors=rb_colors,
            scriptColors = script_colors,
            firsthalfColors = firsthalf_colors,
            showColors = TRUE),
          defaultColDef =
            colDef(
              cell = data_bars(betting_angles_table[], fill_color = viridis::mako(5), bar_height = 30)))
