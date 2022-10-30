library(fitzRoy) # fetch AFL stats
library(tidyverse) # data manipulation
library(lpSolve) # solve linear programming problems
library(gt) # create table outputs
library(gtExtras) # fancier table outputs
library(scales) # oorrect Price formatting

# fetch AFLW player stats and tidy to match AFLW Fantasy details
aflw_2022 <- fetch_player_stats_afl(season=2022, comp="AFLW") %>% # get 2022 AFLW stats
  filter(compSeason.shortName=="AFLW Season 7") %>% # filter only Season 7 (August-October)
  select(-home.team.name,-away.team.name,-extendedStats) %>% 
  mutate(Player= paste(player.givenName,player.surname)) %>% 
  mutate(Team = ifelse(team.name=="Sydney Swans","Sydney",team.name)) %>% # fix team name
  mutate(Player = case_when( # fix player names
    Player == "Eleanor Brown" ~ "Ellie Brown", 
    Player == "Bella R. Smith" ~ "Bella Smith", 
    Player == "Ruby Sargant-Wilson" ~ "Ruby Sargent-Wilson", 
    Player == "Lily-Rose Williamson" ~ "Lily Rose-Williamson", 
    Player == "Ella G. Smith" ~ "Ella Smith", 
    Player == "Genevieve Lawson Tavan" ~ "Genevieve Lawson-Tavan",
    TRUE ~ Player))

# Round 1 ownership details from AFLW Fantasy website
dataown <- read_csv("AFLWOwnR1.csv")

# club icon names inside the folder
images <- data.frame(
  Team = sort(unique(aflw_2022$Team)),
  img = c("adelaide","brisbane","carlton","collingwood","essendon","fremantle",
          "geelong","goldcoast","gws","hawthorn","north-melbourne","melbourne",
          "port","richmond","st-kilda","sydney","west-coast","western-bulldogs")
) %>% mutate(img = paste0("wfantasyicons/",img,".png"))

# AFLW Fantasy overall leader's total points by Round !(Round 2 unknown)!
ranks <- c(1239,0,3599,4745,6015,7247,8541,9892,11114,12525)

# function for finding optimal starting team
optimalwomen <- function(uptoround) {
  
  salary <- 1388400 # starting salary
  bench <- 5 * 28700 # minimum cash required for bench
  
  df <- aflw_2022 %>% 
    filter(round.roundNumber<=uptoround) %>% # filter up to the input round
    group_by(player.playerId) %>% 
    summarise(
      Player = last(Player), # player name
      Team = last(Team), # team name
      Points = sum(dreamTeamPoints), # total points
      .groups = 'drop'
    ) %>% 
    full_join(dataown, by = c("Player","Team")) %>% # join with AFLW Fantasy position data
    mutate(Price = parse_number(Price),isplayer = 1) %>% # convert prices to integers
    filter(!is.na(Position),!is.na(Points)) %>% # only keep players available before Round 1
    pivot_wider(names_from = Position, values_from = isplayer) %>% 
    select(Player,Team, Points, Price, Defender, Midfielder, Ruck, Forward)
  
  df_t <- t(df) # transpose for horizontal constraints
  
  # solve the problem
  mod <- lp(direction="max", # maximising points
            objective.in = df_t[3,],
            const.mat = df_t[4:nrow(df_t),], 
            const.dir = c("<=",rep("==",4)), 
            const.rhs = c(salary-bench, 5, 5, 1, 5), # less than available salary, with correct team structure
            all.bin=T) # only one of each player
  
  final <- cbind(answer = mod[["solution"]],df) %>% 
    filter(answer == 1) %>% 
    select(-answer) %>% 
    arrange(-Defender, -Ruck, -Midfielder, -Price, -Points) %>% # arrange by position, then price
    pivot_longer(cols=5:8,names_to = "Position",values_to = "Match") %>% # condense positions
    filter(Match==1) %>% select(-Match)
  
  total <- sum(final$Points) + max(final$Points) # total points = sum of points + captaincy double
  
  subtitle <- paste0("Would win overall by ",
                   total-ranks[uptoround], # difference to leader
                   " points, with ",
                   dollar_format()(salary-sum(final$Price)-bench), # salary leftover
                   " leftover from R1.")
  
  # prepare data for table output
  output <- final %>% 
    mutate(Player = ifelse(Points == max(Points), paste(Player,"  C"), ifelse(Points == max(Points[Points!=max(Points)]), paste(Player,"  VC"), Player))) %>% # add captain labels
    mutate(Price = dollar_format(prefix = "$", big.mark = ",")(Price)) %>% # convert price to dollar format
    left_join(images, by = "Team") %>% # add on the respective team images
    rbind(data.frame(Player = "", Team = "", Position = "Total Points", Points = as.integer(total),Price= "",img= "wfantasyicons/white.jpg")) %>% 
    select(` `= img, Player, Position, Price, Total = Points)
  
  # create table
  gt(output) %>% 
    tab_header(title=("Optimal AFLW Fantasy Team - 2022B"),subtitle = subtitle) %>% #title and subtitle
    gt_img_rows(columns = 1,img_source = "local",height = 18) %>% # load images from local file
    opt_table_font(font="Segoe UI") %>% # set font to match AFLW Fantasy website
    cols_align(align="right",columns = 1) # right align team images
}

# find a highly owned starting team that still wins
optimalown <- function(uptoround) {
  
  salary <- 1388400 # starting salary
  bench <- 5 * 28700 # minimum cash required for bench
  
  df <- aflw_2022 %>% 
    filter(round.roundNumber<=uptoround) %>% # filter up to the input round
    group_by(player.playerId) %>% 
    summarise(
      Player = last(Player), # player name
      Team = last(Team), # team name
      Points = sum(dreamTeamPoints), # total points
      .groups = 'drop'
    ) %>% 
    full_join(dataown, by = c("Player","Team")) %>% # join with AFLW Fantasy position data
    mutate(Price = parse_number(Price),isplayer = 1) %>% # convert prices to integers
    filter(!is.na(Position),!is.na(Points)) %>% # only keep players with a position at R1
    pivot_wider(names_from = Position, values_from = isplayer) %>% 
    select(Player, Team, OwnR1 = Owned, Points, Price, Defender, Midfielder, Ruck, Forward)
  
  df_t <- t(df) # transpose for horizontal constraints
  
  # solve the problem
  mod <- lp(direction="max", # maximising ownership
            objective.in = df_t[3,],
            const.mat = df_t[4:nrow(df_t),], 
            const.dir = c(">=","<=",rep("==",4)), 
            const.rhs = c(ranks[uptoround]-max(df$Points),salary-bench, 5, 5, 1, 5), # less than available salary, with correct team structure
            all.bin=T) # only one of each player
  
  final <- cbind(answer = mod[["solution"]],df) %>% 
    filter(answer == 1) %>% 
    select(-answer) %>% 
    arrange(-Defender, -Ruck, -Midfielder, -Price, -Points) %>% # arrange by position, then price
    pivot_longer(cols=6:9,names_to = "Position",values_to = "Match") %>% # condense positions
    filter(Match==1) %>% select(-Match)
  
  total <- sum(final$Points) + max(final$Points) # total points = sum of points + captaincy double
  
  subtitle <- paste0("Would win overall by ",
                     total-ranks[uptoround], # difference to leader
                     " points, with ",
                     dollar_format()(salary-sum(final$Price)-bench), # salary leftover
                     " leftover from R1.")
  
  # prepare data for table output
  output <- final %>% 
    mutate(Player = ifelse(Points == max(Points), paste(Player,"  C"), ifelse(Points == max(Points[Points!=max(Points)]), paste(Player,"  VC"), Player))) %>% # add captain labels
    mutate(Price = dollar_format(prefix = "$", big.mark = ",")(Price)) %>% # convert price to dollar format
    left_join(images, by = "Team") %>% # add on the respective team images
    rbind(data.frame(Player = "", Team = "", OwnR1 = "", Position = "Total Points", Points = as.integer(total),Price= "",img= "wfantasyicons/white.jpg")) %>% 
    select(` `= img, Player, Position, OwnR1, Price, Total = Points)
  
  # create table
  gt(output) %>% 
    tab_header(title=("Optimal AFLW Fantasy Team - 2022B"),subtitle = subtitle) %>% #title and subtitle
    gt_img_rows(columns = 1,img_source = "local",height = 18) %>% # load images from local file
    opt_table_font(font="Segoe UI") %>% # set font to match AFLW Fantasy website
    cols_align(align="right",columns = 1) # right align team images
  
}

optimalwomen(10) # find the best starting team

optimalown(10) # find a highly owned starting team that still wins
