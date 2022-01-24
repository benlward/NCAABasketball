library(tidyverse)

r <- function(){runif(1,0,1)}

turnover <- function(to_pct,...){
  if (r()<to_pct) {
    return(TRUE) #turnover
  } else {
    return(FALSE)
  }
}

shot <- function(pct_try_3pts, pct_2pts, pct_3pts,...) {
  if (r() < pct_try_3pts) {
    if (r() < pct_3pts) { #takes 3pt shot
      result = list(shot = "3", points = 3)
      return(result)
    } else {
      result = list(shot = "3", points = 0)
      return(result)
    }
  } else {
    if (r() < pct_2pts) { #takes 2pt shot
      result = list(shot = "2", points = 2)
      return(result)
    } else {
      result = list(shot = "2", points = 0)
    } 
  }
}

rebound <- function(off_rebound,...){
  if (r() < off_rebound){
    keep_pos = TRUE
  } else {
    keep_pos = FALSE
  }
}

#Need to pass values from team analysis once done
values_team1 <- list(
  to_pct       = .10,
  off_rebound  = .10,
  pct_try_3pts = .25,
  pct_2pts     = .65,
  pct_3pts     = .45
)

values_team2 <- list(
  to_pct       = .15,
  off_rebound  = .15,
  pct_try_3pts = .30,
  pct_2pts     = .55,
  pct_3pts     = .70
)

switch_teams <-
  function(off,
           v1,
           v2,
           team1_pts,
           team2_pts) {
    if (off == "team1") {
      values = v2
      off = "team2"
      points = team2_pts
    } else {
      values = v1
      off = "team1"
      points = team1_pts
    }
    return(list(off = off, 
                values = values,
                points = points))
  }


#start game
clock <- 20*60
off <- "team1"
values <- values_team1
points <- team1_pts

game <- function(team1_pts = 0,
                 team2_pts = 0,
                 clock = 1200,
                 v1 = values_team1,
                 v2 = values_team2) {
  #tipoff
  off = "team1"
  values = v1
  points = team1_pts
  
  while (clock > 0) {
    
    #check for turnover
    if (do.call(turnover, values) == TRUE) {
      keep_pos = FALSE
      clock = clock - 5
    } else {
      keep_pos = TRUE
    }
    
    #if no turnover, take shot
    if (keep_pos == TRUE) {
      result <- do.call(shot, values)
      points <- points + result$points
      clock <- clock - 24 #need to pass clock time based on distribution
    }
    
    #missed shot, check rebound
    if (keep_pos == TRUE) {
      if (result$points == 0){
      keep_pos = do.call(rebound, values)
    }}
    
    #record points
    if (off == "team1") {
      team1_pts = points
    } else {
      team2_pts = points
    }
    
    #change off team if no off rebound
    if (keep_pos == FALSE) {
      switch <- switch_teams(off,
                              v1,
                              v2,
                              team1_pts,
                              team2_pts)
      off = switch$off
      values = switch$values
      points = switch$points
    }
    
  }
  return(list(team1_pts, team2_pts))
}

game()

sim_games <- replicate(10000, game()) %>% 
  t() %>% 
  as.data.frame() %>% 
  transmute(Team1 = as.integer(V1),
         Team2 = as.integer(V2)) %>% 
  mutate(win = case_when(
    Team1 > Team2 ~ 1,
    TRUE ~ 0
    ),
    game = row_number(),
    pct = cummean(win))

sim_games %>% 
  ggplot(aes(x=game, y=pct)) +
    geom_line() + 
    geom_hline(yintercept=mean(sim_games$win), color="blue")
  
  

