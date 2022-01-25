library(tidyverse)

Results <- read_csv("Data/DataFiles/RegularSeasonDetailedResults.csv")

WTeam <- Results %>%
  transmute(
    Season,
    DayNum,
    ID = WTeamID,
    OppID = LTeamID,
    Opp_M2 = LFGM - LFGM3,
    Opp_A2 = LFGA - LFGA3,
    Opp_M3 = LFGM3,
    Opp_A3 = LFGA3,
    Opp_OR  = LOR,
    Opp_DR  = LDR,
    Opp_BLK = LBlk,
    Opp_TO  = LTO,
    Opp_PF  = LPF,
    M2 = WFGM - WFGM3,
    A2 = WFGA - WFGA3,
    M3 = WFGM3,
    A3 = WFGA3,
    OR  = WOR,
    DR  = WDR,
    BLK = WBlk,
    TO  = WTO,
    PF  = WPF
  )

LTeam <- Results %>%
  transmute(
    Season,
    DayNum,
    ID = LTeamID,
    OppID = WTeamID,
    Opp_M2 = WFGM - WFGM3,
    Opp_A2 = WFGA - WFGA3,
    Opp_M3 = WFGM3,
    Opp_A3 = WFGA3,
    Opp_OR  = WOR,
    Opp_DR  = WDR,
    Opp_BLK = WBlk,
    Opp_TO  = WTO,
    Opp_PF  = WPF,
    M2 = LFGM - LFGM3,
    A2 = LFGA - LFGA3,
    M3 = LFGM3,
    A3 = LFGA3,
    OR  = LOR,
    DR  = LDR,
    BLK = LBlk,
    TO  = LTO,
    PF  = LPF
  )

Combine <- WTeam %>% 
  bind_rows(LTeam) %>% 
  mutate(
    Opp_PCT2 = Opp_M2/Opp_A2,
    Opp_PCT3 = Opp_M3/Opp_A3,
    Opp_OR_PCT = Opp_OR / (Opp_OR + DR),
    Opp_DR_PCT = Opp_DR / (Opp_DR + OR),
    PCT2 = M2/A2,
    PCT3 = M3/A3,
    OR_PCT = OR / (OR + Opp_DR),
    DR_PCT = DR / (DR + Opp_OR)
  )

Totals <- WTeam %>% 
  bind_rows(LTeam) %>%
  group_by(Season, ID) %>% 
  mutate_at(vars(-group_cols(), -DayNum), funs(cumsum)) %>% 
  mutate_at(vars(-group_cols(), -DayNum), funs(lag)) %>% 
  mutate(
    Opp_PCT2 = Opp_M2/Opp_A2,
    Opp_PCT3 = Opp_M3/Opp_A3,
    Opp_OR_PCT = Opp_OR / (Opp_OR + DR),
    Opp_DR_PCT = Opp_DR / (Opp_DR + OR),
    PCT2 = M2/A2,
    PCT3 = M3/A3,
    OR_PCT = OR / (OR + Opp_DR),
    DR_PCT = DR / (DR + Opp_OR)
  )

Combine <- Combine %>%
  left_join(Totals, #Team Totals
            by = c("Season", "ID", "DayNum"),
            suffix = c("", ".Prior")) %>% 
  left_join( #Opponent Totals
    Totals,
    by = c("Season", "OppID" = "ID", "DayNum"),
    suffix = c(".Result", ".OppPrior")
  ) 

#2 pointer percentage
twoPointer <- Combine %>% 
  filter(DayNum >= 50) %>% #Remove early games
  select(PCT2.Result, PCT2.Prior, Opp_PCT2.OppPrior) %>% 
  drop_na()

model_2pt <- lm(PCT2.Result~., data = twoPointer)

summary(model_2pt)

Int_2pt <- model_2pt$coefficients[1]
Prior_2pt <- model_2pt$coefficients[2]
Def_2pt <- model_2pt$coefficients[3]

predicted_2pt_pct <- function(int=Int_2pt,
                              b1=Prior_2pt,
                              b2=Def_2pt,
                              off_prior,
                              def_prior) {
  return(int + b1 * off_prior + b2 * def_prior)
}

#3 pointer percentage
threePointer <- Combine %>% 
  filter(DayNum >= 50) %>% #Remove early games
  select(PCT3.Result, PCT3.Prior, Opp_PCT3.OppPrior) %>% 
  drop_na()

model_3pt <- lm(PCT3.Result~., data = threePointer)

summary(model_3pt)

Int_3pt <- model_3pt$coefficients[1]
Prior_3pt <- model_3pt$coefficients[2]
Def_3pt <- model_3pt$coefficients[3]

predicted_3pt_pct <- function(int=Int_3pt,
                              b1=Prior_3pt,
                              b2=Def_3pt,
                              off_prior,
                              def_prior) {
  return(int + b1 * off_prior + b2 * def_prior)
}

#offensive rebound percentage
OR <- Combine %>% 
  filter(DayNum >= 50) %>% #Remove early games
  select(OR_PCT.Result, OR_PCT.Prior, Opp_OR_PCT.OppPrior) %>% 
  drop_na()

model_OR <- lm(OR_PCT.Result~., data = OR)

summary(model_OR)

Int_OR <- model_OR$coefficients[1]
Prior_OR <- model_OR$coefficients[2]
Def_OR <- model_OR$coefficients[3]

predicted_OR_pct <- function(int=Int_OR,
                              b1=Prior_OR,
                              b2=Def_OR,
                              off_prior,
                              def_prior) {
  return(int + b1 * off_prior + b2 * def_prior)
}

#offensive rebound percentage
OR <- Combine %>% 
  filter(DayNum >= 50) %>% #Remove early games
  select(OR_PCT.Result, OR_PCT.Prior, Opp_OR_PCT.OppPrior) %>% 
  drop_na()

model_OR <- lm(OR_PCT.Result~., data = OR)

summary(model_OR)

Int_OR <- model_OR$coefficients[1]
Prior_OR <- model_OR$coefficients[2]
Def_OR <- model_OR$coefficients[3]

predicted_OR_pct <- function(int=Int_OR,
                             b1=Prior_OR,
                             b2=Def_OR,
                             off_prior,
                             def_prior) {
  return(int + b1 * off_prior + b2 * def_prior)
}

#defensive rebound percentage
DR <- Combine %>% 
  filter(DayNum >= 50) %>% #Remove early games
  select(DR_PCT.Result, DR_PCT.Prior, Opp_DR_PCT.OppPrior) %>% 
  drop_na()

model_DR <- lm(DR_PCT.Result~., data = DR)

summary(model_DR)

Int_ <- model_DR$coefficients[1]
Prior_ <- model_DR$coefficients[2]
Def_OR <- model_DR$coefficients[3]

predicted_DR_pct <- function(int=Int_DR,
                             b1=Prior_DR,
                             b2=Def_DR,
                             off_prior,
                             def_prior) {
  return(int + b1 * off_prior + b2 * def_prior)
}
