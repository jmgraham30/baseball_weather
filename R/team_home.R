library(tidyverse)
library(retrosheet)
library(furrr)
plan(multisession, workers = 6)


team_games <- function(year_val,team_abbr){
  
  t_df <- get_retrosheet("game", year_val)
  t_df <- t_df %>%
    filter(HmTm == team_abbr) %>%
    select(Date,DblHdr,HmTm,VisRuns,HmRuns,Attendance,Duration)
  
  t_df <- t_df %>%
    mutate(score_diff=HmRuns-VisRuns,
           WL=ifelse(score_diff > 0,"W","NW"))
  
  t_df <- janitor::clean_names(t_df)
  
  return(t_df)
  
}


x <- c(1912:2019,2021,2022)

t_dl_bos <- future_map_dfr(x,team_games,list(team_abbr="BOS"))

glimpse(t_dl_bos)

write_csv(t_dl_bos,"data/boston_home.csv")
