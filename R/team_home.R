# load necessary packages
library(tidyverse) # for data wrangling
library(retrosheet) # download baseball data
library(furrr) # for parallel map functions

# setup parallelization 
plan(multisession, workers = 6)

# team_games function
# for a specific year and team download the 
team_games <- function(year_val,team_abbr){
  
  # download initial data
  t_df <- get_retrosheet("game", year_val)
  # extract values for appropriate home team
  # keep only essential columns
  t_df <- t_df %>%
    filter(HmTm == team_abbr) %>%
    select(Date,DblHdr,HmTm,VisRuns,HmRuns,Attendance,Duration)
  # add column to record difference between home team score and visitor score
  # use score difference to record if home team wins (W) or not (NW)
  t_df <- t_df %>%
    mutate(score_diff=HmRuns-VisRuns,
           WL=ifelse(score_diff > 0,"W","NW"))
  
  # use function from janitor package to 
  # clean up the column names
  t_df <- janitor::clean_names(t_df)
  
  # return the resulting data frame
  return(t_df)
  
}

# the years we want
x <- c(1912:2019,2021,2022)

# apply future map to desired years for Boston Red Sox
t_dl_bos <- future_map_dfr(x,team_games,list(team_abbr="BOS"))

# save the resulting data frame as a csv file for later use
write_csv(t_dl_bos,"data/boston_home.csv")
