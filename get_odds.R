library(tidyverse)
library(rvest)
library(magrittr)
library(stringr)

url <- "https://www.oddschecker.com/football/world-cup/russia-v-saudi-arabia/correct-score"

#oi - odds shortening
#oo - odds drifting

get_odds <- function(html, row, odds_trend = NA){
  odds_trend <- ifelse(is.na(odds_trend), ".o", 
                       ifelse(odds_trend == "increase", ".oo", ".oi"))
  html %>% 
    html_nodes(paste0('#t1 :nth-child(', row, ') ', odds_trend)) %>% 
    html_text()
}

get_all_odds <- function(html, row){
  list(NA, "increase", "decrease") %>% 
    map(., function(x) get_odds(html, row, x)) %>% 
    unlist %>% 
    sapply(., function(x) eval(parse(text = x))) %>% 
    unlist %>% 
    `+`(1) %>% 
    `/`(1, .)
}

# probability = 1/(1 + fraction)
# names of the bets
bets <- read_html(url) %>% 
  html_nodes('.selTxt') %>%
  html_text()

# scrap odds for most probable bets
odds <- lapply(1:15, function(x) get_all_odds(read_html(url), x)) %>% 
  `names<-`(bets[1:15])

odds %<>% 
  sapply(mean) %>% 
  data.frame(bet = names(.), prob = ., row.names = NULL)

odds %<>% 
  mutate(result = bet %>% 
                  str_extract_all("[A-Za-z].* ") %>% 
                  trimws("right"),
         score = str_extract_all(as.character(odds$bet), "[0-9].*-[0-9].*", TRUE) %>% 
                  as.character(),
         goal_diff = sapply(score, function(x) eval(parse(text = x))))

calculate_ev <- function(bets_df, bet){
  bet_result <- str_extract_all(bet, "[A-Za-z].* ") %>% 
    trimws("right")
  
  bet_goal_diff <- str_extract_all(as.character(bet), "[0-9].*-[0-9].*", TRUE) %>% 
    as.character %>% 
    sapply(function(x) eval(parse(text = x)))
  
  
  score_prob <- bets_df[bets_df$bet == bet, "prob"] 
  
  result_diff_prob <- bets_df[(bets_df$result == bet_result) &
                                (bets_df$bet != bet) &
                                (bets_df$goal_diff == bet_goal_diff), "prob"] %>% sum

  result_prob <- bets_df[(bets_df$result == bet_result) & (bets_df$bet != bet) &
                           (bets_df$goal_diff != bet_goal_diff), "prob"] %>% sum

  ev = 4*score_prob + 2*result_diff_prob + 1*result_prob
  
  return(ev)
}

sapply(bets[1:15], function(x) calculate_ev(odds, x)) %>% 
  sort(decreasing = TRUE)


