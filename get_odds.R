library(tidyverse)
library(rvest)

url <- "https://www.oddschecker.com/football/world-cup/russia-v-saudi-arabia/correct-score"

read_html(url) %>% 
  html_nodes('.o') %>%
  html_text() 

read_html(url) %>% 
  html_nodes('.oi') %>%
  html_text()

read_html(url) %>% 
  html_nodes('.oo p') %>%
  html_text()

read_html(url) %>% 
  html_nodes('#t1 :nth-child(6) .oi') %>%
  html_text()

#oi - odds shortening
#oo - odds drifting

get_odds <- function(html, row, odds_trend = NA){
  odds_trend <- ifelse(is.na(odds_trend), ".o", 
                       ifelse(odds_trend == "increase", ".oo", ".oi"))
  html %>% 
    html_nodes(paste0('#t1 :nth-child(', row, ') ', odds_trend)) %>% 
    html_text()
}

read_html(url) %>% 
  get_odds(1, "increase")

# probability = 1/(1 + fraction)
bets <- read_html(url) %>% 
  html_nodes('.selTxt') %>%
  html_text()


lapply(1:10, function(x) get_all_odds(read_html(url), x)) %>% 
  `names<-`(bets[1:10])

get_all_odds <- function(html, row){
  list(NA, "increase", "decrease") %>% 
    map(., function(x) get_odds(html, row, x)) %>% 
    unlist %>% 
    sapply(., function(x) eval(parse(text = x))) %>% 
    unlist %>% 
    `+`(1) %>% 
    `/`(1, .)
}


