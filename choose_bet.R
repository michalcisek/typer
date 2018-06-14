suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(stringr))

args = commandArgs(trailingOnly = TRUE)

#url to odds from different bookmakers
url <- args[1]
#number of most probable bets to consider (all/integer)
nbets <- args[2]
#whether to adjust probs to sum to 1 (yes/no)
adjust <- args[3]

source("functions.R")
source("get_odds.R")

odds <- readRDS(file)

# transform list with odds
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

# remove 'any other score' bet
odds %<>% 
  filter(bet != "Any Other Score")

# adjust probabilities to sum to 1 - INCORRECT APPROACH!
if(adjust == "yes") odds$prob <- odds$prob/sum(odds$prob)

# calculate expected value for every bet
sapply(bets[1:nbets], function(x) calculate_ev(odds, x)) %>%
  unlist %>% 
  sort(decreasing = TRUE) %>% 
  print
