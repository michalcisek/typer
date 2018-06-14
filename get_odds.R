# get names of the bets
bets <- read_html(url) %>% 
  html_nodes('.selTxt') %>%
  html_text()

# how many bets you want to include?
if (nbets == "all"){
  nbets <- length(bets)
} else {
  nbets <- as.numeric(nbets)
}

# scrap odds for most probable bets
odds <- lapply(1:nbets, function(x) get_all_odds(read_html(url), x)) %>% 
  `names<-`(bets[1:nbets])

# save odds to RDS file
file <- paste0("./odds/", unlist(strsplit(url, "/"))[6], "_", Sys.Date(), ".rds")
saveRDS(odds, file)