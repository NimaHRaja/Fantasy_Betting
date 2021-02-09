# Reads the google sheets and extracts odds and stakes and writes them to CSV files.

clean_google_sheets <- function(this_week){
    
    DF_odds <- data.frame()
    DF_stakes <- data.frame()
    
    for(a_market in c("Winner", "Top4", "Relegation")){
        
        DF_raw <- read_xlsx(
            paste("Data/Google Sheets/FantasyPremierLeagueBetting_2020-21_", this_week, ".xlsx", 
                  sep = ""), 
            sheet = a_market, 
            # range = "A2:O22") # week18
            # range = cell_cols("A:Y")
            range = "A2:Y22"
        )
        
        DF_raw <- DF_raw[,c(1,3:5,17:25)]
        
        names(DF_raw)[5:15] <- 
            lapply(names(DF_raw)[5:15],
                   function(x) substr(x,1, (x %>% str_locate("\\."))-1))
        
        
        DF_odds <- rbind(DF_odds, 
                         DF_raw %>% 
                             select(Team, Back, Lay, Prob) %>% 
                             mutate(Week = this_week) %>%
                             mutate(Market = a_market))
        
        DF_stakes <- rbind(DF_stakes, 
                           # DF_raw[,c(1,7:15)] %>% # Week18
                           DF_raw %>% 
                               select(-Back,-Lay,-Prob) %>%
                               melt(id.vars = "Team") %>%
                               rename(Player = variable, Stakes = value) %>%
                               mutate(Stakes = if_else(is.na(Stakes), 0, Stakes)) %>%
                               mutate(Week = this_week) %>%
                               mutate(Market = a_market))
    }
    
    write.csv(DF_odds, paste("Data/odds/odds_", this_week, ".csv", sep = ""), row.names = FALSE)
    write.csv(DF_stakes, paste("Data/stakes/stakes_", this_week, ".csv", sep = ""), row.names = FALSE)
}