first_week <- 18

# Reading Stakes

DF_stakes <- 
    do.call("rbind", lapply(
        list.files("Data/stakes/", full.names = TRUE), read.csv)) %>%
    group_by(Team, Player, Week, Market) %>%
    summarise(Stakes = sum(Stakes)) %>%
    ungroup()

# Reading Odds

DF_odds <- 
    do.call("rbind", lapply(
        list.files("Data/odds/", full.names = TRUE), read.csv)) %>%
    mutate(week_number = substr(Week, 5, 6))

# Merging Odds and Stakes
# If stakes are negative/positive Lay/Back is used
DF_transactions <- 
    inner_join(DF_odds, DF_stakes,
               by = c("Team", "Week", "Market")) %>%
    mutate(Profit = if_else(Stakes > 0, Stakes * Back, Stakes * Lay))