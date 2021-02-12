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
    mutate(week_number = substr(Week, 5, 6) %>% as.integer())

# Merging Odds and Stakes
# If stakes are negative/positive Lay/Back is used
DF_transactions <- 
    inner_join(DF_odds, DF_stakes,
               by = c("Team", "Week", "Market")) %>%
    mutate(Profit = if_else(Stakes > 0, Stakes * Back, Stakes * Lay))

# Profit per Team/Market/Player/Week is stored in DF_weekly_basket

DF_weekly_basket <- 
    inner_join(
        DF_transactions %>%
            select(-Week) %>% 
            rename(WN = week_number) %>%
            mutate(join_value = 1),
        DF_odds %>% select(Week,week_number) %>% unique() %>% mutate(join_value = 1),
        by = "join_value") %>% 
    filter(week_number >= WN) %>%
    group_by(Team, Market, Player, week_number, Week) %>%
    summarise(profit = sum(Profit)) %>%
    ungroup()

# tot_stakes per Player, Market, Week

tot_stakes <- 
    inner_join(
        DF_transactions %>%
            select(-Week) %>% 
            rename(WN = week_number) %>%
            mutate(join_value = 1),
        DF_odds %>% select(Week,week_number) %>% unique() %>% mutate(join_value = 1),
        by = "join_value") %>% 
    filter(week_number >= WN) %>%
    group_by(Market, Player, week_number, Week) %>%
    summarise(tot_stakes = sum(Stakes)) %>%
    ungroup()

# Merging basket with Odds (after transactions)

DF_Team_after <- 
    DF_weekly_basket %>%
    # filter(profit != 0) %>%
    # select(-Week) %>%
    inner_join(DF_odds %>% 
                   select(Team, Back, Lay, week_number, Market), 
               by = c("Team", "week_number", "Market"))#%>%
    # group_by(Market, Player, Week, week_number) %>%
    # summarise(basket_value = sum(profit/Lay)) %>%
    # ungroup() %>%
    # mutate(week_number = week_number + 0.0001) %>%
    # inner_join(tot_stakes %>%
    #                select(-week_number),
    #            by = c("Market", "Player", "Week")) %>%
    # mutate(basket_value = basket_value - tot_stakes)


# Merging basket with Odds (before transactions)

DF_Team_before <- 
    DF_weekly_basket %>%
    # filter(profit != 0) %>%
    # select(-Week) %>%
    inner_join(DF_odds %>% 
                   select(Team, Back, Lay, week_number, Market) %>%
               mutate(week_number = week_number - 1), 
               by = c("Team", "week_number", "Market"))#%>%
# group_by(Market, Player, Week, week_number) %>%
# summarise(basket_value = sum(profit/Lay)) %>%
# ungroup() %>%
# mutate(week_number = week_number + 0.0001) %>%
# inner_join(tot_stakes %>%
#                select(-week_number),
#            by = c("Market", "Player", "Week")) %>%
# mutate(basket_value = basket_value - tot_stakes)



