first_week <- 18

# Reads Stakes

DF_stakes <- 
    do.call("rbind", lapply(
        list.files("Data/stakes/", full.names = TRUE), read.csv)) %>%
    group_by(Team, Player, Week, Market) %>%
    summarise(Stakes = sum(Stakes)) %>%
    ungroup()

DF_odds <- 
    do.call("rbind", lapply(
        list.files("Data/odds/", full.names = TRUE), read.csv)) %>%
    inner_join(read.csv("Data/week_number.csv"), 
               by = "Week")

DF_transactions <- 
    inner_join(DF_odds, DF_stakes,
               by = c("Team", "Week", "Market")) %>%
    mutate(Profit = if_else(Stakes > 0, Stakes * Back, Stakes * Lay))

DF_weekly_basket <- 
    inner_join(
        DF_transactions %>%
            select(-Week) %>% 
            rename(WN = week_number) %>%
            mutate(join_value = 1),
        read.csv("Data/week_number.csv") %>% mutate(join_value = 1),
        by = "join_value") %>% 
    filter(week_number >= WN) %>%
    group_by(Team, Market, Player, week_number, Week) %>%
    summarise(profit = sum(Profit)) %>%
    ungroup()

tot_stakes <- inner_join(
    DF_transactions %>%
        select(-Week) %>% 
        rename(WN = week_number) %>%
        mutate(join_value = 1),
    read.csv("Data/week_number.csv") %>% mutate(join_value = 1),
    by = "join_value") %>% 
    filter(week_number >= WN) %>%
    group_by(Market, Player, Week, week_number) %>% 
    summarise(tot_stakes = sum(Stakes)) %>%
    ungroup()

DF_basket_value_after <- 
    DF_weekly_basket %>%
    # filter(profit != 0) %>%
    # select(-Week) %>%
    inner_join(DF_odds %>% 
                   select(Team, Lay, week_number, Market), 
               by = c("Team", "week_number", "Market")) %>%
    group_by(Market, Player, Week, week_number) %>%
    summarise(basket_value = sum(profit/Lay)) %>%
    ungroup() %>%
    mutate(week_number = week_number + 0.0001) %>%
    inner_join(tot_stakes %>%
                   select(-week_number),
               by = c("Market", "Player", "Week")) %>%
    mutate(basket_value = basket_value - tot_stakes)


DF_basket_value_before <- 
    DF_weekly_basket %>%
    # filter(profit != 0) %>%
    # select(-Week) %>%
    inner_join(DF_odds  %>%
                   mutate(week_number = week_number - 1)%>%
                   select(Team, Lay, week_number, Market), 
               by = c("Team", "week_number", "Market")) %>%
    group_by(Market, Player, Week, week_number) %>%
    summarise(basket_value = sum(profit/Lay)) %>%
    ungroup() %>%
    mutate(week_number = week_number + 1) %>%
    inner_join(tot_stakes %>%
                   select(-week_number),
               by = c("Market", "Player", "Week")) %>%
    mutate(basket_value = basket_value - tot_stakes)

DF_basket_value_all <- 
    rbind(DF_basket_value_before, DF_basket_value_after) %>%
    filter(week_number >= first_week)

DF_profit <- 
    DF_transactions %>%
    group_by(Team, Player, Market) %>%
    summarise(Profit = sum(Profit)) %>% ungroup() 

this_week <- max(DF_basket_value_all$week_number) - 0.0001

DF_weekly_prob_change <- inner_join(
    DF_odds %>% filter(week_number == max(week_number)),
    DF_odds %>% filter(week_number == (max(week_number)-1)),
    by = c("Team", "Market")) %>%
    mutate(prob_back_before = 100/Back.y) %>%
    mutate(prob_lay_after = 100/Lay.x) %>%
    select(Team, Market, prob_back_before, prob_lay_after) %>%
    inner_join(
        DF_stakes %>% filter(Stakes != 0) %>% select(Team, Market) %>% distinct(),
        by = c("Team", "Market")) %>% 
    ungroup() %>%
    mutate(return = (prob_lay_after/prob_back_before - 1)*100) %>%
    arrange(desc(return))

DF_weekly_value_change <- 
    DF_profit %>% inner_join(
        DF_odds %>% filter(week_number >= max(DF_odds$week_number) - 1) %>%
            mutate(this_week = if_else(week_number == max(DF_odds$week_number),"week_now", "week_before")),
        by = c("Team", "Market")) %>%
    mutate(team_value = Profit/Lay) %>%
    select(Team, Player, Market,team_value, this_week) %>%    
    dcast(Team + Player + Market ~ this_week, value.var = "team_value") %>%
    filter(week_now !=0 | week_before != 0) %>%
    mutate(value_change = week_now - week_before)

DF_current_implied_prob <- 
    DF_weekly_value_change %>% 
    select(Team, Player, Market, week_now) %>%
    rename(value = week_now) %>%
    inner_join(read.csv("Data/market_multiplier.csv"), by = "Market") %>%
    group_by(Player, Market) %>% 
    mutate(tot_value = sum(value)) %>% 
    mutate(prob = Multiplier * value/tot_value) 
