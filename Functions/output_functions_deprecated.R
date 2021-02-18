get_basket_graph_market <- function(market){
    ggplot(DF_basket_value_all %>% filter(Market == market), 
           aes(x = week_number, y = basket_value, group = Player, colour = Player)) +
        geom_point() + 
        geom_line() + 
        scale_y_continuous(labels = dollar_format(suffix = "£", prefix = "")) +
        xlab("Week")+ 
        ylab("PnL") + 
        ggtitle(market)
}

get_basket_graph_all <- function(){
    ggplot(DF_basket_value_all %>% 
               group_by(Player, week_number) %>% 
               summarise(basket_value = sum(basket_value)), 
           aes(x = week_number, y = basket_value, group = Player, colour = Player)) +
        geom_point() + 
        geom_line() + 
        scale_y_continuous(labels = dollar_format(suffix = "£", prefix = "")) +
        xlab("Week")+ 
        ylab("PnL") + 
        ggtitle("All games")
}

get_basket_summary <- function(){
    DF_basket_value_all %>% 
        filter(week_number == this_week) %>%
        dcast(Player ~ Market, value.var = "basket_value") %>%
        mutate(TOT = Winner + Top4 + Relegation) %>%
        arrange(-TOT)
}

get_basket_value_change <- function(){
    
    # inner_join(
    #     DF_basket_value_before %>% filter(week_number == max(week_number)),
    #     DF_basket_value_before %>% filter(week_number == (max(week_number)-1)),
    #     by = c("Market", "Player")) %>% 
    #     mutate(value_change = basket_value.x - basket_value.y) %>%
    #     dcast(Player ~ Market, value.var = "value_change") %>%
    #     mutate(TOT = Winner + Top4 + Relegation) %>%
    #     arrange(-TOT)
    
    inner_join(
        DF_Basket_after %>% filter(week_number == max(DF_odds$week_number - 1 + 0.0001)),
        DF_Basket_before %>% filter(week_number == max(DF_odds$week_number)),
        by = c("Market", "Player")) %>% 
        mutate(value_change = basket_value.y - basket_value.x) %>%
        dcast(Player ~ Market, value.var = "value_change") %>%
        mutate(TOT = Winner + Top4 + Relegation) %>%
        arrange(TOT) %>%
        mutate(Player = factor(Player, levels = Player)) %>%
        melt(id.vars = "Player") %>% 
        rename(Market = variable) %>%
        ggplot(aes(x = Market, y = Player, fill = value, label = round(100*value,0)/100)) +
        geom_raster() + 
        scale_fill_gradient2(low = "red", high = "green") +
        geom_text() + 
        theme(legend.position = "none")
}


get_prob_graph <- function(min_prob, market){
    DF_odds %>% filter(Market == market, Prob >= min_prob, week_number >= first_week) %>%
        ggplot(aes(x = Week, 
                   y = Prob, 
                   group = Team, 
                   colour = Team#, 
                   # ymin = prob - prob_errorbar, 
                   # ymax = prob + prob_errorbar
        )) + 
        geom_point() +
        scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
        geom_line() + 
        # geom_errorbar(width = 0.05) +
        ggtitle(market)
}


get_probchange_table <- function(market){
    DF_weekly_prob_change %>% filter(Market == market)
}

get_weekly_change_all_players <- function(market){
    DF_weekly_value_change %>% filter(Market == market) %>% 
        dcast(Team ~ Player, value.var = "value_change")
}


get_probs_compared_to_market <- function (market){
    output <- 
        left_join(
            DF_odds %>% 
                filter(Market == market & week_number == max(week_number)) %>% 
                select(Team, Prob) %>%
                mutate(Prob = Prob * 100),
            DF_current_implied_prob %>% 
                filter(Market == market) %>%
                mutate(prob = prob * 100) %>%
                dcast(Team ~ Player, value.var = "prob"),
            by = "Team") %>% 
        arrange(desc(Prob))
    
    output[is.na(output)] <- 0
    
    output
}