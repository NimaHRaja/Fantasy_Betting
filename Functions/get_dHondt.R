get_dHondt <- function(odds, budget){
    inner_join(
        odds %>% mutate(join_value = 1),
        data.frame(counter = 1:budget, join_value = rep(1,budget)),
        by = "join_value") %>% 
        mutate(DH = prob/counter) %>%
        arrange(-DH) %>%
        head(budget) %>% 
        group_by(Team) %>%
        summarise(n())
}