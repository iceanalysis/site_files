library(tidyverse)
library(elite)
draft_13 <- get_drafts("ohl priority selection", 2013)



stats_13 <- get_player_stats_individual(draft_13)

stats13 <- stats_13 %>% tidyr::unnest(player_statistics)

write.csv(stats13, file = "stats13.csv")




draft_14_16 <- get_drafts("ohl priority selection", c(2014, 2015, 2016))



stats_14_16 <- get_player_stats_individual(draft_14_16)

stats_14_16 <- stats_14_16 %>% tidyr::unnest(player_statistics)

write.csv(stats_14_16, file = "stats14_16.csv")

View(stats_14_16)
View(df)
df <- rbind(stats13, stats_14_16) %>%
  filter(position != "G") %>%
  transmute(name,
            draft_year,
            position = position,
            league = league_,
            season = season_,
            age = age_,
            gp = games_played_,
            goals = goals_,
            assists = assists_) %>%
  mutate(pos = ifelse(str_detect(position, "F|W|C"), "F", "D")) %>%
    group_by(name, position, league, season, age) %>%
    summarize(gp = sum(gp),
              goals = sum(goals),
              assists = sum(assists)) %>%
    mutate(points = goals + assists,
           ppg = points / gp) %>%
    filter(season != "2019-20",
           season != "2020-21") %>%
    ungroup() %>%
    arrange(name, season, gp)


df %>%
  count(league, sort = T)
df %>%
  filter(league %in% c("OHL", "GTHL"),
         gp > 10,
         position == "F") %>%
ggplot(aes(x = age, y = ppg, color = league)) +
  geom_point() +
  geom_smooth(method = "lm")



df13 <- read.csv("stats13.csv")
df14 <- read.csv("stats14_16.csv")


head(df13)
head(df14)
str(df13)
str(df14)
View(rbind(df13, df14))
