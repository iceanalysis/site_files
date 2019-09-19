library(tidyverse)
library(elite)
draft_13 <- get_drafts("ohl priority selection", 2013)



stats_13 <- get_player_stats_individual(draft_13)

stats13 <- stats_13 %>% tidyr::unnest(player_statistics)

write.csv(stats13, file = "stats13.csv")


