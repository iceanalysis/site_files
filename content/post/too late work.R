library(tidyverse)
library(rvest)
library(beepr)
library(gganimate)
library(extrafont)
library(ggrepel)

theme_ice <- theme_bw() +
  theme(
    text = element_text(family = "Consolas"),
    plot.title = element_text(hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    panel.grid.minor = element_blank(),
    legend.position = "bottom")

team_list1819 <- list()
year_list1819 <- list()

teams1819 <- c("ANA",
               "ARI",
               "BOS",
               "BUF",
               "CAR",
               "CBJ",
               "CGY",
               "CHI",
               "COL",
               "DAL",
               "DET",
               "EDM",
               "FLA",
               "LAK",
               "MIN",
               "MTL",
               "NSH",
               "NJD",
               "NYI",
               "NYR",
               "OTT",
               "PHI",
               "PIT",
               "SJS",
               "STL",
               "TBL",
               "TOR",
               "VAN",
               "VEG",
               "WPG",
               "WSH")

for(j in 2018:2019) {
  for(i in teams1819) {
    team_url <- paste0("https://www.hockey-reference.com/teams/",i,"/",j,"_games.html")
    webpage <- read_html(team_url)
    team_tbl <- webpage %>%
      html_nodes("table") %>%
      html_table(fill = T)
    team_df <- data.frame(team_tbl[[1]])
    team_df$team = i
    team_df$year = j
    team_list1819[[i]] <- team_df
  }
  year_list1819[[j]] <- data.frame(data.table::rbindlist(team_list1819))
}  ; beep()
df1819 <- data.frame(data.table::rbindlist(year_list1819))

View(df1819)






team_list1517 <- list()
year_list1517 <- list()

teams1517 <- c("ANA",
               "ARI",
               "BOS",
               "BUF",
               "CAR",
               "CBJ",
               "CGY",
               "CHI",
               "COL",
               "DAL",
               "EDM",
               "FLA",
               "LAK",
               "MIN",
               "MTL",
               "NSH",
               "NJD",
               "NYI",
               "NYR",
               "OTT",
               "PHI",
               "PIT",
               "SJS",
               "STL",
               "TBL",
               "TOR",
               "VAN",
               "WPG",
               "WSH")
for(j in 2015:2017) {
  for(i in teams1517) {
    team_url <- paste0("https://www.hockey-reference.com/teams/",i,"/",j,"_games.html")
    webpage <- read_html(team_url)
    team_tbl <- webpage %>%
      html_nodes("table") %>%
      html_table(fill = T)
    team_df <- data.frame(team_tbl[[1]])
    team_df$team = i
    team_df$year = j
    team_list1517[[i]] <- team_df
  }
  year_list1517[[j]] <- data.frame(data.table::rbindlist(team_list1517))
}  ; beep()
df1517 <- data.frame(data.table::rbindlist(year_list1517, fill = T))

View(df1517)
df_all <- rbind(df1517, df1819)

df <- subset(df_all, GP != "GP") %>%
  select(team, year, GP, Date, W, L, OL,) %>%
  mutate(GP = as.integer(GP),
         Date = as.Date(Date),
         W = as.integer(W),
         L = as.integer(L),
         OL = as.integer(OL),
         div = ifelse(team %in% c("TBL",
                                  "BOS",
                                  "TOR",
                                  "MTL",
                                  "FLA",
                                  "BUF",
                                  "DET",
                                  "OTT"), "Atlantic",
                      ifelse(team %in% c("WSH",
                                         "NYI",
                                         "PIT",
                                         "CAR",
                                         "CBJ",
                                         "PHI",
                                         "NYR",
                                         "NJD"), "Metro",
                             ifelse(team %in% c("NSH",
                                                "WPG",
                                                "STL",
                                                "DAL",
                                                "COL",
                                                "CHI",
                                                "MIN"), "Central", "Pacific")))) %>%
  group_by(team, year) %>%
  mutate(pts = W*2 + OL*1,
         p_82 = (pts / GP) * 82) %>%
  ungroup()


density_df <- df %>%
  group_by(team, year) %>%
  mutate(final = ifelse(GP == 82, pts, NA)) %>%
  fill(final, .direction = "down") %>%
  mutate(diff = p_82 - final)








sd_df <- density_df %>%
  group_by(GP) %>%
  summarize(mean = mean(diff),
            upper = quantile(diff, .95),
            lower = quantile(diff, .05)) %>%
  mutate(safe = 95 + upper,
         out = 95 + lower) %>%
  filter(GP >= 12,
         GP < 82)


sd_df %>%
  ggplot(aes(x = GP)) +
  geom_line(aes(y = safe), color = "green", size = 2) +
  geom_line(aes(y = out), color = "red", size = 2) +
  geom_hline(aes(yintercept = 95), linetype = "dashed") +
  labs(title = "When Teams Can Teams be Deemed Clinched or Eliminated",
       x = "Games Played",
       y = "P/82 Pace") +
  annotate("text", x = 20, y = 130, label = "Clinched Line", color = "darkgreen", family = "Consolas") +
  annotate("text", x = 25, y = 62, label = "Eliminated Line", color = "darkred", family = "Consolas") +
  annotate("text", x= 17, y = 97, label = "95 point cut-off", color = "gray40", family = "Consolas")+
  my_labs +
  theme_ice





density_df %>%
  filter(GP >= 12,
         GP < 82) %>%
  ggplot() +
  geom_vline(data = sd_df, aes(xintercept = lower), color = "red") +
  geom_vline(data = sd_df, aes(xintercept = upper), color = "red") +
  geom_text(data = sd_df, aes(x = lower - 5,y = .05, label = round(lower,0)),
            color = "red", size = 5) +
  geom_text(data = sd_df, aes(x = upper + 5, y = .05, label = round(upper,0)),
            color = "red", size = 5) +
  geom_density(aes(x = diff), color = "darkblue") +
  coord_cartesian(ylim = c(0, .075),
                  xlim = c(-40, 40)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_ice +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  transition_time(GP) +
  labs(title = "How Long Do Teams Have to Turn it Around?",
       subtitle = 'Games Played: {frame_time}',
       x = "Current P/82 Pace Compared to Final Point Total") +
  ease_aes("linear")

library(here)
for (i in teams1819) {
  
density_df %>%
  filter(team == i,
         year == 2019) %>%
  mutate(lab = ifelse(GP == 82, team, NA)) %>%
  filter(GP >=12) %>%
  ggplot() +
  geom_line(aes(x = GP, y = p_82), color = "darkblue",  show.legend = F) +
  geom_text(aes(x = 82, y = p_82, label = lab, group = team), family = "Consolas") +
  geom_hline(aes(yintercept = 95), linetype = "dashed") +
    labs(title = "When Teams Can Teams be Deemed Clinched or Eliminated",
         x = "Games Played",
         y = "P/82 Pace") +
    annotate("text", x = 20, y = 130, label = "Clinched Line", color = "darkgreen", family = "Consolas") +
    annotate("text", x = 25, y = 62, label = "Eliminated Line", color = "darkred", family = "Consolas") +
    annotate("text", x= 20, y = 97, label = "95 point cut-off", color = "gray40", family = "Consolas")+
  geom_line(data = sd_df, aes(x = GP, y = safe), color = "green", size = 2) +
  geom_line(data = sd_df, aes(x = GP, y = out), color = "red", size = 2) +
  theme_ice +
    my_labs +
    labs(title = paste0("2018-19 Results ", i)) +
    ggsave(path = here::here("static/img/"), filename = paste0("2018-19-", i, ".png"))

}


joined_df <- left_join(density_df, sd_df) %>%
  mutate(col = ifelse(p_82 >= safe, "Clinched",
                      ifelse(p_82 <= out, "Eliminated", NA))) %>%
  group_by(team, year) %>%
  fill(col, .direction = "down") %>%
  mutate(col2 = ifelse(!is.na(lag(col)), lag(col2), NA))
  
  
View(joined_df %>%
         filter(team == "CAR",
                year == 2019))
  
density_df
sd_df
joined_df %>%
  filter(year == 2019) %>%
  ggplot() +
  geom_line(aes(x = GP, y = p_82, col = col, group = team)) +
  geom_line(aes(x = GP, y = safe), color = "green", size = 2) +
  geom_line(aes(x = GP, y = out), color = "red", size = 2) +
  geom_hline(aes(yintercept = 95), linetype = "dashed") +
  labs(title = "When Teams Can Teams be Deemed Clinched or Eliminated",
       x = "Games Played",
       y = "P/82 Pace") +
  annotate("text", x = 20, y = 130, label = "Clinched Line", color = "darkgreen", family = "Consolas") +
  annotate("text", x = 25, y = 62, label = "Eliminated Line", color = "darkred", family = "Consolas") +
  annotate("text", x= 15, y = 97, label = "95 point cut-off", color = "gray40", family = "Consolas")+
  my_labs +
  theme_ice

View(joined_df %>% filter(year == 2019,
                          !is.na(col)) %>%
       group_by(team) %>%
       filter(GP == min(GP)) %>%
       select(team, col) %>%
       ungroup())
