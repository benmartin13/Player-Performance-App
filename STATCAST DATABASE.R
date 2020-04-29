require(readr)
require(dplyr)
require(xml2)
require(magrittr)

dates_reduced <- read_csv("https://raw.githubusercontent.com/BillPetti/baseball_research_notebook/master/dates_statcast_build.csv")

x2008season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2008)

x2009season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2009)

x2010season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2010)

x2011season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2011)

x2012season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2012)

x2013season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2013)

x2014season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2014)

x2015season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2015)

x2016season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2016)

x2017season <- dates_reduced %>%
  filter(substr(GameDate, 1, 4) == 2017)

scrape_statcast_savant_pitcher_date <- function(start_date, end_date) {
  
  # extract year
  year <- substr(start_date, 1,4)
  
  # Base URL.
  url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=",year,"%7C&hfSit=&player_type=pitcher&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=",start_date,"&game_date_lt=",end_date,"&team=&position=&hfRO=&home_road=&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&")
  
  payload <- utils::read.csv(url)
  
  if (length(payload$pitch_type) > 0) {
    
    # Clean up formatting.
    payload$game_date <- as.Date(payload$game_date, "%Y-%m-%d")
    payload$des <- as.character(payload$des)
    payload$game_pk <- as.character(payload$game_pk) %>% as.numeric()
    payload$on_1b <- as.character(payload$on_1b) %>% as.numeric()
    payload$on_2b <- as.character(payload$on_2b) %>% as.numeric()
    payload$on_3b <- as.character(payload$on_3b) %>% as.numeric()
    payload$release_pos_x <- as.character(payload$release_pos_x) %>% as.numeric()
    payload$release_pos_z <- as.character(payload$release_pos_z) %>% as.numeric()
    payload$release_pos_y <- as.character(payload$release_pos_y) %>% as.numeric()
    payload$pfx_x <- as.character(payload$pfx_x) %>% as.numeric()
    payload$pfx_z <- as.character(payload$pfx_z) %>% as.numeric()
    payload$hc_x <- as.character(payload$hc_x) %>% as.numeric()
    payload$hc_y <- as.character(payload$hc_y) %>% as.numeric()
    payload$woba_denom <- as.character(payload$woba_denom) %>% as.numeric()
    payload$woba_value <- as.character(payload$woba_value) %>% as.numeric()
    payload$babip_value <- as.character(payload$babip_value) %>% as.numeric()
    payload$iso_value <- as.character(payload$iso_value) %>% as.numeric()
    payload$plate_z <- as.character(payload$plate_z) %>% as.numeric()
    payload$plate_x <- as.character(payload$plate_x) %>% as.numeric()
    payload$vx0 <- as.character(payload$vx0) %>% as.numeric()
    payload$vy0 <- as.character(payload$vy0) %>% as.numeric()
    payload$vz0 <- as.character(payload$vz0) %>% as.numeric()
    payload$ax <- as.character(payload$ax) %>% as.numeric()
    payload$ay <- as.character(payload$ay) %>% as.numeric()
    payload$az <- as.character(payload$az) %>% as.numeric()
    payload$sz_top <- as.character(payload$sz_top) %>% as.numeric()
    payload$sz_bot <- as.character(payload$sz_bot) %>% as.numeric()
    payload$hit_distance_sc <- as.character(payload$hit_distance_sc) %>% as.numeric()
    payload$launch_speed <- as.character(payload$launch_speed) %>% as.numeric()
    payload$launch_speed_angle <- as.character(payload$launch_speed_angle) %>% as.numeric()
    payload$launch_angle <- as.character(payload$launch_angle) %>% as.numeric()
    payload$estimated_ba_using_speedangle <- as.character(payload$estimated_ba_using_speedangle) %>% as.numeric()
    payload$estimated_woba_using_speedangle <- as.character(payload$estimated_woba_using_speedangle) %>% as.numeric()
    payload$effective_speed <- as.character(payload$effective_speed) %>% as.numeric()
    payload$release_speed <- as.character(payload$release_speed) %>% as.numeric()
    payload$zone <- as.character(payload$zone) %>% as.numeric()
    payload$release_spin_rate <- as.character(payload$release_spin_rate) %>% as.numeric()
    payload$release_extension <- as.character(payload$release_extension) %>% as.numeric()
    payload$barrel <- with(payload, ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124, 1, 0))
    payload$home_team <- as.character(payload$home_team)
    payload$away_team <- as.character(payload$away_team)
    if ("fielder_2" %in% names(payload)) {
      
      payload$pitcher   <- as.character(payload$pitcher)   %>% as.integer
      payload$fielder_2 <- as.character(payload$fielder_2) %>% as.integer
      payload$fielder_3 <- as.character(payload$fielder_3) %>% as.integer
      payload$fielder_4 <- as.character(payload$fielder_4) %>% as.integer
      payload$fielder_5 <- as.character(payload$fielder_5) %>% as.integer
      payload$fielder_6 <- as.character(payload$fielder_6) %>% as.integer
      payload$fielder_7 <- as.character(payload$fielder_7) %>% as.integer
      payload$fielder_8 <- as.character(payload$fielder_8) %>% as.integer
      payload$fielder_9 <- as.character(payload$fielder_9) %>% as.integer
    }
    if (("fielder_2.1" %in% names(payload))) {
      payload$fielder_2.1 <- as.character(payload$fielder_2) %>% as.integer
    }
    
    return(payload)
  }

  
  else {
    vars <- names(payload)
    df <- lapply(vars, function(x) x <- NA)
    names(df) <- names(payload)
    payload_na <- bind_rows(df)
    
    return(payload_na)
    
    Sys.sleep(sample(x = runif(20, min = .01, max = 1), size = 1))
  }
}

x2008data <- x2008season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

(missing_2008 <- x2008data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2009data <- x2009season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2009 <- x2009data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2010data <- x2010season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2010 <- x2010data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2011data <- x2011season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2011 <- x2011data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2012data <- x2012season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2012 <- x2012data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2013data <- x2013season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2013 <- x2013data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2014data <- x2014season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2014 <- x2014data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2015data <- x2015season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2015 <- x2015data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2016data <- x2016season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2016 <- x2016data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))

x2017data <- x2017season %>%
  group_by(start_date) %>%
  do(scrape_statcast_savant_pitcher_date(.$start_date, .$end_date)) %>%
  ungroup() %>%
  select(-start_date)

gc()

(missing_2017 <- x2017data %>%
    filter(is.na(pitch_type)) %>%
    distinct(game_date) %>%
    select(game_date))
#2018
stat2018 <- scrape_statcast_savant_pitcher_date(start_date = "2018-03-29", end_date = "2018-03-29")
stat2018 <- stat2018[0,]

#before all star break (march 29 to july 15)
for(i in 0:108){
  datei <- as.Date("2018-03-29") + i
  di <- scrape_statcast_savant_pitcher_date(start_date = datei, end_date = datei)
  stat2018 <- rbind(stat2018, di)
}

#after all star break (july 19 to sep 30)
for(i in 0:73){
  datei <- as.Date("2018-07-19") + i
  di <- scrape_statcast_savant_pitcher_date(start_date = datei, end_date = datei)
  stat2018 <- rbind(stat2018, di)
}

#2019
stat2019 <- scrape_statcast_savant_pitcher_date(start_date = "2019-03-29", end_date = "2019-03-29")
stat2019 <- sc2019[0,]

#Japan series
stat2019 <- scrape_statcast_savant_pitcher_date("2019-03-20","2019-03-21")

#before all star break (march 28 to july 7)
for(i in 0:100){
  datei <- as.Date("2019-03-28") + i
  di <- scrape_statcast_savant_pitcher_date(start_date = datei, end_date = datei)
  stat2019 <- rbind(stat2019, di)
}

#after all star break (july 11 to sep 29)
for(i in 0:79){
  datei <- as.Date("2019-07-11") + i
  di <- scrape_statcast_savant_pitcher_date(start_date = datei, end_date = datei)
  stat2019 <- rbind(stat2019, di)
}




statcast_bind <- rbind(x2008data, x2009data, x2010data, x2011data, x2012data, 
                       x2013data, x2014data, x2015data, x2016data, x2017data)

statcast_bind <- rbind(statcast_bind, stat2018, stat2019)

statcast_bind$game_date <- as.character(statcast_bind$game_date)

statcast_bind <- statcast_bind %>%
  arrange(game_date)

statcast_bind <- statcast_bind %>%
  filter(!is.na(game_date))

save(statcast_bind, file = "statcast_data.Rdata")
