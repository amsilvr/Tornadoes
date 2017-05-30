# Download the data from NOAA if we haven't already done it

if (!exists("weather_events")) {
  source('C:/Users/amsilverman/Box Sync/@Project Support/DevDataProds/Tornadoes/data_download.R')
}


# Isolate tornado events
tornadoes <- filter(weather_events, event_type == "Tornado") %>%
  select(id, geoid, begin_date_time, end_date_time,
         ends_with("direct"), tor_f_scale
         ) %>%
  mutate(year = as.factor(year(begin_date_time))
         , wea_enabled = factor(
           if_else(begin_date_time >= mdy("4/7/2012")
                   , false = "Before"
                   , true = "After")
           , levels = c("Before", "After")
           , ordered = FALSE
         )
  )

# tornado_deaths <- tornadoes %>% 
#   group_by(year) %>% 
#   summarize(count = n()
#             , d_direct = sum(deaths_direct)
#             , d_indirect = sum(deaths_indirect)
#             , i_direct = sum(injuries_direct)
#             , i_indirect = sum(injuries_indirect)
#             )
#   
#compare tornadoes and flash floods to non-wea alerted storm events

tor_flo_wind_light <- filter(weather_events
                          , event_type %in% c("Flash Flood", "Tornado", "Thunderstorm Wind"
                                        ,"Lightning")) %>%
transmute(event_type = factor(as.character(event_type), levels = c("Flash Flood", "Tornado", "Thunderstorm Wind"
                                             ,"Lightning"))
            , begin_date_time
            , year = year(begin_date_time)
            , d_injuries = injuries_direct
            , i_injuries = injuries_indirect
            , d_deaths = deaths_direct
            , i_deaths = deaths_indirect
            , fips = geoid
            , tor_f_scale
            , wea_enabled = factor(
                if_else(begin_date_time >= mdy("4/7/2012")
                    , false = "Before"
                    , true = "After")
                , levels = c("Before", "After")
                , ordered = FALSE
          )
)   

tfwl_sum <- group_by(tor_flo_wind_light, type) %>% 
  summarise(count = n()
            , deaths = sum(d_deaths)
            , death_rate = round((deaths/count),digits = 4)
            , injuries = sum(d_injuries)
            , injury_rate = round((injuries/count),digits = 4)
            ) 

twfl_wea_diff <- group_by(tor_flo_wind_light, event_type, wea_enabled) %>%
summarise(count = n()
          , deaths = sum(d_deaths)
          , death_rate = round((deaths/count),digits = 4)
          , injuries = sum(d_injuries)
          , injury_rate = round((injuries/count),digits = 4)
) 

tornado_sum_tbl <- filter(tornadoes
                          , as.numeric(tor_f_scale) >=0) %>%
  select(year
         , deaths = deaths_direct
         , injuries = injuries_direct
         , tor_f_scale
         , wea_enabled) %>%
  group_by(tor_f_scale, wea_enabled) %>%
  summarize_at(c("deaths", "injuries"), c("mean","sum")) %>%
  mutate(
               death_rate = round(as.numeric(deaths_mean), 4)
            ,  injury_rate = round(as.numeric(injuries_mean), 4)
            ) %>%
  select(-c(3:4))
  
tornadoes_hourly <- transmute(tornadoes
                              , hour = hour(begin_date_time)
                              , deaths = d_direct
                              , injuries = i_direct
                              , tor_f_scale = as.numeric(str_extract(tor_f_scale, "[0-9]"))
                              , wea_enabled) %>%
  group_by(hour, wea_enabled) %>%
  summarize_at(c("deaths", "injuries", "tor_f_scale"), "mean", na.rm = TRUE) %>%
  mutate(
    death_rate = round(as.numeric(deaths), 4)
    , injury_rate = round(as.numeric(injuries), 4)
    , tor_f_scale = round(tor_f_scale, 2)) %>%
  select(hour, wea_enabled, death_rate, injury_rate, tor_f_scale)

t_h_str_distr <- transmute(tornadoes
                            , hour = hour(begin_date_time)
                            , wea_enabled
                            , tor_f_scale) %>%
  group_by(hour, wea_enabled, tor_f_scale) %>%
  summarize(events = n())

str_events_dist <- ungroup(t_h_str_distr) %>%
group_by(tor_f_scale, wea_enabled) %>%
summarize(tot_events = sum(events)) %>%
  inner_join(t_h_str_distr) %>%
  ungroup() %>%
  transmute(hour
            , tor_f_scale
            , wea_enabled
            , dist = round(events/tot_events,4)) %>%
  arrange(hour, tor_f_scale, wea_enabled)
 

t_h_by_strength <- transmute(tornadoes
                               , hour = hour(begin_date_time)
                               , deaths = d_direct
                               , injuries = i_direct
                               , tor_f_scale
                               , wea_enabled) %>%
  group_by(hour, wea_enabled, tor_f_scale) %>%
  summarize_all(funs("mean") )%>%
  mutate(
    death_rate = round(as.numeric(deaths), 4)
    , injury_rate = round(as.numeric(injuries), 4) )%>%
  select(hour, wea_enabled, tor_f_scale, death_rate, injury_rate)

## Hourly distribution of tornadoes before and after WEA
##   
tornadoes_hourly_distro <- transmute(tornadoes
                             , hour = hour(begin_date_time)
                             , wea_enabled) %>%
  group_by(hour, wea_enabled) %>%
  summarize(events = n())

t_dist_after <- filter(tornadoes_hourly_distro
                       , wea_enabled == "After") 
t_dist_after <- mutate(t_dist_after
                       ,pct_after = round(events/sum(t_dist_after$events),4))
t_dist_before <- filter(tornadoes_hourly_distro
                       , wea_enabled == "Before") 
t_dist_before <- mutate(t_dist_before
                       ,pct_before = round(events/sum(t_dist_before$events),4))


t_h_cas_bef <- filter(tornadoes_hourly, wea_enabled == "Before") %>%
  select(hour
            , dth_bef = death_rate
            , inj_bef = injury_rate)


t_h_cas_aft <- filter(tornadoes_hourly, wea_enabled == "After") %>%
  select(hour
            , dth_aft = death_rate
            , inj_aft = injury_rate)

t_dist <- inner_join(
  select(t_dist_before, hour, pct_bef = pct_before),
  select(t_dist_after, hour, pct_aft = pct_after)
) %>%
  inner_join(t_h_cas_bef) %>%
  inner_join(t_h_cas_aft) %>%
  mutate(dist_delta = round(-(pct_bef - pct_aft)/pct_bef,4)
         , dth_delt = round(-(dth_bef - dth_aft)/dth_bef,4)
         , inj_delt = round(-(inj_bef - inj_aft)/inj_bef,4)
         )

rm()


# graph the death rate by hour before and after WEA
# hypothesis - new method of alerting will 
# lower casualty rates overnight

require(ggplot2)
require(reshape2)

t_dist_plot <- melt(t_dist
                    , id.vars = "hour"
                    , variable.name = "WEA_Enabled"
                    , measure.vars = c("pct_bef", "pct_aft")
                    , value.name = "Distribution"
                    )
  
n <- ggplot(t_dist_plot, aes(hour , Distribution)) + 
     geom_point(aes(color = WEA_Enabled))

n + scale_color_brewer(palette = "Paired"
                           , type = "qual"
                           , direction = -1
                           , breaks = waiver()
                           , name = ""
                           , labels = c("Before WEA", "After WEA")
  )
  

# Tornado hourly distribution by strength of storm pre and post WEA
n <- ggplot(str_events_dist
            , aes(hour
                  , dist
                  , color = wea_enabled
                  , facets = tor_f_scale)
            )

n + geom_point() +
  facet_wrap(facets = ~tor_f_scale
             , ncol = 1
             , scales = "free_y") 
    scale_color_brewer(palette = "Paired"
                        , type = "qual"
                        , direction = -1
                        , breaks = waiver()
                        , name = ""
                        , labels = c("Before WEA", "After WEA")
)
 


# Hourly casualty rates

torn_hourly_plot <- melt(tornadoes_hourly
                         , id.vars = c("hour", "wea_enabled")
                         , variable.name = "casualty_type"
                         , measure.vars = c("death_rate", "injury_rate")
                         , value.name = "rate"
                         , factorsAsStrings = TRUE
)

n <- ggplot(torn_hourly_plot
            , mapping = aes(hour, rate
                            , facets = casualty_type
                            , fill = wea_enabled)
)

n + geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(facets = ~casualty_type) + 
  labs(title = "Tornado Casualty Rates by Hour"
       , subtitle = "Before and after WEA alerting enabled April 7, 2012"
       , x = "Hour"
       , y = "Casualty Rate")  +
  scale_fill_brewer(#palette = "Paired",
                    type = "qual"
                    , direction = -1
                    , breaks = waiver()
                    , name = ""
                    , labels = c("Before WEA", "After WEA")
  ) +
  facet_wrap(facets = ~casualty_type
             , ncol = 1
             , scales = "free_y") 
