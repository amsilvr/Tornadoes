tornadoes <- filter(weather_event_tbl, type == "Tornado") %>%
  select(id, 1:3, begin
         , d_direct = DEATHS_DIRECT
         , d_indirect = DEATHS_INDIRECT
         , i_direct = INJURIES_DIRECT
         , i_indirect = INJURIES_INDIRECT
         , fscale
         ) %>%
  mutate(year = year(begin)
         , wea_enabled = factor(
           if_else(begin >= mdy("4/7/2012")
                   , false = "Before"
                   , true = "After")
           , levels = c("Before", "After")
           , ordered = FALSE
         )
  )

tornado_deaths <- tornadoes %>% 
  group_by(year) %>% 
  summarize(count = n()
            , d_direct = sum(d_direct)
            , d_indirect = sum(d_indirect)
            , i_direct = sum(i_direct)
            , i_indirect = sum(i_indirect)
            )
  

tor_flo_wind_light <- filter(weather_event_tbl
                          , type %in% c("Flash Flood", "Tornado", "Thunderstorm Wind"
                                        ,"Lightning")) %>%
transmute(type = factor(as.character(type), levels = c("Flash Flood", "Tornado", "Thunderstorm Wind"
                                             ,"Lightning"))
            , begin
            , year = year(begin)
            , d_injuries = INJURIES_DIRECT
            , i_injuries = INJURIES_INDIRECT
            , d_deaths = DEATHS_DIRECT
            , i_deaths = DEATHS_INDIRECT
            , fips = paste0(st,cz)
            , fscale
            , wea_enabled = factor(
                if_else(begin >= mdy("4/7/2012")
                    , false = "Before"
                    , true = "After")
                , levels = c("Before", "After")
                , ordered = FALSE
          )
)   

tfwl_sum <- group_by(tor_flo_wind_light, type) %>% 
  summarise(count = n()
            , deaths = sum(d_deaths)
            , death_rate = round((deaths/count*100),digits = 2)
            , injuries = sum(d_injuries)
            , injury_rate = round((injuries/count*100),digits = 2)
            ) 

wea_diff <- group_by(tor_flo_wind_light, type, wea_enabled) %>%
summarise(count = n()
          , deaths = sum(d_deaths)
          , death_rate = round((deaths/count*100),digits = 2)
          , injuries = sum(d_injuries)
          , injury_rate = round((injuries/count*100),digits = 2)
) 

tornado_sum_tbl <- filter(tornadoes
                          , as.numeric(fscale) >=0) %>%
  select(year
         , deaths = d_direct
         , injuries = i_direct
         , fscale
         , wea_enabled) %>%
  group_by(fscale, wea_enabled) %>%
  summarize_at(c("deaths", "injuries"), c("mean","sum")) %>%
  mutate(
               death_rate = round(as.numeric(deaths_mean), 4)
            ,  injury_rate = round(as.numeric(injuries_mean), 4)
            ) %>%
  select(-c(3:4))
  

# tornado_wea_tbl <- filter(ungroup(tornado_sum_tbl)
#                           , as.numeric(fscale) >= 0) %>%
#   transmute(EF = paste0("EF", fscale)
#             ,  WEA = if_else(wea_possible
#                              , true = "After"
#                              , false = "Before")
#             , Deaths = round(deaths_mean, 3)
#             , Injuries = round(injuries_mean, 3)
#             , Events = deaths_length) %>%
#   arrange(EF, desc(WEA))

