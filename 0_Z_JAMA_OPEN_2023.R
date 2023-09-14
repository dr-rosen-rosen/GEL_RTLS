################ Recreating JAMA Open analysis for data since that publication




##############
############## Pull data for all target badges for a site
##############

site <- c('jhh') # 'jhh','bmc'
strt <-  lubridate::ymd('2020-07-01')#config$FB_report_start,#lubridate::ymd('2022-02-27'), 
stp <- lubridate::ymd('2023-06-30')#config$FB_report_stop,#lubridate::ymd('2022-03-01'), 
df  <- get_and_locCode_RTLS_data_pg(
  badges = getActiveBadges(config$badge_file), #unique(bayview_active_badges$RTLS_ID), 
  strt = strt, #lubridate::ymd('2022-01-01'),#config$FB_report_start,#lubridate::ymd('2022-02-27'), 
  stp = stp, #lubridate::ymd('2022-7-01'),#config$FB_report_stop,#lubridate::ymd('2022-03-01'), 
  sites = c('jhh','bmc'),#site, #c('bmc'), # 'jhh','bmc'
  use_rules = TRUE # this is currently commented out in the function
)

df <- clean_timelines(df)

cmb_df <- get_tot_sum(df)

# link badges with part_id's 
x <- readxl::read_excel('GEL IDs - RTLS and Local IDs 6.23.23_EDITED_Reconciled.xlsx', sheet = 'combined') %>%
  janitor::clean_names() %>%
  rename(badge = rtls)
by <- join_by(badge, between(date, start_date, end_date))
cmb_df <- cmb_df %>% 
  # fuzzyjoin::fuzzy_left_join(.,x, by = c("date"="start_date","date"="end_date"), match_fun=list('>=','<='))
  left_join(x, by)


skimr::skim(cmb_df)
cmb_df %>%
  filter(is.na(local_id)) %>%
  write.csv('Shifts_with_badges_not_linked_to_local_id.csv')

# link with shift data
y <- readxl::read_excel(here(
  'Data',
  # 'Resident Schedule Data - Merged 5.15.23.updated.xlsx'
  # 'Resident Schedule Data - Merged 8.16.23.xlsx'), sheet = 'Sheet1'
  'Resident Schedule Data - Merged 8.30.23.xlsx'), sheet = 'Sheet1'
  ) %>%
  janitor::clean_names() %>%
  dplyr::select(-pgy)

# test <- y %>% group_by(date,local_id) %>% filter(n() > 1)
# 
# test %>% write.csv('duplicatedScheduleEntries.csv')

cmb_df2 <- cmb_df %>%
  filter(!is.na(local_id)) %>%
  left_join(y, by = c('local_id', 'date')) #, relationship = "many-to-many")

skimr::skim(cmb_df2)
table(cmb_df2[which(cmb_df2$interval == 'all_24'),]$rotation) %>% write.csv('rotation_frequencies.csv')

z <- read_csv(here('Data','rot_cat.csv'))

cmb_df3 <- cmb_df2 %>%
  # filter(is.na(rotation)) %>%
  # write.csv('shifts_without_rotations.csv', row.names = FALSE)
  left_join(z, by = 'rotation')

cmb_df3 %>%
  filter(is.na(rot_cat)) %>%
  group_by(rotation) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  readr::write_excel_csv(here('data','rotationsWithNoCategory.csv'))

# cmb_df3 %>%
#   group_by(local_id,year(date)) %>%
#   summarize(n_shifts_missing_rotation = n()) %>%
#   write.csv('n_missing_by_ID_and_year.csv', row.names = FALSE)

setdiff(unique(y$local_id),unique(cmb_df$local_id))

unique(cmb_df$local_id) #%>% write.csv('local_ids_from_rtls_data.csv')
unique(y$local_id) #%>% write.csv('local_ids_from_schedules.csv')
intersect(unique(cmb_df$local_id),unique(y$local_id))

table(cmb_df2$rotation,cmb_df2$badge)

test <- df %>%
  mutate(d = date(time_in)) %>%
  # filter(d == max(d)) %>%
  group_by(badge, d) %>%
  # table(.$badge)
  summarize(
    duration = difftime(max(time_out), min(time_in), units = 'secs'),
    min_in = min(time_in),
    max_out = max(time_out),
    entropy = getEntropy_sfly(pick(everything()))$result, 
    burstiness = getBurstiness_sfly(pick(everything()))$result,
    .groups = 'keep'
  )


x <- df %>% 
  mutate(d = date(time_in)) %>%
  filter(d == min(d), badge == 268887) %>%
  mutate(
    # id = row_number(),
    receiver_recode = as.integer(factor(receiver_recode)) # records location categories into integers... need better way to track this
    ) %>%
  dplyr::select(-c(duration, site, d, receiver, receiver_name)) %>%
  rename('begin' = time_in, 'end' = time_out, 'status' = 'receiver_recode', 'id' = badge) %>%
  relocate(id, begin, end, status)
# library(TraMineR)
# badge.sts <- TraMineR::seqformat(x, from = "SPELL", to = "STS")



test %>%
  ggplot(aes(x = burstiness, y = entropy)) + geom_point() + ggthemes::theme_tufte()
test %>%
  ggplot(aes(x = duration/60, y = entropy)) + geom_point() + ggthemes::theme_tufte()
test %>%
  ggplot(aes(x = duration/60, y = burstiness)) + geom_point() + ggthemes::theme_tufte()

table(test$badge)
names(test)
test %>%
  filter(badge == 408427) %>%
  ggplot(aes(x = d, y = burstiness)) + geom_point() + geom_line() + ggthemes::theme_tufte()

library(lme4)
m <- min(as.numeric(test$d))
test <- test %>%
  mutate(d2 = as.numeric(d) - m)

M.null <- lm(scale(entropy) ~ 1, data = test)
M.0 <- lmer(scale(entropy) ~ 1 + scale(as.numeric(duration)) + (1|badge)+ (1|d2), data = test)

M.null <- lm(scale(burstiness) ~ 1, data = test)
M.0 <- lmer(scale(burstiness) ~ 1 + scale(as.numeric(duration)) + (1|badge)+ (1|d2), data = test)

anova(M.0, M.null)
sjPlot::tab_model(M.0)
sjPlot::plot_model(M.0,type = 're')

m <- min(as.numeric(daily_sum$date))
daily_sum <- daily_sum %>%
  mutate(d2 = as.numeric(date) - m)

M.null <- lm(scale(patient_room_perc) ~ 1, data = daily_sum)
M.0 <- lmer(scale(patient_room_perc) ~ 1 + scale(total) + (1|badge), data = daily_sum)

library(gt)
tot_sum %>%
  dplyr::select(ends_with('_perc'),interval) %>%
  group_by(interval) %>%
  dplyr::summarize(across(everything(), ~ mean(.x,na.rm = TRUE))) %>%
  gt() %>% fmt_number(decimals = 3)

# daily_sum
# morning_df
# afternoon_df
# evening_df
# night_df
# rounds_df