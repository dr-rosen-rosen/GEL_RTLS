################ Recreating JAMA Open analysis for data since that publication




##############
############## Pull data for all target badges for a site
##############

# site <- c('jhh') # 'jhh','bmc'
strt <- lubridate::ymd('2020-07-01')#config$FB_report_start,#lubridate::ymd('2022-02-27'), 
stp <- lubridate::ymd('2023-06-30')#config$FB_report_stop,#lubridate::ymd('2022-03-01'), 
df <- get_and_locCode_RTLS_data_pg(
  badges = getActiveBadges(config$badge_file), #unique(bayview_active_badges$RTLS_ID), 
  strt = strt, #lubridate::ymd('2022-01-01'),#config$FB_report_start,#lubridate::ymd('2022-02-27'), 
  stp = stp, #lubridate::ymd('2022-7-01'),#config$FB_report_stop,#lubridate::ymd('2022-03-01'), 
  sites = c('jhh','bmc'),#site, #c('bmc'), # 'jhh','bmc'
  use_rules = TRUE # this is currently commented out in the function
)

##############
############## Clean data and get summary measures
##############


df <- clean_timelines(df)
skimr::skim(df)
beepr::beep()

cmb_df0 <- get_tot_sum(df)
beepr::beep()

wrkflw_df <- get_workflow_metrics_by_interval(df)
skimr::skim(wrkflw_df)
beepr::beep()

cmb_df <- cmb_df0 %>%
  left_join(wrkflw_df, by = c('badge','date','interval'))
skimr::skim(cmb_df)

##############
############## Link with participant id's and. shift data
##############


# link badges with part_id's 
x <- readxl::read_excel('GEL IDs - RTLS and Local IDs 6.23.23_EDITED_Reconciled.xlsx', sheet = 'combined') %>%
  janitor::clean_names() %>%
  rename(badge = rtls)
skimr::skim(x)

by <- join_by(badge, between(date, start_date, end_date))
cmb_df <- cmb_df %>% 
  left_join(x, by)
skimr::skim(cmb_df)

# link with shift data
y <- readxl::read_excel(here::here('Data','mergedRotations - deidentified v2.xlsx'), sheet = 'in') %>%
  janitor::clean_names() %>%
  mutate(date = as.Date(date)) %>%
  # dplyr::select(-pgy) %>%
  drop_na() %>%
  filter(date >= lubridate::ymd('2020-07-01'))

# table(y$service) %>% as.data.frame(.) %>% rename(rotation = Var1) %>%
#   # full_join(z, by = 'rotation') %>%
#   write.csv('rotation_categories.csv')
# 
# test <- y %>% filter(
#   is.na(date)
#   #service == '-'
# ) 
# as.data.frame(table(test$service)) %>%
#   write.csv("shifts_w_no_dates.csv")

y %>% 
  filter(!is.na(date),!is.na(service),service != '-') %>%
  group_by(date,local_id) %>% filter(n() > 1) %>%
  write.csv('duplicatedScheduleEntries.csv')

y <- y %>% filter(!is.na(date),!is.na(service),service != '-') %>%
  group_by(date,local_id) %>%
  filter(n() == 1) %>% ungroup()

cmb_df2 <- cmb_df %>%
  filter(!is.na(local_id)) %>%
  left_join(y, by = c('local_id', 'date')) %>%
  filter(date >= lubridate::ymd('2020-07-01')) %>%
  rename(rotation = service)

skimr::skim(cmb_df2)
# table(cmb_df2[which(cmb_df2$interval == 'all_24'),]$service) %>% write.csv('rotation_frequencies.csv')

z <- readxl::read_excel(here('Data','NEW_rotation_categories_111223.xlsx')) %>%
  dplyr::select(rotation,rot_cat) %>%
  mutate(rot_cat = tolower(rot_cat)) %>%
  filter(!is.na(rot_cat)) %>%
  filter(!rot_cat %in% c('na','other','ambulatory','off')) #,'ed','elective'))
table(z$rot_cat)
cmb_df3 <- cmb_df2 %>%
  left_join(z, by = 'rotation') %>%
  mutate(rot_cat = case_when(str_detect(rotation, 'Longcope|Janeway|Barker|Thayer|JHH HS Firm') ~ 'ACS',
                             str_detect(rotation,'MICU|HS CCU') ~ 'icu',
                             substr(rotation,1,3) == 'CCU' ~ 'icu',
                             str_detect(rotation,'Ambulatory|Immersions|Foundations|BMC HS BASIC|MP Clinic|EBM|HS Res Med Clinic|CBP-CCP|JHH HS MP') ~ 'ambulatory',
                             str_detect(rotation,'Brancati|MPC') ~ 'Hospitalist',
                             str_detect(rotation,'BMC HS NT Yellow|BMC HS NT Crosscover|BMC HS NT Green|JHH HS DATO Resident|JHH HS NATO|JHH Housestaff Wolf Moonlighter') ~ 'general medicine',
                             str_detect(rotation,'Polk|addiction|Addiction|Card|Liver|BMC HS PCU Night Intern|BMC HS PCU Day Intern|BMC HS Hematology|JHH HS Neuro') ~ 'Subspecialty',
                             str_detect(rotation,'Off|Vacation|Parental') ~ 'off',
                             str_detect(rotation,'Solids|euks|MTL') ~ 'Oncology',
                             str_detect(rotation,'BMC HS Ecall|Selective|selective|UCM|HS DOD|JHH HS Pathway|Jeopardy|BMC HS Senior Teacher|BMC HS Call|BMC HS Golden Elective|BMC HS FLEX|JHH HS Women|JHH HS Supervisor') ~ 'other',
                             rotation == 'psych' ~ 'other',
                             rotation == 'NT' ~ 'other',
                              .default = rot_cat))
# cmb_df3 %>% filter(is.na(rot_cat)) %>% dplyr::select(rotation) %>% distinct() %>% view()

skimr::skim(cmb_df3)
skimr::skim(cmb_df2)

cmb_df4 <- do_cleaning(cmb_df3)
skimr::skim(cmb_df4)

# df_na <- cmb_df4 |> filter(entropy == 0 | is.na(burstiness))
# 
# df_noMovement <- df_na |> filter(if_any(ends_with('_perc'), ~ . == 1))
# 
# df_na2 <- df_na |> filter(!if_any(ends_with('_perc'), ~ . == 1))
# 
# df_na2 |> filter(is.na(duration))
# df_na2 |> filter(entropy == 0)

cmb_df5 <- cmb_df4 |>
  mutate(
    no_movement = if_else(if_any(ends_with('_perc'), ~ . == 1), TRUE, FALSE),
    meets_min_time_threshold = case_when(
      ((interval == 'rounds') & (duration > 60)) ~ TRUE,
      (interval %in% c('all_24','morning','afternoon','evening','night')) & (duration > 240) ~ TRUE,
      TRUE ~ FALSE,
    ))
cmb_df6 <- cmb_df5 |>
  filter(no_movement == FALSE, meets_min_time_threshold == TRUE)

table(cmb_df6$meets_min_time_threshold,cmb_df6$no_movement)
skimr::skim(cmb_df5 |> filter(no_movement == FALSE))

cmb_df5 |>
  filter(no_movement == FALSE) |>
  group_by(interval) |>
  summarize(
    n = n(),
    n_na = sum(is.na(duration))
  )
cmb_dfx <- cmb_df5 |> 
  filter(no_movement == FALSE) |>
  filter(is.na(burstiness))
cmb_df5 %>% write.csv('rtls_data_03-29-2024.csv')
cmb_df6 %>% write.csv('rtls_data_03-29-2024_PREFILTERD.csv')
rot_mapping <- cmb_df5 |> select(rotation,rot_cat,site) |>
  rename(service = rotation)

rots <- readxl::read_excel(here::here('Data','mergedRotations - deidentified v2.xlsx'), sheet = 'in') |>
  left_join(rot_mapping, by = c('service','site'))
  # distinct() |> write.csv('rotation_to_rot_cat_mapping_02-09-2024.csv')
write.csv(rots,'mergedRotations - deidentified v2_with_rot_cats.csv')

rots |> 
  filter(date >= lubridate::ymd('2020-07-01')) |>
  filter(is.na(rot_cat)) |>
  distinct(service,site) |> write.csv('missing_service_mappings_after_June_2020.csv')

t <- cmb_df5 |> filter(no_movement == FALSE,!is.na(duration))

hist(t$total, na.action = na.omit())
  # group_by(interval) |>
  # summarize(na_ct = sum(is.na(duration)),
  #           n = n())

# table(cmb_df4[which(cmb_df4$rot_cat == 'icu'),'rotation']) %>% write.csv(.,'ICU_rotations2.csv')

icu_rots <- read.csv('ICU_rotations.csv')

icu_data <- cmb_df4 %>% filter(rot_cat == 'icu') %>%
  left_join(icu_rots, by = 'rotation') %>%
  drop_na(ICU_rot) %>%
  filter(interval == 'rounds') %>%
  dplyr::select(date,patient_room_perc,ICU_rot) %>%
  group_by(ICU_rot,date) %>%
  summarize(mean_pt_rm_perc = mean(patient_room_perc,na.rm = TRUE)) %>%
  ungroup() %>%
  complete(
    ICU_rot,
    date = full_seq(date,period = 1)
  )

ICU_rot.means <- aggregate(mean_pt_rm_perc ~ ICU_rot, data = icu_data, FUN = mean, na.rm=TRUE)
BG_rots <- readxl::read_xlsx('BG_ICU_rotations.xlsx') %>%
  mutate(Start = as.Date(Start),Stop = as.Date(Stop))
icu_data %>% 
  # filter(str_starts(ICU_rot,'JHH')) %>%
  ggplot(aes(x=date,y=mean_pt_rm_perc,color = ICU_rot)) + geom_line() + 
  geom_rect(aes(xmin = Start, xmax = Stop, ymin=0, ymax=1),
            alpha = 0.5, fill = "green",
            data = BG_rots,
            inherit.aes = FALSE) +
  facet_wrap(~ICU_rot) +
  geom_hline(data = ICU_rot.means, mapping = aes(yintercept = mean_pt_rm_perc), color = "black") +
  ggthemes::theme_tufte() + guides(fill="none")

skimr::skim(icu_data)

locs <- c('transit','family_waiting_space','md_workroom','supply_and_admin','ward_hall','patient_room','education','other_unknown','total')
library(gtsummary)
cmb_df4 %>%
  # dplyr::select(ends_with('_perc'),interval) %>%
  dplyr::select(all_of(locs),interval) %>%
  gtsummary::tbl_summary(
    by = 'interval',
    statistic = all_continuous() ~ "{mean} ({sd})") %>%
  gtsummary::as_flex_table() %>%
  flextable::save_as_docx(path = 'Overal_minutes_table.docx')



cmb_df3 %>%
  ggplot(aes(x = burstiness, y = entropy)) + geom_point() + ggthemes::theme_tufte()
cmb_df3 %>%
  ggplot(aes(x = duration/60, y = entropy)) + geom_point() + ggthemes::theme_tufte()
cmb_df3 %>%
  ggplot(aes(x = duration/60, y = burstiness)) + geom_point() + ggthemes::theme_tufte()

table(test$badge)
names(test)
cmb_df3 %>%
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






# link with shift data
# y <- readxl::read_excel(here::here('Data','mergedRotations - deidentified v2.xlsx'), sheet = 'in') %>%
#   janitor::clean_names() %>%
#   mutate(date = as.Date(date)) %>%
#   # dplyr::select(-pgy) %>%
#   drop_na()

# Read in files...

f_list <- c(
  'Resident Schedule 2023-2024 - Bayview - PGY1.xlsx',
  'Resident Schedule 2023-2024 - Bayview - PGY2.xlsx',
  'Resident Schedule 2023-2024 - Bayview - PGY3.xlsx',
  'Resident Schedule 2023-2024 - Hopkins - PGY1.xlsx',
  'Resident Schedule 2023-2024 - Hopkins - PGY2.xlsx',
  'Resident Schedule 2023-2024 - Hopkins - PGY3.xlsx'
)

y <- data.frame(
  date = as_datetime(character()),
  service = character(),
  local_id = character(),
  site = character(),
  pgy = character())
for (f in f_list) {
  print(f)
  sheet_list <- readxl::excel_sheets(here::here('Data','schedules_2024',f))
  for (sheet in sheet_list) {
    df <- readxl::read_excel(here::here('Data','schedules_2024',f), sheet = sheet) |>
      select(Date,Service) |>
      mutate(
        local_id = sheet,
        site = case_when(
          str_detect(f,'Hopkins') ~ 'Hopkins',
          str_detect(f,'Bayview') ~ 'Bayview'
        ),
        pgy = case_when(
          str_detect(f,'PGY1') ~ 'PGY1',
          str_detect(f,'PGY2') ~ 'PGY2',
          str_detect(f,'PGY3') ~ 'PGY3'
        )) |>
      janitor::clean_names()
    y <- bind_rows(y,df)
  }
}


y <- y %>% filter(!is.na(date),!is.na(service),service != '-') %>%
  group_by(date,local_id) %>%
  filter(n() == 1) %>% ungroup()

# cmb_df2 <- cmb_df %>%
#   filter(!is.na(local_id)) %>%
#   left_join(y, by = c('local_id', 'date')) %>%
#   filter(date >= lubridate::ymd('2020-07-01')) %>%
#   rename(rotation = service)

# skimr::skim(cmb_df2)
# table(cmb_df2[which(cmb_df2$interval == 'all_24'),]$service) %>% write.csv('rotation_frequencies.csv')

z <- readxl::read_excel(here('Data','NEW_rotation_categories_102-09-2024.xlsx')) %>%
  dplyr::select(rotation,rot_cat) %>%
  mutate(rot_cat = tolower(rot_cat)) %>%
  distinct() #|>
  # filter(!is.na(rot_cat)) %>%
  # filter(!rot_cat %in% c('na','other','ambulatory','off')) 

table(z$rot_cat)
y_cat <- y |>
  rename(rotation = service) |>
  left_join(z, by = 'rotation') %>%
  mutate(rot_cat = case_when(str_detect(rotation, 'Longcope|Janeway|Barker|Thayer|JHH HS Firm') ~ 'ACS',
                             str_detect(rotation,'MICU|HS CCU') ~ 'icu',
                             substr(rotation,1,3) == 'CCU' ~ 'icu',
                             str_detect(rotation,'Ambulatory|Immersions|Foundations|BMC HS BASIC|MP Clinic|EBM|HS Res Med Clinic|CBP-CCP|JHH HS MP') ~ 'ambulatory',
                             str_detect(rotation,'Brancati|MPC') ~ 'Hospitalist',
                             str_detect(rotation,'BMC HS NT Yellow|BMC HS NT Crosscover|BMC HS NT Green|JHH HS DATO Resident|JHH HS NATO|JHH Housestaff Wolf Moonlighter') ~ 'general medicine',
                             str_detect(rotation,'Polk|addiction|Addiction|Card|Liver|BMC HS PCU Night Intern|BMC HS PCU Day Intern|BMC HS Hematology|JHH HS Neuro') ~ 'Subspecialty',
                             str_detect(rotation,'Off|Vacation|Parental') ~ 'off',
                             str_detect(rotation,'Solids|euks|MTL') ~ 'Oncology',
                             str_detect(rotation,'BMC HS Ecall|Selective|selective|UCM|HS DOD|JHH HS Pathway|Jeopardy|BMC HS Senior Teacher|BMC HS Call|BMC HS Golden Elective|BMC HS FLEX|JHH HS Women|JHH HS Supervisor') ~ 'other',
                             rotation == 'psych' ~ 'other',
                             rotation == 'NT' ~ 'other',
                             .default = rot_cat)) |>
  mutate(rot_cat = tolower(rot_cat))

table(y_cat$rot_cat)
# nrow(y_cat |> filter(date >= lubridate::ymd('2020-07-01')) |> filter(is.na(rot_cat)))

y_cat |> write.csv('mergedRotations - deidentified v2_with_rot_cat_05-14-2024.csv')

y_cat |> #write.csv('mergedRotations - deidentified v2_with_rot_cats2.csv')
  filter(is.na(rot_cat)) |> distinct(rotation,rot_cat) |> write.csv('uncateogrized_services_04-26-2024.csv')
