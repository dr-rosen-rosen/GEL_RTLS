################ Recreating JAMA Open analysis for data since that publication




##############
############## Pull data for all target badges for a site
##############

# site <- c('jhh') # 'jhh','bmc'
strt <-  lubridate::ymd('2020-07-01')#config$FB_report_start,#lubridate::ymd('2022-02-27'), 
stp <- lubridate::ymd('2023-06-30')#config$FB_report_stop,#lubridate::ymd('2022-03-01'), 
df  <- get_and_locCode_RTLS_data_pg(
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
cmb_df <- get_tot_sum(df)
# wrkflw_df <- get_workflow_metrics(df)
wrkflw_df <- get_workflow_metrics_by_interval(df)
wrkflw_df <- wrkflw_df %>%
  rename(date = d)
cmb_df <- cmb_df %>%
  left_join(wrkflw_df, by = c('badge','date','interval'))
skimr::skim(cmb_df)

skimr::skim(wrkflw_df)
# DescTools::Entropy(table(c('1','1','1','4')))

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
                             str_detect(rotation,'Ambulatory|Immersions|Foundations|BMC HS BASIC|MP Clinic|EBM|HS Res Med Clinic|CBP-CCP') ~ 'ambulatory',
                             str_detect(rotation,'Brancati|MPC') ~ 'Hospitalist',
                             # str_detect(rotation,'Janeway') ~ 'ACS',
                             str_detect(rotation,'Polk|addiction|Addiction|Card|Liver') ~ 'Subspecialty',
                             str_detect(rotation,'Off|Vacation|Parental') ~ 'off',
                             str_detect(rotation,'Solids|euks|MTL') ~ 'Oncology',
                             str_detect(rotation,'BMC HS Ecall|Selective|selective|UCM|HS DOD|JHH HS Pathway|Jeopardy|BMC HS Senior Teacher') ~ 'other',
                             rotation == 'psych' ~ 'other',
                             rotation == 'NT' ~ 'other',
                              .default = rot_cat))
# cmb_df3 %>% filter(is.na(rot_cat)) %>% dplyr::select(rotation) %>% distinct() %>% view()

skimr::skim(cmb_df3)
skimr::skim(cmb_df2)

cmb_df4 <- do_cleaning(cmb_df3)
skimr::skim(cmb_df4)

cmb_df4 %>% write.csv('rtls_data_12-16-2023.csv')


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


