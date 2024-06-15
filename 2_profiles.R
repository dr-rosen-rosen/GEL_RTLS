###############################################################################################
###############################################################################################
#####################   Developing profiles with both RTLS & Epic data
#####################
###############################################################################################
###############################################################################################
library(tidyverse)

epic_df <- read.csv(here::here('profiles','EpicMeasureData_05-14-2024.csv')) |>
  mutate(date = as.Date(Year)) |> 
  select(-JHED_ID, -Year) |>
  drop_na(GEL_ID) |>
  rename(local_id = GEL_ID)
skimr::skim(epic_df)
rtls_df <- read.csv(here::here('profiles','rtls_data_03-29-2024.csv')) |> 
  filter(interval == 'all_24') |>
  mutate(date = as.Date(date)) |>
  select(local_id,date,patient_room_perc, total, md_workroom_perc, entropy, b_param, rot_cat, site, pgy) |>
  as.data.frame()
skimr::skim(rtls_df)

cmb_df <- epic_df |> full_join(rtls_df, by = c('local_id','date')) |>
  rowwise() |>
  mutate(epic_perc = EpicTimeMinutes / total) |> ungroup() |>
  select(!matches("InBasket|SecureChat|Seconds")) |>
  drop_na()
  
skimr::skim(cmb_df)

min(cmb_df$total
    )

##################################################################################
# Following six steps from Mäkikangas et al., 2018


##################################################################################
# Preliminary data anlaysis

# days per resident

cmb_df %>%
  group_by(local_id) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(count) %>%
  select(count) %>%
  ggplot(aes(x=count)) + geom_histogram(bins =100) +
  scale_y_continuous(breaks= scales::pretty_breaks()) +
  labs(title = '# of days by resident', y = '# of residents', x = '# of days') +
  geom_vline(xintercept=4, color = "red")

# unique shifts per resident
cmb_df |>
  group_by(local_id) |>
  summarize(n_rot_cats = length(unique(rot_cat))) |>
  ungroup() |> ggplot(aes(x=n_rot_cats)) + geom_histogram(bins =100) +
  scale_y_continuous(breaks= scales::pretty_breaks()) +
  scale_x_continuous(breaks= scales::pretty_breaks())

rots_and_days_by_local_id <- cmb_df |>
  group_by(local_id) |>
  summarize(n_rot_cats = length(unique(rot_cat))) |>
  ungroup() |>
  full_join(
    cmb_df %>%
      group_by(local_id) %>%
      summarize(count = n()) %>%
      ungroup(),
    by = 'local_id'
  )

# tentatively use >= 10 shfits from >= 4 rotation types

rots_and_days_by_local_id |> ggplot(aes(x = count, y = n_rot_cats)) +
  geom_point() +
  scale_y_continuous(breaks= scales::pretty_breaks()) +
  scale_x_continuous(breaks= scales::pretty_breaks())

# don't look like we need to filter? Everyone has at least 2 days. But HUGE variability.
# if decide to filter:
# threshold <- 4
# cmb_df <- cmb_df %>%
#   group_by(local_id) %>% filter(n() > !!threshold) %>% ungroup()

# outliers
cmb_df <- cmb_df |>
  dplyr::select(patient_room_perc,md_workroom_perc,entropy,b_param,epic_perc) |>
  mutate(mahad = careless::mahad(pick(everything()))) |>
  select(mahad) |> cbind(cmb_df) |>
  filter(mahad < 38) # check this again

# inter-correlations; vars are supposed to be uncorrelated
cmb_df |>
  dplyr::select(patient_room_perc,md_workroom_perc,entropy,b_param,epic_perc) |>
  modelsummary::datasummary_correlation()

# ICC's 
for (var in c('patient_room_perc','md_workroom_perc','entropy','b_param','epic_perc')) {
  f <- as.formula(paste(var,'~ local_id'))
  ICC1 <- multilevel::ICC1(aov(f,data=cmb_df))
  print(paste0(var,': ',ICC1))
}

library(MplusAutomation)
# https://cran.r-project.org/web/packages/MplusAutomation/vignettes/vignette.html

out_f_dir <- 'profiles/MPLUS_Runs'

ml_lpa_df <- cmb_df %>%
  rename('ID' = 'local_id') %>%
  #dplyr::select(patient_room_perc,md_workroom_perc,entropy,b_param,epic_perc)
  dplyr::select(entropy,b_param,epic_perc)

df_prep <- cmb_df %>%
  rename('ID' = 'local_id') %>%
  #dplyr::select(patient_room_perc,md_workroom_perc,entropy,b_param,epic_perc, ID) |>
  dplyr::select(entropy,b_param,epic_perc, ID) |>
  prepareMplusData(
    filename = glue::glue('{out_f_dir}/GEL_profile_mPlus.dat')
  )

skimr::skim(df_prep)
#### Automating MPLUS models

class_str <- character()

ml_lpa1_10 <- lapply(1:10, function(k)
{
  
  for (x in 1:k) {
    if (x == 1) {
      class_str <<- glue::glue("%C#{x}%
[entropy b_param epic_perc];
entropy b_param epic_perc;")
      #[patient_room_perc md_workroom_perc entropy b_param epic_perc];
      #patient_room_perc md_workroom_perc entropy b_param epic_perc;")
    } else {
      class_str <<- paste(class_str,
                          glue::glue("%C#{x}%
[entropy b_param epic_perc];
entropy b_param epic_perc;")
                          #[patient_room_perc md_workroom_perc entropy b_param epic_perc];
                          #patient_room_perc md_workroom_perc entropy b_param epic_perc;")
                          , sep="\n")
    }
    print(class_str)
  }
  
  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("L1_P_{k}_lpa_freeVar;"),
    VARIABLE = glue::glue("CLASSES = c({k});"),
    # USEVARIABLES = "drives cognition affect social big_words wc;",
    #DEFINE = "STANDARDIZE patient_room_perc md_workroom_perc entropy b_param epic_perc;",
    DEFINE = "STANDARDIZE entropy b_param epic_perc;",
    ANALYSIS = "TYPE = MIXTURE;
    ESTIMATOR=MLR;
    STARTS=1000 50;
    STITERATIONS=50;
    LRTSTARTS=1000 50 1000 50;
    PROCESSORS=4;",
    MODEL = glue::glue('%OVERALL%\n',class_str),
    OUTPUT = "SAMPSTAT STANDARDIZED MOD (5.00) TECH7 TECH11 TECH13 TECH14;",
    # PLOT = "type = plot3;",
    usevariables = colnames(ml_lpa_df),
    rdata = ml_lpa_df
  )
  
  ml_lpa_fit <- MplusAutomation::mplusModeler(lca_enum,
                                              dataout = glue::glue("{out_f_dir}/L1_prof_{k}_ml_lpa_freeVar_enum.dat"),
                                              modelout = glue::glue("{out_f_dir}/L1_prof_{k}_ml_lpa_freeVar_enum.inp"),
                                              check = TRUE, run = TRUE, hashfilename = FALSE)
}
)

output_enum <- readModels(here::here(glue::glue("{out_f_dir}")), quiet = TRUE)
enum_summary <- LatexSummaryTable(output_enum,
                                  keepCols=c("Title",
                                             "LL",
                                             "BIC",
                                             "aBIC"),
                                  sortBy = "Title")
enum_summary %>%
  gt::gt()

l1_k_profiles_plot <- enum_summary %>%
  as.data.frame() %>%
  filter(str_detect(Title, pattern = 'L1_')) %>%
  # filter(str_detect(Title, pattern = 'L2', negate = TRUE)) %>%
  pivot_longer(
    !Title, names_to = 'criteria', values_to = 'value'
  ) %>%
  mutate(
    num_profiles = readr::parse_number(str_remove(Title,'L1_P_'))
  ) %>%
  filter(criteria != 'LL') %>%
  ggplot(aes(x = num_profiles, y = value, group = criteria, color = criteria)) + 
  geom_point() + geom_line() + ggthemes::geom_rangeframe() + ggthemes::theme_tufte() + 
  scale_x_continuous(breaks= scales::pretty_breaks()) + theme(legend.position = 'bottom') +
  labs(
    title='Model fit by number of interaction (L1) profiles',
    x = 'Number of interaction (L1) profiles',
    y = 'Model fit statistic value',
    color = 'Fit statistic')

################################
######## Check for fit for different number of L2 vars
################################

out_f_dir <- 'profiles/MPLUS_Runs'

ml_lpa_df <- cmb_df %>%
  rename('ID' = 'local_id') %>%
  #dplyr::select(patient_room_perc,md_workroom_perc,entropy,b_param,epic_perc)
  dplyr::select(entropy,b_param,epic_perc, ID) |>
  mutate(row_num = row_number())

df_prep <- cmb_df %>%
  rename('ID' = 'local_id') %>%
  #dplyr::select(patient_room_perc,md_workroom_perc,entropy,b_param,epic_perc, ID) |>
  dplyr::select(entropy,b_param,epic_perc, ID)  |>
  mutate(row_num = row_number()) |>
  prepareMplusData(
    filename = glue::glue('{out_f_dir}/GEL_profile_mPlus_ML.dat')
  )


class_str <- character()

k <- 5 # L1 profiles
ml_lpa2 <- lapply(1:10, function(j) # L2 profiles
{
  
  for (x in 1:k) {
    if (x == 1) {
      class_str <<- glue::glue("%C#{x}%
[entropy b_param epic_perc];
entropy b_param epic_perc;")
    } else {
      class_str <<- paste(class_str,
                          glue::glue("%C#{x}%
[entropy b_param epic_perc];
entropy b_param epic_perc;"
                          ), sep="\n")
    }
    print(class_str)
  }
  
  lca_enum <- MplusAutomation::mplusObject(
    TITLE = glue::glue("L2_{j}_P_{k}_lpa_freeVar;"),
    VARIABLE = glue::glue("IDVARIABLE IS row_num;\nCLASSES = BC({j}) c({k});\nCLUSTER IS ID;\nWITHIN ARE entropy b_param epic_perc;\nBETWEEN ARE BC;"),
    DEFINE = "STANDARDIZE entropy b_param epic_perc;",
    ANALYSIS = "TYPE = MIXTURE TWOLEVEL;
    ESTIMATOR=MLR;
    STARTS=1000 200;
    STITERATIONS=50;
    LRTSTARTS=1000 50 1000 50;
    PROCESSORS=4;",
    MODEL = glue::glue('%WITHIN%\n%OVERALL%\n%BETWEEN%\n%OVERALL%\nc ON BC;\nMODEL c:\n%WITHIN%\n',class_str),
    OUTPUT = "SAMPSTAT STANDARDIZED MOD (5.00) TECH7 TECH11 TECH13 TECH14;",
    # PLOT = "type = plot3;",
    SAVEDATA = glue::glue("file=mlLpa_L2_{j}_L1_{k}.dat;\nsave=cprob;\nTECH4 IS tech4.dat;"),
    usevariables = colnames(ml_lpa_df),
    rdata = ml_lpa_df
  )
  
  ml_lpa_fit <- MplusAutomation::mplusModeler(lca_enum,
                                              dataout = glue::glue("{out_f_dir}/L2_{j}_P_{k}_ml_lpa_freeVar.dat"),
                                              modelout = glue::glue("{out_f_dir}/L2_{j}_P_{k}_ml_lpa_freeVar.inp"),
                                              check = TRUE, run = TRUE, hashfilename = FALSE)
}
)


output_enum <- readModels(here::here(glue::glue("{out_f_dir}")), quiet = TRUE)
enum_summary <- LatexSummaryTable(output_enum,
                                  keepCols=c("Title",
                                             "LL",
                                             "BIC",
                                             "aBIC"),
                                  sortBy = "Title")
enum_summary %>%
  gt::gt()

k <- 5
l2_k_range_plot <- enum_summary %>%
  as.data.frame() %>%
  filter(str_detect(Title,pattern = 'L1_', negate = TRUE)) %>%
  filter(str_detect(Title, pattern = glue::glue('_P_{k}_'))) %>%
  pivot_longer(
    !Title, names_to = 'criteria', values_to = 'value'
  ) %>%
  mutate(
    num_profiles = readr::parse_number(str_remove(Title,'L2_'))
  ) %>%
  filter(criteria != 'LL') %>%
  ggplot(aes(x = num_profiles, y = value, group = criteria, color = criteria)) + 
  geom_point() + geom_line() + ggthemes::geom_rangeframe() + ggthemes::theme_tufte() + 
  scale_x_continuous(breaks= scales::pretty_breaks()) + theme(legend.position = 'bottom') +
  labs(
    title=glue::glue('Model fit by number of resident (L2) classes \nfor a {k} L1 profile model'),
    x = 'Number of resident (L2) classes',
    y = 'Model fit statistic value',
    color = 'Fit statistic')



L2_k <- 5
mlLPA_results <- MplusAutomation::readModels(glue::glue("{out_f_dir}/L2_{L2_k}_P_{k}_ml_lpa_freeVar.out"), what="savedata")$savedata

# L! bar charts faceted by profile
l1_by_liwc_plot <- mlLPA_results %>%
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  dplyr::select(ENTROPY:EPIC_PER, L1) %>% 
  # rename('row_num' = ROW_NUM) %>%
  # select(-starts_with("V")) %>%
  pivot_longer(
    cols = ENTROPY:EPIC_PER,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    L1 = as.factor(L1),
    variable = ordered(tolower(variable), levels = c("entropy","b_param","epic_per"))
  ) %>%
  group_by(L1, variable) %>%
  summarize(
    m = mean(value),
    sd = sd(value, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = variable, y = m, fill = variable)) + geom_col() + facet_grid(~L1) + 
  ggthemes::theme_tufte() + theme(
    legend.position="bottom",
    panel.background = element_rect(fill = NA, color = "black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  ) + labs(fill = 'RTLS/EPIC dimension',
           y = 'Mean standardized value',
           x = 'RTLS/EPIC dimension',
           title = 'RTLS/EPIC dimensions by interaction (L1) profile')

# L2 profiles by L1 composition
l2_by_l1_plot <- mlLPA_results %>% 
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  janitor::clean_names() %>%
  select(l1,l2) %>%
  mutate(
    l1 = as.factor(l1),
    l2 = as.factor(l2)
  ) %>%
  group_by(l2,l1) %>%
  summarize(l1_count = n())%>%
  ungroup() %>%
  group_by(l2) %>%
  mutate(l1_perc = l1_count / sum(l1_count)) %>%
  ungroup() %>%
  ggplot(aes(fill=l1, y=l1_perc, x=l2)) + 
  geom_bar(position="fill", stat="identity") + ggthemes::theme_tufte() + 
  theme(legend.position="bottom") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = 'Frequency of L1 interaction profiles in L2 residednt classes',
    x = 'L2 resident class',
    y = 'Percentage of L1 profiles',
    fill = 'L1 profile'
  )

l1_by_l2_tbl <- mlLPA_results %>% 
  rename('L1' = 'C',
         'L2' = 'BC') %>%
  janitor::clean_names() %>%
  select(l1,l2) %>%
  mutate(
    l1 = as.factor(l1),
    l2 = as.factor(l2)
  ) %>%
  gtsummary::tbl_cross(
    row = l1,
    col = l2, 
    percent = 'column',
    label = list(l1 = 'L1 profiles', l2 = 'L2 classes')) %>%
  gtsummary::bold_labels() %>%
  gtsummary::as_gt() %>%
  gt::tab_header(title = 'Frequency of L1 interaction profiles by L2 resident classes') %>%
  bstfun::as_ggplot()

l1_by_liwc_plot / (l2_by_l1_plot + l1_by_l2_tbl) + plot_annotation(tag_levels = 'A')