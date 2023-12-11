################ Recreating JAMA Open analysis for data since that publication

library(lme4)
library(sjstats)
library(sjPlot)
library(sjmisc)
library(lmerTest)
library(lattice)
library(patchwork)
library(here)

detach("package:lattice", unload=TRUE)


interval_var <- c('all_24')

df <- cmb_df4 %>%
  drop_na(local_id,rotation) %>%
  filter(rot_cat != 'off' & rot_cat != 'other') %>%
  mutate(rot_cat = case_match(rot_cat, 
                              c('Subspecialty','subspecialty') ~ 'subspecialty',
                              .default = rot_cat)) #%>%
  # drop_na(patient_room_perc,badge,rot_cat,pgy)
hist(df$patient_room_perc)

M00 <- df %>%
  filter(interval %in% !!interval_var) %>%
  mutate(patient_room_perc = scale(patient_room_perc, center = TRUE, scale = TRUE)) %>%
  lm(patient_room_perc ~ 1, data = .)
summary(M00)

M0a <- df %>%
  filter(interval %in% !!interval_var) %>%
  mutate(patient_room_perc = scale(patient_room_perc, center = TRUE, scale = TRUE)) %>%
  lmer(patient_room_perc ~ 1 + (1|badge), ., REML = FALSE)
summary(M0a)
anova(M0a, M00)
sjPlot::plot_model(M0a, type = 're')


M0b <- df %>%
  filter(interval %in% !!interval_var) %>%
  mutate(patient_room_perc = scale(patient_room_perc, center = TRUE, scale = TRUE)) %>%
  lmer(patient_room_perc ~ 1 + (1|badge) + (1|rot_cat), ., REML = FALSE)
summary(M0b)
anova(M0a,M0b)
sjPlot::plot_model(M0b, type = 're')

M1a <- df %>%
  filter(interval %in% !!interval_var) %>%
  mutate(patient_room_perc = scale(patient_room_perc, center = TRUE, scale = TRUE)) %>%
  lmer(patient_room_perc ~ day_of_year + week_day + pgy + (1|badge) + (1|rot_cat), ., REML = FALSE)
summary(M1a)
anova(M0a,M1a)
sjPlot::tab_model(M1a)
sjPlot::plot_model(M1a,type = 're')
lattice::qqmath(M1a)

# M1b <- df %>%
#   filter(interval %in% !!interval_var) %>%
#   mutate(patient_room_perc = scale(patient_room_perc, center = TRUE, scale = TRUE)) %>%
#   lmer(patient_room_perc ~ day_of_year + #Service_numDays + (1|badge) + (1|rot_cat), ., REML = FALSE)
# summary(M1b)
# anova(M1a,M1b)
# sjPlot::tab_model(M1b)
# plot(M1b)
# sjPlot::plot_model(M1b, sort.est = TRUE)
# lattice::qqmath(M1b)

M2a <- df %>%
  filter(interval %in% !!interval_var) %>%
  mutate(patient_room_perc = scale(patient_room_perc, center = TRUE, scale = TRUE)) %>%
  lmer(patient_room_perc ~ day_of_year + day_of_week + (1|badge) + (1|rot_cat), ., REML = FALSE)   
anova(M1b,M2a)
summary(M2a)

# library(parameters)
# ci(M2a)
# model_parameters(M2a, ci_method = 'residual')
# library(merTools)
# randomSims <- REsim(M2a, n.sims = 500)
# # and to plot it
# plotREsim(REsim(M2a, n.sims = 500)) +theme_tufte()

p_rounds <- sjPlot::plot_model(M2a, type = 're')[[2]] + 
  theme_tufte() +
  labs(title = 'Rounds',
       y = 'Percent time at the bedside (centered)',
       x = 'Service') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black')
show(p_rounds)

p_all24 <- sjPlot::plot_model(M2a, type = 're')[[2]] + 
  theme_tufte() +
  labs(title = '24 hour period',
       y = 'Percent time at the bedside (centered)',
       x = 'Service') +
  geom_hline(yintercept = 0, linetype = 'solid', color = 'black')
show(p_all24)

p_all24 / p_rounds + plot_layout(guides = "collect")

sjPlot::tab_model(M2a)
lattice::qqmath(M2a)

sjPlot::tab_model(M00,M0a,M0b,M2a,
                  digits = 5#)
                  , file = here('output','tables','Model_table_all24_5digits.html'))


anova(M00)
anova(M0a,M00)
anova(M0a,M0b)
anova(M0b,M2a)


df %>%
  filter(Interval == 'rounds') %>%
  aov(Patient.room_perc ~ Service_grouped, data = .) %>%
  #summary()
  plot()
#TukeyHSD() %>% plot(las=1)



interval_var <- c('all_24')

df <- cmb_df4 %>%
  drop_na(local_id,rotation) %>%
  filter(rot_cat != 'off' & rot_cat != 'other') %>%
  mutate(rot_cat = case_match(rot_cat, 
                              c('Subspecialty','subspecialty') ~ 'subspecialty',
                              .default = rot_cat)) #%>%
# drop_na(patient_room_perc,badge,rot_cat,pgy)
hist(df$entropy)

M00 <- df %>%
  filter(interval %in% !!interval_var) %>%
  mutate(entropy = scale(entropy, center = TRUE, scale = TRUE)) %>%
  lm(entropy ~ 1, data = .)
summary(M00)

M0a <- df %>%
  filter(interval %in% !!interval_var) %>%
  mutate(entropy = scale(entropy, center = TRUE, scale = TRUE)) %>%
  lmer(entropy ~ 1 + (1|badge), ., REML = FALSE)
summary(M0a)
anova(M0a, M00)
sjPlot::plot_model(M0a, type = 're')


M0b <- df %>%
  filter(interval %in% !!interval_var) %>%
  mutate(entropy = scale(entropy, center = TRUE, scale = TRUE)) %>%
  lmer(entropy ~ 1 + (1|badge) + (1|rot_cat), ., REML = FALSE)
summary(M0b)
anova(M0a,M0b)
sjPlot::plot_model(M0b, type = 're')

M1a <- df %>%
  filter(interval %in% !!interval_var) %>%
  mutate(entropy = scale(entropy, center = TRUE, scale = TRUE)) %>%
  lmer(entropy ~ day_of_year + week_day + pgy + (1|badge) + (1|rot_cat), ., REML = FALSE)
summary(M1a)
anova(M0a,M1a)
sjPlot::tab_model(M1a)
sjPlot::plot_model(M1a, type = 're')
lattice::qqmath(M1a)

interval_var <- c('all_24')

df <- cmb_df4 %>%
  drop_na(local_id,rotation) %>%
  filter(rot_cat != 'off' & rot_cat != 'other') %>%
  mutate(rot_cat = case_match(rot_cat, 
                              c('Subspecialty','subspecialty') ~ 'subspecialty',
                              .default = rot_cat)) #%>%
# drop_na(patient_room_perc,badge,rot_cat,pgy)
hist(df$burstiness)

M00 <- df %>%
  filter(interval %in% !!interval_var) %>%
  mutate(burstiness = scale(burstiness, center = TRUE, scale = TRUE)) %>%
  lm(burstiness ~ 1, data = .)
summary(M00)

M0a <- df %>%
  filter(interval %in% !!interval_var) %>%
  mutate(burstiness = scale(burstiness, center = TRUE, scale = TRUE)) %>%
  lmer(burstiness ~ 1 + (1|badge), ., REML = FALSE)
summary(M0a)
anova(M0a, M00)
sjPlot::plot_model(M0a, type = 're')


M0b <- df %>%
  filter(interval %in% !!interval_var) %>%
  mutate(burstiness = scale(burstiness, center = TRUE, scale = TRUE)) %>%
  lmer(burstiness ~ 1 + (1|badge) + (1|rot_cat), ., REML = FALSE)
summary(M0b)
anova(M0a,M0b)
sjPlot::plot_model(M0b, type = 're')

M1a <- df %>%
  filter(interval %in% !!interval_var) %>%
  mutate(burstiness = scale(burstiness, center = TRUE, scale = TRUE)) %>%
  lmer(burstiness ~ day_of_year + week_day + pgy + (1|badge) + (1|rot_cat), ., REML = FALSE)
summary(M1a)
anova(M0a,M1a)
sjPlot::tab_model(M1a)
sjPlot::plot_model(M1a, type = 're')
lattice::qqmath(M1a)
