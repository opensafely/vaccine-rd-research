# log file
# zz <- file(here::here("logs","1-basic-rd.log"), open = "wt")
# sink(zz)
# sink(zz, type = "message")

# load packages
library('tidyverse')

## import data
df_input <- read_csv(
  here::here("output", "input.csv"),
  col_types = cols(
    patient_id = col_integer(),
    stp = col_character()
  )
)

dim(df_input)
class(df_input)
colnames(df_input)
sapply(df_input, class)

# greater than or equal to 80 indicator
df_input <- df_input %>% mutate(gr80 = as.numeric(age >= 80))
sum(is.na(df_input$gr80))
table(df_input$gr80)

# week of 2020-12-08 to 2020-12-15

startweek <- "2020-12-08"
endweek   <- "2020-12-15"

# positive test in week

df_input <- df_input %>% 
  mutate(pos_test_in_week = positive_test_1_date > startweek &
           positive_test_1_date <= endweek)

class(df_input$pos_test_in_week)
table(df_input$pos_test_in_week)

df_input %>% 
  drop_na(age, pos_test_in_week) %>%
  filter(age >= 50) %>%
  ggplot(aes(x = age, y = pos_test_in_week)) + 
  geom_point() + 
  theme_bw()

# ggsave(
#   plot= plot_stppop_map,
#   filename="plot_stppop_map.png", 
#   path=here::here("output", "plots"),
#   units = "cm",
#   height = 10,
#   width = 10
# )

# primary care covid case in week

df_input <- df_input %>%
  mutate(pc_case_in_week = primary_care_covid_case_1_date > startweek & 
           primary_care_covid_case_1_date <= endweek)

df_input %>% 
  drop_na(age, pc_case_in_week) %>%
  filter(age >= 50) %>%
  ggplot(aes(x = age, y = pc_case_in_week)) + 
  geom_point() + 
  theme_bw()

# admitted to hospital in week

df_input <- df_input %>%
  mutate(admitted_in_week = admitted_1_date > startweek &
           admitted_1_date <= endweek)

df_input %>% 
  drop_na(age, admitted_in_week) %>%
  filter(age >= 50) %>%
  ggplot(aes(x = age, y = admitted_in_week)) + 
  geom_point() + 
  theme_bw()

# death from covid in week

df_input <- df_input %>%
  mutate(coviddeath_in_week = coviddeath_date > startweek &
           coviddeath_date <= endweek)

df_input %>% 
  drop_na(age, coviddeath_in_week) %>%
  filter(age >= 50) %>%
  ggplot(aes(x = age, y = coviddeath_in_week)) + 
  geom_point() + 
  theme_bw()

# regression discontinuity analysis

# install.packages("ivmodel")
# library(ivmodel)
# 
# ivfit1 <- ivmodel(as.numeric(df_input$pos_test_in_week), 
#                   df_input$age, 
#                   df_input$gr80)
# summary(ivfit1)
# 
# ivfit2 <- ivmodel(as.numeric(df_input$pc_case_in_week), 
#                   df_input$age, 
#                   df_input$gr80)
# summary(ivfit2)
# 
# ivfit3 <- ivmodel(as.numeric(df_input$admitted_in_week), 
#                   df_input$age, 
#                   df_input$gr80)
# summary(ivfit3)
# 
# ivfit4 <- ivmodel(as.numeric(df_input$coviddeath_in_week), 
#                   df_input$age, 
#                   df_input$gr80)
# summary(ivfit4)

source(here::here("analysis","tsls.R"))

ivfit1 <- tsls(as.numeric(pos_test_in_week) ~ age, ~ gr80, 
               data = df_input)
summary(ivfit1)

ivfit2 <- tsls(as.numeric(pc_case_in_week) ~ age, ~ gr80,
               data = df_input)
summary(ivfit2)

ivfit3 <- tsls(as.numeric(admitted_in_week) ~ age, ~ gr80,
               data = df_input)
summary(ivfit3)

ivfit4 <- tsls(as.numeric(coviddeath_in_week) ~ age, ~ gr80,
               data = df_input)
summary(ivfit4)
