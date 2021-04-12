# load packages
library('tidyverse')

# tsls() taken from sem package from CRAN mirror
# here https://github.com/cran/sem/blob/master/R/tsls.R
source(here::here("analysis","tsls.R")) 

## import data
df_input <- read_csv(
  here::here("output", "input_3.csv"),
  col_types = cols(
    # identifiers
    patient_id = col_integer(),
    practice_id = col_integer(),

    # demographic / administrative
    msoa = col_character(),
    stp = col_character(),
    region = col_character(),
    imd = col_character(),
    rural_urban = col_integer(),
    care_home_type = col_character(),
    care_home_tpp = col_logical(),
    care_home_primis = col_logical(),
    stp = col_character(),

    registered_at_latest = col_logical(),
    has_follow_up_previous_year = col_logical(),

    age = col_integer(),
    sex = col_character(),
    # ethnicity = col_character(),
    # ethnicity_6_sus = col_character(),
    ethnicity_16 = col_character()

  )
)

dim(df_input)
class(df_input)
colnames(df_input)
sapply(df_input, class)

# greater than or equal to 80 indicator
df_input <- df_input %>% 
  mutate(gr80 = as.numeric(age >= 80))
sum(is.na(df_input$gr80))
table(df_input$gr80, useNA = "ifany")

# non-care home residents
class(df_input$care_home_type)
table(df_input$care_home_type, useNA = "ifany")

class(df_input$care_home_tpp)
table(df_input$care_home_tpp, useNA = "ifany")

class(df_input$care_home_primis)
table(df_input$care_home_primis, useNA = "ifany")

df_input_tpp <- df_input %>%
  filter(care_home_tpp == FALSE)
dim(df_input_tpp)

df_input_primis <- df_input %>%
  filter(care_home_primis == FALSE)
dim(df_input_primis)

# setup week start and end dates
startweek <- c("2020-12-07","2020-12-14","2020-12-21","2020-12-28","2021-01-04",
              "2021-01-11","2021-01-18","2021-01-25","2021-02-01","2021-02-08",
              "2021-02-15")
endweek   <- c("2020-12-14","2020-12-21","2020-12-28","2021-01-04","2021-01-11",
              "2021-01-18","2021-01-25","2021-02-01","2021-02-08","2021-02-15",
              "2021-02-22")

# initialise lists for results and plots
results <- pt_plot <- vector("list", length(startweek))

for (i in 1:length(startweek)) {
  
  # print iteration
  print(paste("Week", i))
  print(paste("Start date", startweek[i]))
  
  # positive test in week
  df_input <- df_input %>% 
    mutate(pos_test_in_week = positive_test_1_date > startweek[i] &
             positive_test_1_date <= endweek[i])
  
  class(df_input$pos_test_in_week)
  table(df_input$pos_test_in_week)
  
  # pt_plot[[i]] <- df_input %>% 
  #   drop_na(age, pos_test_in_week) %>%
  #   filter(age >= 18) %>%
  #   ggplot(aes(x = age, y = pos_test_in_week)) + 
  #   geom_point() + 
  #   theme_bw()
  # 
  # # save plots
  # ggsave(
  #   plot = pt_plot[[i]],
  #   filename = paste0("pt_plot_week_", i, ".png"),
  #   path = here::here("output"),
  #   units = "cm",
  #   height = 15,
  #   width = 15
  # )
  
  # fit IV estimator
  ivfit <- try(tsls(as.numeric(pos_test_in_week) ~ age, ~ gr80, 
                data = df_input))
  if (class(ivfit) != "try-error") {
    results[[i]] <- ivfit
    print(summary(ivfit))
    print(cbind(coef(ivfit), confint.default(ivfit)))
  }
}
