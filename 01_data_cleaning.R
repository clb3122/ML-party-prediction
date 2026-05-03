# LOAD, CLEAN, AND IMPUTE DATA

library(tidyverse)
library(mice)

anes_2024 <- read.csv("data/ANES2024.csv")

anes_2024_subset <- anes_2024 |>
  transmute(
    
    # DEMOGRAPHICS
    
    age = case_when(
      V241458x %in% 18:80 ~ as.numeric(V241458x),
      TRUE ~ NA_real_),
    
    gender = case_when(
      V241551 == 1 ~ "Man",
      V241551 == 2 ~ "Woman",
      V241551 %in% c(3, 4) ~ "Nonbinary/Other",
      TRUE ~ NA_character_),
    
    is_transgender = case_when(
      V241552 == 1 ~ "Yes",
      V241552 == 2 ~ "No",
      TRUE ~ NA_character_),
    
    sexual_orientation = case_when(
      V241553 == 1 ~ "Heterosexual",
      V241553 == 2 ~ "Gay/Lesbian",
      V241553 == 3 ~ "Bisexual",
      V241553 == 4 ~ "Other",
      TRUE ~ NA_character_),
    
    race = case_when(
      V241501x == 1 ~ "White",
      V241501x == 2 ~ "Black",
      V241501x == 3 ~ "Hispanic",
      V241501x == 4 ~ "Asian/Pacific Islander",
      V241501x == 5 ~ "Native/Other",
      V241501x == 6 ~ "Multiracial",
      TRUE ~ NA_character_),
    
    birthplace = case_when(
      V241507 == 1 ~ "US state or DC",
      V241507 == 2 ~ "Puerto Rico",
      V241507 == 3 ~ "Other US territory",
      V241507 == 4 ~ "Other country",
      TRUE ~ NA_character_),
    
    marital_status = case_when(
      V241461x == 1 ~ "Married",
      V241461x == 2 ~ "Widowed",
      V241461x == 3 ~ "Divorced",
      V241461x == 4 ~ "Separated",
      V241461x == 5 ~ "Never married",
      TRUE ~ NA_character_),
    
    education = case_when(
      V241465x == 1 ~ "Less than HS",
      V241465x == 2 ~ "HS diploma",
      V241465x == 3 ~ "Some college",
      V241465x == 4 ~ "Bachelors",
      V241465x == 5 ~ "Graduate",
      TRUE ~ NA_character_),
    education = ordered(
      education,
      levels = c("Less than HS", "HS diploma", "Some college", "Bachelors", "Graduate")),
    
    hh_income = case_when(
      V241567x == 1 ~ "Under $9,999",
      V241567x == 2 ~ "$10,000–$29,999",
      V241567x == 3 ~ "$30,000–$59,999",
      V241567x == 4 ~ "$60,000–$99,999",
      V241567x == 5 ~ "$100,000–$249,999",
      V241567x == 6 ~ "$250,000+",
      TRUE ~ NA_character_),
    hh_income = ordered(
      hh_income,
      levels = c(
        "Under $9,999",
        "$10,000–$29,999",
        "$30,000–$59,999",
        "$60,000–$99,999",
        "$100,000–$249,999",
        "$250,000+")),
    
    home_tenure = case_when(
      V241530 == 1 ~ "Own/buying",
      V241530 == 2 ~ "Rent",
      V241530 == 3 ~ "No cash rent",
      TRUE ~ NA_character_),
    home_tenure = factor(home_tenure, levels = c("Own/buying", "Rent", "No cash rent")),
    
    occupation = case_when(
      V241488x == 1 ~ "Working",
      V241488x %in% c(2, 4) ~ "Unemployed",
      V241488x %in% c(5, 6, 7, 8) ~ "Not working",
      TRUE ~ NA_character_),
    
    union_status = case_when(
      V241497 == 1 ~ "Yes",
      V241497 == 2 ~ "No",
      TRUE ~ NA_character_),
    
    military = case_when(
      V241470 == 1 ~ "Active duty",
      V241470 == 2 ~ "Previously served",
      V241470 == 3 ~ "Never served",
      TRUE ~ NA_character_),
    
    urban_rural = case_when(
      V242341 == 1 ~ "Urban",
      V242341 == 2 ~ "Suburban",
      V242341 == 3 ~ "Small town",
      V242341 == 4 ~ "Rural",
      V242341 == 5 ~ "Other",
      TRUE ~ NA_character_),
    urban_rural = factor(
      urban_rural,
      levels = c("Urban", "Suburban", "Small town", "Rural", "Other")),
    
    religion = case_when(
      V241445x == 1 ~ "Mainline Protestant",
      V241445x == 2 ~ "Evangelical Protestant",
      V241445x == 3 ~ "Black Protestant",
      V241445x == 4 ~ "Roman Catholic",
      V241445x == 5 ~ "Other Christian",
      V241445x == 6 ~ "Jewish",
      V241445x == 7 ~ "Other religion",
      V241445x == 8 ~ "Not religious",
      TRUE ~ NA_character_),
    
    religious_attendance = case_when(
      V241440 == 1 ~ "Every week",
      V241440 == 2 ~ "Almost every week",
      V241440 == 3 ~ "Once or twice a month",
      V241440 == 4 ~ "A few times a year",
      V241440 == 5 ~ "Never",
      TRUE ~ NA_character_),
    religious_attendance = factor(
      religious_attendance,
      levels = c(
        "Never",
        "A few times a year",
        "Once or twice a month",
        "Almost every week",
        "Every week"),
      ordered = TRUE),
    
    # PARTY AFFILIATION
    
    party_id_3 = case_when(
      V241221 == 1 ~ "Democrat",
      V241221 == 2 ~ "Republican",
      V241221 == 3 ~ "Independent",
      TRUE ~ NA_character_),
    
    party_id_7 = case_when(
      V241227x == 1 ~ "Strong Democrat",
      V241227x == 2 ~ "Weak Democrat",
      V241227x == 3 ~ "Independent-Democrat",
      V241227x == 4 ~ "Independent",
      V241227x == 5 ~ "Independent-Republican",
      V241227x == 6 ~ "Weak Republican",
      V241227x == 7 ~ "Strong Republican",
      TRUE ~ NA_character_),
    
    party_id_7 = factor(
      party_id_7,
      levels = c(
        "Strong Democrat",
        "Weak Democrat",
        "Independent-Democrat",
        "Independent",
        "Independent-Republican",
        "Weak Republican",
        "Strong Republican"),
      ordered = TRUE),

    guns_in_household = case_when(V241583 >= 0 ~ as.numeric(V241583), TRUE ~ NA_real_),
    
    # ISSUE OPINIONS
    
    # voting_duty_choice_7: 1 = Very strongly a duty ... 4 = Neither ... 7 = Very strongly a choice
    voting_duty_choice_7 = case_when(
      V241218x %in% 1:7 ~ as.numeric(V241218x),
      TRUE ~ NA_real_),
    
    gov_run_by = case_when(
      V241231 == 1 ~ "Big interests",
      V241231 == 2 ~ "Benefit of all",
      TRUE ~ NA_character_),
    
    gov_waste_3 = case_when(
      V241232 == 1 ~ "Waste a lot",
      V241232 == 2 ~ "Waste some",
      V241232 == 3 ~ "Waste very little",
      TRUE ~ NA_character_),
    
    spend_services_7 = case_when(V241239 %in% 1:7 ~ as.numeric(V241239), TRUE ~ NA_real_),
    # spend_services_7: 1 = Fewer services ... 7 = More services
    
    defense_spend_7 = case_when(V241242 %in% 1:7 ~ as.numeric(V241242), TRUE ~ NA_real_),
    # defense_spend_7: 1 = Decrease defense spending ... 7 = Increase defense spending
    
    health_insurance_7 = case_when(V241245 %in% 1:7 ~ as.numeric(V241245), TRUE ~ NA_real_),
    # health_insurance_7: 1 = Government insurance plan ... 7 = Private insurance plan
    
    abortion_7 = case_when(V241248 %in% 1:7 ~ as.numeric(V241248), TRUE ~ NA_real_),
    # abortion_7: 1 = Always permitted ... 7 = Never permitted
    
    job_income_7 = case_when(V241252 %in% 1:7 ~ as.numeric(V241252), TRUE ~ NA_real_),
    # job_income_7: 1 = Gov should see to jobs/standard of living ... 7 = People get ahead on own
    
    assist_black_7 = case_when(V241255 %in% 1:7 ~ as.numeric(V241255), TRUE ~ NA_real_),
    # assist_black_7: 1 = Gov should help Black Americans ... 7 = Black Americans should help themselves
    
    fedspend_social_security_5 = case_when(V241263x %in% 1:5 ~ as.numeric(V241263x), TRUE ~ NA_real_),
    fedspend_public_schools_5 = case_when(V241266x %in% 1:5 ~ as.numeric(V241266x), TRUE ~ NA_real_),
    fedspend_border_security_5 = case_when(V241269x %in% 1:5 ~ as.numeric(V241269x), TRUE ~ NA_real_),
    fedspend_crime_5 = case_when(V241272x %in% 1:5 ~ as.numeric(V241272x), TRUE ~ NA_real_),
    fedspend_highways_5 = case_when(V241278x %in% 1:5 ~ as.numeric(V241278x), TRUE ~ NA_real_),
    fedspend_aid_poor_5 = case_when(V241281x %in% 1:5 ~ as.numeric(V241281x), TRUE ~ NA_real_),
    fedspend_environment_5 = case_when(V241284x %in% 1:5 ~ as.numeric(V241284x), TRUE ~ NA_real_),
    # fedspend_x_5: 1 = Increased a lot ... 3 = Kept same ... 5 = Decreased a lot
    
    dei_7 = case_when(V241290x %in% 1:7 ~ as.numeric(V241290x), TRUE ~ NA_real_),
    # dei_7: 1 = Favor a great deal ... 4 = Neither ... 7 = Oppose a great deal
    
    death_penalty_4 = case_when(V241308x %in% 1:4 ~ as.numeric(V241308x), TRUE ~ NA_real_),
    # death_penalty_4: 1 = Favor strongly ... 4 = Oppose strongly
    
    stay_out_foreign_4 = case_when(V241312x %in% 1:4 ~ as.numeric(V241312x), TRUE ~ NA_real_),
    # stay_out_foreign_4: 1 = Agree strongly ... 4 = Disagree strongly
    
    use_force_5 = case_when(V241313 %in% 1:5 ~ as.numeric(V241313), TRUE ~ NA_real_),
    # use_force_5: 1 = Extremely willing ... 5 = Not at all willing
    
    votes_counted_5 = case_when(V241314 %in% 1:5 ~ as.numeric(V241314), TRUE ~ NA_real_),
    # votes_counted_5: 1 = Not at all accurately ... 5 = Completely accurately
    
    voter_id_7 = case_when(V241319x %in% 1:7 ~ as.numeric(V241319x), TRUE ~ NA_real_),
    felons_vote_7 = case_when(V241322x %in% 1:7 ~ as.numeric(V241322x), TRUE ~ NA_real_),
    climate_action_7 = case_when(V241366x %in% 1:7 ~ as.numeric(V241366x), TRUE ~ NA_real_),
    paid_leave_7 = case_when(V241369x %in% 1:7 ~ as.numeric(V241369x), TRUE ~ NA_real_),
    trans_bathroom_7 = case_when(V241372x %in% 1:7 ~ as.numeric(V241372x), TRUE ~ NA_real_),
    trans_sports_7 = case_when(V241375x %in% 1:7 ~ as.numeric(V241375x), TRUE ~ NA_real_),
    gay_job_protect_4 = case_when(V241378x %in% 1:4 ~ as.numeric(V241378x), TRUE ~ NA_real_),
    # gay_job_protect_4: 1 = Favor strongly ... 4 = Oppose strongly
    
    gay_adoption_6 = case_when(V241381x %in% 1:6 ~ as.numeric(V241381x), TRUE ~ NA_real_),
    # gay_adoption_6: 1 = Very strongly permit ... 6 = Very strongly not permit
    
    gay_marriage_7 = case_when(V241385x %in% 1:7 ~ as.numeric(V241385x), TRUE ~ NA_real_),
    birthright_7 = case_when(V241389x %in% 1:7 ~ as.numeric(V241389x), TRUE ~ NA_real_),
    
    immig_children_6 = case_when(V241392x %in% 1:6 ~ as.numeric(V241392x), TRUE ~ NA_real_),
    # immig_children_6: 1 = Great deal send back ... 6 = Great deal allow stay/work
    
    border_wall_7 = case_when(V241395x %in% 1:7 ~ as.numeric(V241395x), TRUE ~ NA_real_),
    
    urban_unrest_7 = case_when(V241397 %in% 1:7 ~ as.numeric(V241397), TRUE ~ NA_real_),
    # urban_unrest_7: 1 = Solve racism/police violence ... 7 = Use all available force
    
    ukraine_aid_7 = case_when(V241400x %in% 1:7 ~ as.numeric(V241400x), TRUE ~ NA_real_),
    israel_aid_7 = case_when(V241403x %in% 1:7 ~ as.numeric(V241403x), TRUE ~ NA_real_),
    palestinians_aid_7 = case_when(V241406x %in% 1:7 ~ as.numeric(V241406x), TRUE ~ NA_real_)
    
  )

# IMPUTATION PREP 
anes_2024_impdata <- anes_2024_subset |>
  mutate(
    across(where(is.character), as.factor),

    party_id_3 = factor(party_id_3),
    party_id_7 = factor(party_id_7, ordered = TRUE),

    gender = factor(gender),
    is_transgender = factor(is_transgender),
    sexual_orientation = factor(sexual_orientation),
    race = factor(race),
    birthplace = factor(birthplace),
    marital_status = factor(marital_status),
    home_tenure = factor(home_tenure),
    occupation = factor(occupation),
    union_status = factor(union_status),
    military = factor(military),
    urban_rural = factor(urban_rural),
    religion = factor(religion),
    religious_attendance = factor(religious_attendance, ordered = TRUE)
  ) |>
  filter(!is.na(party_id_3) | !is.na(party_id_7))

# MULTIPLE IMPUTATION
set.seed(123)

imp <- mice(
  data = anes_2024_impdata,
  m = 5,
  maxit = 10,
  printFlag = TRUE
)

anes_list <- lapply(1:imp$m, function(i) complete(imp, i)) 

# Randomly select ONE imputation for simplicity
imp_pick <- sample(seq_along(anes_list), 1)
anes_model_data <- anes_list[[imp_pick]] # use this variable for modeling!
