if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

# -------------------------------------------------- #
# (1) canton/city data 2018
# -------------------------------------------------- #

ensanut_2018 <- 
  mutate(ensanut_2018,
         
         # canton code
         cantcode2 = as.factor(substr(upm,1,4)), 
         
         # survey date
         esdate = paste(fecha_mes, fecha_dia, fecha_anio, sep = "-"),
         esdate = as.Date(esdate, format = "%m-%d-%Y"),
         
         # indigenous
         indrace_2018 = etnia == 1)

# -------------------------------------------------- #
# (2) canton/city data 2012
# -------------------------------------------------- #

ensanut_2012 <- 
  mutate(ensanut_2012,
         
         # canton code
         cantcode2 = case_when(prov >= 10 ~ substr(ciudad,1,4),
                               prov < 10 ~ paste(0, substr(ciudad,1,3), sep = "")),
         cantcode2 = as.factor(cantcode2),
         
         # indigenous
         indrace_2012 = gr_etn == 1)


# -------------------------------------------------- #
# (3) outcome variables 2018
# -------------------------------------------------- #

ensanut_2018 <- 
  mutate(ensanut_2018,
         
         # had health problem in past 30 days
         hproblem_2018 = f1_s4_2 == 1,
         
         # hospitalization in past 12 months
         hosp_2018 = f1_s4_54 == 1,
         
         # received care to address health problems in past 30 days
         gotcare_2018 = case_when(is.na(f1_s4_6) ~ NA,
                                 f1_s4_6 %in% c(1,2,4) ~ TRUE, 
                                 TRUE ~ FALSE),
         
         # received preventive care services in past 30 days
         prev_2018 = f1_s4_41 == 1,
         
         # health status perception
         hpercep_2018 = case_when(f1_s4_58 == 1 ~ 5, f1_s4_58 == 2 ~ 4,
                                 f1_s4_58 == 4 ~ 2, f1_s4_58 == 5 ~ 1, TRUE ~ 3),
         
         # health status perception dummy
         hpercepd_2018 = hpercep_2018 %in% c(4,5))


pmb_data_2018 <- ensanut_2018 %>% group_by(cantcode2) %>% 
  summarize(esdate = median(as.numeric(esdate), na.rm = TRUE),
            n_2018 = n(),
            hproblem_2018 = weighted.mean(hproblem_2018, w = fexp, na.rm = TRUE),
            hosp_2018 = weighted.mean(hosp_2018, w = fexp, na.rm = TRUE),
            gotcare_2018 = weighted.mean(gotcare_2018, w = fexp, na.rm = TRUE),
            prev_2018 = weighted.mean(prev_2018, w = fexp, na.rm = TRUE),
            hpercepd_2018 = weighted.mean(hpercepd_2018, w = fexp, na.rm = TRUE), 
            indrace_2018 = weighted.mean(indrace_2018, w = fexp, na.rm = TRUE))

# cantons with high indigenous concentration
pmb_data_2018$ind_2018 <- pmb_data_2018$indrace_2018 > mean(pmb_data_2018$indrace_2018, na.rm = TRUE)

# -------------------------------------------------- #
# (1) outcome variables 2012
# -------------------------------------------------- #

ensanut_2012 <- 
  mutate(ensanut_2012,
         
         # had health problem in past 30 days
         hproblem_2012 = ps02 == 1,
         
         # hospitalization in past 12 months
         hosp_2012 = ps55 == 1,
       
         # received care to address health problems in past 30 days
         gotcare_2012 = case_when(is.na(ps06) ~ NA,
                                 ps06 %in% c(1,2,4) ~ TRUE, 
                                 TRUE ~ FALSE),
       
         # received preventive care services in past 30 days
         prev_2012 = ps40 == 1,
         
         # health status perception
         hpercep_2012 = case_when(ps71 == 1 ~ 5, ps71 == 2 ~ 4,
                                 ps71 == 4 ~ 2, ps71 == 5 ~ 1, TRUE ~ 3),
         
         # health status perception binary
         hpercepd_2012 = hpercep_2012 %in% c(4,5))


pmb_data_2012 <- ensanut_2012 %>% group_by(cantcode2) %>% 
  summarize(n_2012 = n(),
            hproblem_2012 = weighted.mean(hproblem_2012, w = pw, na.rm = TRUE),
            hosp_2012 = weighted.mean(hosp_2012, w = pw, na.rm = TRUE),
            gotcare_2012 = weighted.mean(gotcare_2012, w = pw, na.rm = TRUE),
            prev_2012 = weighted.mean(prev_2012, w = pw, na.rm = TRUE),
            hpercepd_2012 = weighted.mean(hpercepd_2012, w = pw, na.rm = TRUE), 
            indrace_2012 = weighted.mean(indrace_2012, w = pw, na.rm = TRUE))

# cantons with high indigenous concentration
pmb_data_2012$ind_2012 <- pmb_data_2012$indrace_2012 > mean(pmb_data_2012$indrace_2012, na.rm = TRUE)

# -------------------------------------------------- #
# (1) PMB dates
# -------------------------------------------------- #

pmb_cantons_date <- 
  mutate(pmb_cantons_date,
         
         # canton code
         cantcode2 = case_when(province_code < 10 & canton_code < 10 ~ 
                                 paste(0, province_code, 0, canton_code, sep = ""),
                               province_code < 10 & canton_code >= 10 ~
                                 paste(0, province_code, canton_code, sep = ""),
                               province_code >= 10 & canton_code < 10 ~
                                 paste(province_code, 0, canton_code, sep = ""),
                               province_code >= 10 & canton_code >= 10 ~
                                 paste(province_code, canton_code, sep = "")),
         
         # PMB start date
         pmb_start_date = as.Date(pmb_start_date, format = "%m/%d/%Y"),
         pmb_start_date = as.integer(pmb_start_date)) %>% 
  select(cantcode2, pmb_start_date)

# removes duplicates
pmb_cantons_date <- pmb_cantons_date[!duplicated(pmb_cantons_date),]

# -------------------------------------------------- #
# (4) treatment variables
# -------------------------------------------------- #

pmb_data <- left_join(pmb_data_2018, pmb_data_2012, by = "cantcode2") %>%
  left_join(pmb_cantons_date, by = "cantcode2")

# number of weeks a given canton was receiving the MBP program before the survey
pmb_data$weeks_ensanut18 <- (pmb_data$esdate - pmb_data$pmb_start_date) / (365/52)

# PMB dummy
pmb_data$pmb <- pmb_data$weeks_ensanut18 > 0
mean(pmb_data$pmb, na.rm = TRUE) # percentage of treatment cantons

# maximum number of covered weeks by canton
# PMB program officially started on August 30 2017
weeks_possible_ensanut18 <- (max(pmb_data$esdate) - as.integer(as.Date("2017-08-30"))) / (365/52)

pmb_data$pweekscov <- pmb_data$weeks_ensanut18 / weeks_possible_ensanut18
pmb_data$pweekscov <- ifelse(pmb_data$pweekscov < 0, 0, pmb_data$pweekscov)

pmb_data <- pmb_data[complete.cases(pmb_data), ] # remove incomplete cases

# final dataset
pmb_data <- pmb_data %>% gather("var", "value", n_2018: ind_2012, convert = TRUE) %>%
  separate(var, c("var", "year")) %>% spread(var, value)

# post variable
pmb_data$post <- pmb_data$year == 2018

saveRDS(pmb_data, "data/pmb_data.rds")
