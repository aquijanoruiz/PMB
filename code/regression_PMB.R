if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lfe)) install.packages("lfe", repos = "http://cran.us.r-project.org")
if(!require(AER)) install.packages("AER", repos = "http://cran.us.r-project.org")

pmb_data <- readRDS("data/pmb_data.rds")

# summary statistics
pmb_data %>% select(cantcode2, esdate, pmb_start_date, hproblem, gotcare, 
                    hosp, hpercepd, pmb, pweekscov, post) %>% summary()

# -------------------------------------------------- #
# (1) First DID specification model
# -------------------------------------------------- #

# DID means (hproblem)
D <- pmb_data %>% group_by(post, pmb) %>% summarize(hproblem = mean(hproblem, na.rm = TRUE))

# ATT hproblem
(D$hproblem[D$pmb & D$post] - D$hproblem[D$pmb & !D$post]) - 
  (D$hproblem[!D$pmb & D$post] - D$hproblem[!D$pmb & !D$post])

# DID model hproblem
model1 <- lm(hproblem ~ pmb + pmb * post + post + cantcode2, data = pmb_data)
model1 <- coeftest(model1, vcov = vcovCL, cluster = ~cantcode2)
model1[which(!startsWith(row.names(model1), 'cantcode2')), ]

# DID model hosp
model2 <- lm(hosp ~ pmb + pmb * post + post + cantcode2, data = pmb_data)
model2 <- coeftest(model2, vcov = vcovCL, cluster = ~cantcode2)
model2[which(!startsWith(row.names(model2), 'cantcode2')), ]

# DID model gotcare
model3 <- lm(hosp ~ pmb + pmb * post + post + cantcode2, data = pmb_data)
model3 <- coeftest(model3, vcov = vcovCL, cluster = ~cantcode2)
model3[which(!startsWith(row.names(model3), 'cantcode2')), ]

# DID model hpercepd
model4 <- lm(hpercepd ~ pmb + pmb * post + post + cantcode2, data = pmb_data)
model4 <- coeftest(model4, vcov = vcovCL, cluster = ~cantcode2)
model4[which(!startsWith(row.names(model4), 'cantcode2')), ]

# -------------------------------------------------- #
# (2) Second DID specification model
# -------------------------------------------------- #

# DID model hproblem vs pweekscov
model11 <- lm(hproblem ~ pweekscov + pweekscov * post + post + cantcode2, data = pmb_data)
model11 <- coeftest(model11, vcov = vcovCL, cluster = ~cantcode2)
model11[which(!startsWith(row.names(model11), 'cantcode2')), ]

# DID model hosp vs pweekscov
model22 <- lm(hosp ~ pweekscov + pweekscov * post + post + cantcode2, data = pmb_data)
model22 <- coeftest(model22, vcov = vcovCL, cluster = ~cantcode2)
model22[which(!startsWith(row.names(model22), 'cantcode2')), ]

# DID model gotcare vs pweekscov
model33 <- lm(gotcare ~ pweekscov + pweekscov * post + post + cantcode2, data = pmb_data)
model33 <- coeftest(model33, vcov = vcovCL, cluster = ~cantcode2)
model33[which(!startsWith(row.names(model33), 'cantcode2')), ]

# DID model hpercepd vs pweekscov
model44 <- lm(hpercepd ~ pweekscov + pweekscov * post + post + cantcode2, data = pmb_data)
model44 <- coeftest(model44, vcov = vcovCL, cluster = ~cantcode2)
model44[which(!startsWith(row.names(model44), 'cantcode2')), ]
