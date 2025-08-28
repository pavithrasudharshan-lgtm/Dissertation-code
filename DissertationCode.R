# install packages 
install.packages("haven")
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("modelsummary")
install.packages("car")
install.packages("poLCA")
install.packages("brant")

# Load the package
library(haven)
library(readr)
library(modelsummary)
library(dplyr)
library(poLCA)
library(ggplot2)
library(car)
library (brant)
library(broom) # to generate p values from plor
library(tidyr)

# Import the data set and create a new dataset (dissert_data) with the needed variables
HAGISData <- read_csv("HAGISDataset.csv",show_col_types = FALSE)
dissert_data <- HAGISData %>%
  dplyr::select (
    UID, DEM_Age_Band,DEM_Gender,DEM_Ethnicity,DEM_Education,DEM_Employment,DEM_Religion,DEM_SexIdentity,EA_IncomeMonthly, #Demographics
    WB_ThingsWorthwhile,WB_BounceBack, WB_HardTimeStress, WB_NotLongRecover, WB_HardSnapBack,WB_LittleTroubHardT,WB_LongT_GetOver,WB_BeyondControl,WB_EnoughT,WB_EXPLive,WB_EXPLiveYou,#Well-being variables
    COV_ShieldList,COV_Shielding, #Risk proxies 
    SOC_Partner,SOC_PartnerClose,SOC_PartnerYouFeel,SOC_PartnerRelyOn,SOC_PartnerOpenup,SOC_ChildRelyOn,SOC_ChildOpenup, SOC_FMRelyOn,SS_illHelp,SS_illHelpWho_1,SS_illHelpWho_2,SS_illHelpWho_3,SS_illHelpWho_4,SS_illHelpWho_5,SS_illHelpWho_6,SS_illHelpWho_7,SS_illHelpWho_8,SS_illHelpWho_9,SS_illHelpWho_99,SS_LackCompanion,SS_LeftOut,SS_FeelIsolated,SS_FeelLonely,SS_FeelDepressed,SS_SenseEmptiness,SS_MissPeople,SS_FeelRejected,SS_PeopleRelyOn,SS_PeopleTrust,SS_PeopleClose, #loneliness and social connectedness variables
    INTUse_Activities_3, INTUse_Activities_9,INTUse_Activities_11,INTUse_Activities_17,INTUse_Activities_18,INTUse_Activities_19, #Internet & information
    HE_Exercise,HE_PortionFruit,HE_EverSmoked,HE_TimesAlcohol,HE_UnitsAlcohol,HEWR_GenHealth,HEWR_HaveHECondition, #General health 
    HE_CancerStatus_1,HE_CancerStatus_2,HE_CancerStatus_3,HE_CancerStatus_4,HE_CancerStatus_5,HE_HECondition_2,HEWR_GetCancer, #General cancer status
    HEB_MSCPreCOVStool,HEB_FSCPreCOVStool,HEB_MSCUptakeStool,HEB_FSCUptakeStool,HEBS_NoSC_For1_1,HEBS_NoSC_For1_2,HEBS_NoSC_For1_5,HEBS_NoSC_For1_6,HEBS_NoSC_For1_7,HEBS_NoSC_For1_8,HEBS_NoSC_For1_98,HEBS_NoSC_For1_99 #Bowel cancer screening
  )

# Identify rows where ALL values (excluding UID) are either 247, 248, 249 or 250 and filter out those rows
only_240codes <- dissert_data %>%
  dplyr::select(-UID) %>%      # exclude UID 
  apply(1, function(row) all(row %in% c(247, 248, 249, 250)))  # check row-wise

dissert_data <- dissert_data[!only_240codes, ]

# RE-CODING
# Past screening - Combine male and female pre-covid screening as one variable
dissert_data <- dissert_data %>%
  mutate(
    PastScreening = coalesce(
      na_if(HEB_MSCPreCOVStool, 249),
      na_if(HEB_FSCPreCOVStool, 249)
    )
  ) %>%
  dplyr::select(-HEB_MSCPreCOVStool, -HEB_FSCPreCOVStool)

table(dissert_data$PastScreening)

# Making screening test a binary variable - either test has been done before or never before
dissert_data <- dissert_data %>%
  mutate(ScreeningTest_binary = case_when(
    PastScreening %in% c(1, 2) ~ 1,
    PastScreening %in% c(3, 4) ~ 0,
    TRUE ~ NA_real_
  ))

# Future Screening - Combine male and female future screening uptake intention as one variable
dissert_data <- dissert_data %>%
  mutate(
    FutureScreening = coalesce(
      na_if(HEB_MSCUptakeStool, 249),
      na_if(HEB_FSCUptakeStool, 249)
    )
  ) %>%
  dplyr::select(-HEB_MSCUptakeStool, -HEB_FSCUptakeStool)

# Make vairables as factors
dissert_data <- dissert_data %>%
  mutate(
    DEM_Gender = factor(DEM_Gender,
                        levels = c(1, 2),
                        labels = c("Male", "Female")),
    SOC_PartnerRelyOn = factor(SOC_PartnerRelyOn,
                               levels = c(1, 2, 3, 4),  
                               labels = c("A lot", "Some", "A little", "Not at all")),
    SS_illHelp = factor(SS_illHelp,
                        levels = c(1, 2),       
                        labels = c("Yes", "No"))
  )

# DESCRIPTIVE ANALYSIS 
# 1. OUTCOME VARIABLES
# 1.1 Past screening frequency table and bar chart 
screening_table <- table(factor(dissert_data$PastScreening, levels = 1:4,
                                labels = c("Yes, once", "Yes, more than once", "No, never", "Don’t know")))
print(screening_table)
round(prop.table(screening_table) * 100, 1)

df <- as.data.frame(screening_table)
colnames(df) <- c("Response", "Freq")

PastScreening_barchart <- ggplot(df, aes(x = Response, y = Freq, fill = Response)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Participation in NHS Bowel Screening Test",
    x = "Response",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c(
    "Yes, once" = "mediumpurple4",
    "Yes, more than once" = "mediumpurple3",
    "No, never" = "mediumpurple2",
    "Don’t know" = "mediumpurple1"
  ))
print(PastScreening_barchart)
ggsave("Screening_BarChart.png", plot = PastScreening_barchart, width = 7, height = 5, dpi = 300)

# 1.2 Future screening table
futurescreening_table <- table(factor(dissert_data$FutureScreening, levels = 1:5,
                                      labels = c("Very likely", "Likely", "Neutral", "Not likely", "Not at all likely")))
round(prop.table(futurescreening_table) * 100, 1)

# 2 DEMOGRAPHIC, HEALTH RELATED AND PSYCHOSOCIAL VARIABLES
# 2.1 Gender 
round(prop.table(table(factor(dissert_data$DEM_Gender, levels = c(1,2,3), labels = c("Male", "Female", "Other")))) * 100, 1)

# 2.2 Education 
ggplot(
  dissert_data %>% 
    filter(DEM_Education %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) %>% 
    mutate(Education = factor(DEM_Education, 
                              levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                              labels = c("Some primary not complete", "Primary or equivalent", "O level / O grade or equivalent", "Highers or equivalent", "Sixth year studies or equivalent", "Apprenticeship", "HNC / HND or equivalent", "First degree", "Postgraduate / higher degree", "None"))),
  aes(x = Education)
) +
  geom_bar(fill = "grey60") +
  labs(title = "Education Distribution", x = "Education", y = "Count") +
  theme_minimal()

# 2.3 Ethnicity - checking Scottish proportion
ethnicity_labels <- c(
  "Scottish",
  "British",
  "Irish",
  "Gypsy",
  "Polish",
  "Other white ethnic",
  "Mixed ethnic",
  "Pakistani",
  "Indian",
  "Bangladeshi",
  "Chinese",
  "Other Asian",
  "African",
  "Other African",
  "Caribbean",
  "Black",
  "Other Caribbean or Black",
  "Arab",
  "Other ethnic group"
)
table(
  factor(dissert_data$DEM_Ethnicity, levels = 1:19, labels = ethnicity_labels)
)
round(prop.table(table(factor(dissert_data$DEM_Ethnicity, levels = 1:19, labels = ethnicity_labels))) * 100, 1)

# 2.4 Age 
ageband_label <- c(
  "50-54",
  "55-59",
  "60-64",
  "65-69",
  "70-74",
  "75 plus"
)
age_counts <- table(
  factor(dissert_data$DEM_Age_Band, levels = 1:6, labels = ageband_label)
)
round(prop.table(age_counts) * 100, 1)

# 2.6 Worry about getting cancer
round(prop.table(table(factor(dissert_data$HEWR_GetCancer, levels = 1:4, 
                              labels = c("Never", "Sometimes", "Often", "Very often")))) * 100, 1)

# 2.7 - Worry about general health
round(prop.table(table(factor(dissert_data$HEWR_GenHealth, levels = 1:4, 
                              labels = c("Never", "Sometimes", "Often", "Very often")))) * 100, 1)

# 2.8 - Health condition worried about most
round(prop.table(table(factor(dissert_data$HEWR_HaveHECondition, levels = 1:6, 
                              labels = c("COVID-19", "Cancer", "Diabetes", "Dementia/Alzheimer's", "Heart disease", "Stroke")))) * 100, 1)

# 2.9 - Family member (2) or close friend (3) had cancer
round(prop.table(table(factor(dissert_data$HE_CancerStatus_2, levels = 0:1, labels = c("No", "Yes")))) * 100, 1)
round(prop.table(table(factor(dissert_data$HE_CancerStatus_3, levels = 0:1, labels = c("No", "Yes")))) * 100, 1)

# 2.10 - Psychological variables 
round(prop.table(table(factor(dissert_data$WB_LongT_GetOver, levels = 1:5, 
                              labels = c("Strongly disagree", "Disagree", "Neither", "Agree", "Strongly agree")))) * 100, 1)
round(prop.table(table(factor(dissert_data$WB_BounceBack, levels = 1:5, 
                              labels = c("Strongly disagree", "Disagree", "Neither", "Agree", "Strongly agree")))) * 100, 1)
round(prop.table(table(factor(dissert_data$SOC_PartnerRelyOn, levels = 1:4, 
                              labels = c("A lot", "Some", "A little", "Not at all")))) * 100, 1)
round(prop.table(table(factor(dissert_data$SS_illHelp, levels = 1:2, labels = c("Yes", "No")))) * 100, 1)

# 2.11 - No screen variables
# Defining variables and labels
nosc_vars <- c("HEBS_NoSC_For1_1", "HEBS_NoSC_For1_2", "HEBS_NoSC_For1_5",
               "HEBS_NoSC_For1_6", "HEBS_NoSC_For1_7", "HEBS_NoSC_For1_8", "HEBS_NoSC_For1_99")

nosc_labels <- c(
  "Prefer not to know",
  "Test is too difficult",
  "Test is not accurate",
  "Feel well, not needed",
  "Other health concerns",
  "Under regular review",
  "Prefer not to say"
)

# Counts the 1s using colSums
nosc_counts <- colSums(
  sapply(dissert_data[nosc_vars], as.numeric) == 1,  
  na.rm = TRUE
)

# Create & print table
nosc_summary <- data.frame(
  Reason = nosc_labels,
  Yes_Count = nosc_counts,
  row.names = NULL
)
print(nosc_summary <- nosc_summary %>%
        arrange(desc(Yes_Count)))

# 3 - INTERACTION OF VARIABLES
# 3.1 - Past screening & gender 
screening_gender <- table(
  factor(dissert_data$PastScreening, levels = 1:4, labels = c("Yes, once", "Yes, more than once", "No, never", "Don't know")),
  factor(dissert_data$DEM_Gender, levels = 1:2, labels = c("Male", "Female"))
)

print(screening_gender)

# 3.2 - Past screening & worry of getting cancer 
worry_screening <- dissert_data %>%
  filter(
    HEWR_GetCancer %in% 1:4,
    PastScreening %in% 1:4
  )

screeningxworry <- table(
  factor(worry_screening$HEWR_GetCancer, levels = 1:4,
         labels = c("Never", "Sometimes", "Often", "Very often")),
  factor(worry_screening$PastScreening, levels = 1:4,
         labels = c("Yes, once", "Yes, more than once", "No, never", "Don’t know"))
)
print(screeningxworry)

# 3.3 - Past screening & future screening
# Past screening (binary) & future screening intentions 
future_int <- dissert_data %>%
  filter(
    ScreeningTest_binary %in% 0:1,
    FutureScreening %in% 1:5
  )

screeningxintent <- table(
  factor(future_int$FutureScreening, levels = 1:5,
         labels = c("Very likely", "Likely", "Neutral", "Not likely", "Not at all likely")),
  factor(future_int$ScreeningTest_binary, levels = 0:1,
         labels = c("Never screened", "Ever screened"))
)
print(screeningxintent)

# Analysis 1.1 - Past screening status vs. demographics, psycho-social and health worry variables
# Set the reference categories
dissert_data$DEM_Gender <- relevel(dissert_data$DEM_Gender, ref = "Male")
dissert_data$SOC_PartnerRelyOn <- relevel(dissert_data$SOC_PartnerRelyOn, ref = "A lot")
dissert_data$SS_illHelp <- relevel(dissert_data$SS_illHelp, ref = "Yes")

# Model 1 - Demographic variables
model1 <- glm(ScreeningTest_binary ~ DEM_Age_Band + DEM_Gender+ EA_IncomeMonthly,
              data = dissert_data, family = binomial)

# Model 2 - Psycho-social variables 
model2 <- glm(ScreeningTest_binary ~ DEM_Age_Band + DEM_Gender + EA_IncomeMonthly +
                WB_BounceBack + WB_HardTimeStress + 
                WB_EXPLive + WB_EXPLiveYou + 
                SOC_PartnerRelyOn + SS_illHelp,
              data = dissert_data, family = binomial)

# Model 3 - Health worry variables 
model3 <- glm(ScreeningTest_binary ~ DEM_Age_Band + DEM_Gender + EA_IncomeMonthly +
                WB_BounceBack + WB_HardTimeStress + 
                WB_EXPLive + WB_EXPLiveYou + 
                SOC_PartnerRelyOn + SS_illHelp +
                HEWR_GenHealth + HEWR_GetCancer,
              data = dissert_data, family = binomial)

summary(model1)
summary(model2)
summary(model3)

# Output table
model1to3 <- list(
  "Model 1 (Demographics)" = model1,
  "Model 2 (+ Psychosocial)" = model2,
  "Model 3 (+ Health worry)" = model3
)

# To calculate N for each model 
nobs(model1) 
nobs(model2)
nobs(model3)

# Output table 
modelsummary(
  model1to3,
  exponentiate = TRUE,
  estimate = "{estimate}{stars} [{conf.low}, {conf.high}]",     # show OR & 95% CI
  statistic = NULL,                                             # to not show SEs
  conf_level = 0.95,
  gof_map = data.frame(                                         # GOF
    raw   = c("n","aic","bic","logLik"),
    clean = c("N","AIC","BIC","Log-Lik."),
    fmt   = c(0,1,1,2)
  ),
  stars = c("+" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001),
  fmt = 2,                                                       # 2 decimal points
  notes = "Entries are odds ratios with 95% confidence intervals; stars denote p-values.",
  output = "markdown" 
)

# ANALYSIS 1.3 - testing if people who worry more about getting cancer have higher screening rates
table(dissert_data$HEWR_GetCancer, dissert_data$ScreeningTest_binary)

dissert_data %>%
  filter(HEWR_GetCancer %in% c(1, 2, 3, 4)) %>%  # exclude 248, 249
  ggplot(aes(x = factor(HEWR_GetCancer, labels = c("Never", "Sometimes", "Often", "Very often")),
             fill = factor(ScreeningTest_binary, labels = c("Not screened", "Screened")))) +
  scale_fill_manual(values = c("Not screened" = "#cbc9e2",  
                               "Screened" = "#9e9ac8")) + 
  geom_bar(position = "fill") +  # proportion bars
  labs(x = "Worry about getting cancer", y = "Proportion", fill = "Screening Status",
       title = "Proportion screened by cancer worry level") +
  theme_minimal()

model_worry_screenstatus <- glm(ScreeningTest_binary ~ HEWR_GetCancer, data = dissert_data, family = binomial)
summary(model_worry_screenstatus) 

# ANALYSIS 2.1 - Do people who live healthier think they’ll live longer?
issert_data <- dissert_data %>%
  # Replace codes like -8, -7, 247, 248, 249, etc. with NA across selected variables
  mutate(
    WB_EXPLive = ifelse(WB_EXPLive %in% c(-8, -7, 247, 248, 249, 250), NA, WB_EXPLive),
    WB_EXPLiveYou = ifelse(WB_EXPLiveYou %in% c(-8, -7, 247, 248, 249, 250), NA, WB_EXPLiveYou),
    HE_Exercise = ifelse(HE_Exercise %in% c(-8, -7, 247, 248, 249, 250), NA, HE_Exercise),
    HE_PortionFruit = ifelse(HE_PortionFruit %in% c(-8, -7, 247, 248, 249,250), NA, HE_PortionFruit),
    HE_TimesAlcohol = ifelse(HE_TimesAlcohol %in% c(-8, -7, 247, 248, 249, 250), NA, HE_TimesAlcohol)
  )

# Linear regression for WB_EXPLiveYou (expected lifespan of self)
model_expliveyou <- lm(WB_EXPLiveYou ~ HE_Exercise + HE_PortionFruit + HE_TimesAlcohol, data = dissert_data)
summary(model_expliveyou) 

# Does screening status act as a mediating factor
model_med <- lm(WB_EXPLiveYou ~ HE_Exercise + HE_PortionFruit + HE_TimesAlcohol + ScreeningTest_binary, data = dissert_data)
summary(model_med)

# ANALYSIS 2.2 - not signficant - Do healthier people beleive they will outlive others? 
dissert_data <- dissert_data %>%
  mutate(Overconfidence = WB_EXPLiveYou - WB_EXPLive)

model_conf <- lm(Overconfidence ~ HE_Exercise + HE_PortionFruit + HE_TimesAlcohol, data = dissert_data)
summary(model_conf)

# ANALYSIS 2.3 - not significant - Do health behaviors predict worry about general health and cancer
clean_data <- dissert_data %>%
  filter(HEWR_GenHealth %in% 1:4, # include only valid worry values
         HEWR_GetCancer %in% 1:4)  # include only valid worry values  

model31 <- lm(HEWR_GenHealth ~ HE_Exercise + HE_PortionFruit + HE_UnitsAlcohol, data = clean_data)
summary(model31)

model31C <- lm(HEWR_GetCancer ~ HE_Exercise + HE_PortionFruit + HE_UnitsAlcohol, data = clean_data)
summary(model31C)

# ANALYSIS 4.1 - future screening 
library(MASS)
# Clean future screening outcome
dissert_data <- dissert_data %>%
  filter(FutureScreening %in% 1:5) %>%
  mutate(FutureScreening = ordered(FutureScreening, levels = 1:5))
dissert_data <- dissert_data %>%
  mutate(
    DEM_Gender = droplevels(DEM_Gender),
    SOC_PartnerRelyOn = droplevels(SOC_PartnerRelyOn),
    SS_illHelp = droplevels(SS_illHelp)
  )

# Full ordinal model with polr
full_futurescreen_model <- polr(
 FutureScreening ~ DEM_Age_Band + DEM_Gender + EA_IncomeMonthly +
    WB_BounceBack + WB_HardTimeStress +
    WB_EXPLive + WB_EXPLiveYou +
    SOC_PartnerRelyOn + SS_illHelp +
    HEWR_GenHealth + HEWR_GetCancer,
  data = dissert_data,
  Hess = TRUE,
)
brant(full_futurescreen_model)                                 # checking PO assumption to see if multinomial logistic regression should be run
summary(full_futurescreen_model)                                 # regression coefficients, SEs, z values
tidy_full <- tidy(full_futurescreen_model, conf.int = TRUE) %>%  # tidy the model and get p-values
  filter(coef.type == "coefficient") %>%   # drops cutpoints
  mutate(
    p.value = 2 * (1 - pnorm(abs(statistic))),  # calculate p
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ "+",
      TRUE ~ ""
    )
  )
print(tidy_full)

# testing collinearity to see why health worry's significance reduced in the complex model
vif(lm(as.numeric(FutureScreening) ~ DEM_Age_Band + DEM_Gender + 
         EA_IncomeMonthly + WB_BounceBack + WB_HardTimeStress +
         WB_EXPLive + WB_EXPLiveYou + 
         SOC_PartnerRelyOn + SS_illHelp + HEWR_GenHealth + HEWR_GetCancer, 
       data = dissert_data))

# Reduced polr model 
simplified_futurescreen_model <- polr(
  FutureScreening ~ DEM_Gender + EA_IncomeMonthly +
    SOC_PartnerRelyOn + SS_illHelp, 
  data = dissert_data, 
  Hess = TRUE
)
brant(simplified_futurescreen_model)                                         # checking PO assumption to see if multinomial logistic regression should be run
summary(simplified_futurescreen_model)                                       # regression coefficients, SEs, z values
tidy_simplified <- tidy(simplified_futurescreen_model, conf.int = TRUE) %>%  # tidy the model and get p-values
  filter(coef.type == "coefficient") %>%   
  mutate(
    p.value = 2 * (1 - pnorm(abs(statistic))),  
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ "+",
      TRUE ~ ""
    )
  )
print(tidy_simplified)

# Do people who both worry about cancer and struggle to recover have the highest screening intention?
interaction_data <- dissert_data %>%
  filter(FutureScreening %in% 1:5) %>%
  mutate(
    WB_LongT_GetOver = ifelse(WB_LongT_GetOver %in% c(247, 248, 249, 250), NA, WB_LongT_GetOver),
    HEWR_GetCancer = ifelse(HEWR_GetCancer %in% c(247, 248, 249, 250), NA, HEWR_GetCancer)
  ) %>%
  drop_na(WB_LongT_GetOver, HEWR_GetCancer)
interaction_model <- polr(
  FutureScreening ~ WB_LongT_GetOver * HEWR_GetCancer,
  data = interaction_data, Hess = TRUE
)
summary(interaction_model)

# Regression tables of both full and reduced models
future_models <- list(
  "Full model" = full_futurescreen_model,
  "Reduced model" = simplified_futurescreen_model
)

modelsummary(
  future_models,
  exponentiate = TRUE,                                   # ORs instead of log-odds
  estimate = "{estimate}{stars} [{conf.low}, {conf.high}]",     # OR with 95% CI
  conf_level = 0.95,
  stars = TRUE,
  gof_map = data.frame(
    raw   = c("n","aic","bic","logLik"),
    clean = c("N","AIC","BIC","Log-Lik."),
    fmt   = c(0,1,1,2)
  ),
  fmt = 2,
  notes = "Entries are odds ratios with 95% confidence intervals; stars denote p-values.",
  output = "markdown"  
)

# Does worrying most about cancer predict future or past screening
dissert_data <- dissert_data %>%
  mutate (
    worriesmostcancer = case_when(
      HEWR_HaveHECondition == 2 ~ 1,  # cancer code 2
      HEWR_HaveHECondition %in% c(1,3,4,5,6) ~ 0, #other illness codes
      TRUE ~ NA_real_  # Exclude invalids like 247, 248, 250
    )
  )

model_past_cancer_worry <- glm(
  ScreeningTest_binary ~ worriesmostcancer,
  data = dissert_data,
  family = binomial
)
summary(model_past_cancer_worry)

model_future_cancer_worry <- polr(
  as.factor(FutureScreening) ~ worriesmostcancer,
  data = dissert_data %>% filter(FutureScreening %in% 1:5),
  Hess = TRUE
)

summary(model_future_cancer_worry)

# 4.2 - Status quo bias - past vs. future screening 
statusquo_data <- dissert_data %>%
  filter(ScreeningTest_binary %in% 0:1, FutureScreening %in% 1:5)

model_statusquo <- polr(
  as.factor(FutureScreening) ~ as.factor(ScreeningTest_binary),
  data = statusquo_data,
  Hess = TRUE
)
summary(model_statusquo)

# ANALYSIS 4.3 - failure model - Prioritization of other illnesses and fatalism
table(dissert_data$HEWR_HaveHECondition) 

fatalism_data <- dissert_data %>%
  filter(
    FutureScreening %in% 1:5,
    HEWR_HaveHECondition %in% 1:9  
  )
fatalism_data$HEWR_HaveHECondition <- factor(fatalism_data$HEWR_HaveHECondition)
fatalism_data$HEWR_HaveHECondition <- relevel(fatalism_data$HEWR_HaveHECondition, ref = "2")

fatalismfuture_model <- polr(as.factor(FutureScreening) ~ as.factor(HEWR_HaveHECondition), 
                             data = fatalism_data, Hess = TRUE)
summary(fatalismfuture_model)

# ANALYSIS 5 - Latent Class Analysis - 4 trial and errors are run - 5.3 trial was the final model that was reported

# 5.1 - trial and error 1 (variables: cancer worry, past screening, future screening)
# recode variables 
lca_data <- dissert_data %>%
  mutate(
    # cancer worry: 1 low, 2 moderate, 3 high
    HEWR_GetCancer_rec = case_when(
      HEWR_GetCancer == 1 ~ 1,
      HEWR_GetCancer == 2 ~ 2,
      HEWR_GetCancer %in% c(3,4) ~ 3,
      TRUE ~ NA_real_
    ),
    # bounce back: 1 low, 2 neutral, 3 high
    WB_BounceBack_rec = case_when(
      WB_BounceBack %in% c(1,2) ~ 1,
      WB_BounceBack == 3 ~ 2,
      WB_BounceBack %in% c(4,5) ~ 3,
      TRUE ~ NA_real_
    ),
    # future intention: 1 likely, 2 unsure, 3 unlikely
    FutureScreening_rec = case_when(
      FutureScreening %in% c(1,2) ~ 1,
      FutureScreening == 3 ~ 2,
      FutureScreening %in% c(4,5) ~ 3,
      TRUE ~ NA_real_
    ),
    # past screening binary to 1/2 (poLCA wants 1 and above, not 0/1)
    ScreeningTest_bin12 = case_when(
      ScreeningTest_binary == 0 ~ 1,
      ScreeningTest_binary == 1 ~ 2,
      TRUE ~ NA_real_
    )
  )
set.seed(123)
lca_model_3class <- poLCA(cbind(HEWR_GetCancer_rec, ScreeningTest_bin12, FutureScreening_rec) ~ 1, 
                          data = lca_data, nclass = 3, na.rm = FALSE, maxiter = 1000)
lca_model_3class$P
lca_model_3class$numiter

set.seed(123)
lca_model_2class <- poLCA(cbind(HEWR_GetCancer_rec, ScreeningTest_bin12, FutureScreening_rec) ~ 1, 
                            data = lca_data, nclass = 2, na.rm = FALSE, maxiter = 1000)
lca_model_2class$P
lca_model_2class$numiter

set.seed(123)
lca_model_4class <- poLCA(cbind(HEWR_GetCancer_rec, ScreeningTest_bin12, FutureScreening_rec) ~ 1, 
                          data = lca_data, nclass = 4, na.rm = FALSE, maxiter = 1000)
lca_model_4class$P
lca_model_4class$numiter

barplot(lca_model_2class$P,
        names.arg = paste("Class", 1:2),
        main = "Latent Class Sizes (2-class model)",
        ylab = "Proportion",
        col = "lightblue")

barplot(lca_model_3class$P,
        names.arg = paste("Class", 1:3),
        main = "Latent Class Sizes (3-class model)",
        ylab = "Proportion",
        col = "violet")

lca_model_3class$probs

# 5.2 - trial and error 2 (variables: cancer worry, bounce back after a setback, people to rely on when ill, past screening status)
dissert_data <- dissert_data %>%
  mutate(
    HEWR_GetCancer_rec = case_when(
      HEWR_GetCancer == 1 ~ 1,                # Low worry
      HEWR_GetCancer == 2 ~ 2,                # Moderate worry
      HEWR_GetCancer %in% c(3,4) ~ 3,         # High worry
      TRUE ~ NA_real_
    ),
    
    # Bounce back (1 Strongly disagree – 5 Strongly agree)
    WB_BounceBack_rec = case_when(
      WB_BounceBack %in% c(1,2) ~ 1,          # Low resilience
      WB_BounceBack == 3 ~ 2,                 # Neutral
      WB_BounceBack %in% c(4,5) ~ 3,          # High resilience
      TRUE ~ NA_real_
    ),
    
    # Future screening intention (1 Very likely – 5 Not at all likely)
    FutureScreening_rec = case_when(
      FutureScreening %in% c(1,2) ~ 1,        # Likely
      FutureScreening == 3 ~ 2,               # Unsure
      FutureScreening %in% c(4,5) ~ 3,        # Unlikely
      TRUE ~ NA_real_
    )
  )

lca_data <- dissert_data %>%
  dplyr::select(
    HEWR_GetCancer_rec,     # Worry about cancer (1–4)
    WB_BounceBack_rec,      # 1 - 5
    SS_PeopleRelyOn,        # 1-4
    ScreeningTest_binary    # past screening (0, 1)
  ) %>%
  filter(
    across(everything(), ~ . %in% 0:5)  # keep valid responses
  )

lca_data <- lca_data %>%
  lapply(as.factor) %>%
  as.data.frame()

# Run LCA for 2, 3 & 4 classes 
set.seed(123)
lca_model_3class <- poLCA(cbind(HEWR_GetCancer_rec, WB_BounceBack_rec, SS_PeopleRelyOn, ScreeningTest_binary) ~ 1, 
                          data = lca_data, nclass = 3, na.rm = FALSE, maxiter = 2000)
lca_model_3class$P
lca_model_3class$numiter

set.seed(123)
lca_model_2class <- poLCA(cbind(HEWR_GetCancer_rec, WB_BounceBack_rec, SS_PeopleRelyOn, ScreeningTest_binary) ~ 1, 
                          data = lca_data, nclass = 2, na.rm = FALSE, maxiter = 2000)
lca_model_2class$P
lca_model_2class$numiter

set.seed(123)
lca_model_4class <- poLCA(cbind(HEWR_GetCancer_rec, WB_BounceBack_rec, SS_PeopleRelyOn, ScreeningTest_binary) ~ 1, 
                          data = lca_data, nclass = 4, na.rm = FALSE, maxiter = 2000)
lca_model_4class$P
lca_model_4class$numiter

# Function to calculate entropy for a given LCA model
calc_entropy <- function(model) {
  post <- model$posterior
  N <- nrow(post)
  K <- ncol(post)
  
  indiv_entropy <- -rowSums(post * log(post + 1e-10)) # avoid log(0)
  entropy <- 1 - sum(indiv_entropy) / (N * log(K))
  return(entropy)
}

# Calculate entropy for each model
entropy_2class <- calc_entropy(lca_model_2class)
entropy_3class <- calc_entropy(lca_model_3class)
entropy_4class <- calc_entropy(lca_model_4class)

# Put results in a table
entropy_results <- data.frame(
  Classes = c(2, 3, 4),
  Entropy = c(entropy_2class, entropy_3class, entropy_4class),
  AIC = c(lca_model_2class$aic, lca_model_3class$aic, lca_model_4class$aic),
  BIC = c(lca_model_2class$bic, lca_model_3class$bic, lca_model_4class$bic)
)

print(entropy_results)

# 5.3 - FINAL MODEL THAT WAS USED - trial and error 3 with only non-screeners 
dissert_data <- dissert_data %>%
  mutate(
    HEWR_GetCancer_rec = case_when(
      HEWR_GetCancer == 1 ~ 1,                # Low worry
      HEWR_GetCancer == 2 ~ 2,                # Moderate worry
      HEWR_GetCancer %in% c(3,4) ~ 3,         # High worry
      TRUE ~ NA_real_
    ),
    
    # Bounce back (1 Strongly disagree – 5 Strongly agree)
    WB_BounceBack_rec = case_when(
      WB_BounceBack %in% c(1,2) ~ 1,          # Low resilience
      WB_BounceBack == 3 ~ 2,                 # Neutral
      WB_BounceBack %in% c(4,5) ~ 3,          # High resilience
      TRUE ~ NA_real_
    ),
    
    # Future screening intention (1 Very likely – 5 Not at all likely)
    FutureScreening_rec = case_when(
      FutureScreening %in% c(1,2) ~ 1,        # Likely
      FutureScreening == 3 ~ 2,               # Unsure
      FutureScreening %in% c(4,5) ~ 3,        # Unlikely
      TRUE ~ NA_real_
    )
  )

lca_data <- dissert_data %>%
  filter(ScreeningTest_binary == 0) %>%
  dplyr::select(
    HEWR_GetCancer_rec,    # Worry about cancer (1–4)
    WB_BounceBack_rec,      # 1 - 5
    SS_PeopleRelyOn,        # 1-4
  ) %>%
  filter(
    across(everything(), ~ . %in% 0:5)  # keep valid responses
  ) %>%
  lapply(as.factor) %>%
  as.data.frame()

# Run LCA for 2, 3 & 4 classes 
set.seed(123)
lca_model_3class <- poLCA(cbind(HEWR_GetCancer_rec, WB_BounceBack_rec, SS_PeopleRelyOn) ~ 1, 
                          data = lca_data, nclass = 3, na.rm = FALSE, maxiter = 2000)
lca_model_3class$P
lca_model_3class$numiter

set.seed(123)
lca_model_2class <- poLCA(cbind(HEWR_GetCancer_rec, WB_BounceBack_rec, SS_PeopleRelyOn) ~ 1, 
                          data = lca_data, nclass = 2, na.rm = FALSE, maxiter = 2000)
lca_model_2class$P
lca_model_2class$numiter

set.seed(123)
lca_model_4class <- poLCA(cbind(HEWR_GetCancer_rec, WB_BounceBack_rec, SS_PeopleRelyOn) ~ 1, 
                          data = lca_data, nclass = 4, na.rm = FALSE, maxiter = 2000)
lca_model_4class$P
lca_model_4class$numiter

# Function to calculate entropy for a given LCA model
calc_entropy <- function(model) {
  post <- model$posterior
  N <- nrow(post)
  K <- ncol(post)
  
  indiv_entropy <- -rowSums(post * log(post + 1e-10)) # avoid log(0)
  entropy <- 1 - sum(indiv_entropy) / (N * log(K))
  return(entropy)
}

# Calculate entropy for each model
entropy_2class <- calc_entropy(lca_model_2class)
entropy_3class <- calc_entropy(lca_model_3class)
entropy_4class <- calc_entropy(lca_model_4class)

# Put results in a table
entropy_results <- data.frame(
  Classes = c(2, 3, 4),
  Entropy = c(entropy_2class, entropy_3class, entropy_4class),
  AIC = c(lca_model_2class$aic, lca_model_3class$aic, lca_model_4class$aic),
  BIC = c(lca_model_2class$bic, lca_model_3class$bic, lca_model_4class$bic)
)
print(entropy_results)

# 5.4 - trial and error 4 with non-screeners and NoSC variables (reduced the sample size too much)
dissert_data <- dissert_data %>%
  mutate(
    HEWR_GetCancer_rec = case_when(
      HEWR_GetCancer == 1 ~ 1,                # Low worry
      HEWR_GetCancer == 2 ~ 2,                # Moderate worry
      HEWR_GetCancer %in% c(3,4) ~ 3,         # High worry
      TRUE ~ NA_real_
    ),
    
    # Bounce back (1 Strongly disagree – 5 Strongly agree)
    WB_BounceBack_rec = case_when(
      WB_BounceBack %in% c(1,2) ~ 1,          # Low resilience
      WB_BounceBack == 3 ~ 2,                 # Neutral
      WB_BounceBack %in% c(4,5) ~ 3,          # High resilience
      TRUE ~ NA_real_
    ),
    
    # Future screening intention (1 Very likely – 5 Not at all likely)
    FutureScreening_rec = case_when(
      FutureScreening %in% c(1,2) ~ 1,        # Likely
      FutureScreening == 3 ~ 2,               # Unsure
      FutureScreening %in% c(4,5) ~ 3,        # Unlikely
      TRUE ~ NA_real_
    )
  )

lca_data <- dissert_data %>%
  filter(ScreeningTest_binary == 0) %>%
  dplyr::select(
    HEWR_GetCancer_rec,    # Worry about cancer (1–4)
    WB_BounceBack_rec,      # 1 - 5
    SS_PeopleRelyOn,        # 1-4
    HEBS_NoSC_For1_1, 
    HEBS_NoSC_For1_6,
  ) %>%
  filter(
    across(everything(), ~ . %in% 0:5)  # keep valid responses
  ) %>%
  lapply(as.factor) %>%
  as.data.frame()

# Run LCA for 2, 3 & 4 classes 
set.seed(123)
lca_model_3class <- poLCA(cbind(HEWR_GetCancer_rec, WB_BounceBack_rec, SS_PeopleRelyOn, HEBS_NoSC_For1_1, HEBS_NoSC_For1_6) ~ 1, 
                          data = lca_data, nclass = 3, na.rm = FALSE, maxiter = 2000)
lca_model_3class$P
lca_model_3class$numiter

set.seed(123)
lca_model_2class <- poLCA(cbind(HEWR_GetCancer_rec, WB_BounceBack_rec, SS_PeopleRelyOn, HEBS_NoSC_For1_1, HEBS_NoSC_For1_6) ~ 1, 
                          data = lca_data, nclass = 2, na.rm = FALSE, maxiter = 2000)
lca_model_2class$P
lca_model_2class$numiter

set.seed(123)
lca_model_4class <- poLCA(cbind(HEWR_GetCancer_rec, WB_BounceBack_rec, SS_PeopleRelyOn, HEBS_NoSC_For1_1, HEBS_NoSC_For1_6) ~ 1, 
                          data = lca_data, nclass = 4, na.rm = FALSE, maxiter = 2000)
lca_model_4class$P
lca_model_4class$numiter

# Function to calculate entropy for a given LCA model
calc_entropy <- function(model) {
  post <- model$posterior
  N <- nrow(post)
  K <- ncol(post)
  
  indiv_entropy <- -rowSums(post * log(post + 1e-10)) # avoid log(0)
  entropy <- 1 - sum(indiv_entropy) / (N * log(K))
  return(entropy)
}

# Calculate entropy for each model
entropy_2class <- calc_entropy(lca_model_2class)
entropy_3class <- calc_entropy(lca_model_3class)
entropy_4class <- calc_entropy(lca_model_4class)

# Put results in a table
entropy_results <- data.frame(
  Classes = c(2, 3, 4),
  Entropy = c(entropy_2class, entropy_3class, entropy_4class),
  AIC = c(lca_model_2class$aic, lca_model_3class$aic, lca_model_4class$aic),
  BIC = c(lca_model_2class$bic, lca_model_3class$bic, lca_model_4class$bic)
)

print(entropy_results)