#install.packages("rms")
#install.packages("data.table")
#install.packages("gtsummary")
#install.packages("tableone")
#install.packages("survey")
#install.packages("ggdist")
#install.packages("cowplot")
#install.packages("purrr")
library(tidyverse)
library(bigrquery)
library(rms)
library(data.table)
library(gtsummary)
library(tableone)
library(survey)
library(ggdist)
library(cowplot)
library(purrr)

sessionInfo()
print("done")

# This query represents dataset "B + HL + MH" for domain "survey" and was generated for All of Us Controlled Tier Dataset v6
dataset_85163323_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (
                1384487, 1384522, 1585772, 1585899, 1384430, 1585838, 43528428, 1384592, 1585778, 1585940, 43528758, 1585845, 43528760, 1586140, 1585375, 1585766, 43528660, 43530404
            )
        )  
        AND (
            answer.PERSON_ID IN (
                SELECT
                    distinct person_id  
                FROM
                    `cb_search_person` cb_search_person  
                WHERE
                    cb_search_person.person_id IN (
                        SELECT
                            person_id 
                        FROM
                            `cb_search_all_events` 
                        WHERE
                            person_id IN (
                                SELECT
                                    person_id 
                                FROM
                                    `cb_search_all_events` 
                                WHERE
                                    concept_id IN (
                                        SELECT
                                            DISTINCT c.concept_id 
                                        FROM
                                            `cb_criteria` c 
                                        JOIN
                                            (
                                                select
                                                    cast(cr.id as string) as id 
                                                FROM
                                                    `cb_criteria` cr 
                                                WHERE
                                                    concept_id IN (1384430, 1585772, 1384522, 43528760, 43528758, 43528660, 1586134, 1585766, 1585778, 1384487, 43530404, 1384592, 43528660, 43530404) 
                                                    AND full_text LIKE '%_rank1]%'
                                            ) a 
                                                ON (
                                                    c.path LIKE CONCAT('%.',
                                                a.id,
                                                '.%') 
                                                OR c.path LIKE CONCAT('%.',
                                                a.id) 
                                                OR c.path LIKE CONCAT(a.id,
                                                '.%') 
                                                OR c.path = a.id) 
                                            WHERE
                                                is_standard = 0 
                                                AND is_selectable = 1
                                            ) 
                                            AND is_standard = 0 
                                    )
                                ) 
                        )
                    )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_85163323_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_85163323",
  "survey_85163323_*.csv")
message(str_glue('The data will be written to {survey_85163323_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_85163323_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_85163323_path,
  destination_format = "CSV")

print("done")

# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_85163323_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- NULL
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_85163323_survey_df <- read_bq_export_from_workspace_bucket(survey_85163323_path)

dim(dataset_85163323_survey_df)

head(dataset_85163323_survey_df, 5)

# first rename the dataset into something more useful
long_data_survey <- dataset_85163323_survey_df

# now figure out how many unique person_ids there are
length(unique(long_data_survey$person_id)) # 372052

# convert survey_datetime to a date object
long_data_survey$date = as.Date(long_data_survey$survey_datetime)

head(long_data_survey, 5)

# Save
Save(long_data_survey)

print("done")

# load long data
load("/home/jupyter/workspaces/sdohstudycontrolledtier/long_data_survey.rda")
dim(long_data_survey)

# need to get first date for each person_id
dates <- long_data_survey %>% select(person_id, date) %>%
    group_by(person_id) %>%
    summarise(date = min(date))

head(dates)
nrow(dates)

# pivot_wider
# https://stackoverflow.com/questions/65744153/pivot-to-wide-dataframe-with-repeating-column-names
wide_data_survey <- long_data_survey %>% 
    mutate(question = str_c(question, "_", rowid(person_id, question))) %>%
    pivot_wider(id_cols = c(person_id),
                names_from = question,
                values_from = answer)

# dimensions
dim(wide_data_survey)

# get some more info about the wide dataframe
head(wide_data_survey)
names(wide_data_survey)
head(table(wide_data_survey$person_id))
Save(wide_data_survey)
print("done")

# This query represents dataset "B + HL + MH" for domain "person" and was generated for All of Us Controlled Tier Dataset v6
dataset_85163323_person_sql <- paste("
    SELECT
        person.person_id,
        person.birth_datetime as date_of_birth 
    FROM
        `person` person   
    WHERE
        person.PERSON_ID IN (
            SELECT
                distinct person_id  
            FROM
                `cb_search_person` cb_search_person  
            WHERE
                cb_search_person.person_id IN (
                    SELECT
                        person_id 
                    FROM
                        `cb_search_all_events` 
                    WHERE
                        person_id IN (
                            SELECT
                                person_id 
                            FROM
                                `cb_search_all_events` 
                            WHERE
                                concept_id IN (
                                    SELECT
                                        DISTINCT c.concept_id 
                                    FROM
                                        `cb_criteria` c 
                                    JOIN
                                        (
                                            select
                                                cast(cr.id as string) as id 
                                            FROM
                                                `cb_criteria` cr 
                                            WHERE
                                                concept_id IN (1384430, 1585772, 1384522, 43528760, 43528758, 43528660, 1586134, 1585766, 1585778, 1384487, 43530404, 1384592, 43528660, 43530404) 
                                                AND full_text LIKE '%_rank1]%'
                                        ) a 
                                            ON (
                                                c.path LIKE CONCAT('%.',
                                            a.id,
                                            '.%') 
                                            OR c.path LIKE CONCAT('%.',
                                            a.id) 
                                            OR c.path LIKE CONCAT(a.id,
                                            '.%') 
                                            OR c.path = a.id) 
                                        WHERE
                                            is_standard = 0 
                                            AND is_selectable = 1
                                        ) 
                                        AND is_standard = 0 
                                )
                            ) 
                    )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
person_85163323_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "person_85163323",
  "person_85163323_*.csv")
message(str_glue('The data will be written to {person_85163323_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_85163323_person_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  person_85163323_path,
  destination_format = "CSV")

print("done")

# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {person_85163323_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- NULL
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
dataset_85163323_person_df <- read_bq_export_from_workspace_bucket(person_85163323_path)

dim(dataset_85163323_person_df)

head(dataset_85163323_person_df, 5)

# save as easier to name df
person_df <- dataset_85163323_person_df
print("done")

# now need to combine dob with date of first survey
age_calc <- left_join(person_df, dates, by = "person_id") %>%
    mutate(age = (as.Date(date) - as.Date(date_of_birth))/365.25,
          age = as.numeric(age)) %>%
    select(-date_of_birth) # to protect privacy
head(age_calc, 5)
table(round(age_calc$age))

# now left_join baseline_survery_data to person_df
baseline_data <- left_join(wide_data_survey, age_calc, by = "person_id")
dim(baseline_data)
head(baseline_data)

# remove cases where all columns are missing
all_na <- function(x) any(!is.na(x))
baseline_data <- baseline_data %>% select_if(all_na)
dim(baseline_data)

# save dataset
Save(baseline_data)
print("done")

# load dataset (can start from here)
Load(baseline_data)
# to show that it's done
dim(baseline_data)

glimpse(baseline_data)
names(baseline_data)

# Age
table(round(baseline_data$age))

# Enrollment Date
summary(baseline_data$date)

# Sex
table(baseline_data$`Biological Sex At Birth: Sex At Birth_1`)

# clean sex at birth
baseline_data <- baseline_data %>%
    mutate(Sex = str_remove(`Biological Sex At Birth: Sex At Birth_1`, "Sex At Birth: "),
          Sex = str_remove(Sex, "PMI: "))
table(baseline_data$Sex)

# Gender Identity `Gender: Gender Identity_1`
for(i in 1:4){
    var <- paste0('Gender: Gender Identity_', i)
    print(var)
    print(table(baseline_data[var]))
}

# clean Gender
baseline_data <- baseline_data %>%
    mutate(Gender = ifelse(is.na(`Gender: Gender Identity_2`), 
                             `Gender: Gender Identity_1`, "Multiple"),
          Gender = str_remove(Gender, "Gender Identity: "),
          Gender = str_remove(Gender, "PMI: "),
          MultGender = ifelse(Gender == "Multiple",
                                paste0(str_remove(`Gender: Gender Identity_2`,
                                            "Gender Identity: "), ", ",
                                  str_remove(`Gender: Gender Identity_3`,
                                            "Gender Identity: "), ", ",
                                  str_remove(`Gender: Gender Identity_4`,
                                            "Gender Identity: ")), NA),
          Gender = ifelse(Gender == "Multiple" & MultGender == "Woman, NA, NA", "Woman", Gender),
          Gender = ifelse(Gender == "Multiple" & MultGender == "Man, NA, NA", "Man", Gender),
          Gender = ifelse(Gender == "Multiple" & MultGender == "Transgender, NA, NA", "Transgender", Gender))
table(baseline_data$Gender)
table(baseline_data$MultGender)

# Sexual Orientation
for(i in 1:4){
    var <- paste0('The Basics: Sexual Orientation_', i)
    print(var)
    print(table(baseline_data[var]))
}

# clean sexual orientation
baseline_data <- baseline_data %>%
    mutate(Orientation = ifelse(is.na(`The Basics: Sexual Orientation_2`), 
                             `The Basics: Sexual Orientation_1`, "Multiple"),
          Orientation = str_remove(Orientation, "Sexual Orientation: "),
          Orientation = str_remove(Orientation, "PMI: "),
          MultOrientation = ifelse(Orientation == "Multiple",
                                paste0(str_remove(`The Basics: Sexual Orientation_2`,
                                            "Sexual Orientation: "), ", ",
                                  str_remove(`The Basics: Sexual Orientation_3`,
                                            "Sexual Orientation: "), ", ",
                                  str_remove(`The Basics: Sexual Orientation_4`,
                                            "Sexual Orientation: ")), NA),
          Orientation = ifelse(Orientation == "Multiple" & 
                               MultOrientation == "Bisexual, NA, NA", "Bisexual", Orientation),
          Orientation = ifelse(Orientation == "Multiple" & 
                               MultOrientation == "Gay, NA, NA", "Gay", Orientation),
          Orientation = ifelse(Orientation == "Multiple" & 
                               MultOrientation == "Straight, NA, NA", "Straight", Orientation))
table(baseline_data$Orientation)
table(baseline_data$MultOrientation)

# race and ethnicity
for(i in 1:6){
    var <- paste0('Race: What Race Ethnicity_', i)
    print(var)
    print(table(baseline_data[var]))
}

# clean race/ethnicity
baseline_data <- baseline_data %>%
    mutate(Category = ifelse(is.na(`Race: What Race Ethnicity_2`), 
                             `Race: What Race Ethnicity_1`, "Multiple"),
          Category = str_remove(Category, "What Race Ethnicity: "),
          Category = str_remove(Category, "PMI: "),
          MultCategory = ifelse(Category == "Multiple",
                                paste0(str_remove(`Race: What Race Ethnicity_2`,
                                            "What Race Ethnicity: "), ", ",
                                  str_remove(`Race: What Race Ethnicity_3`,
                                            "What Race Ethnicity: "), ", ",
                                  str_remove(`Race: What Race Ethnicity_4`,
                                            "What Race Ethnicity: "), ", ",
                                  str_remove(`Race: What Race Ethnicity_5`,
                                            "What Race Ethnicity: "), ", ",
                                  str_remove(`Race: What Race Ethnicity_6`,
                                            "What Race Ethnicity: ")), NA))
table(baseline_data$Category)
table(baseline_data$MultCategory)

# clean race/ethnicity
baseline_data <- baseline_data %>%
    mutate(Category = ifelse(is.na(`Race: What Race Ethnicity_2`), 
                             `Race: What Race Ethnicity_1`, "Multiple"),
          Category = str_remove(Category, "What Race Ethnicity: "),
          Category = str_remove(Category, "PMI: "),
          MultCategory = ifelse(Category == "Multiple",
                                paste0(str_remove(`Race: What Race Ethnicity_2`,
                                            "What Race Ethnicity: "), ", ",
                                  str_remove(`Race: What Race Ethnicity_3`,
                                            "What Race Ethnicity: "), ", ",
                                  str_remove(`Race: What Race Ethnicity_4`,
                                            "What Race Ethnicity: "), ", ",
                                  str_remove(`Race: What Race Ethnicity_5`,
                                            "What Race Ethnicity: "), ", ",
                                  str_remove(`Race: What Race Ethnicity_6`,
                                            "What Race Ethnicity: ")), NA),
          Category = ifelse(Category == "Multiple" & MultCategory == "Asian, NA, NA, NA, NA", "Asian", Category),
          Category = ifelse(Category == "Multiple" & MultCategory == "Black, NA, NA, NA, NA", "Black", Category),
          Category = ifelse(Category == "Multiple" & MultCategory == "Hispanic, NA, NA, NA, NA", "Hispanic", Category),
          Category = ifelse(Category == "Multiple" & MultCategory == "MENA, NA, NA, NA, NA", "MENA", Category),
          Category = ifelse(Category == "Multiple" & MultCategory == "NHPI, NA, NA, NA, NA", "NHPI", Category),
          Category = ifelse(Category == "Multiple" & MultCategory == "White, NA, NA, NA, NA", "White", Category))
table(baseline_data$Category)
table(baseline_data$MultCategory)

# Education
table(baseline_data$`Education Level: Highest Grade_1`)

# clean education
baseline_data <- baseline_data %>%
    mutate(Education = str_remove(`Education Level: Highest Grade_1`, "Highest Grade: "),
          Education = str_remove(Education, "PMI: "))
table(baseline_data$Education)

# Income
table(baseline_data$`Income: Annual Income_1`)

# clean Income
baseline_data <- baseline_data %>%
    mutate(AnnIncome = str_remove(`Income: Annual Income_1`, "Annual Income: "),
          AnnIncome = str_remove(AnnIncome, "PMI: "),
          AnnIncome = factor(AnnIncome, levels = c("Prefer Not To Answer",
                                      "Skip",
                                      "less 10k",
                                      "10k 25k",
                                      "25k 35k",
                                      "35k 50k",
                                      "50k 75k",
                                      "75k 100k",
                                      "100k 150k",
                                      "150k 200k",
                                      "more 200k")))
table(baseline_data$AnnIncome, useNA ="always")

# Health Insurance
# insurance type update
for(i in 1:6){
    var <- paste0('Health Insurance: Insurance Type Update_', i)
    print(var)
    print(table(baseline_data[var]))
}

baseline_data <- baseline_data %>%
    mutate(InsuranceTypeUpdate = ifelse(is.na(`Health Insurance: Insurance Type Update_2`), 
                             `Health Insurance: Insurance Type Update_1`, "Multiple"),
          InsuranceTypeUpdate = str_remove(InsuranceTypeUpdate, "Insurance Type Update: "),
          InsuranceTypeUpdate = str_remove(InsuranceTypeUpdate, "PMI: "),
          MultInsuranceTypeUpdate = ifelse(InsuranceTypeUpdate == "Multiple",
                                paste0(str_remove(`Health Insurance: Insurance Type Update_2`,
                                            "Insurance Type Update: "), ", ",
                                  str_remove(`Health Insurance: Insurance Type Update_3`,
                                            "Insurance Type Update: "), ", ",
                                  str_remove(`Health Insurance: Insurance Type Update_4`,
                                            "Insurance Type Update: "), ", ",
                                  str_remove(`Health Insurance: Insurance Type Update_5`,
                                            "Insurance Type Update: "), ", ",
                                  str_remove(`Health Insurance: Insurance Type Update_6`,
                                            "Insurance Type Update: ")), NA),
          InsuranceTypeUpdate = ifelse(InsuranceTypeUpdate == "Multiple" & 
                                       MultInsuranceTypeUpdate == "Employer Or Union, NA, NA, NA, NA", 
                                       "Employer Or Union", InsuranceTypeUpdate),
          InsuranceTypeUpdate = ifelse(InsuranceTypeUpdate == "Multiple" & 
                                       MultInsuranceTypeUpdate == "Indian, NA, NA, NA, NA", 
                                       "Indian", InsuranceTypeUpdate),
          InsuranceTypeUpdate = ifelse(InsuranceTypeUpdate == "Multiple" & 
                                       MultInsuranceTypeUpdate == "Medicare, NA, NA, NA, NA", 
                                       "Medicare", InsuranceTypeUpdate),
          InsuranceTypeUpdate = ifelse(InsuranceTypeUpdate == "Multiple" & 
                                       MultInsuranceTypeUpdate == "Military, NA, NA, NA, NA", 
                                       "Military", InsuranceTypeUpdate),
          InsuranceTypeUpdate = ifelse(InsuranceTypeUpdate == "Multiple" & 
                                       MultInsuranceTypeUpdate == "Purchased, NA, NA, NA, NA", 
                                       "Purchased", InsuranceTypeUpdate),
          InsuranceTypeUpdate = ifelse(InsuranceTypeUpdate == "Multiple" & 
                                       MultInsuranceTypeUpdate == "VA, NA, NA, NA, NA", 
                                       "VA", InsuranceTypeUpdate),
          InsuranceTypeUpdate = ifelse(InsuranceTypeUpdate == "Multiple" & 
                                       MultInsuranceTypeUpdate == "Employer Or Union, NA, NA, NA, NA", 
                                       "Employer Or Union", InsuranceTypeUpdate))
table(baseline_data$InsuranceTypeUpdate)
table(baseline_data$MultInsuranceTypeUpdate)

# Health Literacy

# Medical Form Confidence
table(baseline_data$`Overall Health: Medical Form Confidence_1`)
baseline_data <- baseline_data %>%
    mutate(MedFormConf = str_remove(`Overall Health: Medical Form Confidence_1`, "Medical Form Confidence: "),
          MedFormConf = str_remove(MedFormConf, "PMI: "),
          MedFormConf = factor(MedFormConf, levels = c("Skip",
                                                      "Not At All",
                                                      "A Little Bit",
                                                      "Somewhat",
                                                      "Quite A Bit",
                                                      "Extremely")))
table(baseline_data$MedFormConf)
# Health Material Assistance
table(baseline_data$`Overall Health: Health Material Assistance_1`)
baseline_data <- baseline_data %>%
    mutate(HealthMatAssist = str_remove(`Overall Health: Health Material Assistance_1`, "Health Material Assistance: "),
          HealthMatAssist = str_remove(HealthMatAssist, "PMI: "),
          HealthMatAssist = factor(HealthMatAssist, levels = c("Skip",
                                                      "Never",
                                                      "Occasionally",
                                                      "Sometimes",
                                                      "Often",
                                                      "Always")))
table(baseline_data$HealthMatAssist)
# Difficulty Understanding Information
table(baseline_data$`Overall Health: Difficult Understand Info_1`)
baseline_data <- baseline_data %>%
    mutate(HealthInfoDif = str_remove(`Overall Health: Difficult Understand Info_1`, "Difficult Understand Info: "),
          HealthInfoDif = str_remove(HealthInfoDif, "PMI: "),
          HealthInfoDif = factor(HealthInfoDif, levels = c("Skip",
                                                      "Never",
                                                      "Occasionally",
                                                      "Sometimes",
                                                      "Often",
                                                      "Always")))
table(baseline_data$HealthInfoDif)

# Seen a doctor
table(baseline_data$`Health Advice: Spoken To General Doctor_1`)
baseline_data <- baseline_data %>%
    mutate(SeenDoc = str_remove(`Health Advice: Spoken To General Doctor_1`, "Spoken To General Doctor: "),
          SeenDoc = str_remove(SeenDoc, "PMI: "),
          SeenDoc = factor(SeenDoc, levels = c("Skip",
                                                      "Dont Know",
                                                      "No",
                                                      "Yes")))
table(baseline_data$SeenDoc)

# Seen an APP
table(baseline_data$`Health Advice: Spoken To Nurse Practitioner_1`)
baseline_data <- baseline_data %>%
    mutate(SeenAPP = str_remove(`Health Advice: Spoken To Nurse Practitioner_1`, "Spoken To Nurse Practitioner: "),
          SeenAPP = str_remove(SeenAPP, "PMI: "),
          SeenAPP = factor(SeenAPP, levels = c("Skip",
                                                      "Dont Know",
                                                      "No",
                                                      "Yes")))
table(baseline_data$SeenAPP)

# heart and blood conditions
for(i in 1:30){
    var <- paste0('Circulatory: Circulatory Conditions_', i)
    print(var)
    print(table(baseline_data[var]))
}

# cancer
for(i in 1:18){
    var <- paste0('Cancer: Cancer Conditions_', i)
    print(var)
    print(table(baseline_data[var]))
}

# respiratory conditions
for(i in 1:8){
    var <- paste0('Respiratory: Respiratory Conditions_', i)
    print(var)
    print(table(baseline_data[var]))
}

# endocrine conditions
for(i in 1:14){
    var <- paste0('Endocrine: Endocrine Conditions_', i)
    print(var)
    print(table(baseline_data[var]))
}

# other conditions
for(i in 1:22){
    var <- paste0('Other: Other Conditions_', i)
    print(var)
    print(table(baseline_data[var]))
}

# kidney conditions
for(i in 1:10){
    var <- paste0('Kidney: Kidney Conditions_', i)
    print(var)
    print(table(baseline_data[var]))
}

# will create columns of each outcome of interest: HTN, CAD, Cancer, Skin Cancer, Lung Disease, Diabetes, 
# Obesity, and CKD

# start by creating list of names
vec_htn <- c("Circulatory Conditions: Hypertension")
vec_cad <- c("Circulatory Conditions: Coronary Artery")
vec_any_cancer <- c("Cancer Conditions: Bladder Cancer",
            "Cancer Conditions: Blood Cancer",
            "Cancer Conditions: Bone Cancer",
            "Cancer Conditions: Brain Cancer",
            "Cancer Conditions: Breast Cancer",
            "Cancer Conditions: Cervical Cancer",
            "Cancer Conditions: Colon Rectal Cancer",
            "Cancer Conditions: Endocrine Cancer",
            "Cancer Conditions: Endometrial Cancer",
            "Cancer Conditions: Esophageal Cancer",
            "Cancer Conditions: Eye Cancer", 
                "Cancer Conditions: Head Neck Cancer",
                "Cancer Conditions: Kidney Cancer",
                "Cancer Conditions: Lung Cancer",
                "Cancer Conditions: Other Cancer",
                "Cancer Conditions: Ovarian Cancer",
                "Cancer Conditions: Pancreatic Cancer",
                "Cancer Conditions: Prostate Cancer", 
                "Cancer Conditions: Skin Cancer", 
                "Cancer Conditions: Stomach Cancer", 
                "Cancer Conditions: Thyroid Cancer")
vec_skin_cancer <- c("Cancer Conditions: Skin Cancer")
vec_lung <- c("Respiratory Conditions: Asthma",
          "Respiratory Conditions: Chronic Lung",
          "Respiratory Conditions: Other Lung Condition")
vec_diabetes <- c("Endocrine Conditions: Type 1 Diabetes",
              "Endocrine Conditions: Type 2 Diabetes",
              "Endocrine Conditions: Other Diabetes")
vec_obesity <- c("Other Conditions: Obesity")
vec_ckd <- c("Kidney Conditions: Kidney With Dialysis",
         "Kidney Conditions: Kidney Without Dialysis")
skip <- c("PMI: Skip", "No matching concept", "How Old Were You Urinary Tract: Adolescent")
print("done")

# need to start by getting patient id and each outcome into it's own dataframe
# then can categorize outcomes for each person, then merge it back with the orginal

# htn
htn_df <- baseline_data %>% select(person_id, starts_with("Circulatory")) %>%
    mutate(c1 = ifelse(`Circulatory: Circulatory Conditions_1` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_1` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_1`), NA, 0))),
          c2 = ifelse(`Circulatory: Circulatory Conditions_2` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_2` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_2`), NA, 0))),
          c3 = ifelse(`Circulatory: Circulatory Conditions_3` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_3` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_3`), NA, 0))),
          c4 = ifelse(`Circulatory: Circulatory Conditions_4` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_4` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_4`), NA, 0))),
          c5 = ifelse(`Circulatory: Circulatory Conditions_5` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_5` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_5`), NA, 0))),
          c6 = ifelse(`Circulatory: Circulatory Conditions_6` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_6` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_6`), NA, 0))),
          c7 = ifelse(`Circulatory: Circulatory Conditions_7` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_7` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_7`), NA, 0))),
          c8 = ifelse(`Circulatory: Circulatory Conditions_8` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_8` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_8`), NA, 0))),
          c9 = ifelse(`Circulatory: Circulatory Conditions_9` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_9` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_9`), NA, 0))),
          c10 = ifelse(`Circulatory: Circulatory Conditions_10` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_10` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_10`), NA, 0))),
          c11 = ifelse(`Circulatory: Circulatory Conditions_11` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_11` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_11`), NA, 0))),
          c12 = ifelse(`Circulatory: Circulatory Conditions_12` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_12` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_12`), NA, 0))),
          c13 = ifelse(`Circulatory: Circulatory Conditions_13` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_13` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_13`), NA, 0))),
          c14 = ifelse(`Circulatory: Circulatory Conditions_14` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_14` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_14`), NA, 0))),
          c15 = ifelse(`Circulatory: Circulatory Conditions_15` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_15` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_15`), NA, 0))),
          c16 = ifelse(`Circulatory: Circulatory Conditions_16` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_16` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_16`), NA, 0))),
          c17 = ifelse(`Circulatory: Circulatory Conditions_17` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_17` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_17`), NA, 0))),
          c18 = ifelse(`Circulatory: Circulatory Conditions_18` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_18` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_18`), NA, 0))),
          c19 = ifelse(`Circulatory: Circulatory Conditions_19` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_19` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_19`), NA, 0))),
          c20 = ifelse(`Circulatory: Circulatory Conditions_20` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_20` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_20`), NA, 0))),
          c21 = ifelse(`Circulatory: Circulatory Conditions_21` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_21` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_21`), NA, 0))),
          c22 = ifelse(`Circulatory: Circulatory Conditions_22` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_22` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_22`), NA, 0))),
          c23 = ifelse(`Circulatory: Circulatory Conditions_23` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_23` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_23`), NA, 0))),
          c24 = ifelse(`Circulatory: Circulatory Conditions_24` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_24` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_24`), NA, 0))),
          c25 = ifelse(`Circulatory: Circulatory Conditions_25` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_25` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_25`), NA, 0))),
          c26 = ifelse(`Circulatory: Circulatory Conditions_26` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_26` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_26`), NA, 0))),
          c27 = ifelse(`Circulatory: Circulatory Conditions_27` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_27` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_27`), NA, 0))),
          c28 = ifelse(`Circulatory: Circulatory Conditions_28` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_28` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_28`), NA, 0))),
          c29 = ifelse(`Circulatory: Circulatory Conditions_29` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_29` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_29`), NA, 0))),
          c30 = ifelse(`Circulatory: Circulatory Conditions_30` %in% vec_htn, 1,
                       ifelse(`Circulatory: Circulatory Conditions_30` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_30`), NA, 0)))) %>%
    rowwise() %>%
    mutate(htn = ifelse(is.nan(c1), NaN,
                               ifelse(is.na(c1), NA,
                                     sum(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, 
                                         c11, c12, c13, c14, c15, c16, c17, c18, c19, 
                                         c20, c21, c22, c23, c24, c25, c26, c27, c28, 
                                         c29, c30, na.rm = TRUE))))
head(htn_df %>% select(person_id, starts_with("Circulatory"), htn), 20)
# final dataframe for merging
htn_final <- htn_df %>% select(person_id, htn) %>%
    mutate(HTN1 = ifelse(is.nan(htn), "Skip", NA),
          HTN2 = ifelse(htn == 0, "No", NA),
          HTN3 = ifelse(htn >= 1, "Yes", NA),
          HTN = ifelse(!is.na(HTN1), HTN1, 
                      ifelse(!is.na(HTN2), HTN2,
                            ifelse(!is.na(HTN3), HTN3, NA)))) %>%
    select(-HTN1, -HTN2, -HTN3)
table(htn_final$htn, useNA = "always")
prop.table(table(htn_final$htn))
table(htn_final$HTN, useNA = "always")
prop.table(table(htn_final$HTN))

# cad
cad_df <- baseline_data %>% select(person_id, starts_with("Circulatory")) %>%
    mutate(c1 = ifelse(`Circulatory: Circulatory Conditions_1` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_1` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_1`), NA, 0))),
          c2 = ifelse(`Circulatory: Circulatory Conditions_2` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_2` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_2`), NA, 0))),
          c3 = ifelse(`Circulatory: Circulatory Conditions_3` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_3` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_3`), NA, 0))),
          c4 = ifelse(`Circulatory: Circulatory Conditions_4` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_4` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_4`), NA, 0))),
          c5 = ifelse(`Circulatory: Circulatory Conditions_5` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_5` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_5`), NA, 0))),
          c6 = ifelse(`Circulatory: Circulatory Conditions_6` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_6` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_6`), NA, 0))),
          c7 = ifelse(`Circulatory: Circulatory Conditions_7` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_7` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_7`), NA, 0))),
          c8 = ifelse(`Circulatory: Circulatory Conditions_8` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_8` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_8`), NA, 0))),
          c9 = ifelse(`Circulatory: Circulatory Conditions_9` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_9` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_9`), NA, 0))),
          c10 = ifelse(`Circulatory: Circulatory Conditions_10` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_10` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_10`), NA, 0))),
          c11 = ifelse(`Circulatory: Circulatory Conditions_11` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_11` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_11`), NA, 0))),
          c12 = ifelse(`Circulatory: Circulatory Conditions_12` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_12` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_12`), NA, 0))),
          c13 = ifelse(`Circulatory: Circulatory Conditions_13` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_13` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_13`), NA, 0))),
          c14 = ifelse(`Circulatory: Circulatory Conditions_14` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_14` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_14`), NA, 0))),
          c15 = ifelse(`Circulatory: Circulatory Conditions_15` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_15` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_15`), NA, 0))),
          c16 = ifelse(`Circulatory: Circulatory Conditions_16` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_16` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_16`), NA, 0))),
          c17 = ifelse(`Circulatory: Circulatory Conditions_17` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_17` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_17`), NA, 0))),
          c18 = ifelse(`Circulatory: Circulatory Conditions_18` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_18` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_18`), NA, 0))),
          c19 = ifelse(`Circulatory: Circulatory Conditions_19` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_19` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_19`), NA, 0))),
          c20 = ifelse(`Circulatory: Circulatory Conditions_20` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_20` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_20`), NA, 0))),
          c21 = ifelse(`Circulatory: Circulatory Conditions_21` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_21` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_21`), NA, 0))),
          c22 = ifelse(`Circulatory: Circulatory Conditions_22` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_22` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_22`), NA, 0))),
          c23 = ifelse(`Circulatory: Circulatory Conditions_23` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_23` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_23`), NA, 0))),
          c24 = ifelse(`Circulatory: Circulatory Conditions_24` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_24` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_24`), NA, 0))),
          c25 = ifelse(`Circulatory: Circulatory Conditions_25` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_25` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_25`), NA, 0))),
          c26 = ifelse(`Circulatory: Circulatory Conditions_26` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_26` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_26`), NA, 0))),
          c27 = ifelse(`Circulatory: Circulatory Conditions_27` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_27` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_27`), NA, 0))),
          c28 = ifelse(`Circulatory: Circulatory Conditions_28` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_28` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_28`), NA, 0))),
          c29 = ifelse(`Circulatory: Circulatory Conditions_29` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_29` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_29`), NA, 0))),
          c30 = ifelse(`Circulatory: Circulatory Conditions_30` %in% vec_cad, 1,
                       ifelse(`Circulatory: Circulatory Conditions_30` %in% skip, NaN, 
                             ifelse(is.na(`Circulatory: Circulatory Conditions_30`), NA, 0)))) %>%
    rowwise() %>%
    mutate(cad = ifelse(is.nan(c1), NaN,
                               ifelse(is.na(c1), NA,
                                     sum(c1, c2, c3, c4, c5, c6, c7, c8, c9, 
                                         c10, c11, c12, c13, c14, c15, c16, 
                                         c17, c18, c19, c20, c21, c22, c23, c24, 
                                         c25, c26, c27, c28, c29, c30, na.rm = TRUE))))
head(cad_df %>% select(person_id, starts_with("Circulatory"), cad), 20)
# final dataframe for merging
cad_final <- cad_df %>% select(person_id, cad) %>%
    mutate(CAD1 = ifelse(is.nan(cad), "Skip", NA),
          CAD2 = ifelse(cad == 0, "No", NA),
          CAD3 = ifelse(cad >= 1, "Yes", NA),
          CAD = ifelse(!is.na(CAD1), CAD1, 
                      ifelse(!is.na(CAD2), CAD2,
                            ifelse(!is.na(CAD3), CAD3, NA)))) %>%
    select(-CAD1, -CAD2, -CAD3)
table(cad_final$cad, useNA = "always")
prop.table(table(cad_final$cad))
table(cad_final$CAD, useNA = "always")
prop.table(table(cad_final$CAD))

# any cancer
any_cancer_df <- baseline_data %>% select(person_id, starts_with("Cancer")) %>%
    mutate(c1 = ifelse(`Cancer: Cancer Conditions_1` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_1` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_1`), NA, 0))),
          c2 = ifelse(`Cancer: Cancer Conditions_2` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_2` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_2`), NA, 0))),
          c3 = ifelse(`Cancer: Cancer Conditions_3` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_3` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_3`), NA, 0))),
          c4 = ifelse(`Cancer: Cancer Conditions_4` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_4` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_4`), NA, 0))),
          c5 = ifelse(`Cancer: Cancer Conditions_5` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_5` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_5`), NA, 0))),
          c6 = ifelse(`Cancer: Cancer Conditions_6` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_6` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_6`), NA, 0))),
          c7 = ifelse(`Cancer: Cancer Conditions_7` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_7` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_7`), NA, 0))),
          c8 = ifelse(`Cancer: Cancer Conditions_8` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_8` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_8`), NA, 0))),
          c9 = ifelse(`Cancer: Cancer Conditions_9` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_9` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_9`), NA, 0))),
          c10 = ifelse(`Cancer: Cancer Conditions_10` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_10` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_10`), NA, 0))),
          c11 = ifelse(`Cancer: Cancer Conditions_11` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_11` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_11`), NA, 0))),
          c12 = ifelse(`Cancer: Cancer Conditions_12` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_12` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_12`), NA, 0))),
          c13 = ifelse(`Cancer: Cancer Conditions_13` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_13` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_13`), NA, 0))),
          c14 = ifelse(`Cancer: Cancer Conditions_14` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_14` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_14`), NA, 0))),
          c15 = ifelse(`Cancer: Cancer Conditions_15` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_15` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_15`), NA, 0))),
          c16 = ifelse(`Cancer: Cancer Conditions_16` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_16` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_16`), NA, 0))),
          c17 = ifelse(`Cancer: Cancer Conditions_17` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_17` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_17`), NA, 0))),
          c18 = ifelse(`Cancer: Cancer Conditions_18` %in% vec_any_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_18` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_18`), NA, 0)))) %>%
    rowwise() %>%
    mutate(any_cancer = ifelse(is.nan(c1), NaN,
                               ifelse(is.na(c1), NA,
                                     sum(c1, c2, c3, c4, c5, c6, c7, c8, c9,
                                         c10, c11, c12, c13, c14, c15, c16,
                                         c17, c18, na.rm = TRUE))))
head(any_cancer_df, 20)
# final dataframe for merging
any_cancer_final <- any_cancer_df %>% select(person_id, any_cancer) %>% 
    mutate(any_cancer_fin = ifelse(any_cancer >= 1, 1, any_cancer),
           CANCER1 = ifelse(is.nan(any_cancer), "Skip", NA),
           CANCER2 = ifelse(any_cancer_fin == 0, "No", NA),
           CANCER3 = ifelse(any_cancer_fin == 1, "Yes", NA),
           CANCER = ifelse(!is.na(CANCER1), CANCER1, 
                      ifelse(!is.na(CANCER2), CANCER2,
                            ifelse(!is.na(CANCER3), CANCER3, NA)))) %>%
    select(-CANCER1, -CANCER2, -CANCER3)
table(any_cancer_final$any_cancer, useNA = "always")
table(any_cancer_final$any_cancer_fin, useNA = "always")
table(any_cancer_final$CANCER, useNA = "always")
prop.table(table(any_cancer_final$any_cancer_fin))
prop.table(table(any_cancer_final$CANCER))

# skin cancer
skin_cancer_df <- baseline_data %>% select(person_id, starts_with("Cancer")) %>%
    mutate(s1 = ifelse(`Cancer: Cancer Conditions_1` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_1` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_1`), NA, 0))),
          s2 = ifelse(`Cancer: Cancer Conditions_2` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_2` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_2`), NA, 0))),
          s3 = ifelse(`Cancer: Cancer Conditions_3` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_3` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_3`), NA, 0))),
          s4 = ifelse(`Cancer: Cancer Conditions_4` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_4` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_4`), NA, 0))),
          s5 = ifelse(`Cancer: Cancer Conditions_5` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_5` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_5`), NA, 0))),
          s6 = ifelse(`Cancer: Cancer Conditions_6` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_6` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_6`), NA, 0))),
          s7 = ifelse(`Cancer: Cancer Conditions_7` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_7` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_7`), NA, 0))),
          s8 = ifelse(`Cancer: Cancer Conditions_8` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_8` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_8`), NA, 0))),
          s9 = ifelse(`Cancer: Cancer Conditions_9` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_9` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_9`), NA, 0))),
          s10 = ifelse(`Cancer: Cancer Conditions_10` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_10` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_10`), NA, 0))),
          s11 = ifelse(`Cancer: Cancer Conditions_11` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_11` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_11`), NA, 0))),
          s12 = ifelse(`Cancer: Cancer Conditions_12` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_12` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_12`), NA, 0))),
          s13 = ifelse(`Cancer: Cancer Conditions_13` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_13` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_13`), NA, 0))),
          s14 = ifelse(`Cancer: Cancer Conditions_14` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_14` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_14`), NA, 0))),
          s15 = ifelse(`Cancer: Cancer Conditions_15` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_15` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_15`), NA, 0))),
          s16 = ifelse(`Cancer: Cancer Conditions_16` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_16` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_16`), NA, 0))),
          s17 = ifelse(`Cancer: Cancer Conditions_17` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_17` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_17`), NA, 0))),
          s18 = ifelse(`Cancer: Cancer Conditions_18` %in% vec_skin_cancer, 1,
                       ifelse(`Cancer: Cancer Conditions_18` %in% skip, NaN, 
                             ifelse(is.na(`Cancer: Cancer Conditions_18`), NA, 0)))) %>%
    rowwise() %>%
    mutate(skin_cancer = ifelse(is.nan(s1), NaN,
                               ifelse(is.na(s1), NA,
                                     sum(s1, s2, s3, s4, s5, s6, s7, s8, s9,
                                         s10, s11, s12, s13, s14, s15, s16,
                                         s17, s18,na.rm = TRUE))))
head(skin_cancer_df, 20)
# final dataframe for merging
skin_cancer_final <- skin_cancer_df %>% select(person_id, skin_cancer) %>%
    mutate(SKINCANCER1 = ifelse(is.nan(skin_cancer), "Skip", NA),
          SKINCANCER2 = ifelse(skin_cancer == 0, "No", NA),
          SKINCANCER3 = ifelse(skin_cancer >= 1, "Yes", NA),
          SKINCANCER = ifelse(!is.na(SKINCANCER1), SKINCANCER1, 
                      ifelse(!is.na(SKINCANCER2), SKINCANCER2,
                            ifelse(!is.na(SKINCANCER3), SKINCANCER3, NA)))) %>%
    select(-SKINCANCER1, -SKINCANCER2, -SKINCANCER3)
table(skin_cancer_final$skin_cancer, useNA = "always")
prop.table(table(skin_cancer_final$skin_cancer))
table(skin_cancer_final$SKINCANCER, useNA = "always")
prop.table(table(skin_cancer_final$SKINCANCER))

# lung disease
lung_df <- baseline_data %>% select(person_id, starts_with("Respiratory")) %>%
    mutate(l1 = ifelse(`Respiratory: Respiratory Conditions_1` %in% vec_lung, 1,
                       ifelse(`Respiratory: Respiratory Conditions_1` %in% skip, NaN, 
                             ifelse(is.na(`Respiratory: Respiratory Conditions_1`), NA, 0))),
          l2 = ifelse(`Respiratory: Respiratory Conditions_2` %in% vec_lung, 1,
                       ifelse(`Respiratory: Respiratory Conditions_2` %in% skip, NaN, 
                             ifelse(is.na(`Respiratory: Respiratory Conditions_2`), NA, 0))),
          l3 = ifelse(`Respiratory: Respiratory Conditions_3` %in% vec_lung, 1,
                       ifelse(`Respiratory: Respiratory Conditions_3` %in% skip, NaN, 
                             ifelse(is.na(`Respiratory: Respiratory Conditions_3`), NA, 0))),
          l4 = ifelse(`Respiratory: Respiratory Conditions_4` %in% vec_lung, 1,
                       ifelse(`Respiratory: Respiratory Conditions_4` %in% skip, NaN, 
                             ifelse(is.na(`Respiratory: Respiratory Conditions_4`), NA, 0))),
          l5 = ifelse(`Respiratory: Respiratory Conditions_5` %in% vec_lung, 1,
                       ifelse(`Respiratory: Respiratory Conditions_5` %in% skip, NaN, 
                             ifelse(is.na(`Respiratory: Respiratory Conditions_5`), NA, 0))),
          l6 = ifelse(`Respiratory: Respiratory Conditions_6` %in% vec_lung, 1,
                       ifelse(`Respiratory: Respiratory Conditions_6` %in% skip, NaN, 
                             ifelse(is.na(`Respiratory: Respiratory Conditions_6`), NA, 0))),
          l7 = ifelse(`Respiratory: Respiratory Conditions_7` %in% vec_lung, 1,
                       ifelse(`Respiratory: Respiratory Conditions_7` %in% skip, NaN, 
                             ifelse(is.na(`Respiratory: Respiratory Conditions_7`), NA, 0))),
          l8 = ifelse(`Respiratory: Respiratory Conditions_8` %in% vec_lung, 1,
                       ifelse(`Respiratory: Respiratory Conditions_8` %in% skip, NaN, 
                             ifelse(is.na(`Respiratory: Respiratory Conditions_8`), NA, 0)))) %>%
    rowwise() %>%
    mutate(lung = ifelse(is.nan(l1), NaN,
                               ifelse(is.na(l1), NA,
                                     sum(l1, l2, l3, l4, l5, l6, l7, l8, na.rm = TRUE))))
head(lung_df, 20)
# final dataframe for merging
lung_final <- lung_df %>% select(person_id, lung) %>% 
    mutate(lung_fin = ifelse(lung >= 1, 1, lung),
           LUNG1 = ifelse(is.nan(lung), "Skip", NA),
           LUNG2 = ifelse(lung_fin == 0, "No", NA),
           LUNG3 = ifelse(lung_fin == 1, "Yes", NA),
           LUNG = ifelse(!is.na(LUNG1), LUNG1, 
                      ifelse(!is.na(LUNG2), LUNG2,
                            ifelse(!is.na(LUNG3), LUNG3, NA)))) %>%
    select(-LUNG1, -LUNG2, -LUNG3)
table(lung_final$lung, useNA = "always")
table(lung_final$lung_fin, useNA = "always")
table(lung_final$LUNG, useNA = "always")
prop.table(table(lung_final$LUNG))

# diabetes
diab_df <- baseline_data %>% select(person_id, starts_with("Endocrine")) %>%
    mutate(e1 = ifelse(`Endocrine: Endocrine Conditions_1` %in% vec_diabetes, 1,
                       ifelse(`Endocrine: Endocrine Conditions_1` %in% skip, NaN, 
                             ifelse(is.na(`Endocrine: Endocrine Conditions_1`), NA, 0))),
          e2 = ifelse(`Endocrine: Endocrine Conditions_2` %in% vec_diabetes, 1,
                       ifelse(`Endocrine: Endocrine Conditions_2` %in% skip, NaN, 
                             ifelse(is.na(`Endocrine: Endocrine Conditions_2`), NA, 0))),
          e3 = ifelse(`Endocrine: Endocrine Conditions_3` %in% vec_diabetes, 1,
                       ifelse(`Endocrine: Endocrine Conditions_3` %in% skip, NaN, 
                             ifelse(is.na(`Endocrine: Endocrine Conditions_3`), NA, 0))),
          e4 = ifelse(`Endocrine: Endocrine Conditions_4` %in% vec_diabetes, 1,
                       ifelse(`Endocrine: Endocrine Conditions_4` %in% skip, NaN, 
                             ifelse(is.na(`Endocrine: Endocrine Conditions_4`), NA, 0))),
          e5 = ifelse(`Endocrine: Endocrine Conditions_5` %in% vec_diabetes, 1,
                       ifelse(`Endocrine: Endocrine Conditions_5` %in% skip, NaN, 
                             ifelse(is.na(`Endocrine: Endocrine Conditions_5`), NA, 0))),
          e6 = ifelse(`Endocrine: Endocrine Conditions_6` %in% vec_diabetes, 1,
                       ifelse(`Endocrine: Endocrine Conditions_6` %in% skip, NaN, 
                             ifelse(is.na(`Endocrine: Endocrine Conditions_6`), NA, 0))),
          e7 = ifelse(`Endocrine: Endocrine Conditions_7` %in% vec_diabetes, 1,
                       ifelse(`Endocrine: Endocrine Conditions_7` %in% skip, NaN, 
                             ifelse(is.na(`Endocrine: Endocrine Conditions_7`), NA, 0))),
          e8 = ifelse(`Endocrine: Endocrine Conditions_8` %in% vec_diabetes, 1,
                       ifelse(`Endocrine: Endocrine Conditions_8` %in% skip, NaN, 
                             ifelse(is.na(`Endocrine: Endocrine Conditions_8`), NA, 0))),
          e9 = ifelse(`Endocrine: Endocrine Conditions_9` %in% vec_diabetes, 1,
                       ifelse(`Endocrine: Endocrine Conditions_9` %in% skip, NaN, 
                             ifelse(is.na(`Endocrine: Endocrine Conditions_9`), NA, 0))),
          e10 = ifelse(`Endocrine: Endocrine Conditions_10` %in% vec_diabetes, 1,
                       ifelse(`Endocrine: Endocrine Conditions_10` %in% skip, NaN, 
                             ifelse(is.na(`Endocrine: Endocrine Conditions_10`), NA, 0))),
          e11 = ifelse(`Endocrine: Endocrine Conditions_11` %in% vec_diabetes, 1,
                       ifelse(`Endocrine: Endocrine Conditions_11` %in% skip, NaN, 
                             ifelse(is.na(`Endocrine: Endocrine Conditions_11`), NA, 0))),
          e12 = ifelse(`Endocrine: Endocrine Conditions_12` %in% vec_diabetes, 1,
                       ifelse(`Endocrine: Endocrine Conditions_12` %in% skip, NaN, 
                             ifelse(is.na(`Endocrine: Endocrine Conditions_12`), NA, 0))),
          e13 = ifelse(`Endocrine: Endocrine Conditions_13` %in% vec_diabetes, 1,
                       ifelse(`Endocrine: Endocrine Conditions_13` %in% skip, NaN, 
                             ifelse(is.na(`Endocrine: Endocrine Conditions_13`), NA, 0))),
          e14 = ifelse(`Endocrine: Endocrine Conditions_14` %in% vec_diabetes, 1,
                       ifelse(`Endocrine: Endocrine Conditions_14` %in% skip, NaN, 
                             ifelse(is.na(`Endocrine: Endocrine Conditions_14`), NA, 0)))) %>%
    rowwise() %>%
    mutate(diab = ifelse(is.nan(e1), NaN,
                               ifelse(is.na(e1), NA,
                                     sum(e1, e2, e3, e4, e5, e6, e7,
                                         e8, e9, e10, e11, e12, e13, e14,na.rm = TRUE))))
head(diab_df, 20)
# final dataframe for merging
diab_final <- diab_df %>% select(person_id, diab) %>% 
    mutate(diab_fin = ifelse(diab >= 1, 1, diab),
           DIAB1 = ifelse(is.nan(diab), "Skip", NA),
           DIAB2 = ifelse(diab_fin == 0, "No", NA),
           DIAB3 = ifelse(diab_fin == 1, "Yes", NA),
           DIAB = ifelse(!is.na(DIAB1), DIAB1, 
                      ifelse(!is.na(DIAB2), DIAB2,
                            ifelse(!is.na(DIAB3), DIAB3, NA)))) %>%
    select(-DIAB1, -DIAB2, -DIAB3)
table(diab_final$diab, useNA = "always")
table(diab_final$diab_fin, useNA = "always")
table(diab_final$DIAB, useNA = "always")
prop.table(table(diab_final$diab_fin))
prop.table(table(diab_final$DIAB))

# obesity
obesity_df <- baseline_data %>% select(person_id, starts_with("Other")) %>%
    mutate(o1 = ifelse(`Other: Other Conditions_1` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_1` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_1`), NA, 0))),
          o2 = ifelse(`Other: Other Conditions_2` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_2` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_2`), NA, 0))),
          o3 = ifelse(`Other: Other Conditions_3` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_3` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_3`), NA, 0))),
          o4 = ifelse(`Other: Other Conditions_4` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_4` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_4`), NA, 0))),
          o5 = ifelse(`Other: Other Conditions_5` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_5` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_5`), NA, 0))),
          o6 = ifelse(`Other: Other Conditions_6` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_6` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_6`), NA, 0))),
          o7 = ifelse(`Other: Other Conditions_7` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_7` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_7`), NA, 0))),
          o8 = ifelse(`Other: Other Conditions_8` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_8` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_8`), NA, 0))),
          o9 = ifelse(`Other: Other Conditions_9` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_9` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_9`), NA, 0))),
          o10 = ifelse(`Other: Other Conditions_10` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_10` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_10`), NA, 0))),
          o11 = ifelse(`Other: Other Conditions_11` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_11` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_11`), NA, 0))),
          o12 = ifelse(`Other: Other Conditions_12` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_12` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_12`), NA, 0))),
          o13 = ifelse(`Other: Other Conditions_13` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_13` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_13`), NA, 0))),
          o14 = ifelse(`Other: Other Conditions_14` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_14` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_14`), NA, 0))),
          o15 = ifelse(`Other: Other Conditions_15` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_15` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_15`), NA, 0))),
          o16 = ifelse(`Other: Other Conditions_16` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_16` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_16`), NA, 0))),
          o17 = ifelse(`Other: Other Conditions_17` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_17` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_17`), NA, 0))),
          o18 = ifelse(`Other: Other Conditions_18` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_18` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_18`), NA, 0))),
          o19 = ifelse(`Other: Other Conditions_19` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_19` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_19`), NA, 0))),
          o20 = ifelse(`Other: Other Conditions_20` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_20` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_20`), NA, 0))),
          o21 = ifelse(`Other: Other Conditions_21` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_21` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_21`), NA, 0))),
          o22 = ifelse(`Other: Other Conditions_22` %in% vec_obesity, 1,
                       ifelse(`Other: Other Conditions_22` %in% skip, NaN, 
                             ifelse(is.na(`Other: Other Conditions_22`), NA, 0)))) %>%
    rowwise() %>%
    mutate(obesity = ifelse(is.nan(o1), NaN,
                               ifelse(is.na(o1), NA,
                                     sum(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11,
                                         o12, o13, o14, o15, o16, o17, o18, o19,
                                         o20, o21, o22, na.rm = TRUE))))
head(obesity_df, 20)
# final dataframe for merging
obesity_final <- obesity_df %>% select(person_id, obesity) %>% 
    mutate(OBESITY1 = ifelse(is.nan(obesity), "Skip", NA),
           OBESITY2 = ifelse(obesity == 0, "No", NA),
           OBESITY3 = ifelse(obesity >= 1, "Yes", NA),
           OBESITY = ifelse(!is.na(OBESITY1), OBESITY1, 
                      ifelse(!is.na(OBESITY2), OBESITY2,
                            ifelse(!is.na(OBESITY3), OBESITY3, NA)))) %>%
    select(-OBESITY1, -OBESITY2, -OBESITY3)
table(obesity_final$obesity, useNA = "always")
table(obesity_final$OBESITY, useNA = "always")
prop.table(table(obesity_final$obesity))
prop.table(table(obesity_final$OBESITY))

# ckd
ckd_df <- baseline_data %>% select(person_id, starts_with("Kidney")) %>%
    mutate(k1 = ifelse(`Kidney: Kidney Conditions_1` %in% vec_ckd, 1,
                       ifelse(`Kidney: Kidney Conditions_1` %in% skip, NaN, 
                             ifelse(is.na(`Kidney: Kidney Conditions_1`), NA, 0))),
          k2 = ifelse(`Kidney: Kidney Conditions_2` %in% vec_ckd, 1,
                       ifelse(`Kidney: Kidney Conditions_2` %in% skip, NaN, 
                             ifelse(is.na(`Kidney: Kidney Conditions_2`), NA, 0))),
          k3 = ifelse(`Kidney: Kidney Conditions_3` %in% vec_ckd, 1,
                       ifelse(`Kidney: Kidney Conditions_3` %in% skip, NaN, 
                             ifelse(is.na(`Kidney: Kidney Conditions_3`), NA, 0))),
          k4 = ifelse(`Kidney: Kidney Conditions_4` %in% vec_ckd, 1,
                       ifelse(`Kidney: Kidney Conditions_4` %in% skip, NaN, 
                             ifelse(is.na(`Kidney: Kidney Conditions_4`), NA, 0))),
          k5 = ifelse(`Kidney: Kidney Conditions_5` %in% vec_ckd, 1,
                       ifelse(`Kidney: Kidney Conditions_5` %in% skip, NaN, 
                             ifelse(is.na(`Kidney: Kidney Conditions_5`), NA, 0))),
          k6 = ifelse(`Kidney: Kidney Conditions_6` %in% vec_ckd, 1,
                       ifelse(`Kidney: Kidney Conditions_6` %in% skip, NaN, 
                             ifelse(is.na(`Kidney: Kidney Conditions_6`), NA, 0))),
          k7 = ifelse(`Kidney: Kidney Conditions_7` %in% vec_ckd, 1,
                       ifelse(`Kidney: Kidney Conditions_7` %in% skip, NaN, 
                             ifelse(is.na(`Kidney: Kidney Conditions_7`), NA, 0))),
          k8 = ifelse(`Kidney: Kidney Conditions_8` %in% vec_ckd, 1,
                       ifelse(`Kidney: Kidney Conditions_8` %in% skip, NaN, 
                             ifelse(is.na(`Kidney: Kidney Conditions_8`), NA, 0))),
          k9 = ifelse(`Kidney: Kidney Conditions_9` %in% vec_ckd, 1,
                       ifelse(`Kidney: Kidney Conditions_9` %in% skip, NaN, 
                             ifelse(is.na(`Kidney: Kidney Conditions_9`), NA, 0))),
          k10 = ifelse(`Kidney: Kidney Conditions_10` %in% vec_ckd, 1,
                       ifelse(`Kidney: Kidney Conditions_10` %in% skip, NaN, 
                             ifelse(is.na(`Kidney: Kidney Conditions_10`), NA, 0)))) %>%
    rowwise() %>%
    mutate(ckd = ifelse(is.nan(k1), NaN,
                               ifelse(is.na(k1), NA,
                                     sum(k1, k2, k3, k4, k5,
                                         k6, k7, k8, k9, k10,na.rm = TRUE))))
head(ckd_df, 20)
# final dataframe for merging
ckd_final <- ckd_df %>% select(person_id, ckd) %>% 
    mutate(ckd_fin = ifelse(ckd >= 1, 1, ckd),
           CKD1 = ifelse(is.nan(ckd), "Skip", NA),
           CKD2 = ifelse(ckd_fin == 0, "No", NA),
           CKD3 = ifelse(ckd_fin == 1, "Yes", NA),
           CKD = ifelse(!is.na(CKD1), CKD1, 
                      ifelse(!is.na(CKD2), CKD2,
                            ifelse(!is.na(CKD3), CKD3, NA)))) %>%
    select(-CKD1, -CKD2, -CKD3)
table(ckd_final$ckd, useNA = "always")
table(ckd_final$ckd_fin, useNA = "always")
table(ckd_final$CKD, useNA = "always")
prop.table(table(ckd_final$ckd_fin))
prop.table(table(ckd_final$CKD))

# merge back with baseline data
baseline_data <- baseline_data %>% left_join(htn_final) %>%
    left_join(cad_final) %>% left_join(any_cancer_final) %>%
    left_join(skin_cancer_final) %>% left_join(lung_final) %>%
    left_join(diab_final) %>% left_join(obesity_final) %>%
    left_join(ckd_final)
names(baseline_data)

print("HTN")
table(baseline_data$HTN, useNA = "always")
prop.table(table(baseline_data$HTN))
print("CAD")
table(baseline_data$CAD, useNA = "always")
prop.table(table(baseline_data$CAD))
print("Any Cancer")
table(baseline_data$CANCER, useNA = "always")
prop.table(table(baseline_data$CANCER))
print("Skin Cancer")
table(baseline_data$SKINCANCER, useNA = "always")
prop.table(table(baseline_data$SKINCANCER))
print("Lung Disease")
table(baseline_data$LUNG, useNA = "always")
prop.table(table(baseline_data$LUNG))
print("Diabetes")
table(baseline_data$DIAB, useNA = "always")
prop.table(table(baseline_data$DIAB))
print("Obesity")
table(baseline_data$OBESITY, useNA = "always")
prop.table(table(baseline_data$OBESITY))
print("CKD")
table(baseline_data$CKD, useNA = "always")
prop.table(table(baseline_data$CKD))

# cleaned baseline variables
cleaned_baseline <- baseline_data %>% select(age:SeenAPP,
                                            HTN, CAD, CANCER, SKINCANCER, 
                                            LUNG, DIAB, OBESITY, CKD) %>%
                        mutate(outcome = ifelse(is.na(HTN) & is.na(CAD)
                                               & is.na(CANCER) & is.na(SKINCANCER)
                                               & is.na(LUNG) & is.na(DIAB)
                                               & is.na(OBESITY) & is.na(CKD), 0, 1))
names(cleaned_baseline)
table(cleaned_baseline$outcome)
nrow(cleaned_baseline)
Save(cleaned_baseline)
print("done")

Load(cleaned_baseline)
print("done")

table(round(cleaned_baseline$age), useNA = "always")
table(cut(round(cleaned_baseline$age), 
   breaks = c(18, 21, 24, 29, 34, 44, 54, 64, 74, 119),
         include.lowest = TRUE,
         labels = c("18-21", "22-24", "25-29", "30-34",
                   "35-44", "45-54", "55-64", "65-74", "75+")), useNA = "always")

# rows in original dataset
nrow(cleaned_baseline)
# make dataset for analysis
df <- cleaned_baseline %>%
    select(age, Sex, Gender, Orientation, Category,
           AnnIncome, Education, InsuranceTypeUpdate,
           MedFormConf, HealthMatAssist, HealthInfoDif,
           SeenDoc, SeenAPP,
           HTN:CKD, outcome) %>%
    filter(!is.na(Category) & !is.na(HealthInfoDif)) %>%
    mutate(Age = cut(round(age), 
                     breaks = c(18, 21, 24, 29, 34, 44, 54, 64, 74, 119),
                     include.lowest = TRUE,
                     labels = c("18-21", "22-24", "25-29", "30-34",
                   "35-44", "45-54", "55-64", "65-74", "75+")),
           Sex = ifelse(is.na(Sex), "Missing", Sex),
          InsuranceTypeUpdate = ifelse(is.na(InsuranceTypeUpdate), "Missing", InsuranceTypeUpdate),
          Sex = factor(Sex, levels = c("Missing", "Prefer Not To Answer", "Skip",
                                        "Female", "Male",
                                        "Sex At Birth None Of These",
                                        "Intersex")),
          Gender = factor(Gender, levels = c("Prefer Not To Answer", "Skip",
                                             "Woman", "Man",
                                             "Multiple", "Non Binary",
                                             "Transgender", "Additional Options")),
          Orientation = factor(Orientation, levels = c("Prefer Not To Answer", "Skip",
                                                       "Straight",
                                                      "Bisexual", 
                                                      "Gay", "None",
                                                       "Lesbian", "Multiple")),
          Category = factor(Category, levels = c("Asian", "Black", "Hispanic",
                                                "MENA", "Multiple", "NHPI",
                                                "White", "Prefer Not To Answer", "Skip",
                                                 "Race Ethnicity None Of These")),
          Education = factor(Education, levels = c("Prefer Not To Answer", "Skip",
                                                  "Never Attended", "One Through Four",
                                                  "Five Through Eight", "Nine Through Eleven",
                                                  "Twelve Or GED", "College One to Three",
                                                  "College Graduate", "Advanced Degree")),
          InsuranceTypeUpdate = ifelse(InsuranceTypeUpdate == "Indian",
                                      "Other Health Plan",
                                      InsuranceTypeUpdate),
          InsuranceTypeUpdate = factor(InsuranceTypeUpdate, levels = c("Missing", 
                                                                       "Skip",
                                                                       "Employer Or Union",
                                                                      "Medicaid",
                                                                      "Multiple",
                                                                      "Medicare",
                                                                      "Purchased",
                                                                      "Other Health Plan",
                                                                       "VA",
                                                                      "None",
                                                                      "Military")),
          MedFormConf = factor(MedFormConf, levels = c("Skip", 
                                                       "Not At All",
                                                       "A Little Bit",
                                                       "Somewhat",
                                                       "Quite A Bit",
                                                       "Extremely")),
           MFC = ifelse(MedFormConf == "Skip", NA, as.numeric(MedFormConf) - 1),
          HealthMatAssist = factor(HealthMatAssist, levels = c("Skip",
                                                               "Always",
                                                               "Often",
                                                               "Sometimes",
                                                               "Occasionally",
                                                               "Never")),
           HMA = ifelse(HealthMatAssist == "Skip", NA, as.numeric(HealthMatAssist) - 1),
          HealthInfoDif = factor(HealthInfoDif, levels = c("Skip",
                                                           "Always",
                                                           "Often",
                                                           "Sometimes",
                                                           "Occasionally",
                                                           "Never")),
           HID = ifelse(HealthInfoDif == "Skip", NA, as.numeric(HealthInfoDif) - 1),
          HealthLit = MFC + HMA + HID,
          HealthLit_Dich = ifelse(HealthLit > 9, "High", NA),
          HealthLit_Dich = ifelse(HealthLit <= 9, "Low", HealthLit_Dich), 
          SeenDoc = factor(SeenDoc, levels = c("Skip", "Dont Know", "No", "Yes")),
          SeenAPP = factor(SeenAPP, levels = c("Skip", "Dont Know", "No", "Yes")),
          SeenPCP = ifelse(SeenDoc == "Yes" | SeenAPP == "Yes", "Yes", NA),
          SeenPCP = ifelse((SeenDoc == "No" & (SeenAPP == "No"
                                              | SeenAPP == "Skip" 
                                              | SeenAPP == "Dont Know")) |
                           (SeenAPP == "No" & (SeenDoc == "No"
                                              | SeenDoc == "Skip" 
                                              | SeenDoc == "Dont Know")), "No", SeenPCP),
          SeenPCP = ifelse((SeenDoc == "Dont Know" & (SeenAPP == "Skip" 
                                              | SeenAPP == "Dont Know")) |
                           (SeenAPP == "Dont Know" & (SeenDoc == "Skip" 
                                              | SeenDoc == "Dont Know")), 
                           "Dont Know", SeenPCP),
          SeenPCP = ifelse(SeenDoc == "Skip" & SeenAPP == "Skip", "Skip", SeenPCP),
          SeenPCP = factor(SeenPCP, levels = c("Skip", "Dont Know", "No", "Yes")))
nrow(df)
head(df)
table(df$HealthLit_Dich, useNA = "always")
summary(df$HealthLit)

# graph age distribution by Category
# cut off at age = 80
age_plot <- df %>%
    select(age, Category) %>%
    mutate(age = ifelse(age > 80, 80, age)) %>%
    ggplot(aes(x = age, y = fct_rev(Category))) +
    stat_halfeye() +
    ylab("") +
    xlab("Age") +
    theme_classic() +
    theme(text = element_text(family = "Arial"), 
               axis.line = element_line(), 
               axis.text = element_text(family = "Arial", color = "black"),
               axis.ticks.y = element_blank(),
               axis.line.y = element_blank(),
               strip.background = element_rect(fill = "grey95"))
age_plot

# graph Health Literacy distribution by Category
hl_plot <- df %>%
    select(HealthLit, Category) %>%
    ggplot(aes(x = HealthLit, y = fct_rev(Category))) +
    stat_halfeye() +
    geom_vline(xintercept = 9, linetype = "dashed", color = "grey50") +
    annotate(geom = "text", x = 10, y = 0.75, label = "High") +
    annotate(geom = "text", x = 8, y = 0.75, label = "Low") +
    ylab("") +
    scale_x_continuous(name = "Brief Health Literacy Screen Score",
                     limits = c(3, 15),
                     breaks = c(3, 6, 9, 12, 15)) +
    theme_classic() +
    theme(text = element_text(family = "Arial"), 
               axis.line = element_line(), 
               axis.text = element_text(family = "Arial", color = "black"),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank(),
               axis.line.y = element_blank(),
               strip.background = element_rect(fill = "grey95"))
hl_plot

# combine
plot_grid(age_plot,
          hl_plot,
          nrow = 1,
          rel_widths = c(1.5, 1))
ggsave("./quant_fig.png", width = 10, height = 8)

# function for bar plot distribution by Category
bar_func_y <- function(var, title) {
    df %>%
    ggplot(aes(fill = .data[[var]], y = fct_rev(.data$Category))) +
    geom_bar(position = position_fill()) +
    scale_fill_viridis_d(na.value = "grey90") +
    ylab("") +
    xlab("Proportion") +
    ggtitle(title) +
    guides(fill = guide_legend(title = "",
                              nrow = 5)) +
    theme_classic() +
    theme(text = element_text(family = "Arial", size = 20), 
               axis.line = element_line(), 
               axis.text = element_text(family = "Arial", color = "black", size = 16),
               legend.text = element_text(size = 12),
               axis.text.y = element_text(angle = 30),
               axis.ticks.y = element_blank(),
               axis.line.y = element_blank(),
               legend.position = "bottom") +
    coord_cartesian(clip = "off")
}

bar_func <- function(var, title) {
    df %>%
    ggplot(aes(fill = .data[[var]], y = fct_rev(.data$Category))) +
    geom_bar(position = position_fill()) +
    scale_fill_viridis_d(na.value = "grey90") +
    ylab("") +
    xlab("Proportion") +
    ggtitle(title) +
    guides(fill = guide_legend(title = "",
                              nrow = 5)) +
    theme_classic() +
    theme(text = element_text(family = "Arial", size = 20), 
               axis.line = element_line(), 
               axis.text = element_text(family = "Arial", color = "black", size = 16),
               legend.text = element_text(size = 12),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank(),
               axis.line.y = element_blank(),
               legend.position = "bottom") +
    coord_cartesian(clip = "off")
}

# test with Age
bar_func_y("Age", "Age")

# Sex
bar_func_y("Sex", "Sex")

# Gender
bar_func_y("Gender", "Gender")

# Orientation
bar_func_y("Orientation", "Sexual Orientation")

# Income
bar_func_y("AnnIncome", "Annual Income")

# Education
bar_func_y("Education", "Education")

# Insurance
bar_func_y("InsuranceTypeUpdate", "Health Insurance Type")

# MedFormConf
bar_func_y("MedFormConf", "Medical Form Confidence")

# HealthMatAssist
bar_func_y("HealthMatAssist", "Health Material Assistance")

# HealthInfoDif
bar_func_y("HealthInfoDif", "Health Information Difficulty")

# Seen doctor for primary care
bar_func_y("SeenDoc", "Doctor for Primary Care")

# Seen app for primary care
bar_func_y("SeenAPP", "APP for Primary Care")

# Seen anyone for primary care
bar_func_y("SeenPCP", "Has PCP") 

pcp_plot <- df %>%
    ggplot(aes(fill = SeenPCP, y = fct_rev(.data$Category))) +
    geom_bar(position = position_fill()) +
    scale_fill_viridis_d(na.value = "grey90",
                              labels = c("Skip", "Don't Know",
                                         "No", "Yes", "Missing")) +
    ylab("") +
    xlab("Proportion") +
    ggtitle("F) PCP") +
    guides(fill = guide_legend(title = "",
                              nrow = 5)) +
    theme_classic() +
    theme(text = element_text(family = "Arial", size = 20), 
               axis.line = element_line(), 
               axis.text = element_text(family = "Arial", color = "black", size = 16),
               legend.text = element_text(size = 12),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank(),
               axis.line.y = element_blank(),
               legend.position = "bottom") +
    coord_cartesian(clip = "off") 
pcp_plot

# Dichotomized Health Literacy
hl_dich_plot <- df %>%
    ggplot(aes(fill = HealthLit_Dich, y = fct_rev(.data$Category))) +
    geom_bar(position = position_fill()) +
    scale_fill_viridis_d(na.value = "grey90",
                              labels = c("High", "Low",
                                         "Missing")) +
    ylab("") +
    xlab("Proportion") +
    ggtitle("F) Health Literacy") +
    guides(fill = guide_legend(title = "",
                              nrow = 5)) +
    theme_classic() +
    theme(text = element_text(family = "Arial", size = 20), 
               axis.line = element_line(), 
               axis.text = element_text(family = "Arial", color = "black", size = 16),
               legend.text = element_text(size = 12),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank(),
               axis.line.y = element_blank(),
               legend.position = "bottom") +
    coord_cartesian(clip = "off") 
hl_dich_plot

plot_grid(bar_func_y("Age", "A) Age"),
          bar_func("Gender", "B) Gender"),
          bar_func("AnnIncome", "C) Annual Income"),
          bar_func_y("InsuranceTypeUpdate", "D) Insurance"),
          bar_func("Education", "E) Education"),
          pcp_plot,
          nrow = 2,
          rel_widths = c(1.5, 1, 1))
ggsave("./fig1.png", width = 20, height = 10)

plot_grid(bar_func_y("Sex", "A) Sex"),
          bar_func("Orientation", "B) Sexual Orientation"),
          bar_func("MedFormConf", "C) Medical Form Confidence"),
          bar_func_y("HealthMatAssist", "D) Health Material Assistance"),
          bar_func("HealthInfoDif", "E) Health Information Difficulty"),
          hl_dich_plot,
          nrow = 2,
          rel_widths = c(1.5, 1, 1))
ggsave("./supfig1.png", width = 20, height = 10)

# start by generating a propensity score for whether or not each person has an outcome
# then generate stabalized inverse probability weights
mod_res <- glm(outcome ~ rcs(age, 5) + Sex + Gender + Orientation + Category +
               AnnIncome + Education + InsuranceTypeUpdate + 
               MedFormConf + HealthMatAssist + HealthInfoDif,
               family = binomial,
               data = df)

# add propensity score to dataframe
df$ps <- mod_res$fitted.values
print("done")

# plot it
ggplot(df, aes(x = ps, fill = factor(outcome))) +
    geom_histogram(bins = 100) +
    ggtitle("") +
    ylab("Count") +
    xlab("Probability (propensity score)") +
    xlim(0, 1) +
    scale_fill_manual(name = "Personal Medical History Instrument Completed", 
                      labels = c("No", "Yes"),
                      values = c("#2f9aa0FF", "#541352FF")) +
    theme_classic() +
    theme(text = element_text(family = "Arial", size = 12), 
               axis.line = element_line(), 
               axis.text = element_text(family = "Arial", color = "black", size = 10),
               legend.text = element_text(size = 8),
               axis.text.y = element_blank(),
               axis.ticks.y = element_blank(),
               axis.line.y = element_blank(),
               legend.position = "bottom")
# save it
ggsave("./fig_ps_dist.png", width = 6, height = 4)

# create ate weights
df$wt <- 1/df$ps

# compare weighted population size to full population size
print("full population")
nrow(df)
print("weighted population")
round(sum(df %>% filter(outcome == 1) %>% pull(wt)))

# save and load df
#Save(df)
Load(df)
names(df)
print("done")

# create a table with the orginal table 1 and the weighted population
tab1_original <- tbl_summary(df %>% select(age:HealthInfoDif, HealthLit, HealthLit_Dich, SeenPCP, HTN:outcome), by = outcome,
                            digits = everything() ~ 1) %>%
                    add_overall() %>% 
                    as_tibble()
tab1_original

# now create weighted table (no need to rake because of large sample size)
tab1_wt <- svydesign(~1, data = as.data.frame(df %>% 
                                              select(age:HealthInfoDif, HealthLit, HealthLit_Dich,
                                                     SeenPCP, HTN:outcome, wt) %>% 
                                              filter(outcome == 1)), 
                     weights = ~wt) %>%
    tbl_svysummary(digits = everything() ~ 1) %>% 
                    as_tibble()
tab1_wt

# now combine them into one table
# add two rows to tab1_orgiginal
tab1_fin <- tab1_original %>%
    add_row(`**Characteristic**` = "outcome",
           `**Overall**, N = 372,050` = NA,
           `**0**, N = 230,172` = NA,
           `**1**, N = 141,878` = NA) %>%
    add_row(`**Characteristic**` = "wt",
           `**Overall**, N = 372,050` = "1",
           `**0**, N = 230,172` = "1",
           `**1**, N = 141,878` = "1")
# add unknown rows after clinical things to tab1_wt
tab1_wt_fin <- tab1_wt %>%
    add_row(`**Characteristic**` = "Unknown",
           `**N = 378,825**` = "0",
           .before = 111) %>%
    add_row(`**Characteristic**` = "Unknown",
           `**N = 378,825**` = "0",
           .before = 116) %>%
    add_row(`**Characteristic**` = "Unknown",
           `**N = 378,825**` = "0",
           .before = 121) %>%
    add_row(`**Characteristic**` = "Unknown",
           `**N = 378,825**` = "0",
           .before = 126) %>%
    add_row(`**Characteristic**` = "Unknown",
           `**N = 378,825**` = "0",
           .before = 131) %>%
    add_row(`**Characteristic**` = "Unknown",
           `**N = 378,825**` = "0",
           .before = 136) %>%
    add_row(`**Characteristic**` = "Unknown",
           `**N = 378,825**` = "0",
           .before = 141) %>%
    add_row(`**Characteristic**` = "Unknown",
           `**N = 378,825**` = "0",
           .before = 146)
tab1_fin <- tab1_fin %>%
    add_column(`**N = 378,825**` = tab1_wt_fin$`**N = 378,825**`)
tab1_fin

# export
write_csv(tab1_fin, file = "./tab1_fin.csv")
print("done")

# HTN with weighting
# make svydesign object
d_htn <- svydesign(~1, data = as.data.frame(df %>% filter(outcome == 1, HTN != "Skip") %>%
                                           mutate(HTN = ifelse(HTN == "Yes", 1, 0))), 
               weights = ~wt)
# model
htn_mod_wt <- svyglm(HTN ~ rcs(age, 5) + Sex + Gender + Orientation +
                     Category + rcs(HealthLit, 5) + Category:HealthLit +
                     AnnIncome + Education + InsuranceTypeUpdate + SeenPCP,
                     design = d_htn,
                     family = binomial)
print("raw n")
nrow(htn_mod_wt$model)
print("weighted n")
round(sum(htn_mod_wt$survey.design$variables$wt))
print("outcomes")
sum(htn_mod_wt$model$HTN)
print("df")
htn_mod_wt$rank
print("aic")
round(htn_mod_wt$aic, 2)

# create adequacy table from htn_mod_wt with ranks

# start with printing LRT for all variables
regTermTest(htn_mod_wt, ~ rcs(age, 5) + Sex + Gender + Orientation + Category + rcs(HealthLit, 5) +
              AnnIncome + Education + InsuranceTypeUpdate + SeenPCP,
                   method = "LRT")
# now print LRT for each variable
regTermTest(htn_mod_wt, ~ rcs(age, 5),
                   method = "LRT")
regTermTest(htn_mod_wt, ~ Sex,
                   method = "LRT")
regTermTest(htn_mod_wt, ~ Gender,
                   method = "LRT")
regTermTest(htn_mod_wt, ~ Orientation,
                   method = "LRT")
regTermTest(htn_mod_wt, ~ Category,
                   method = "LRT")
regTermTest(htn_mod_wt, ~ rcs(HealthLit, 5),
                   method = "LRT")
regTermTest(htn_mod_wt, ~ Category:HealthLit,
                   method = "LRT")
regTermTest(htn_mod_wt, ~ AnnIncome,
                   method = "LRT")
regTermTest(htn_mod_wt, ~ Education,
                   method = "LRT")
regTermTest(htn_mod_wt, ~ InsuranceTypeUpdate,
                   method = "LRT")
regTermTest(htn_mod_wt, ~ SeenPCP,
                   method = "LRT")

# make table of all LRTs, Adequacy Index, and Rankings
htn_ai <- tibble(Variable = c("Age", "Sex", "Gender", "Orientation",
                              "Race/Ethnicity", "Health Literacy", "Race/Ethnicity:Health Literacy",
                              "Income", "Education", "Insurance", "PCP"),
                htn_LR_s = c(5490.345, 9.8342, 5.235704, 12.537,
                             17.98816, 0.4195928, 9.828441, 
                             323.1424, 128.245, 38.62211, 2869.303),
                htn_LR = 11862.6) %>%
    mutate(htn_A = htn_LR_s / htn_LR) %>%
    arrange(-htn_A) %>%
    rowid_to_column("htn_rank")
htn_ai

# CAD with weighting
# make svydesign object
d_cad <- svydesign(~1, data = as.data.frame(df %>% filter(outcome == 1, CAD != "Skip") %>%
                                           mutate(CAD = ifelse(CAD == "Yes", 1, 0))), 
               weights = ~wt)
# model
cad_mod_wt <- svyglm(CAD ~ rcs(age, 5) + Sex + Gender + Orientation +
                     Category + rcs(HealthLit, 5) + Category:HealthLit +
                     AnnIncome + Education + InsuranceTypeUpdate + SeenPCP,
                     design = d_cad,
                     family = binomial)
print("raw n")
nrow(cad_mod_wt$model)
print("weighted n")
round(sum(cad_mod_wt$survey.design$variables$wt))
print("outcomes")
sum(cad_mod_wt$model$CAD)
print("df")
cad_mod_wt$rank
print("aic")
round(cad_mod_wt$aic, 2)

# create adequacy table from cad_mod_wt with ranks

# start with printing LRT for all variables
regTermTest(cad_mod_wt, ~ rcs(age, 5) + Sex + Gender + Orientation + Category + rcs(HealthLit, 5) +
              AnnIncome + Education + InsuranceTypeUpdate + SeenPCP,
                   method = "LRT")
# now print LRT for each variable
regTermTest(cad_mod_wt, ~ rcs(age, 5),
                   method = "LRT")
regTermTest(cad_mod_wt, ~ Sex,
                   method = "LRT")
regTermTest(cad_mod_wt, ~ Gender,
                   method = "LRT")
regTermTest(cad_mod_wt, ~ Orientation,
                   method = "LRT")
regTermTest(cad_mod_wt, ~ Category,
                   method = "LRT")
regTermTest(cad_mod_wt, ~ rcs(HealthLit, 5),
                   method = "LRT")
regTermTest(cad_mod_wt, ~ Category:HealthLit,
                   method = "LRT")
regTermTest(cad_mod_wt, ~ AnnIncome,
                   method = "LRT")
regTermTest(cad_mod_wt, ~ Education,
                   method = "LRT")
regTermTest(cad_mod_wt, ~ InsuranceTypeUpdate,
                   method = "LRT")
regTermTest(cad_mod_wt, ~ SeenPCP,
                   method = "LRT")

# make table of all LRTs, Adequacy Index, and Rankings
cad_ai <- tibble(Variable = c("Age", "Sex", "Gender", "Orientation",
                              "Race/Ethnicity", "Health Literacy", "Race/Ethnicity:Health Literacy",
                              "Income", "Education", "Insurance", "PCP"),
                cad_LR_s = c(1238.568, 12.98387, 4.605069, 10.71794,
                             28.0299, 0.4391817, 17.86887, 
                             49.62517, 21.14111, 62.62619, 935.4918),
                cad_LR = 4240.078) %>%
    mutate(cad_A = cad_LR_s / cad_LR) %>%
    arrange(-cad_A) %>%
    rowid_to_column("cad_rank")
cad_ai

# CANCER with weighting
# make svydesign object
d_cancer <- svydesign(~1, data = as.data.frame(df %>% filter(outcome == 1, CANCER != "Skip") %>%
                                           mutate(CANCER = ifelse(CANCER == "Yes", 1, 0))), 
               weights = ~wt)
# model
cancer_mod_wt <- svyglm(CANCER ~ rcs(age, 5) + Sex + Gender + Orientation +
                     Category + rcs(HealthLit, 5) + Category:HealthLit +
                     AnnIncome + Education + InsuranceTypeUpdate + SeenPCP,
                     design = d_cancer,
                     family = binomial)
print("raw n")
nrow(cancer_mod_wt$model)
print("weighted n")
round(sum(cancer_mod_wt$survey.design$variables$wt))
print("outcomes")
sum(cancer_mod_wt$model$CANCER)
print("df")
cancer_mod_wt$rank
print("aic")
round(cancer_mod_wt$aic, 2)

# create adequacy table from cancer_mod_wt with ranks

# start with printing LRT for all variables
regTermTest(cancer_mod_wt, ~ rcs(age, 5) + Sex + Gender + Orientation + Category + rcs(HealthLit, 5) +
              AnnIncome + Education + InsuranceTypeUpdate + SeenPCP,
                   method = "LRT")
# now print LRT for each variable
regTermTest(cancer_mod_wt, ~ rcs(age, 5),
                   method = "LRT")
regTermTest(cancer_mod_wt, ~ Sex,
                   method = "LRT")
regTermTest(cancer_mod_wt, ~ Gender,
                   method = "LRT")
regTermTest(cancer_mod_wt, ~ Orientation,
                   method = "LRT")
regTermTest(cancer_mod_wt, ~ Category,
                   method = "LRT")
regTermTest(cancer_mod_wt, ~ rcs(HealthLit, 5),
                   method = "LRT")
regTermTest(cancer_mod_wt, ~ Category:HealthLit,
                   method = "LRT")
regTermTest(cancer_mod_wt, ~ AnnIncome,
                   method = "LRT")
regTermTest(cancer_mod_wt, ~ Education,
                   method = "LRT")
regTermTest(cancer_mod_wt, ~ InsuranceTypeUpdate,
                   method = "LRT")
regTermTest(cancer_mod_wt, ~ SeenPCP,
                   method = "LRT")

# make table of all LRTs, Adequacy Index, and Rankings
cancer_ai <- tibble(Variable = c("Age", "Sex", "Gender", "Orientation",
                              "Race/Ethnicity", "Health Literacy", "Race/Ethnicity:Health Literacy",
                              "Income", "Education", "Insurance", "PCP"),
                cancer_LR_s = c(6051.413, 9.934874, 5.731217, 7.670623, 
                                42.01917, 9.558477, 22.85209, 
                                36.94436, 113.7245, 52.11415, 1851.415),
                cancer_LR = 11815.55) %>%
    mutate(cancer_A = cancer_LR_s / cancer_LR) %>%
    arrange(-cancer_A) %>%
    rowid_to_column("cancer_rank")
cancer_ai

# SKIN CANCER with weighting
# make svydesign object
d_skincancer <- svydesign(~1, data = as.data.frame(df %>% filter(outcome == 1, SKINCANCER != "Skip") %>%
                                           mutate(SKINCANCER = ifelse(SKINCANCER == "Yes", 1, 0))), 
               weights = ~wt)
# model
skincancer_mod_wt <- svyglm(SKINCANCER ~ rcs(age, 5) + Sex + Gender + Orientation +
                     Category + rcs(HealthLit, 5) + Category:HealthLit +
                     AnnIncome + Education + InsuranceTypeUpdate + SeenPCP,
                     design = d_skincancer,
                     family = binomial)
print("raw n")
nrow(skincancer_mod_wt$model)
print("weighted n")
round(sum(skincancer_mod_wt$survey.design$variables$wt))
print("outcomes")
sum(skincancer_mod_wt$model$SKINCANCER)
print("df")
skincancer_mod_wt$rank
print("aic")
round(skincancer_mod_wt$aic, 2)

# create adequacy table from skincancer_mod_wt with ranks

# start with printing LRT for all variables
regTermTest(skincancer_mod_wt, ~ rcs(age, 5) + Sex + Gender + Orientation + Category + rcs(HealthLit, 5) +
              AnnIncome + Education + InsuranceTypeUpdate + SeenPCP,
                   method = "LRT")
# now print LRT for each variable
regTermTest(skincancer_mod_wt, ~ rcs(age, 5),
                   method = "LRT")
regTermTest(skincancer_mod_wt, ~ Sex,
                   method = "LRT")
regTermTest(skincancer_mod_wt, ~ Gender,
                   method = "LRT")
regTermTest(skincancer_mod_wt, ~ Orientation,
                   method = "LRT")
regTermTest(skincancer_mod_wt, ~ Category,
                   method = "LRT")
regTermTest(skincancer_mod_wt, ~ rcs(HealthLit, 5),
                   method = "LRT")
regTermTest(skincancer_mod_wt, ~ Category:HealthLit,
                   method = "LRT")
regTermTest(skincancer_mod_wt, ~ AnnIncome,
                   method = "LRT")
regTermTest(skincancer_mod_wt, ~ Education,
                   method = "LRT")
regTermTest(skincancer_mod_wt, ~ InsuranceTypeUpdate,
                   method = "LRT")
regTermTest(skincancer_mod_wt, ~ SeenPCP,
                   method = "LRT")

# make table of all LRTs, Adequacy Index, and Rankings
skincancer_ai <- tibble(Variable = c("Age", "Sex", "Gender", "Orientation",
                              "Race/Ethnicity", "Health Literacy", "Race/Ethnicity:Health Literacy",
                              "Income", "Education", "Insurance", "PCP"),
                skincancer_LR_s = c(4562.085, 5.677088, 9.493959, 22.22419,
                                    39.49814, 0.00014022, 14.8494, 
                                    79.48138, 57.28921, 18.64233, 1273.812),
                skincancer_LR = 7945.507) %>%
    mutate(skincancer_A = skincancer_LR_s / skincancer_LR) %>%
    arrange(-skincancer_A) %>%
    rowid_to_column("skincancer_rank")
skincancer_ai

# LUNG with weighting
# make svydesign object
d_lung <- svydesign(~1, data = as.data.frame(df %>% filter(outcome == 1, LUNG != "Skip") %>%
                                           mutate(LUNG = ifelse(LUNG == "Yes", 1, 0))), 
               weights = ~wt)
# model
lung_mod_wt <- svyglm(LUNG ~ rcs(age, 5) + Sex + Gender + Orientation +
                     Category + rcs(HealthLit, 5) + Category:HealthLit +
                     AnnIncome + Education + InsuranceTypeUpdate + SeenPCP,
                     design = d_lung,
                     family = binomial)
print("raw n")
nrow(lung_mod_wt$model)
print("weighted n")
round(sum(lung_mod_wt$survey.design$variables$wt))
print("outcomes")
sum(lung_mod_wt$model$LUNG)
print("df")
lung_mod_wt$rank
print("aic")
round(lung_mod_wt$aic, 2)

# create adequacy table from lung_mod_wt with ranks

# start with printing LRT for all variables
regTermTest(lung_mod_wt, ~ rcs(age, 5) + Sex + Gender + Orientation + Category + rcs(HealthLit, 5) +
              AnnIncome + Education + InsuranceTypeUpdate + SeenPCP,
                   method = "LRT")
# now print LRT for each variable
regTermTest(lung_mod_wt, ~ rcs(age, 5),
                   method = "LRT")
regTermTest(lung_mod_wt, ~ Sex,
                   method = "LRT")
regTermTest(lung_mod_wt, ~ Gender,
                   method = "LRT")
regTermTest(lung_mod_wt, ~ Orientation,
                   method = "LRT")
regTermTest(lung_mod_wt, ~ Category,
                   method = "LRT")
regTermTest(lung_mod_wt, ~ rcs(HealthLit, 5),
                   method = "LRT")
regTermTest(lung_mod_wt, ~ Category:HealthLit,
                   method = "LRT")
regTermTest(lung_mod_wt, ~ AnnIncome,
                   method = "LRT")
regTermTest(lung_mod_wt, ~ Education,
                   method = "LRT")
regTermTest(lung_mod_wt, ~ InsuranceTypeUpdate,
                   method = "LRT")
regTermTest(lung_mod_wt, ~ SeenPCP,
                   method = "LRT")

# make table of all LRTs, Adequacy Index, and Rankings
lung_ai <- tibble(Variable = c("Age", "Sex", "Gender", "Orientation",
                              "Race/Ethnicity", "Health Literacy", "Race/Ethnicity:Health Literacy",
                              "Income", "Education", "Insurance", "PCP"),
                lung_LR_s = c(79.82763, 7.812217, 6.062027, 88.2416,
                              45.24548, 0.01649466, 28.68682, 
                              155.9627, 86.33025, 166.6391, 3214.618),
                lung_LR = 5871.691) %>%
    mutate(lung_A = lung_LR_s / lung_LR) %>%
    arrange(-lung_A) %>%
    rowid_to_column("lung_rank")
lung_ai

# DIAB with weighting
# make svydesign object
d_diab <- svydesign(~1, data = as.data.frame(df %>% filter(outcome == 1, DIAB != "Skip") %>%
                                           mutate(DIAB = ifelse(DIAB == "Yes", 1, 0))), 
               weights = ~wt)
# model
diab_mod_wt <- svyglm(DIAB ~ rcs(age, 5) + Sex + Gender + Orientation +
                     Category + rcs(HealthLit, 5) + Category:HealthLit +
                     AnnIncome + Education + InsuranceTypeUpdate + SeenPCP,
                     design = d_diab,
                     family = binomial)
print("raw n")
nrow(diab_mod_wt$model)
print("weighted n")
round(sum(diab_mod_wt$survey.design$variables$wt))
print("outcomes")
sum(diab_mod_wt$model$DIAB)
print("df")
diab_mod_wt$rank
print("aic")
round(diab_mod_wt$aic, 2)

# create adequacy table from diab_mod_wt with ranks

# start with printing LRT for all variables
regTermTest(diab_mod_wt, ~ rcs(age, 5) + Sex + Gender + Orientation + Category + rcs(HealthLit, 5) +
              AnnIncome + Education + InsuranceTypeUpdate + SeenPCP,
                   method = "LRT")
# now print LRT for each variable
regTermTest(diab_mod_wt, ~ rcs(age, 5),
                   method = "LRT")
regTermTest(diab_mod_wt, ~ Sex,
                   method = "LRT")
regTermTest(diab_mod_wt, ~ Gender,
                   method = "LRT")
regTermTest(diab_mod_wt, ~ Orientation,
                   method = "LRT")
regTermTest(diab_mod_wt, ~ Category,
                   method = "LRT")
regTermTest(diab_mod_wt, ~ rcs(HealthLit, 5),
                   method = "LRT")
regTermTest(diab_mod_wt, ~ Category:HealthLit,
                   method = "LRT")
regTermTest(diab_mod_wt, ~ AnnIncome,
                   method = "LRT")
regTermTest(diab_mod_wt, ~ Education,
                   method = "LRT")
regTermTest(diab_mod_wt, ~ InsuranceTypeUpdate,
                   method = "LRT")
regTermTest(diab_mod_wt, ~ SeenPCP,
                   method = "LRT")

# make table of all LRTs, Adequacy Index, and Rankings
diab_ai <- tibble(Variable = c("Age", "Sex", "Gender", "Orientation",
                              "Race/Ethnicity", "Health Literacy", "Race/Ethnicity:Health Literacy",
                              "Income", "Education", "Insurance", "PCP"),
                diab_LR_s = c(1676.82, 4.222545, 10.77197, 8.039792, 
                              24.36788, 0.03410694, 50.51483, 
                              275.4606, 138.7056, 77.5465, 1427.503),
                diab_LR = 5736.609) %>%
    mutate(diab_A = diab_LR_s / diab_LR) %>%
    arrange(-diab_A) %>%
    rowid_to_column("diab_rank")
diab_ai

# OBESITY with weighting
# make svydesign object
d_obesity <- svydesign(~1, data = as.data.frame(df %>% filter(outcome == 1, OBESITY != "Skip") %>%
                                           mutate(OBESITY = ifelse(OBESITY == "Yes", 1, 0))), 
               weights = ~wt)
# model
obesity_mod_wt <- svyglm(OBESITY ~ rcs(age, 5) + Sex + Gender + Orientation +
                     Category + rcs(HealthLit, 5) + Category:HealthLit +
                     AnnIncome + Education + InsuranceTypeUpdate + SeenPCP,
                     design = d_obesity,
                     family = binomial)
print("raw n")
nrow(obesity_mod_wt$model)
print("weighted n")
round(sum(obesity_mod_wt$survey.design$variables$wt))
print("outcomes")
sum(obesity_mod_wt$model$OBESITY)
print("df")
obesity_mod_wt$rank
print("aic")
round(obesity_mod_wt$aic, 2)

# create adequacy table from obesity_mod_wt with ranks

# start with printing LRT for all variables
regTermTest(obesity_mod_wt, ~ rcs(age, 5) + Sex + Gender + Orientation + Category + rcs(HealthLit, 5) +
              AnnIncome + Education + InsuranceTypeUpdate + SeenPCP,
                   method = "LRT")
# now print LRT for each variable
regTermTest(obesity_mod_wt, ~ rcs(age, 5),
                   method = "LRT")
regTermTest(obesity_mod_wt, ~ Sex,
                   method = "LRT")
regTermTest(obesity_mod_wt, ~ Gender,
                   method = "LRT")
regTermTest(obesity_mod_wt, ~ Orientation,
                   method = "LRT")
regTermTest(obesity_mod_wt, ~ Category,
                   method = "LRT")
regTermTest(obesity_mod_wt, ~ rcs(HealthLit, 5),
                   method = "LRT")
regTermTest(obesity_mod_wt, ~ Category:HealthLit,
                   method = "LRT")
regTermTest(obesity_mod_wt, ~ AnnIncome,
                   method = "LRT")
regTermTest(obesity_mod_wt, ~ Education,
                   method = "LRT")
regTermTest(obesity_mod_wt, ~ InsuranceTypeUpdate,
                   method = "LRT")
regTermTest(obesity_mod_wt, ~ SeenPCP,
                   method = "LRT")

# make table of all LRTs, Adequacy Index, and Rankings
obesity_ai <- tibble(Variable = c("Age", "Sex", "Gender", "Orientation",
                              "Race/Ethnicity", "Health Literacy", "Race/Ethnicity:Health Literacy",
                              "Income", "Education", "Insurance", "PCP"),
                obesity_LR_s = c(1351.889, 14.30626, 7.148299, 46.21695,
                                 55.43028, 0.01671765, 59.69202, 
                                 668.8949, 163.5817, 75.37221, 3047.508),
                obesity_LR = 6434.681) %>%
    mutate(obesity_A = obesity_LR_s / obesity_LR) %>%
    arrange(-obesity_A) %>%
    rowid_to_column("obesity_rank")
obesity_ai

# CKD with weighting
# make svydesign object
d_ckd <- svydesign(~1, data = as.data.frame(df %>% filter(outcome == 1, CKD != "Skip") %>%
                                           mutate(CKD = ifelse(CKD == "Yes", 1, 0))), 
               weights = ~wt)
# model
ckd_mod_wt <- svyglm(CKD ~ rcs(age, 5) + Sex + Gender + Orientation +
                     Category + rcs(HealthLit, 5) + Category:HealthLit +
                     AnnIncome + Education + InsuranceTypeUpdate + SeenPCP,
                     design = d_ckd,
                     family = binomial)
print("raw n")
nrow(ckd_mod_wt$model)
print("weighted n")
round(sum(ckd_mod_wt$survey.design$variables$wt))
print("outcomes")
sum(ckd_mod_wt$model$CKD)
print("df")
ckd_mod_wt$rank
print("aic")
round(ckd_mod_wt$aic, 2)

# create adequacy table from ckd_mod_wt with ranks

# start with printing LRT for all variables
regTermTest(ckd_mod_wt, ~ rcs(age, 5) + Sex + Gender + Orientation + Category + rcs(HealthLit, 5) +
              AnnIncome + Education + InsuranceTypeUpdate + SeenPCP,
                   method = "LRT")
# now print LRT for each variable
regTermTest(ckd_mod_wt, ~ rcs(age, 5),
                   method = "LRT")
regTermTest(ckd_mod_wt, ~ Sex,
                   method = "LRT")
regTermTest(ckd_mod_wt, ~ Gender,
                   method = "LRT")
regTermTest(ckd_mod_wt, ~ Orientation,
                   method = "LRT")
regTermTest(ckd_mod_wt, ~ Category,
                   method = "LRT")
regTermTest(ckd_mod_wt, ~ rcs(HealthLit, 5),
                   method = "LRT")
regTermTest(ckd_mod_wt, ~ Category:HealthLit,
                   method = "LRT")
regTermTest(ckd_mod_wt, ~ AnnIncome,
                   method = "LRT")
regTermTest(ckd_mod_wt, ~ Education,
                   method = "LRT")
regTermTest(ckd_mod_wt, ~ InsuranceTypeUpdate,
                   method = "LRT")
regTermTest(ckd_mod_wt, ~ SeenPCP,
                   method = "LRT")

# make table of all LRTs, Adequacy Index, and Rankings
ckd_ai <- tibble(Variable = c("Age", "Sex", "Gender", "Orientation",
                              "Race/Ethnicity", "Health Literacy", "Race/Ethnicity:Health Literacy",
                              "Income", "Education", "Insurance", "PCP"),
                ckd_LR_s = c(276.4599, 8.249658, 24.02287, 4.489094,
                             19.83719, 6.297869, 18.83068, 
                             114.7761, 11.53398, 100.1868, 578.8346),
                ckd_LR = 1859.223) %>%
    mutate(ckd_A = ckd_LR_s / ckd_LR) %>%
    arrange(-ckd_A) %>%
    rowid_to_column("ckd_rank")
ckd_ai

# combine them all into one large anova table
ai_fin <- htn_ai %>%
    left_join(cad_ai) %>%
    left_join(cancer_ai) %>%
    left_join(skincancer_ai) %>%
    left_join(lung_ai) %>%
    left_join(diab_ai) %>%
    left_join(obesity_ai) %>%
    left_join(ckd_ai)
ai_fin

# figure out order...
ai_fin %>%
    select(Variable, ends_with("rank")) %>%
    pivot_longer(!Variable, names_to = "Disease", values_to = "Rank") %>%
    group_by(Variable) %>% summarise(MeanRank = mean(Rank)) %>%
    arrange(MeanRank)

# make fig2 as heatmap
ai_fin %>%
    select(Variable, ends_with("rank")) %>%
    pivot_longer(!Variable, names_to = "Disease", values_to = "Rank") %>%
    mutate(Disease = factor(Disease, levels = c("htn_rank",
                                               "cad_rank",
                                               "cancer_rank",
                                               "skincancer_rank",
                                               "lung_rank",
                                               "diab_rank",
                                               "obesity_rank",
                                               "ckd_rank"),
                           labels = c("Hypertension", "Coronary Artery Disease", "Any Cancer",
                                     "Skin Cancer", "Lung Disease",
                                     "Diabetes", "Obesity", "Chronic Kidney Disease")),
          terms = factor(Variable, levels = c("Age",
                                          "PCP",
                                          "Income",
                                          "Education",
                                          "Insurance",
                                          "Race/Ethnicity",
                                          "Race/Ethnicity:Health Literacy",
                                          "Orientation",
                                          "Sex",
                                          "Gender",
                                          "Health Literacy"),
                        labels = c("Age",
                                          "Primary Care Practitioner",
                                          "Income",
                                          "Education",
                                          "Insurance",
                                          "Race/Ethnicity",
                                          "Race/Ethnicity:Health Literacy",
                                          "Sexual Orientation",
                                          "Sex",
                                          "Gender",
                                          "Health Literacy"))) %>%
    ggplot(aes(x = Disease, y = fct_rev(terms), fill = Rank, label = Rank)) +
    geom_tile() +
    geom_text(aes(color = factor(ifelse(Rank < 5, 1, 0))), fontface = "bold") +
    ylab("") +
    xlab("") +
    ggtitle("") +
    scale_fill_viridis_c(direction = 1, limits = c(1, 11), guide = "none") +
    scale_color_manual(values = c("black", "white"), guide = "none") +
    theme_classic() +
    theme(text = element_text(family = "Arial", size = 12),
          axis.line = element_line(), 
          axis.text = element_text(family = "Arial", color = "black", size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 8),
          legend.position = "bottom")
# save it
ggsave("./fig2.png", width = 6, height = 6)

names(df)

# make table 2 - category with full dataset
theme_gtsummary_language("en", big.mark = "")
suptab2 <- tbl_summary(df %>% select(age:HealthInfoDif, HealthLit, HealthLit_Dich, SeenPCP, HTN:CKD), by = Category,
                            digits = everything() ~ 1) %>%
                    as_tibble()
suptab2

# specifically print out health literacy rows prior to suppressing cells with <20
suptab2 %>% filter(`**Characteristic**` == "HealthLit")

# now hide any cells with n < 20
#function to determine whether or not to change to "—"
func_hide <- function(cell){
    ifelse(as.numeric(str_extract(cell, "\\d+")) < 21, "—", cell)
}

suptab2_out <- suptab2 %>% mutate_all(func_hide) %>%
    mutate(`**Characteristic**` = suptab2$`**Characteristic**`)
suptab2_out

# export
write_csv(suptab2_out, file = "./suptab2.csv")
print("done")

# now examine breakdown of multiple categories
table(cleaned_baseline$Category)
table(cleaned_baseline$MultCategory)
# now making table manually from below output

# now look at lung outcomes by insurance status
table(cleaned_baseline$InsuranceTypeUpdate, cleaned_baseline$LUNG)
tbl_summary(df %>% select(InsuranceTypeUpdate, LUNG), by = InsuranceTypeUpdate,
                            digits = everything() ~ 1) %>%
                    as_tibble()

# now look at kidney outcomes by gender
table(cleaned_baseline$Gender, cleaned_baseline$CKD)
tbl_summary(df %>% select(Gender, CKD), by = Gender,
                            digits = everything() ~ 1) %>%
                    as_tibble()
