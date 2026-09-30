# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This script cleans the raw data downloaded from Qualtrics.
# Because the downloaded data contains PII, we do not include 
# them in the replication archive. This script writes
# deepfake_00.RData, which is contained in the replication archive 
# and includes comprehensive anonymous data from the experiment.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Make data from deepfake surveys.
# 
# Author: Soubhik Barari
# 
# Runtime: ~10 seconds
# 
# Instructions:
# 1. Set `dfsurv_id` to a desired Qualtrics survey ID 
# 2. Run script
# 3. Repeat 1-2 until all survey outputs appended to `datlist`
#
# Input:
# - CSVs from both survey waves (raw_data_from_experiment/SV_0xlqWlOfO10wuYl.csv and raw_data_from_experiment/SV_eyxdeXOuISXzakt.csv
# - supplemental_data/respondent_demographics.RData
# - supplemental_data/id_location.csv
#
# Output:
# - intermediate/deepfake_00.RData:
# - Saved cleaned `dat` output and raw `dfsurvdat` output
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#####------------------------------------------------------#
##### Pre-amble ####
#####------------------------------------------------------#

library(tidyverse)
library(optparse)
library(caret)
options(readr.show_col_types = FALSE)

select <- dplyr::select

#####------------------------------------------------------#
##### Settings ####
#####------------------------------------------------------#

arg_list <- list(
    make_option(c("--idtask_NA_threshold"), type="numeric", default=999, 
        help="Number of missing (NA) responses in ID task for a respondent to be dropped.",
        metavar="idtask_NA_threshold"),
    make_option(c("--save"), type="numeric", default=1,
                help="Whether or not to save.",
                metavar="save")
)
ARGS <- parse_args(OptionParser(option_list=arg_list))
SAVE <- TRUE 

cat("ID task NA threshold=", ARGS$idtask_NA_threshold,"\n")

dfsurv_ids <- c(
    "SV_0xlqWlOfO10wuYl",
    "SV_eyxdeXOuISXzakt"
)

id_vars <- c(
    "IPAddress",
    "RecipientFirstName",
    "RecipientLastName",
    "LocationLatitude",
    "LocationLongitude",
    "RecordedDate",
    "ExternalReference",
    "RecipientEmail",
    "rid"
)

#####------------------------------------------------------#
##### Merge raw survey results ####
#####------------------------------------------------------#

surveylist <- list('SV_0xlqWlOfO10wuYl' = read_csv('raw_data_from_experiment/SV_0xlqWlOfO10wuYl.csv'),
                   'SV_eyxdeXOuISXzakt' = read_csv('raw_data_from_experiment/SV_eyxdeXOuISXzakt.csv'))

dfsurvdat <- data.frame()
for (dfsurv_id in dfsurv_ids) {
 dfsurvdat_id <- surveylist[[dfsurv_id]]
 dfsurvdat_id <- dfsurvdat_id[3:nrow(dfsurvdat_id),]
 dfsurvdat_id$response_wave_ID <- dfsurv_id
 dfsurvdat <- bind_rows(dfsurvdat, dfsurvdat_id)
}

## coalesce columns that don't auto-merge
dup_cols <- colnames(dfsurvdat)[grepl("\\.\\..*$",colnames(dfsurvdat))]
orig_cols <- gsub("\\.\\..*","",dup_cols)
orig_cols <- unique(orig_cols)
for (orig_col_c in orig_cols){
 dup_cols_c <- colnames(dfsurvdat)[grepl(paste0("^",orig_col_c),colnames(dfsurvdat))]
 dup_cols_X <- dfsurvdat[dup_cols_c]
 dfsurvdat[dup_cols_c] <- NULL
 dfsurvdat[[orig_col_c]] <- coalesce(!!!dup_cols_X)
}

#####------------------------------------------------------#
##### Filter out obvious flotsam ####
#####------------------------------------------------------#

cat("Number of responses =",nrow(dfsurvdat),"\n")
stopifnot(nrow(dfsurvdat) == 17501)
table(dfsurvdat$response_wave_ID)

id_locations <- read_csv("supplemental_data/id_location.csv")

## drop non-Americans (which shouldn't be in the sample) and tests
IP_blacklist <- c("128.252.199.225") ## Christopher's IP
outofUS <- sapply(id_locations$location, function(s) {
   ss <- strsplit(s, split=",")[[1]]
   if (length(ss) == 1 | all(is.na(ss))) {
       return(FALSE)
   } else {
       if (any(grepl("United States", ss))) {
           return(FALSE)
       } else {
           return(TRUE)
       }
   }
})

table(outofUS)
IP_blacklist <- c(IP_blacklist, id_locations$IPAddress[outofUS])
dfsurvdat <- dfsurvdat[!(dfsurvdat$IPAddress %in% IP_blacklist),]
cat("\n ++ respondents in the United States =",nrow(dfsurvdat),"\n")
stopifnot(nrow(dfsurvdat) == 17165)

## consent
dfsurvdat <- dfsurvdat %>%
  filter(consent == "Yes" & (is.na(age) | !(as.numeric(age) <= 18)))
cat("\n ++ .. and also consenting =",nrow(dfsurvdat),"\n")
stopifnot(nrow(dfsurvdat) == 16430)

## anonymize raw data
dfsurvdat <- dfsurvdat[!(colnames(dfsurvdat) %in% id_vars)]

table(dfsurvdat$response_wave_ID, useNA="always")

#####------------------------------------------------------#
##### Measure and examine attention checks ####
#####------------------------------------------------------#

## attention check (front-end)
failed1 <- !(dfsurvdat$easy%in%"Quick and easy")|!(dfsurvdat$wikihow%in%"wikiHow")|!(dfsurvdat$careful%in%"I have a question") 
failed2 <- !(dfsurvdat$bothand%in%'Extremely interested,Slightly interested') 
table(!failed1 & !failed2)
dfsurvdat$quality_failed_frontend_attncheck <- as.numeric(failed1|failed2)
cat(" ++ ...and passing front-end attn check =",sum(!dfsurvdat$quality_failed_frontend_attncheck),"\n")
stopifnot(sum(!dfsurvdat$quality_failed_frontend_attncheck) == 5816)

dfsurvdat <- dfsurvdat[!dfsurvdat$quality_failed_frontend_attncheck,]

## attention check (back-end)
failed3 <- dfsurvdat$attn_check1 != "blue"
failed4 <- dfsurvdat$attn_check2 != 2
table(!failed3 & !failed4)
dfsurvdat$quality_failed_backend_attncheck <- as.numeric(failed3|failed4)

## pre-treatment survey time
crt_time_mins <- (
  as.numeric(dfsurvdat$`pre_crt_1_timer_Page Submit`) +
  as.numeric(dfsurvdat$`pre_crt_2_timer_Page Submit`) +
  as.numeric(dfsurvdat$`pre_crt_3_timer_Page Submit`)
)/(60*3)

timeq <- quantile(crt_time_mins[!dfsurvdat$quality_failed_frontend_attncheck], probs=seq(0,100,5)/100, na.rm=T)
timeq[c(2,length(timeq))]
tooquick <- crt_time_mins < 1
tooslow <- crt_time_mins > 30
table(!tooquick & !tooslow)
dfsurvdat$quality_pretreat_duration_tooquick <- as.numeric(tooquick)
dfsurvdat$quality_pretreat_duration_tooslow <- as.numeric(tooslow)

#####------------------------------------------------------#
##### Did randomization work? ####
#####------------------------------------------------------#

dfsurvdat$treat_control    <- as.numeric(dfsurvdat$FL_92_DO == 'Experimentalstimulus:control')
dfsurvdat$treat_attackad   <- as.numeric(dfsurvdat$FL_92_DO == 'Experimentalstimulus:video_attack')
dfsurvdat$treat_fake_text  <- as.numeric(grepl('Experimentalstimulus:text', dfsurvdat$FL_125_DO))
dfsurvdat$treat_fake_audio <- as.numeric(grepl('Experimentalstimulus:audio', dfsurvdat$FL_111_DO))
dfsurvdat$treat_fake_video <- as.numeric(grepl('Experimentalstimulus:video', dfsurvdat$FL_124_DO))
dfsurvdat$treat_skit       <- as.numeric(grepl('skit', dfsurvdat$FL_129_DO))

dfsurvdat$treat <- factor(ifelse(
      dfsurvdat$treat_fake_text, "text", ifelse(
          dfsurvdat$treat_fake_audio, "audio", ifelse(
              dfsurvdat$treat_fake_video, "video", ifelse(
                  dfsurvdat$treat_attackad, "ad", ifelse(
                      dfsurvdat$treat_skit, "skit", ifelse(
                          dfsurvdat$treat_control, "control", NA
                      )
                  )
              )
          )
      )
), levels=c("control", "video", "audio", "text", "skit", "ad"))

###no evidence that randomization failed
table(dfsurvdat$treat)
chisq.test(table(dfsurvdat$treat), correct=FALSE)
binom.test(sum(dfsurvdat$treat=="video",na.rm=T), sum(dfsurvdat$treat=="video",na.rm=T)+sum(dfsurvdat$treat=="control",na.rm=T), p=0.5)
binom.test(sum(dfsurvdat$treat=="audio",na.rm=T), sum(dfsurvdat$treat=="audio",na.rm=T)+sum(dfsurvdat$treat=="control",na.rm=T), p=0.5)
table(dfsurvdat$treat, dfsurvdat$response_wave_ID)
prop.table(table(dfsurvdat$treat, dfsurvdat$response_wave_ID), 2)
table(is.na(dfsurvdat$treat), dfsurvdat$response_wave_ID)

#####------------------------------------------------------#
##### Merge with demographics, measure mismatch, anonymize ####
#####------------------------------------------------------#

load("supplemental_data/respondent_demographics.RData")

dem$rid <- tolower(dem$RID)
dfsurvdat <- dfsurvdat %>%
   left_join(id_locations, by = "ResponseId") %>%
   mutate(rid = tolower(rid))

dfsurvdat <- dfsurvdat %>% 
    left_join(dem %>%
                select(rid, Age, Gender, Zip, Region, Hispanic, Ethnicity, Education, Party, HHI), 
              by="rid")

## check whether demographic questions in survey line up with demographic variables Lucid provided,
## correct where there are obvious miscodings

###age
age_mismatch <- !is.na(dfsurvdat$age) & !is.na(dfsurvdat$Age) & (abs(as.numeric(dfsurvdat$age) - dfsurvdat$Age) > 10)
prop.table(table(age_mismatch)) ## <1%
table(dfsurvdat$age[age_mismatch], dfsurvdat$Age[age_mismatch])

###gender
dfsurvdat$gender[dfsurvdat$gender == 1] <- "Male"
dfsurvdat$gender[dfsurvdat$gender == 2] <- "Female"
gender_mismatch <- dfsurvdat$gender != dfsurvdat$Gender & !is.na(dfsurvdat$gender) & !is.na(dfsurvdat$Gender)
prop.table(table(gender_mismatch)) ## <1%
table(dfsurvdat$gender[gender_mismatch], dfsurvdat$Gender[gender_mismatch])

###PID
dfsurvdat$PID_presurvey <- dfsurvdat$Party
party_nonna <- !is.na(dfsurvdat$PID_main) & !is.na(dfsurvdat$PID_presurvey)
party_mismatch <- dfsurvdat$PID_main[party_nonna] != dfsurvdat$PID_presurvey[party_nonna]
prop.table(table(party_mismatch)) ## ~8% -- not a good screen of quality since PID can change
table(dfsurvdat$PID_main[party_mismatch], dfsurvdat$PID_presurvey[party_mismatch])
  
dfsurvdat$quality_demographic_mismatch <- 0
dfsurvdat$quality_demographic_mismatch[gender_mismatch|age_mismatch] <- 1 
dfsurvdat$quality <- dfsurvdat$quality_demographic_mismatch|
                     dfsurvdat$quality_pretreat_duration_tooquick|
                     dfsurvdat$quality_pretreat_duration_tooslow|
                     dfsurvdat$quality_failed_frontend_attncheck|
                     dfsurvdat$quality_failed_backend_attncheck
dfsurvdat$quality <- factor(
    ifelse(dfsurvdat$quality, 
           "failed >=1 quality screen", 
           "passed all quality screens"),
    levels=c("failed >=1 quality screen",
             "passed all quality screens")
)
table(dfsurvdat$quality)

## edit other demographic variables
dfsurvdat$Ethnicity[grepl("White", dfsurvdat$Ethnicity)] <- "White"
dfsurvdat$Ethnicity[grepl("Asian", dfsurvdat$Ethnicity)] <- "Asian"
dfsurvdat$Ethnicity[grepl("Black", dfsurvdat$Ethnicity)] <- "Black"
dfsurvdat$Ethnicity[grepl("Pacific Islander", dfsurvdat$Ethnicity)] <- "Other"
dfsurvdat$Ethnicity[grepl("Some other race", dfsurvdat$Ethnicity)] <- "Other"
dfsurvdat$Ethnicity[grepl("American Indian", dfsurvdat$Ethnicity)] <- "Other"
dfsurvdat$Ethnicity[dfsurvdat$Ethnicity=="Prefer not to answer"|is.na(dfsurvdat$Ethnicity)] <- NA
dfsurvdat$Hispanic[grepl("Yes", dfsurvdat$Hispanic)] <- "Hispanic"
dfsurvdat$Hispanic[grepl("No", dfsurvdat$Hispanic)] <- "Not Hispanic"
dfsurvdat$Hispanic[grepl("Prefer not to answer", dfsurvdat$Hispanic)] <- NA

dfsurvdat$HHI <- as.character(dfsurvdat$HHI)
dfsurvdat$HHI <- ifelse(is.na(dfsurvdat$HHI), "Prefer not to answer", dfsurvdat$HHI)

dfsurvdat$HHI <- fct_collapse(dfsurvdat$HHI,
  "N/A" = c("Prefer not to answer"),
  "<$25k"= c("Less than $5,000",
             "$5,000 to $9,999",
             "$10,000 to $14,999",
             "$15,000 to $19,999",
             "$20,000 to $24,999"),
  "$25k-$49k" = c("$25,000 to $29,999",
                  "$30,000 to $34,999",
                  "$35,000 to $39,999",
                  "$40,000 to $44,999",
                  "$45,000 to $49,999"),
  "$50k-$74k" = c("$50,000 to $54,999",
                  "$55,000 to $59,999",
                  "$60,000 to $64,999",
                  "$65,000 to $69,999",
                  "$70,000 to $74,999"),
  "$75k-$99k" = c("$75,000 to $79,999",
                  "$80,000 to $84,999",
                  "$85,000 to $89,999",
                  "$90,000 to $94,999",
                  "$95,000 to $99,999"),
  "$100k-$150k" = c("$100,000 to $124,999",
                    "$125,000 to $149,999"),
   ">$150k" = c("$150,000 to $174,999",
                "$175,000 to $199,999")
)

## merge in demographic data and anonymize
dat <- dfsurvdat[,c(
  "Zip", 
  "age", 
  "gender", 
  "educ",
  "HHI", 
  "PID_presurvey", 
  "Ethnicity",
  "Hispanic",
  "Region",
  "rid",
  "comments",
  "response_wave_ID",
  "StartDate",
  "EndDate",
  "quality", 
  "quality_demographic_mismatch", 
  "quality_failed_backend_attncheck",
  "quality_pretreat_duration_tooquick",
  "quality_pretreat_duration_tooslow"
)]
  

#####------------------------------------------------------#
##### ++ META ####
#####------------------------------------------------------#

dat$meta_OS <- dfsurvdat$`meta_Operating System`
dat$meta_OS <- ifelse(grepl("Android|iPhone|iPod|iPad", dat$meta_OS), "mobile", "desktop")
dat$meta_resolution <- dfsurvdat$meta_Resolution

quantile(as.numeric(gsub("x.*", "", dat$meta_resolution)),na.rm=T) # 320 # 414 # 1366 # 1536 # 3840
quantile(as.numeric(gsub(".*x", "", dat$meta_resolution)),na.rm=T) # 317 # 768 # 812 # 900 # 2160
  
dat$meta_resolution <- dfsurvdat$meta_Resolution
w <- as.numeric(gsub("x.*", "", dat$meta_resolution))
h <- as.numeric(gsub(".*x", "", dat$meta_resolution))
w_x_h <- w*h
w_x_h <- factor(as.numeric(cut(w_x_h, quantile(w_x_h,na.rm=T), include.lowest=TRUE)))
levels(w_x_h) <- c("XS","S","M","L")
  
dat$meta_screenres <- w_x_h
dat$duration_secs <- dfsurvdat$`Duration (in seconds)`
  
agree_lvls <- c(NA, 
                "Strongly disagree",
                "Somewhat disagree",
                "Neither agree nor disagree",
                "Somewhat agree",
                "Strongly agree")
  
#####------------------------------------------------------#
##### ++ AGE ####
#####------------------------------------------------------#

dat$agegroup <- as.numeric(dfsurvdat$Age)
dat$agegroup <- ifelse(
      dat$agegroup %in% 18:24, "18-24", ifelse(
          dat$agegroup %in% 25:34, "25-34", ifelse(
              dat$agegroup %in% 35:44, "35-44", ifelse(
                  dat$agegroup %in% 45:64, "45-64", ifelse(
                      dat$agegroup %in% 65:999, "65+", "N/A"
                  )
              )
          )
      )
)
dat$age_65 <- ifelse(dfsurvdat$Age > 60, ">65", "<=65")
table(dat$age_65)
  
#####------------------------------------------------------#
##### ++ EDUCATION ####
#####------------------------------------------------------#

dat$educ <- as.character(dfsurvdat$educ)
dat$educ <- ifelse(is.na(dat$educ), "N/A", dat$educ)
dat$educ <- ifelse(dat$educ=="Have not finished high school", "<High school", dat$educ)
dat$educ <- ifelse(dat$educ=="Postgraduate degree", "Postgraduate", dat$educ)

dat$educ <- factor(
      dat$educ,
      levels = c(
        "N/A", "<High school","High school","College","Postgraduate"
      )
)
table(dat$educ)
  
#####------------------------------------------------------#
##### ++ PARTISANSHIP ####
#####------------------------------------------------------#

dat$PID_main <- as.character(dfsurvdat$PID_main)
dat$PID_leaners <- as.character(dfsurvdat$PID_leaners)
dat$PID[dat$PID_main=="Democrat"|dat$PID_leaners=="Democrat"] <- "Democrat"
dat$PID[dat$PID_main=="Republican"|dat$PID_leaners=="Republican"] <- "Republican"
dat$PID[dat$PID_main=="Independent"&!(dat$PID_leaners %in% c("Democrat","Republican"))] <- "Independent"
dat$PID[is.na(dat$PID)] <- "N/A"
  
dat$PID <- factor(dat$PID, levels=c("N/A","Democrat","Independent","Republican"))
      
table(dat$PID)
table(dat$PID_leaners)
  
### sanity check: make sure no contradictory PIDs (should all be FALSE)
table(dfsurvdat$PID_main == "Democrat" & dfsurvdat$PID_leaners == "Republican")
table(dfsurvdat$PID_main == "Republican" & dfsurvdat$PID_leaners == "Democrat")
  
#####------------------------------------------------------#
##### ++ SEXISM ####
#####------------------------------------------------------#

dat$ambivalent_sexism_1 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_1, 
         levels=agree_lvls))
dat$ambivalent_sexism_2 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_2, 
         levels=agree_lvls))
dat$ambivalent_sexism_3 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_3, 
         levels=agree_lvls))
dat$ambivalent_sexism_4 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_4, 
         levels=agree_lvls))
dat$ambivalent_sexism_5 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_5,
        levels=agree_lvls))
dat$ambivalent_sexism <- rowMeans(dat[grepl("ambivalent_sexism", colnames(dat))], na.rm=T)
hist(dat$ambivalent_sexism) ###should show mean at 3
  
#####------------------------------------------------------#
##### ++ POLITICAL KNOWLEDGE ####
#####------------------------------------------------------#

dat$polknow_speaker   <- as.numeric(dfsurvdat$polknow_speaker == "Nancy Pelosi")
dat$polknow_medicare  <- as.numeric(dfsurvdat$polknow_medicare == "A program run by the US federal government to pay for old people’s health care")
dat$polknow_house     <- as.numeric(dfsurvdat$polknow_house == "Democrats")
dat$polknow_senate    <- as.numeric(dfsurvdat$polknow_senate == "Republicans")
dat$polknow_veto      <- as.numeric(dfsurvdat$polknow_veto == "Two-thirds")
dat$polknow_warren    <- as.numeric(dfsurvdat$polknow_warren == "Elizabeth Warren")
dat$polknow_boris     <- as.numeric(dfsurvdat$polknow_boris == "Boris Johnson")
dat$polknow           <- rowMeans(dat[grepl("polknow_", colnames(dat))], na.rm=T)
hist(dat$polknow)
  
#####------------------------------------------------------#
##### ++ MEDIA STIMULI ####
#####------------------------------------------------------#

dat$treat_control    <- as.numeric(dfsurvdat$FL_92_DO == 'Experimentalstimulus:control')
dat$treat_attackad   <- as.numeric(dfsurvdat$FL_92_DO == 'Experimentalstimulus:video_attack')
dat$treat_fake_text  <- as.numeric(grepl('Experimentalstimulus:text', dfsurvdat$FL_125_DO))
dat$treat_fake_audio <- as.numeric(grepl('Experimentalstimulus:audio', dfsurvdat$FL_111_DO))
dat$treat_fake_video <- as.numeric(grepl('Experimentalstimulus:video', dfsurvdat$FL_124_DO))
dat$treat_skit       <- as.numeric(grepl('skit', dfsurvdat$FL_129_DO))

### sanity check: make sure each respondent is in one condition
### (should be 1 with some 0's for drop outs)
table(dat$treat_control + dat$treat_attackad + 
      dat$treat_fake_text + dat$treat_fake_audio + 
      dat$treat_fake_video + dat$treat_skit)

dat$treat <- factor(ifelse(
      dat$treat_fake_text, "text", ifelse(
          dat$treat_fake_audio, "audio", ifelse(
              dat$treat_fake_video, "video", ifelse(
                  dat$treat_attackad, "ad", ifelse(
                      dat$treat_skit, "skit", ifelse(
                          dat$treat_control, "control", NA
                      )
                  )
              )
          )
      )
), levels=c("control", "video", "audio", "text", "skit", "ad"))
table(dat$treat)
table(is.na(dat$treat))

dat$script_bidenshit <- apply(dfsurvdat[,grepl("*bidenshit_yes",colnames(dfsurvdat))], 1, function(x) any(x == 'Yes'))
dat$script_trumpshit <- apply(dfsurvdat[,grepl("*trumpshit_yes",colnames(dfsurvdat))], 1, function(x) any(x == 'Yes'))
dat$script_cherokee  <- apply(dfsurvdat[,grepl("*cherokee_yes",colnames(dfsurvdat))], 1, function(x) any(x == 'Yes'))
dat$script_lgbtq     <- apply(dfsurvdat[,grepl("*lgbtq_yes",colnames(dfsurvdat))], 1, function(x) any(x == 'Yes'))
dat$script_loans     <- apply(dfsurvdat[,grepl("*loans_yes",colnames(dfsurvdat))], 1, function(x) any(x == 'Yes'))

dat$script <- NA

dat$script[dat$script_bidenshit] <- "bidenshit"
dat$script[dat$script_trumpshit] <- "trumpshit"
dat$script[dat$script_cherokee] <- "cherokee"
dat$script[dat$script_lgbtq] <- "lgbtq"
dat$script[dat$script_loans] <- "loans"

dat$script <- as.factor(dat$script)
table(is.na(dat$script))

#####------------------------------------------------------#
##### ++ BELIEF ####
#####------------------------------------------------------#

X <- cbind( ## disagree that is fake --> believed is real
      as.numeric(factor(dfsurvdat$bidenshit_fake_3, levels=rev(agree_lvls))),
      as.numeric(factor(dfsurvdat$trumpshit_fake_3, levels=rev(agree_lvls))),
      as.numeric(factor(dfsurvdat$cherokee_fake_3, levels=rev(agree_lvls))),
      as.numeric(factor(dfsurvdat$lgbtq_fake_3, levels=rev(agree_lvls))),
      as.numeric(factor(dfsurvdat$loans_fake_3, levels=rev(agree_lvls))),
      as.numeric(factor(dfsurvdat$attackad_fake_3, levels=rev(agree_lvls)))
)
dat$believed_true <- apply(X, 1, function(r) ifelse(all(is.na(r)), NA, sum(r,na.rm=T)))
dat$believed1_true <- as.numeric(dat$believed_true > 3)
  
table(is.na(dat$believed_true)); hist(dat$believed_true)
  
dat$believed_attackad_true  <- as.numeric(factor(dfsurvdat$attackad_fake_3, levels=rev(agree_lvls)))
dat$believed1_attackad_true <- as.numeric(dat$believed_attackad_true > 3)
  
belief_q_idxs <- c(1,2,4)
belief_q_names <- c("offensive","funny","informative")
for (i in 1:3) {
      q_name <- belief_q_names[i]
      q_idx <- belief_q_idxs[i]
      X <- cbind(
          as.numeric(factor(dfsurvdat[[paste0("bidenshit_fake_",q_idx)]], levels=(agree_lvls))),
          as.numeric(factor(dfsurvdat[[paste0("trumpshit_fake_",q_idx)]], levels=(agree_lvls))),
          as.numeric(factor(dfsurvdat[[paste0("cherokee_fake_",q_idx)]], levels=(agree_lvls))),
          as.numeric(factor(dfsurvdat[[paste0("lgbtq_fake_",q_idx)]], levels=(agree_lvls))),
          as.numeric(factor(dfsurvdat[[paste0("loans_fake_",q_idx)]], levels=(agree_lvls))),
          as.numeric(factor(dfsurvdat[[paste0("attackad_fake_",q_idx)]], levels=(agree_lvls)))
      )
      dat[paste0("believed_",q_name)] <- apply(X, 1, function(r) ifelse(all(is.na(r)), NA, sum(r,na.rm=T)))
      dat[paste0("believed_",q_name,"1")] <- as.numeric(dat[paste0("believed_",q_name)] > 3)
}
rm(i)

#####------------------------------------------------------#
##### ++ BELIEF (real clips) ####
#####------------------------------------------------------#

dat$believed_bidenauto_true <- as.numeric(factor(dfsurvdat$bidenauto_fake_3, levels=rev(agree_lvls)))
dat$believed1_bidenauto_true <- as.numeric(dat$believed_bidenauto_true > 3)

dat$believed_klobuchar_true <- as.numeric(factor(dfsurvdat$klobuchar_fake_3, levels=rev(agree_lvls)))
dat$believed1_klobuchar_true <- as.numeric(dat$believed_klobuchar_true > 3)

dat$believed_bloo_true <- as.numeric(factor(dfsurvdat$bloo_fake_3, levels=rev(agree_lvls)))
dat$believed1_bloo_true <- as.numeric(dat$believed_bloo_true > 3)

dat$believed_snl_true <- as.numeric(factor(dfsurvdat$snl_fake_3, levels=rev(agree_lvls)))
dat$believed1_snl_true <- as.numeric(dat$believed_snl_true > 3)
  
#####------------------------------------------------------#
##### ++ EXP 1 INFO TREATMENT  ####
#####------------------------------------------------------#

dat$exp_1_prompt_control <- dfsurvdat$FL_80_DO == 'Experimentalprompt:control'
dat$exp_1_prompt_info <- dfsurvdat$FL_80_DO == 'Experimentalprompt:information'
dat$exp_1_prompt <- factor(ifelse(
      dat$exp_1_prompt_control, "control", ifelse(
          dat$exp_1_prompt_info, "info", NA)
), levels=c("control", "info"))
  
## debrief before ID task
dat$exp_2_prompt_accuracy <- dfsurvdat$FL_103_DO == 'preIDaccuracy'
dat$exp_2_prompt_control <- dfsurvdat$FL_103_DO == 'preIDcontrol'
dat$exp_2_prompt <- factor(ifelse(
      dat$exp_2_prompt_control, "control", ifelse(
          dat$exp_2_prompt_accuracy, "accuracy", NA
      )
))

#####------------------------------------------------------#
##### ++ FEELING THERMOMETER  ####
#####------------------------------------------------------#

dat$post_favor_Klobuchar <- as.numeric(dfsurvdat$post_favor_1)
dat$post_favor_Sanders   <- as.numeric(dfsurvdat$post_favor_2)
dat$post_favor_Warren    <- as.numeric(dfsurvdat$post_favor_3)
dat$post_favor_Biden     <- as.numeric(dfsurvdat$post_favor_4)
dat$post_favor_Bloomberg <- as.integer(dfsurvdat$post_favor_5)
  
#####------------------------------------------------------#
##### ++ ID TASK  ####
#####------------------------------------------------------#

dat$exp_2_after_debrief <- as.factor(dfsurvdat$exp_2_after_debrief)
dat$exp_2_before_debrief <- as.factor(as.numeric(dat$exp_2_after_debrief == 0))

### annotate which condition
dat$exp_2_nofake <- !is.na(dfsurvdat$VideoIDnofake_DO)
dat$exp_2_lofake <- !is.na(dfsurvdat$VideoIDlowfakes_DO)
dat$exp_2_hifake <- !is.na(dfsurvdat$VideoIDhighfakes_DO)
dat$exp_2 <- factor(
      ifelse(
          dat$exp_2_nofake, "nofake", ifelse(
                 dat$exp_2_lofake, "lofake", ifelse(
                     dat$exp_2_hifake, "hifake", NA
                 )
             )
      )
)

### sanity check
table(dat$exp_2)
table(as.numeric(dat$exp_2_nofake) + as.numeric(dat$exp_2_lofake) + as.numeric(dat$exp_2_hifake))

isfake <- "This video is fake or doctored"
isreal <- "This video is not fake or doctored"
  
### -- grade `no fakes` respondents
nofake_vids <- c("real_trump_soup","real_biden_fight",
                 "real_biden_stumble","real_trump_covid",
                 "real_obama_missile","real_obama_smoking",
                 "real_warrenbeer", "real_warrenliar")
nofake_posD <- c("real_trump_soup")
nofake_posR <- c("real_biden_fight",
                 "real_biden_stumble","real_trump_covid",
                 "real_obama_missile","real_obama_smoking",
                 "real_warrenbeer", "real_warrenliar")
nofake_res <- dfsurvdat[dat$exp_2_nofake, nofake_vids]
nofake_nmiss <- apply(nofake_res, 1, function(x) sum(is.na(x)))
table(nofake_nmiss, useNA="always")
table(apply(nofake_res, 1, function(x) any(is.na(x))))

dfsurvdat[nofake_nmiss > ARGS$idtask_NA_threshold, nofake_vids] <- NA

####accuracy
dat$exp_2_pct_correct <- NULL
dat$exp_2_pct_correct[dat$exp_2_nofake] <- apply(
      dfsurvdat[dat$exp_2_nofake, nofake_vids],
      1, function(r) { mean(r == isreal,na.rm=T) }
)
table(is.na(dat$exp_2_pct_correct[dat$exp_2_nofake]), useNA = "always")
hist(dat$exp_2_pct_correct[dat$exp_2_nofake])

dat$exp_2_pct_correct.congenial <- NULL
dat$exp_2_pct_correct.congenial[dat$exp_2_nofake & dat$PID == "Democrat"] <- as.numeric(dfsurvdat[dat$exp_2_nofake & dat$PID == "Democrat", nofake_posD] == isreal)
dat$exp_2_pct_correct.congenial[dat$exp_2_nofake & dat$PID == "Republican"] <- apply(
      dfsurvdat[dat$exp_2_nofake & dat$PID == "Republican", nofake_posR],
      1, function(r) { mean(r == isreal,na.rm=T) }
)

####FNR P(real|fake)
dat$exp_2_pct_false_real <- NULL
dat$exp_2_pct_false_real.congenial <- NULL

####FPR P(fake|real)
dat$exp_2_pct_false_fake <- NULL
dat$exp_2_pct_false_fake.congenial <- NULL

dat$exp_2_pct_false_fake[dat$exp_2_nofake] <- apply(
      dfsurvdat[dat$exp_2_nofake, nofake_vids],
      1, function(r) { mean(r == isfake,na.rm=T) }
)
dat$exp_2_pct_false_fake.congenial[dat$exp_2_nofake & dat$PID == "Democrat"] <- as.numeric(dfsurvdat[dat$exp_2_nofake & dat$PID == "Democrat", nofake_posD] == isfake)
dat$exp_2_pct_false_fake.congenial[dat$exp_2_nofake & dat$PID == "Republican"] <- apply(
      dfsurvdat[dat$exp_2_nofake & dat$PID == "Republican", nofake_posR],
      1, function(r) { mean(r == isfake,na.rm=T) }
)

### -- grade `low fakes` respondents
lowfake_vids <- c("fake_trump_aids","fake_obama_buzzfeed",
                  "real_trump_covid","real_biden_stumble",
                  "real_trump_apple","real_obama_missile",
                  "real_warrenbeer","real_warrenliar")
lowfake_posD <- c("real_trump_apple")
lowfake_posR <- c("fake_trump_aids","real_trump_covid",
                  "real_biden_stumble","real_obama_missile",
                  "real_warrenbeer","real_warrenliar")
lowfake_res <- dfsurvdat[dat$exp_2_lofake, lowfake_vids]
lowfake_nmiss <- apply(lowfake_res, 1, function(x) sum(is.na(x)))
table(apply(lowfake_res, 1, function(x) sum(is.na(x))))
table(apply(lowfake_res, 1, function(x) any(is.na(x))))

dfsurvdat[lowfake_nmiss > ARGS$idtask_NA_threshold, lowfake_vids] <- NA

####accuracy
dat$exp_2_pct_correct[dat$exp_2_lofake] <- apply(
      dfsurvdat[dat$exp_2_lofake, lowfake_vids],
      1, function(r) { mean(r == c(isfake,isfake,isreal,isreal,isreal,isreal,isreal,isreal), na.rm=T) }
)
dat$exp_2_pct_correct.congenial[dat$exp_2_lofake & dat$PID == "Democrat"] <- as.numeric(dfsurvdat[dat$exp_2_lofake & dat$PID == "Democrat", lowfake_posD] == isreal)
dat$exp_2_pct_correct.congenial[dat$exp_2_lofake & dat$PID == "Republican"] <- apply(
      dfsurvdat[dat$exp_2_lofake & dat$PID == "Republican", lowfake_posR],
      1, function(r) { mean(r == c(isfake,isreal,isreal,isreal,isreal,isreal), na.rm=T) }
)

####FNR P(real|fake)
dat$exp_2_pct_false_real[dat$exp_2_lofake] <- apply(
      dfsurvdat[dat$exp_2_lofake, lowfake_vids],
      1, function(r) { mean(r[1:2] == c(isreal,isreal), na.rm=T) }
)
dat$exp_2_pct_false_real.congenial[dat$exp_2_lofake & dat$PID == "Republican"] <- apply(
      dfsurvdat[dat$exp_2_lofake & dat$PID == "Republican", lowfake_posR],
      1, function(r) { mean(r[1] == isreal, na.rm=T) }
)

####FPR P(fake|real)
dat$exp_2_pct_false_fake[dat$exp_2_lofake] <- apply(
      dfsurvdat[dat$exp_2_lofake, lowfake_vids],
      1, function(r) { mean(r[3:8] == c(isfake,isfake,isfake,isfake,isfake,isfake), na.rm=T) }
)
dat$exp_2_pct_false_fake.congenial[dat$exp_2_lofake & dat$PID == "Republican"] <- apply(
      dfsurvdat[dat$exp_2_lofake & dat$PID == "Republican", lowfake_posR],
      1, function(r) { mean(r[2:6] == c(isfake,isfake,isfake,isfake,isfake), na.rm=T) }
)
dat$exp_2_pct_false_fake.congenial[dat$exp_2_lofake & dat$PID == "Democrat"] <- as.numeric(dfsurvdat[dat$exp_2_lofake & dat$PID == "Democrat", lowfake_posD] == isfake)

### -- grade `high fakes` respondents
hifake_vids <- c("fake_bernie1","fake_boris",
                 "fake_trump_resign","fake_hilary2",
                 "fake_obama_buzzfeed","fake_trump_aids",
                 "real_bidenfight","real_warrenbeer")
hifake_posD <- c("fake_trump_resign")
hifake_posR <- c("fake_trump_aids",
                 "real_bidenfight","real_warrenbeer")
hifake_res <- dfsurvdat[dat$exp_2_hifake, hifake_vids]
hifake_nmiss <- apply(hifake_res, 1, function(x) sum(is.na(x)))

table(apply(hifake_res, 1, function(x) sum(is.na(x))))
table(apply(hifake_res, 1, function(x) any(is.na(x))))

dfsurvdat[hifake_nmiss > ARGS$idtask_NA_threshold, hifake_vids] <- NA

####accuracy
dat$exp_2_pct_correct[dat$exp_2_hifake] <- apply(
      dfsurvdat[dat$exp_2_hifake, hifake_vids],
      1, function(r) { mean(r == c(isfake,isfake,isfake,isfake,isfake,isfake,isreal,isreal), na.rm=T) }
)
dat$exp_2_pct_correct.congenial[dat$exp_2_hifake & dat$PID == "Democrat"] <- as.numeric(dfsurvdat[dat$exp_2_hifake & dat$PID == "Democrat", hifake_posD] == isfake)
dat$exp_2_pct_correct.congenial[dat$exp_2_hifake & dat$PID == "Republican"] <- apply(
      dfsurvdat[dat$exp_2_hifake & dat$PID == "Republican", hifake_posR],
      1, function(r) { mean(r == c(isfake,isreal,isreal), na.rm=T) }
)

####FNR P(real|fake)
dat$exp_2_pct_false_real[dat$exp_2_hifake] <- apply(
      dfsurvdat[dat$exp_2_hifake, hifake_vids],
      1, function(r) { mean(r[1:6] == c(isreal,isreal,isreal,isreal,isreal,isreal), na.rm=T) }
)
dat$exp_2_pct_false_real.congenial[dat$exp_2_hifake & dat$PID == "Democrat"] <- as.numeric(dfsurvdat[dat$exp_2_hifake & dat$PID == "Democrat", hifake_posD] == isreal)
dat$exp_2_pct_false_real.congenial[dat$exp_2_hifake & dat$PID == "Republican"] <- apply(
      dfsurvdat[dat$exp_2_hifake & dat$PID == "Republican", hifake_posR],
      1, function(r) { mean(r[1] == c(isreal), na.rm=T) }
)


####FPR P(fake|real)
dat$exp_2_pct_false_fake[dat$exp_2_hifake] <- apply(
      dfsurvdat[dat$exp_2_hifake, hifake_vids],
      1, function(r) { mean(r[7:8] == c(isfake,isfake), na.rm=T) }
)
dat$exp_2_pct_false_fake.congenial[dat$exp_2_hifake & dat$PID == "Republican"] <- apply(
      dfsurvdat[dat$exp_2_hifake & dat$PID == "Republican", hifake_posR],
      1, function(r) { mean(r[2:3] == c(isfake), na.rm=T) }
)
  
#####------------------------------------------------------#
##### ++ MEDIA TRUST ####
#####------------------------------------------------------#

dat$post_media_trust1 <- as.numeric(factor(dfsurvdat$post_media_trust1, 
                                              levels=c("None at all", "Not very much", "A fair amount", "A great deal")))
dat$post_media_trust2 <- as.numeric(factor(dfsurvdat$post_media_trust2, 
                                              levels=c("None at all", "Not very much", "A fair amount", "A great deal")))
dat$post_media_trust3 <- as.numeric(factor(dfsurvdat$post_media_trust3, 
                                              levels=c("None at all", "Not very much", "A fair amount", "A great deal")))
X <- cbind(dat$post_media_trust1, dat$post_media_trust2, dat$post_media_trust3)
  
dat$post_media_trust <- apply(X, 1, function(r) ifelse(all(is.na(r)), NA, mean(r,na.rm=T)))
  
#####------------------------------------------------------#
##### ++ DIG LIT ####
#####------------------------------------------------------#

dat$post_dig_lit_1 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_1))
dat$post_dig_lit_2 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_2))
dat$post_dig_lit_3 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_3))
dat$post_dig_lit_4 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_4))
dat$post_dig_lit_5 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_5))
dat$post_dig_lit_6 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_6))
dat$post_dig_lit_7 <- 6-as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_7))
X <- cbind(
      dat$post_dig_lit_1, dat$post_dig_lit_2, 
      dat$post_dig_lit_3, dat$post_dig_lit_4, 
      dat$post_dig_lit_5, dat$post_dig_lit_6, 
      dat$post_dig_lit_7
)
dat$post_dig_lit <- apply(X, 1, function(r) ifelse(all(is.na(r)), NA, sum(r,na.rm=T)))
dat$post_dig_lit <- dat$post_dig_lit/35
  
dat$internet_usage <- recode(
    dfsurvdat$internet_usage,
    'Less often' = 1,
    'Every few weeks' = 2,
    '1 to 2 days a week' = 3,
    '3 to 6 days a week' = 4,
    'About once a day' = 5,
    'Several times a day' = 6,
    'Pretty much all the time' = 7
)

#####------------------------------------------------------#
##### ++ CRT ####
#####------------------------------------------------------#

dat$crt1 <- suppressWarnings(as.numeric(gsub("(\\$| |\\¢|[a-zA-Z])", "", dfsurvdat$pre_crt_1)) %in% c(5, 0.05))
dat$crt2 <- suppressWarnings(as.numeric(gsub("[a-zA-Z]", "", dfsurvdat$pre_crt_2)) %in% c(100))
dat$crt3 <- suppressWarnings(as.numeric(gsub("[a-zA-Z]", "", dfsurvdat$pre_crt_3)) %in% c(47))
dat$crt  <- dat$crt1 + dat$crt2 + dat$crt3
X <- cbind(dat$crt1, dat$crt2, dat$crt3)

dat$crt <- apply(X, 1, function(r) ifelse(all(is.na(r)), NA, mean(r,na.rm=T)))
hist(dat$crt)

#####------------------------------------------------------#
##### Finalize and save ####
#####------------------------------------------------------#

## bind and save
dat <- dat[!duplicated(dat$rid),]
colnames(dat) <- make.unique(names(dat))

## Used to create cleaned datasets with different thresholds in 02.1-prereg_sensitivity.R
if (ARGS$idtask_NA_threshold != 999) {
  save(dat, dfsurvdat, nofake_vids, lowfake_vids, hifake_vids, 
       file = "deepfake_tmp.RData")
  SAVE <- FALSE
}

if (SAVE) {
  save(dat, dfsurvdat, nofake_vids, lowfake_vids, hifake_vids, 
       file = "intermediate/deepfake_00.RData")
}
