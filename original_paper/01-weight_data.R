# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Weight data to be more representative of national population.
# 
# Author: Soubhik Barari
# 
# Environment:
# - must use R 3.6
# 
# Runtime: ~few seconds
# 
# Input:
# - cps2018_crosstabs*
# - deepfake.RData
#
# Output:
# - deepfake.RData
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(tidyverse)
library(survey)
library(stargazer)
library(weights)

rm(list=ls())

load("deepfake.RData")

select <- dplyr::select

#####------------------------------------------------------#
#####  Aggregate/clean CPS 2018 ####
#####------------------------------------------------------#

cps2018_ <- readRDS("cps2018_crosstabs.rds")
cps2018 <- cps2018_
cps2018$value <- cps2018$name

vars_select <- c(
    # "age",
    "sex",
    "race",
    "hispan"
)
cps2018 <- cps2018[cps2018$variable %in% vars_select,]

## sex
cps2018$value[cps2018$name == "male"] <- "Male"
cps2018$value[cps2018$name == "female"] <- "Female"

## race
cps2018 <- cps2018 %>%
    mutate(value = ifelse(name == "white", "White", 
                        ifelse(name == "black/negro", "Black", 
                               ifelse(name == "asian only", "Asian", 
                                      ifelse(variable == "race", "Other", value)))))
cps2018 <- cps2018 %>%
    mutate(value = ifelse(name == "not hispanic", "Not Hispanic", 
                        ifelse(variable == "hispan", "Hispanic", value)))

## age
# cps2018$value[cps2018$name == "18 to 24 years"] <- "18-24"
# cps2018$value[cps2018$name == "25 to 34 years"] <- "25-34"
# cps2018$value[cps2018$name == "35 to 44 years"] <- "35-44"
# cps2018$value[cps2018$name == "45 to 64 years"] <- "45-64"
# cps2018$value[cps2018$name == "65 years and over"] <- "65+"
# cps2018$value[cps2018$variable == "age" & is.na(cps2018$name)] <- "N/A"
cps2018age <- read_delim("cps2018_crosstabs_age.txt", delim="|",
                         col_names = c("cat", "n"))
cps2018age$prop <- as.numeric(gsub(",", "", cps2018age$n))/323156
cps2018age$cat <- as.factor(cps2018age$cat)
cps2018age$cat <- fct_collapse(cps2018age$cat,
    "18-24" = c(
        "18-20",
        "20-24"
    ),
    "25-34" = c(
        "25-29",
        "30-34"    
    ),
    "35-44" = c(
        "35-39",
        "40-44"
    ),
    "45-64" = c(
        "45-49",
        "50-54",
        "55-59",
        "60-64"
    ),
    "65+" = c(
        "65-69",
        "70-74",
        "75-79",
        "80-84",
        ">85"
    )
)

agg_cps2018age <- cps2018age %>% 
    group_by(cat) %>%
    summarise(prop=sum(prop))

## educ
# cps2018$value[cps2018$name == "none or preschool"] <- "<High school"
# cps2018$value[cps2018$name == "grades 1, 2, 3, or 4"] <- "<High school"
# cps2018$value[cps2018$name == "grades 5 or 6"] <- "<High school"
# cps2018$value[cps2018$name == "grades 7 or 8"] <- "<High school"
# cps2018$value[cps2018$name == "grade 9"] <- "<High school"
# cps2018$value[cps2018$name == "grade 10"] <- "<High school"
# cps2018$value[cps2018$name == "grade 11"] <- "<High school"
# cps2018$value[cps2018$name == "12th grade, no diploma"] <- "<High school"
# cps2018$value[cps2018$name == "high school diploma or equivalent"] <- "High school"
# cps2018$value[cps2018$name == "some college but no degree"] <- "High school"
# cps2018$value[cps2018$name == "associate's degree, occupational/vocational program"] <- "College"
# cps2018$value[cps2018$name == "associate's degree, academic program"] <- "College"
# cps2018$value[cps2018$name == "bachelor's degree"] <- "College"
# cps2018$value[cps2018$name == "master's degree"] <- "Postgraduate"
# cps2018$value[cps2018$name == "professional school degree"] <- "Postgraduate"
# cps2018$value[cps2018$name == "doctorate degree"] <- "Postgraduate"
# cps2018$value[cps2018$name == "niu or blank"] <- "N/A"
cps2018educ <- read_delim("cps2018_crosstabs_educ.txt", delim="|",
                       col_names = c("cat", "n"))
cps2018educ$prop <- cps2018educ$n/sum(cps2018educ$n)
cps2018educ$cat <- as.factor(cps2018educ$cat)
cps2018educ$cat <- fct_collapse(cps2018educ$cat,
       "<High school" = c(
           "Less than 9th grade",
           "9th-12th grade, no diploma"
       ),
       "High school" = c(
           "High school diploma or equivalent",
           "Some college, no Assoc. or 4-yr degree"
       ),
       "College" = c(
            "Associate degree",
            "Bachelor's degree"
       ),
       "Postgraduate" = c(
           "Master's degree",
           "Professional degree (such as DDS or JD)",
           "Doctorate (such as PhD or EdD)"
       )
)

agg_cps2018educ <- cps2018educ %>% 
    group_by(cat) %>%
    summarise(prop=sum(prop))

## income
cps2018inc_f <- readLines("cps2018_crosstabs_income.txt")
cps2018inc <- data.frame(
    cat = strsplit(cps2018inc_f[1], split="\t")[[1]],
    n = strsplit(cps2018inc_f[2], split="\t")[[1]]
)
cps2018inc <- cps2018inc[cps2018inc$cat != "Total ",]
cps2018inc$n <- as.numeric(gsub(",", "", cps2018inc$n))
cps2018inc$prop <- cps2018inc$n/sum(cps2018inc$n)
cps2018inc$cat <- as.factor(cps2018inc$cat)

cps2018inc$cat <- fct_collapse(cps2018inc$cat,
  "<$25k"= c("Under $5,000",
             "$5,000-$9,999",
             "$10,000-$14,999",
             "$15,000-$19,999",
             "$20,000-$24,999"),
  "$25k-$49k" = c("$25,000-$29,999",
                  "$30,000-$34,999",
                  "$35,000-$39,999",
                  "$40,000-$44,999",
                  "$45,000-$49,999"),
  "$50k-$74k" = c("$50,000-$54,999",
                  "$55,000-$59,999",
                  "$60,000-$64,999",
                  "$65,000-$69,999",
                  "$70,000-$74,999"),
  "$75k-$99k" = c("$75,000-$79,999",
                  "$80,000-$84,999",
                  "$85,000-$89,999",
                  "$90,000-$94,999",
                  "$95,000-$99,999"),
  "$100k-$150k" = c("$100,000-$104,999",
                    "$105,000-$109,999",
                    "$110,000-$114,999",
                    "$115,000-$119,999",
                    "$120,000-$124,999",
                    "$125,000-$129,999",
                    "$130,000-$134,999",
                    "$135,000-$139,999",
                    "$140,000-$144,999",
                    "$145,000-$149,999"),
  ">$150k" = c("$150,000-$154,999",
               "$155,000-$159,999",
               "$160,000-$164,999",
               "$165,000-$169,999",
               "$170,000-$174,999",
               "$175,000-$179,999",
               "$180,000-$184,999",
               "$185,000-$189,999",
               "$190,000-$194,999",
               "$195,000-$199,999",
               "$200,000 and over")
)

agg_cps2018inc <- cps2018inc %>% 
    group_by(cat) %>%
    summarise(prop=sum(prop))

## aggregate all 
cps2018 <- cps2018 %>% 
    dplyr::select(-name, -year) %>%
    group_by(variable, value) %>%
    summarise(n=sum(n)) %>%
    ungroup() %>% 
    group_by(variable) %>% 
    mutate(prop=n/sum(n))

cps2018 <- bind_rows(
    cps2018,
    agg_cps2018inc %>% 
        rename(value=cat) %>%
        mutate(variable="income"),
    agg_cps2018educ %>%
        rename(value=cat) %>%
        mutate(variable="educ"),
    agg_cps2018age %>%
        rename(value=cat) %>%
        mutate(variable="age")
)
cps2018 <- cps2018 %>% dplyr::select(-n)

cps2018 <- as.data.frame(cps2018)

# write_csv(cps2018, "cps2018.csv")

#####------------------------------------------------------#
#####  Compare CPS 2018 with survey ####
#####------------------------------------------------------#

dat_props <- bind_rows(
    dat %>% group_by(educ=educ) %>% summarise(prop=n()) %>% mutate(prop=prop/sum(prop)) %>% rename(value=educ) %>% mutate(variable="educ"),
    dat %>% group_by(age=agegroup) %>% summarise(prop=n()) %>% mutate(prop=prop/sum(prop)) %>% rename(value=age) %>% mutate(variable="age"),
    dat %>% group_by(income=HHI) %>% summarise(prop=n()) %>% mutate(prop=prop/sum(prop)) %>% rename(value=income) %>% mutate(variable="income"),
    dat %>% group_by(sex=gender) %>% summarise(prop=n()) %>% mutate(prop=prop/sum(prop)) %>% rename(value=sex) %>% mutate(variable="sex"),
    dat %>% group_by(race=Ethnicity) %>% summarise(prop=n()) %>% mutate(prop=prop/sum(prop)) %>% rename(value=race) %>% mutate(variable="race"),
    dat %>% group_by(hispan=Hispanic) %>% summarise(prop=n()) %>% mutate(prop=prop/sum(prop)) %>% rename(value=hispan) %>% mutate(variable="hispan")
)

dat_cps_props <- dat_props %>% 
    full_join(cps2018, suffix=c(".dat", ".cps"), by=c("variable", "value"))
dat_cps_props$variable[dat_cps_props$variable == "age"] <- "agegroup"
dat_cps_props$variable[dat_cps_props$variable == "income"] <- "HHI"
dat_cps_props$variable[dat_cps_props$variable == "sex"] <- "gender"
dat_cps_props$variable[dat_cps_props$variable == "race"] <- "Ethnicity"
dat_cps_props$variable[dat_cps_props$variable == "hispan"] <- "Hispanic"

#####------------------------------------------------------#
#####  Generate survey weights -- rake ####
#####------------------------------------------------------#

## make marginal Freq tables
pop.HHI <- agg_cps2018inc %>% 
    rename(HHI=cat, Freq=prop) %>% 
    mutate(Freq=Freq*nrow(dat))
pop.educ <- agg_cps2018educ %>% 
    rename(educ=cat, Freq=prop) %>% 
    mutate(Freq=Freq*nrow(dat))
pop.agegroup <- agg_cps2018age %>% 
    rename(agegroup=cat, Freq=prop) %>% 
    mutate(Freq=Freq*nrow(dat))
pop.gender <- cps2018 %>% ungroup() %>%
    filter(variable == "sex") %>% 
    dplyr::select(gender=value, Freq=prop) %>%
    mutate(Freq=Freq*nrow(dat))
pop.Ethnicity <- cps2018 %>% ungroup() %>%
    filter(variable == "race") %>% 
    dplyr::select(Ethnicity=value, Freq=prop) %>%
    mutate(Freq=Freq*nrow(dat))
pop.Hispanic <- cps2018 %>% ungroup() %>%
    filter(variable == "hispan") %>% 
    dplyr::select(Hispanic=value, Freq=prop) %>%
    mutate(Freq=Freq*nrow(dat))


## create `survey` objects
dd <- dat %>% 
    filter(!is.na(gender), !is.na(Ethnicity), 
           !is.na(agegroup), !is.na(HHI), 
           !is.na(Hispanic), !is.na(educ)) %>%
    filter(gender %in% pop.gender$gender) %>%
    filter(Ethnicity %in% pop.Ethnicity$Ethnicity) %>%
    filter(agegroup %in% pop.agegroup$agegroup) %>%
    filter(HHI %in% pop.HHI$HHI) %>%
    filter(Hispanic %in% pop.Hispanic$Hispanic) %>%
    filter(educ %in% pop.educ$educ)
dd_svy_uwt <- svydesign(ids = ~1, data = dd)

dd_svy_rake <- rake(design = dd_svy_uwt, ## full
                     sample.margins = list(~gender, ~Ethnicity, ~agegroup, ~HHI, ~Hispanic, ~educ),
                     population.margins = list(pop.gender, pop.Ethnicity, pop.agegroup, pop.HHI, pop.Hispanic, pop.educ))

if (FALSE) {
  dd_svy_rake <- rake(design = dd_svy_uwt, ## simple
                       sample.margins = list(~gender),
                       population.margins = list(pop.gender))
}
dd_svy_rake_trim <- trimWeights(dd_svy_rake, lower=0.3, upper=3,
                                strict=TRUE)

## inspect and evaluate weights on marginals 
max(weights(dd_svy_rake))
table(weights(dd_svy_rake_trim))

svymean(~gender, dd_svy_rake_trim)
svymean(~gender, dd_svy_rake)
with(pop.gender, wpct(gender, Freq))
prop.table(table(dd$gender))

svymean(~agegroup, dd_svy_rake_trim)
svymean(~agegroup, dd_svy_rake)
with(pop.agegroup, wpct(agegroup, Freq))
prop.table(table(dd$agegroup))

raked_wts <- svymean(~agegroup + educ + HHI + gender + Ethnicity + Hispanic, 
                     dd_svy_rake_trim)
raked_wts_df <- raked_wts %>%
    as.data.frame() %>%
    rownames_to_column("value") %>%
    mutate(variable=stringr::str_extract(value, "(agegroup|educ|HHI|gender|Ethnicity|^Hispanic)")) %>%
    mutate(value=gsub("(agegroup|educ|HHI|gender|Ethnicity|^Hispanic)", "", value))

props_df <- dat_cps_props %>% 
    left_join(raked_wts_df %>% rename(prop.wt=mean)) %>%
    dplyr::select(variable, value, prop.cps, prop.dat, prop.wt) %>%
    filter(value != "N/A", !is.na(value), !is.na(prop.cps), !(value=="Other"&variable=="gender")) %>%
    mutate(prop.cps=paste0(round(prop.cps*100, 2),"%"),
           prop.dat=paste0(round(prop.dat*100, 2),"%"),
           prop.wt=paste0(round(prop.wt*100, 2), "%"))

props_df <- props_df %>% 
    group_by(variable) %>%
    mutate(variable = ifelse(value == first(value), variable, "")) %>%
    ungroup() %>%
    mutate(value = replace(value, value == "Hispanic", "Yes")) %>%
    mutate(value = replace(value, value == "Not Hispanic", "No")) %>%
    mutate(variable = replace(variable, variable == "Hispanic", "Hispanic")) %>%
    mutate(variable = replace(variable, variable == "Ethnicity", "Race")) %>%
    mutate(variable = replace(variable, variable == "gender", "Gender")) %>%
    mutate(variable = replace(variable, variable == "HHI", "Household Income")) %>%
    mutate(variable = replace(variable, variable == "agegroup", "Age")) %>%
    mutate(variable = replace(variable, variable == "educ", "Education"))


colnames(props_df) <- c(
    "",
    "",
    "CPS",
    "Unweighted Sample",
    "Weighted Sample"
)
View(props_df)

#####------------------------------------------------------#
##### Save weights and summary ####
#####------------------------------------------------------#

if (!file.exists("tables")) {
  system("mkdir tables")
}

stargazer(props_df,
          header=FALSE,
          no.space=TRUE,
          title="\\textbf{Sample Demographics and Representativeness after Post-stratification}",
          summary=FALSE, 
          rownames=FALSE,
          font.size = "footnotesize",
          label = "tab:weights",
          notes=c("\\scriptsize \\textit{Notes:} Weights are constructed via Iterative Proportional Fitting to match sample marginal totals to CPS",
                  "\\scriptsize marginal totals on displayed demographic traits. Weights in the final column used for all analyses in paper."
          ),
          out="tables/weights.tex")


dd$weight <- weights(dd_svy_rake_trim)
dat <- dat %>% 
  left_join(dd %>% dplyr::select(rid, weight))
dat$weight[is.na(dat$weight)] <- 0

## update data file
save(dat, dfsurvdat, nofake_vids, lowfake_vids, hifake_vids, 
     file = "deepfake.RData")

