# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Replicate supplementary analyses on deepfake studies (i.e. 
# exploratory non-registered analyses, reviewer-requested robustness 
# checks).
# 
# Author: Soubhik Barari
# 
# Environment:
# - must use R 3.6
# 
# Runtime: <1 min
# 
# Input:
# - code/deepfake.Rdata:
#       `dat` object made from `deepfake_make_data`       
#
# Output:
# - figures_exploratory/*
# - tables_exploratory/*
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(optparse)
library(tidyverse)
library(ggplot2)
library(broom)
library(stargazer)
library(lmtest)
library(sandwich)


rm(list=ls())
setwd("~/Research_Group Dropbox/Soubhik Barari/Projects/repos/deepfakes_project")
load("code/deepfake.Rdata")

if (!file.exists("tables_exploratory")) {
    system("mkdir tables_exploratory")
}
if (!file.exists("figures_exploratory")) {
    system("mkdir figures_exploratory")
}

COVARS <- c("educ", "meta_OS", "age_65", "PID", "crt", "gender", "polknow", 
            "internet_usage", "ambivalent_sexism")

z.prop.test = function(x1, x2, n1, n2, alpha=0.05, t = 2){
  nume = (x1/n1) - (x2/n2)
  p = (x1 + x2) / (n1 + n2)
  deno = sqrt(p * (1 - p) * (1/n1 + 1/n2))
  z = nume / deno
  print(c("Z value:",abs(round(z,4))))
  print(c("Cut-off quantile:", 
    abs(round(qnorm(alpha/t),2))))
  print(c("pvalue:", pnorm(-abs(z))))
  return(list(z.stat=abs(round(z,4)),
              diff=nume,
              p.value=pnorm(-abs(z))))
}

polknow_quantile <- quantile(dat$polknow, probs=c(0,0.5,1), include.lowest=T)
polknow_quantile[1] <- -Inf
polknow_quantile[3] <- Inf

diglit_quantile <- quantile(dat$post_dig_lit, probs=c(0,0.33,0.66,1),na.rm=T,include.lowest=T)
diglit_quantile[1] <- -Inf
diglit_quantile[4] <- Inf

sexism_quantile <- quantile(dat$ambivalent_sexism, probs=c(0,0.33,0.66,1),na.rm=T,include.lowest=T)
sexism_quantile[1] <- -Inf
sexism_quantile[4] <- Inf

select <- dplyr::select

## Re-do some pre-processing
agree_lvls <- c(NA, "Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")

dfsurvdat$crt1 <- suppressWarnings(as.numeric(gsub("(\\$| |\\¢|[a-zA-Z])", "", dfsurvdat$pre_crt_1)) %in% c(5, 0.05))
dfsurvdat$crt2 <- suppressWarnings(as.numeric(gsub("[a-zA-Z]", "", dfsurvdat$pre_crt_2)) %in% c(100))
dfsurvdat$crt3 <- suppressWarnings(as.numeric(gsub("[a-zA-Z]", "", dfsurvdat$pre_crt_3)) %in% c(47))
dfsurvdat$crt  <- dfsurvdat$crt1 + dfsurvdat$crt2 + dfsurvdat$crt3
X <- cbind(dfsurvdat$crt1, dfsurvdat$crt2, dfsurvdat$crt3)
dfsurvdat$crt <- apply(X, 1, function(r) ifelse(all(is.na(r)), NA, mean(r,na.rm=T)))

dfsurvdat$polknow_speaker   <- as.numeric(dfsurvdat$polknow_speaker == "Nancy Pelosi")
dfsurvdat$polknow_medicare  <- as.numeric(dfsurvdat$polknow_medicare == "A program run by the US federal government to pay for old people’s health care")
dfsurvdat$polknow_house     <- as.numeric(dfsurvdat$polknow_house == "Democrats")
dfsurvdat$polknow_senate    <- as.numeric(dfsurvdat$polknow_senate == "Republicans")
dfsurvdat$polknow_veto      <- as.numeric(dfsurvdat$polknow_veto == "Two-thirds")
dfsurvdat$polknow_warren    <- as.numeric(dfsurvdat$polknow_warren == "Elizabeth Warren")
dfsurvdat$polknow_boris     <- as.numeric(dfsurvdat$polknow_boris == "Boris Johnson")
dfsurvdat$polknow           <- rowMeans(dfsurvdat[grepl("polknow_", colnames(dfsurvdat))], na.rm=T)

dfsurvdat$meta_OS <- dfsurvdat$`meta_Operating System`
dfsurvdat$meta_OS <- ifelse(grepl("Android|iPhone|iPod|iPad", dfsurvdat$meta_OS), "mobile", "desktop")

dfsurvdat$ambivalent_sexism_1 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_1, levels=agree_lvls))
dfsurvdat$ambivalent_sexism_2 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_2, levels=agree_lvls))
dfsurvdat$ambivalent_sexism_3 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_3, levels=agree_lvls))
dfsurvdat$ambivalent_sexism_4 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_4, levels=agree_lvls))
dfsurvdat$ambivalent_sexism_5 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_5, levels=agree_lvls))
dfsurvdat$ambivalent_sexism <- rowMeans(dfsurvdat[grepl("ambivalent_sexism", colnames(dfsurvdat))], na.rm=T)

dfsurvdat$PID_main <- as.character(dfsurvdat$PID_main)
dfsurvdat$PID_leaners <- as.character(dfsurvdat$PID_leaners)
dfsurvdat$PID[dfsurvdat$PID_main=="Democrat"|dfsurvdat$PID_leaners=="Democrat"] <- "Democrat"
dfsurvdat$PID[dfsurvdat$PID_main=="Republican"|dfsurvdat$PID_leaners=="Republican"] <- "Republican"
dfsurvdat$PID[dfsurvdat$PID_main=="Independent"&!(dfsurvdat$PID_leaners %in% c("Democrat","Republican"))] <- "Independent"
dfsurvdat$PID[is.na(dfsurvdat$PID)] <- "N/A"
  
dfsurvdat$PID <- factor(dfsurvdat$PID, levels=c("N/A","Democrat","Independent","Republican"))
      
dfsurvdat$age_65 <- ifelse(dfsurvdat$Age > 60, ">65", "<=65")

dfsurvdat$post_dig_lit_1 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_1))
dfsurvdat$post_dig_lit_2 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_2))
dfsurvdat$post_dig_lit_3 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_3))
dfsurvdat$post_dig_lit_4 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_4))
dfsurvdat$post_dig_lit_5 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_5))
dfsurvdat$post_dig_lit_6 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_6))
dfsurvdat$post_dig_lit_7 <- 6-as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_7))
X <- cbind(
      dfsurvdat$post_dig_lit_1, dfsurvdat$post_dig_lit_2, 
      dfsurvdat$post_dig_lit_3, dfsurvdat$post_dig_lit_4, 
      dfsurvdat$post_dig_lit_5, dfsurvdat$post_dig_lit_6, 
      dfsurvdat$post_dig_lit_7
)
dfsurvdat$post_dig_lit <- apply(X, 1, function(r) ifelse(all(is.na(r)), NA, sum(r,na.rm=T)))
dfsurvdat$post_dig_lit <- dfsurvdat$post_dig_lit/35


#####------------------------------------------------------#
##### Settings ####
#####------------------------------------------------------#

arg_list <- list(     
    make_option(c("--response_quality"), type="character", default="all", 
        help="Which quality of responses to condition on.",
        metavar="response_quality"),
    # make_option(c("--weight"), type="numeric", default=0,
    #             help="Use weights?",
    #             metavar="weight"),
    make_option(c("--show_pdfs"), type="numeric", default=0,
                help="Show PDFs in real time?",
                metavar="show_pdfs")
)
ARGS <- parse_args(OptionParser(option_list=arg_list))

SHOW_PDFS <- ARGS$show_pdfs

dat$lowq <- FALSE
dat$lowq[dat$quality_pretreat_duration_tooquick | dat$quality_pretreat_duration_tooslow | dat$quality_demographic_mismatch] <- TRUE

# if (ARGS$response_quality == "low") {
#     dat <- dat[dat$quality_pretreat_duration_tooquick | dat$quality_pretreat_duration_tooslow | dat$quality_demographic_mismatch,] ## cond on low quality    
# }
# 
# if (ARGS$response_quality == "high") {
#     dat <- dat[!dat$quality_pretreat_duration_tooquick & !dat$quality_pretreat_duration_tooslow & !dat$quality_demographic_mismatch,] ## cond on high quality
# }

# if (ARGS$weight == 0 | !("weight" %in% colnames(dat))) {
#     dat$weight <- 1
# } 

theme_linedraw2 <- theme_linedraw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(2, "lines"))

#####------------------------------------------------------#
##### Helpers ####
#####------------------------------------------------------#

weighted.sd <- function(x, w, na.rm = FALSE) {
    if (na.rm) {
        w <- w[i <- !is.na(x)]
        x <- x[i]
    }
    sum.w <- sum(w)
    sqrt((sum(w*x^2) * sum.w - sum(w*x)^2) / (sum.w^2 - sum(w^2)))
}

cor(dat$polknow_warren, dat$believed1_true, use="pairwise.complete.obs")
#[1] 0.0247331
cor(dat$polknow_warren, dat$believed_true, use="pairwise.complete.obs")
#[1] -0.02772854

#####------------------------------------------------------#
##### Heterogeneity by specific clip ####
#####------------------------------------------------------#

script_lbls <- c("In-party\nincivility", "Out-party\nincivility", "Past\ncontroversy", "Novel\ncontroversy", "Political\ninsincerity")

## categorical belief
dat %>%
    filter(!is.na(script)) %>%
    filter(exp_1_prompt_control) %>%
    group_by(script) %>% 
    summarise(believed_true_mean = mean(believed_true, na.rm=T),
              believed_true_sd = sd(believed_true, na.rm=T)/sqrt(n())) %>%
    ggplot(aes(x=script, y=believed_true_mean, 
               ymax=believed_true_mean+1.96*believed_true_sd, 
               ymin=believed_true_mean-1.96*believed_true_sd)) +
    geom_bar(stat="identity") +
    ylab("Credibility confidence") + xlab("Script") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    scale_x_discrete(labels=script_lbls) +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
ggsave("figures_exploratory/firststage_belief_byscript.pdf", width=6, height=3.05)
if(SHOW_PDFS) system("open figures_exploratory/firststage_belief_byscript.pdf")

## binary belief
dat %>%
    filter(!is.na(script)) %>%
    filter(exp_1_prompt_control==T) %>%
    group_by(script) %>% 
    summarise(believed_true_mean = mean(believed1_true, na.rm=T)) %>%
    ggplot(aes(x=script, y=believed_true_mean)) +
    geom_bar(stat="identity") +
    ylab("Binarized credibility confidence") + xlab("Script") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    scale_x_discrete(labels=script_lbls) +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=12))
ggsave("figures_exploratory/firststage_belief_byscript_binary.pdf", width=6, height=3.05)
if(SHOW_PDFS) system("open figures_exploratory/firststage_belief_byscript_binary.pdf")

## belief by condition
dat %>%
    filter(!is.na(script), treat != "skit") %>%
    filter(exp_1_prompt_control) %>%
    group_by(script, treat) %>% 
    summarise(believed_true_mean = mean(believed_true, na.rm=T),
              believed_true_sd = sd(believed_true, na.rm=T)/sqrt(n())) %>%
    ggplot(aes(x=script, y=believed_true_mean, 
               ymax=believed_true_mean+1.96*believed_true_sd,
               ymin=believed_true_mean-1.96*believed_true_sd,
               fill=treat, label=treat)) +
    geom_bar(stat="identity", position=position_dodge(width=.9), color="black") +
    ylab("Credibility confidence") + xlab("Script") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +    
    scale_x_discrete(labels=script_lbls) +
    geom_text(aes(y=believed_true_mean+0.4), position=position_dodge(width=1.05), size=3) +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
ggsave("figures_exploratory/firststage_belief_byscript_bycond.pdf", width=6.5, height=3.05)
if(SHOW_PDFS) system("open figures_exploratory/firststage_belief_byscript_bycond.pdf")

## binary belief by condition
dat %>%
    filter(!is.na(script), treat != "skit") %>%
    filter(exp_1_prompt_control) %>%
    group_by(script, treat) %>% 
    summarise(believed_true_mean = mean(believed1_true, na.rm=T)) %>%
    ggplot(aes(x=script, y=believed_true_mean, fill=treat, label=treat)) +
    geom_bar(stat="identity", position=position_dodge(width=.9), color="black") +
    ylab("Binarized credibility confidence ") + xlab("Script") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    scale_x_discrete(labels=script_lbls) +
    geom_text(aes(y=believed_true_mean+0.04), position=position_dodge(width=1.05), size=3) +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=12))
ggsave("figures_exploratory/firststage_belief_byscript_binary_bycond.pdf", width=6.5, height=3.05)
if(SHOW_PDFS) system("open figures_exploratory/firststage_belief_byscript_binary_bycond.pdf")

#####------------------------------------------------------#
##### Second-stage clip performance ####
#####------------------------------------------------------#

# ## code NAs as 0
# scores_na <- dfsurvdat[,c(nofake_vids, lowfake_vids, hifake_vids)] %>%
#     gather(key="video", value="response") %>%
#     mutate(is_real=grepl("real", video)) %>%
#     mutate(video=gsub("(_|\\.)\\d$", "", video)) %>%
#     mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
#                           ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
#     mutate(correct=replace_na(correct, 0)) %>%
#     group_by(video) %>%
#     summarise(pct_correct=mean(correct, na.rm=T)) %>% 
#     as.data.frame()

scores <- dfsurvdat[,c(nofake_vids, lowfake_vids, hifake_vids)] %>%
    gather(key="video", value="response") %>%
    filter(!is.na(as.character(response))) %>%
    mutate(is_real=grepl("real", video)) %>%
    mutate(video=gsub("(_|\\.)\\d$", "", video)) %>%
    mutate(video=ifelse(video=="real_bidenfight", "real_biden_fight", video)) %>%
    mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
                          ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
    mutate(correct=replace_na(correct, 0)) %>%
    group_by(video) %>%
    summarise(pct_correct=mean(correct, na.rm=T), .keep = "all") %>% 
    mutate(video_lbl=case_when(video == "fake_hilary2" ~ "Hillary Clinton\n(fake debate)",
                               video == "fake_obama_buzzfeed" ~ "Barack Obama\n(fake news announcement)",
                               video == "real_obama_missile" ~ "Barack Obama\n(Russian president hot mic)",
                               video == "fake_bernie1" ~ "Bernie Sanders\n(fake debate)",
                               video == "real_obama_smoking" ~ "Barack Obama\n(smoking hot mic)",
                               video == "real_warrenbeer" ~ "Elizabeth Warren\n(Instagram beer gaffe)",
                               video == "real_trump_soup" ~ 'Donald Trump\n("soup" press conference gaffe)',
                               video == "real_trump_apple" ~ "Donald Trump\n(Apple press conference gaffe)",
                               video == "real_biden_fight" ~ "Joe Biden\n(town hall 'push-up contest' gaffe)",
                               video == "fake_boris" ~ "Boris Johnson\n(fake Brexit announcement)",
                               video == "real_warrenliar" ~ "Elizabeth Warren\n(post-debate hot mic)",
                               video == "real_biden_stumble" ~ "Joe Biden\n(stutter gaffe)",
                               video == "real_trump_covid" ~ "Donald Trump\n(COVID-19 precautions announcement)",
                               video == "fake_trump_aids" ~ "Donald Trump\n(fake AIDS cure announcment)",
                               video == "fake_trump_resign" ~ "Donald Trump\n(fake resignation announcement)")) %>%
    mutate(is_logo=case_when(video == "fake_hilary2" ~ TRUE,
                             video == "fake_obama_buzzfeed" ~ FALSE,
                             video == "real_obama_missile" ~ TRUE,
                             video == "fake_bernie1" ~ TRUE,
                             video == "real_obama_smoking" ~ FALSE,
                             video == "real_warrenbeer" ~ TRUE,
                             video == "real_trump_soup" ~ TRUE,
                             video == "real_trump_apple" ~ TRUE,
                             video == "real_biden_fight" ~ FALSE,
                             video == "fake_boris" ~ FALSE,
                             video == "real_warrenliar" ~ TRUE,
                             video == "real_biden_stumble" ~ TRUE,
                             video == "real_trump_covid" ~ TRUE,
                             video == "fake_trump_aids" ~ TRUE,
                             video == "fake_trump_resign" ~ TRUE)) %>%
    mutate(is_faceswap=case_when(video == "fake_hilary2" ~ TRUE,
                                 video == "fake_obama_buzzfeed" ~ FALSE,
                                 video == "real_obama_missile" ~ NA,
                                 video == "fake_bernie1" ~ TRUE,
                                 video == "real_obama_smoking" ~ NA,
                                 video == "real_warrenbeer" ~ NA,
                                 video == "real_trump_soup" ~ NA,
                                 video == "real_trump_apple" ~ NA,
                                 video == "real_biden_fight" ~ NA,
                                 video == "fake_boris" ~ FALSE,
                                 video == "real_warrenliar" ~ NA,
                                 video == "real_biden_stumble" ~ NA,
                                 video == "real_trump_covid" ~ NA,
                                 video == "fake_trump_aids" ~ FALSE,
                                 video == "fake_trump_resign" ~ FALSE)) %>%  
    mutate(is_trump=case_when(video == "fake_hilary2" ~ FALSE,
                              video == "fake_obama_buzzfeed" ~ FALSE,
                              video == "real_obama_missile" ~ FALSE,
                              video == "fake_bernie1" ~ FALSE,
                              video == "real_obama_smoking" ~ FALSE,
                              video == "real_warrenbeer" ~ FALSE,
                              video == "real_trump_soup" ~ TRUE,
                              video == "real_trump_apple" ~ TRUE,
                              video == "real_biden_fight" ~ FALSE,
                              video == "fake_boris" ~ FALSE,
                              video == "real_warrenliar" ~ FALSE,
                              video == "real_biden_stumble" ~ FALSE,
                              video == "real_trump_covid" ~ TRUE,
                              video == "fake_trump_aids" ~ TRUE,
                              video == "fake_trump_resign" ~ TRUE)) %>%
    mutate(is_obama=case_when(video == "fake_hilary2" ~ FALSE,
                              video == "fake_obama_buzzfeed" ~ TRUE,
                              video == "real_obama_missile" ~ TRUE,
                              video == "fake_bernie1" ~ FALSE,
                              video == "real_obama_smoking" ~ TRUE,
                              video == "real_warrenbeer" ~ FALSE,
                              video == "real_trump_soup" ~ FALSE,
                              video == "real_trump_apple" ~ FALSE,
                              video == "real_biden_fight" ~ FALSE,
                              video == "fake_boris" ~ FALSE,
                              video == "real_warrenliar" ~ FALSE,
                              video == "real_biden_stumble" ~ FALSE,
                              video == "real_trump_covid" ~ FALSE,
                              video == "fake_trump_aids" ~ FALSE,
                              video == "fake_trump_resign" ~ FALSE)) %>%
    as.data.frame()

## overall
scores %>%
    arrange(-pct_correct) %>%
    mutate(video=as_factor(video)) %>%
    mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct)) + 
    geom_bar(stat="identity") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    coord_flip() +
    xlab("Video clip in detection task") + ylab("% of correct detections") +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
ggsave("figures_exploratory/secondstage_byclip.pdf", width=6.5, height=7)
if(SHOW_PDFS) system("open figures_exploratory/secondstage_byclip.pdf")

## overall by clip
p_scores_fake <- scores %>% mutate(is_fake=grepl("fake_", video)) %>% filter(is_fake) %>% 
    mutate(is_fake="Deepfake videos") %>%
    arrange(-pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct)) + 
    geom_bar(stat="identity") +
    geom_hline(yintercept=mean(scores$pct_correct[grepl("fake_", scores$video)]), size=1, lty=1, color="red") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    coord_flip() +
    facet_grid(is_fake ~ .) +
    xlab("") + ylab("") +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

p_scores_real <- scores %>% mutate(is_real=grepl("real_", video)) %>% filter(is_real) %>% 
    mutate(is_real="Authentic videos") %>%
    arrange(-pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct)) + 
    geom_bar(stat="identity") +
    geom_hline(yintercept=mean(scores$pct_correct[grepl("real_", scores$video)]), size=1, lty=1, color="red") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    coord_flip() +
    facet_grid(is_real ~ .) +
    xlab("") + ylab("% of correct detections") +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
pp <- cowplot::plot_grid(p_scores_fake, p_scores_real, nrow=2, rel_heights = c(1,1.4), align="v")

cowplot::save_plot("figures_exploratory/secondstage_byclip_byfake.pdf", plot=pp, base_width=6.5, base_height=7)
if(SHOW_PDFS) system("open figures_exploratory/secondstage_byclip_byfake.pdf")

## overall by Trump or not
p_scores_trump <- scores %>% filter(is_trump) %>% 
    mutate(is_trump="Trump") %>%
    arrange(-pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct)) + 
    geom_bar(stat="identity") +
    geom_hline(yintercept=mean(scores$pct_correct[scores$is_trump]), size=1, lty=1, color="red") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    coord_flip() +
    facet_grid(is_trump ~ .) +
    xlab("") + ylab("% of correct detections") +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

p_scores_nottrump <- scores %>% filter(!is_trump) %>% 
    mutate(is_trump="Not Trump") %>%
    arrange(-pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct)) + 
    geom_bar(stat="identity") +
    geom_hline(yintercept=mean(scores$pct_correct[!scores$is_trump]), size=1, lty=1, color="red") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    coord_flip() +
    facet_grid(is_trump ~ .) +
    xlab("") + ylab("% of correct detections") +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
pp <- cowplot::plot_grid(p_scores_trump, p_scores_nottrump, nrow=2, rel_heights = c(1,1.4), align="v")

cowplot::save_plot("figures_exploratory/secondstage_byclip_bytrump.pdf", plot=pp, base_width=6.5, base_height=7)
if(SHOW_PDFS) system("open figures_exploratory/secondstage_byclip_bytrump.pdf")

## overall by Obama or not
p_scores_obama <- scores %>% filter(is_obama) %>% 
    mutate(is_obama="Obama") %>%
    arrange(-pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct)) + 
    geom_bar(stat="identity") +
    geom_hline(yintercept=mean(scores$pct_correct[scores$is_obama]), size=1, lty=1, color="red") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    coord_flip() +
    facet_grid(is_obama ~ .) +
    xlab("") + ylab("% of correct detections") +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

p_scores_notobama <- scores %>% filter(!is_obama) %>% 
    mutate(is_obama="Not Obama") %>%
    arrange(-pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct)) + 
    geom_bar(stat="identity") +
    geom_hline(yintercept=mean(scores$pct_correct[!scores$is_obama]), size=1, lty=1, color="red") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    coord_flip() +
    facet_grid(is_obama ~ .) +
    xlab("") + ylab("% of correct detections") +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
pp <- cowplot::plot_grid(p_scores_obama, p_scores_notobama, nrow=2, rel_heights = c(1,2.8), align="v")

cowplot::save_plot("figures_exploratory/secondstage_byclip_byobama.pdf", plot=pp, base_width=6.5, base_height=7)
if(SHOW_PDFS) system("open figures_exploratory/secondstage_byclip_byobama.pdf")

## overall by source cue
pp_scores_logo <- scores %>% 
    filter(is_logo) %>% 
    mutate(is_logo="Source logo") %>%
    arrange(-pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct)) + 
    geom_bar(stat="identity") +
    geom_hline(yintercept=mean(scores$pct_correct[scores$is_logo]), size=1, lty=1, color="red") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    coord_flip() +
    facet_grid(is_logo ~ .) +
    xlab("") + ylab("") +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

pp_scores_nologo <- scores %>% 
    filter(!is_logo) %>% 
    mutate(is_logo="No logo") %>%
    arrange(-pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct)) + 
    geom_bar(stat="identity") +
    geom_hline(yintercept=mean(scores$pct_correct[!scores$is_logo]), size=1, lty=1, color="red") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    coord_flip() +
    facet_grid(is_logo ~ .) +
    xlab("") + ylab("% of correct detections") +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
ppp <- cowplot::plot_grid(pp_scores_logo, pp_scores_nologo, nrow=2, rel_heights = c(1.5,0.62), align="v")

cowplot::save_plot("figures_exploratory/secondstage_byclip_bylogo.pdf", plot=ppp, base_width=6.5, base_height=7)
if(SHOW_PDFS) system("open figures_exploratory/secondstage_byclip_bylogo.pdf")

## fakes by face-swap or not
pp_scores_faceswap <- scores %>% 
    filter(is_faceswap==TRUE) %>% 
    mutate(is_faceswap="Face-swap\ndeepfake") %>%
    arrange(-pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct)) + 
    geom_bar(stat="identity") +
    geom_hline(yintercept=mean(scores$pct_correct[scores$is_faceswap==T],na.rm=T), size=1, lty=1, color="red") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    coord_flip() +
    facet_grid(is_faceswap ~ .) +
    xlab("") + ylab("") +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

pp_scores_lipsync <- scores %>% 
    filter(is_faceswap==FALSE) %>% 
    mutate(is_faceswap="Lip-sync\ndeepfake") %>%
    arrange(-pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct)) + 
    geom_bar(stat="identity") +
    geom_hline(yintercept=mean(scores$pct_correct[scores$is_faceswap==F],na.rm=T), size=1, lty=1, color="red") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    coord_flip() +
    facet_grid(is_faceswap ~ .) +
    xlab("") + ylab("% of correct detections") +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
ppp <- cowplot::plot_grid(pp_scores_faceswap, pp_scores_lipsync, nrow=2, rel_heights = c(0.38,0.6), align="v")


cowplot::save_plot("figures_exploratory/secondstage_byclip_byfaceswap.pdf", plot=ppp, base_width=6.5, base_height=4.8)
if(SHOW_PDFS) system("open figures_exploratory/secondstage_byclip_byfaceswap.pdf")


#####------------------------------------------------------#
##### Second-stage clip performance by subgroup: knowledge ####
#####------------------------------------------------------#

scores_byknow <- dfsurvdat[,c("polknow",nofake_vids, lowfake_vids, hifake_vids)] %>%
    mutate(polknow=cut(polknow, breaks=polknow_quantile, 
                       labels=c("Less", "More"))) %>%
    gather(key="video", value="response", -polknow) %>%
    filter(!is.na(as.character(response))) %>%
    mutate(is_real=grepl("real", video)) %>%
    mutate(video=gsub("(_|\\.)\\d$", "", video)) %>%
    mutate(video=ifelse(video=="real_bidenfight", "real_biden_fight", video)) %>%
    mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
                          ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
    mutate(correct=replace_na(correct, 0)) %>%
    group_by(video,polknow) %>%
    summarise(pct_correct=mean(correct, na.rm=T), .keep = "all") %>% 
    mutate(video_lbl=case_when(video == "fake_hilary2" ~ "Hillary Clinton\n(fake debate)",
                               video == "fake_obama_buzzfeed" ~ "Barack Obama\n(fake news announcement)",
                               video == "real_obama_missile" ~ "Barack Obama\n(Russian president hot mic)",
                               video == "fake_bernie1" ~ "Bernie Sanders\n(fake debate)",
                               video == "real_obama_smoking" ~ "Barack Obama\n(smoking hot mic)",
                               video == "real_warrenbeer" ~ "Elizabeth Warren\n(Instagram beer gaffe)",
                               video == "real_trump_soup" ~ 'Donald Trump\n("soup" press conference gaffe)',
                               video == "real_trump_apple" ~ "Donald Trump\n(Apple press conference gaffe)",
                               video == "real_biden_fight" ~ "Joe Biden\n(town hall 'push-up contest' gaffe)",
                               video == "fake_boris" ~ "Boris Johnson\n(fake Brexit announcement)",
                               video == "real_warrenliar" ~ "Elizabeth Warren\n(post-debate hot mic)",
                               video == "real_biden_stumble" ~ "Joe Biden\n(stutter gaffe)",
                               video == "real_trump_covid" ~ "Donald Trump\n(COVID-19 precautions announcement)",
                               video == "fake_trump_aids" ~ "Donald Trump\n(fake AIDS cure announcment)",
                               video == "fake_trump_resign" ~ "Donald Trump\n(fake resignation announcement)")) %>%
    as.data.frame()

diffs_bypolknow <- dfsurvdat[,c("polknow", nofake_vids, lowfake_vids, hifake_vids)] %>%
    mutate(polknow=cut(polknow, breaks=polknow_quantile, 
                       labels=c("Less", "More"))) %>%
    gather(key="video", value="response", -polknow) %>%
    filter(!is.na(as.character(response))) %>%
    mutate(is_real=grepl("real", video)) %>%
    mutate(video=gsub("(_|\\.)\\d$", "", video)) %>%
    mutate(video=ifelse(video=="real_bidenfight", "real_biden_fight", video)) %>%
    mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
                          ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
    mutate(correct=replace_na(correct, 0)) %>%
    group_by(is_real, video) %>% summarise(n.d=sum(polknow == "Less"),
                                  x.d=sum(polknow == "Less" & correct),
                                  n.r=sum(polknow == "More"),
                                  x.r=sum(polknow == "More" & correct)) %>%
    group_by(is_real, video) %>%
    do(as.data.frame(z.prop.test(.$x.d, .$x.r, .$n.d, .$n.r)))
    
## BHq
alpha <- 0.05 ##original threshold
k <- nrow(diffs_bypolknow) ##number of hypotheses 
r <- order(diffs_bypolknow$p.value) ##ranks of p-values
alpha_adj <- (r*alpha)/k ##step-up 'tailored' thresholds
diffs_bypolknow$color <- ifelse(diffs_bypolknow$p.value < alpha_adj, "black", "grey50") ##step-up hypothesis tests 
diffs_bypolknow$z_crit_adj <- qnorm(1 - (alpha_adj)/2) ##new critical values for CIs (asymptotic)
diffs_bypolknow$p.range <- cut(diffs_bypolknow$p.value, breaks=c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), labels=c("***","**","*",".",""))
diffs_bypolknow$color <- ifelse(diffs_bypolknow$diff < 0, "lightgreen", "yellow2")
diffs_bypolknow$p.font <- ifelse(diffs_bypolknow$p.range %in% c("***","**"), "bold", "plain")


d_scores_fake3 <- scores_byknow %>% mutate(is_fake=grepl("fake_", video)) %>% filter(is_fake) %>% 
    mutate(is_fake="Deepfake Videos") %>%
    group_by(video_lbl) %>% mutate(ymax=max(pct_correct)) %>% ungroup() %>%
    arrange(desc(polknow), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl))
    

p_scores_fake3 <- d_scores_fake3  %>%
    ggplot(aes(x=video_lbl, y=pct_correct, fill=polknow, label=polknow)) + 
    geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
    ## comparisons -- brackets
    geom_segment(data = d_scores_fake3 %>% filter(polknow == "Less"), ##horizontal
                 aes(x=as.numeric(video_lbl)-0.2, xend=as.numeric(video_lbl)+0.2, 
                     y=ymax+0.05, yend=ymax+0.05)) + 
    ## comparisons -- p-values
    geom_text(data = diffs_bypolknow %>% 
                        filter(!is_real) %>% 
                        mutate(PID = "I") %>% 
                        left_join(d_scores_fake3), 
              aes(label=paste0(round(-diff*100, 1),"%",p.range), 
                  color=color, y=ymax+0.05, x=video_lbl,fontface=p.font), hjust=-0.1) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 1.15)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    scale_fill_brewer(palette="YlGnBu") +
    scale_color_identity() + 
    coord_flip() +
    facet_grid(is_fake ~ .) +
    xlab("") + ylab("") +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

d_scores_real3 <- scores_byknow %>% mutate(is_real=grepl("real_", video)) %>% filter(is_real) %>% select(-is_real) %>%
    mutate(is_fake="Authentic Videos") %>%
    arrange(desc(polknow), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    group_by(video_lbl) %>% mutate(ymax=max(pct_correct)) %>% ungroup()
    

p_scores_real3 <- d_scores_real3 %>%
    ggplot(aes(x=video_lbl, y=pct_correct, fill=polknow, label=polknow)) + 
    geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
    ## comparisons -- brackets
    geom_segment(data = d_scores_real3 %>% filter(polknow == "Less"), ##horizontal
                 aes(x=as.numeric(video_lbl)-0.2, xend=as.numeric(video_lbl)+0.2, 
                     y=ymax+0.05, yend=ymax+0.05)) + 
    ## comparisons -- p-values
    geom_text(data = diffs_bypolknow %>% 
                        filter(is_real) %>%
                        mutate(polknow = "Less") %>% 
                        left_join(d_scores_real3), 
              aes(label=paste0(round(-diff*100, 1),"%",p.range), 
                  color=color, y=ymax+0.05, x=video_lbl,fontface=p.font), hjust=-0.1) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 1.1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    scale_fill_brewer(palette="YlGnBu", name="Political knowledge:") +
    scale_color_identity() + 
    coord_flip() +
    facet_grid(is_fake ~ .) +
    xlab("") + ylab("% of correct detections") +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "bottom",
              legend.text = element_text(size=12),
              legend.title = element_text(size=14),
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

pp2 <- cowplot::plot_grid(p_scores_fake3, p_scores_real3, nrow=2, rel_heights = c(1,1.4), align="v")

cowplot::save_plot("figures_exploratory/secondstage_byclip_bypolknow.pdf", plot=pp2, base_width=6.5, base_height=7)
if(SHOW_PDFS) system("open figures_exploratory/secondstage_byclip_bypolknow.pdf")

#####------------------------------------------------------#
##### Second-stage clip performance by subgroup: digital literacy ####
#####------------------------------------------------------#

scores_bydiglit <- dfsurvdat[,c("post_dig_lit",nofake_vids, lowfake_vids, hifake_vids)] %>%
    mutate(post_dig_lit=cut(post_dig_lit, breaks=diglit_quantile, 
                            labels=c("Low", "Moderate","High"))) %>%
    gather(key="video", value="response", -post_dig_lit) %>%
    filter(!is.na(as.character(response))) %>%
    mutate(is_real=grepl("real", video)) %>%
    mutate(video=gsub("(_|\\.)\\d$", "", video)) %>%
    mutate(video=ifelse(video=="real_bidenfight", "real_biden_fight", video)) %>%
    mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
                          ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
    mutate(correct=replace_na(correct, 0)) %>%
    group_by(video,post_dig_lit) %>%
    summarise(pct_correct=mean(correct, na.rm=T), .keep = "all") %>% 
    mutate(video_lbl=case_when(video == "fake_hilary2" ~ "Hillary Clinton\n(fake debate)",
                               video == "fake_obama_buzzfeed" ~ "Barack Obama\n(fake news announcement)",
                               video == "real_obama_missile" ~ "Barack Obama\n(Russian president hot mic)",
                               video == "fake_bernie1" ~ "Bernie Sanders\n(fake debate)",
                               video == "real_obama_smoking" ~ "Barack Obama\n(smoking hot mic)",
                               video == "real_warrenbeer" ~ "Elizabeth Warren\n(Instagram beer gaffe)",
                               video == "real_trump_soup" ~ 'Donald Trump\n("soup" press conference gaffe)',
                               video == "real_trump_apple" ~ "Donald Trump\n(Apple press conference gaffe)",
                               video == "real_biden_fight" ~ "Joe Biden\n(town hall 'push-up contest' gaffe)",
                               video == "fake_boris" ~ "Boris Johnson\n(fake Brexit announcement)",
                               video == "real_warrenliar" ~ "Elizabeth Warren\n(post-debate hot mic)",
                               video == "real_biden_stumble" ~ "Joe Biden\n(stutter gaffe)",
                               video == "real_trump_covid" ~ "Donald Trump\n(COVID-19 precautions announcement)",
                               video == "fake_trump_aids" ~ "Donald Trump\n(fake AIDS cure announcment)",
                               video == "fake_trump_resign" ~ "Donald Trump\n(fake resignation announcement)")) %>%
    as.data.frame()

diffs_bydiglit <- dfsurvdat[,c("post_dig_lit", nofake_vids, lowfake_vids, hifake_vids)] %>%
    mutate(post_dig_lit=cut(post_dig_lit, breaks=diglit_quantile, 
                       labels=c("Low", "Moderate", "High"))) %>%
    gather(key="video", value="response", -post_dig_lit) %>%
    filter(!is.na(as.character(response))) %>%
    mutate(is_real=grepl("real", video)) %>%
    mutate(video=gsub("(_|\\.)\\d$", "", video)) %>%
    mutate(video=ifelse(video=="real_bidenfight", "real_biden_fight", video)) %>%
    mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
                          ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
    mutate(correct=replace_na(correct, 0)) %>%
    group_by(is_real, video) %>% summarise(n.d=sum(post_dig_lit == "Low"),
                                  x.d=sum(post_dig_lit == "Low" & correct),
                                  n.r=sum(post_dig_lit == "High"),
                                  x.r=sum(post_dig_lit == "High" & correct)) %>%
    group_by(is_real, video) %>%
    do(as.data.frame(z.prop.test(.$x.d, .$x.r, .$n.d, .$n.r)))

## BHq
alpha <- 0.05 ##original threshold
k <- nrow(diffs_bydiglit) ##number of hypotheses 
r <- order(diffs_bydiglit$p.value) ##ranks of p-values
alpha_adj <- (r*alpha)/k ##step-up 'tailored' thresholds
diffs_bydiglit$color <- ifelse(diffs_bydiglit$p.value < alpha_adj, "black", "grey50") ##step-up hypothesis tests 
diffs_bydiglit$z_crit_adj <- qnorm(1 - (alpha_adj)/2) ##new critical values for CIs (asymptotic)
diffs_bydiglit$p.range <- cut(diffs_bydiglit$p.value, breaks=c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), labels=c("***","**","*",".",""))
diffs_bydiglit$color <- ifelse(diffs_bydiglit$diff < 0, "red", "yellow3")
diffs_bydiglit$p.font <- ifelse(diffs_bydiglit$p.range %in% c("***","**"), "bold", "plain")


d_scores_fake4 <- scores_bydiglit %>% mutate(is_fake=grepl("fake_", video)) %>% filter(is_fake) %>% 
    mutate(is_fake="Deepfake Videos") %>%
    arrange(desc(post_dig_lit), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    group_by(video_lbl) %>% mutate(ymax=max(pct_correct)) %>% ungroup()

p_scores_fake4 <- d_scores_fake4 %>%
    ggplot(aes(x=video_lbl, y=pct_correct, fill=post_dig_lit, label=post_dig_lit)) + 
    geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
    ## comparisons -- brackets
    geom_segment(data = d_scores_fake4 %>% filter(post_dig_lit == "Moderate"), ##horizontal
                 aes(x=as.numeric(video_lbl)-0.25, xend=as.numeric(video_lbl)+0.25, 
                     y=ymax+0.05, yend=ymax+0.05)) + 
    geom_segment(data = d_scores_fake4 %>% filter(post_dig_lit == "Low"), ##left tip
                 aes(x=as.numeric(video_lbl)-0.25, xend=as.numeric(video_lbl)-0.25, 
                     y=ymax+0.05, yend=pct_correct)) + 
    geom_segment(data = d_scores_fake4 %>% filter(post_dig_lit == "High"), ##right tip
                 aes(x=as.numeric(video_lbl)+0.25, xend=as.numeric(video_lbl)+0.25, 
                     y=ymax+0.05, yend=pct_correct)) + 
    ## comparisons -- p-values
    geom_text(data = diffs_bydiglit %>% 
                        filter(!is_real) %>% 
                        mutate(post_dig_lit = "Moderate") %>% 
                        left_join(d_scores_fake4), 
              aes(label=paste0(round(-diff*100, 1),"%",p.range), 
                  color=color, y=ymax+0.05, x=video_lbl,fontface=p.font), hjust=-0.1) + 
    scale_color_identity() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 1.1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    scale_fill_brewer(palette="YlOrRd") +
    coord_flip() +
    facet_grid(is_fake ~ .) +
    xlab("") + ylab("") +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

d_scores_real4 <- scores_bydiglit %>% mutate(is_real=grepl("real_", video)) %>% filter(is_real) %>% select(-is_real) %>%
  mutate(is_fake="Authentic Videos") %>%
  group_by(video_lbl) %>% mutate(ymax=max(pct_correct)) %>% ungroup() %>% 
  arrange(desc(post_dig_lit), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl))

p_scores_real4 <- d_scores_real4 %>%
    mutate(is_real="Authentic videos") %>%
    arrange(desc(post_dig_lit), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct, fill=post_dig_lit, label=post_dig_lit)) + 
    geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
    ## comparisons -- brackets
    geom_segment(data = d_scores_real4 %>% filter(post_dig_lit == "Moderate"), ##horizontal
                 aes(x=as.numeric(video_lbl)-0.25, xend=as.numeric(video_lbl)+0.25, 
                     y=ymax+0.05, yend=ymax+0.05)) + 
    geom_segment(data = d_scores_real4 %>% filter(post_dig_lit == "Low"), ##left tip
                 aes(x=as.numeric(video_lbl)-0.25, xend=as.numeric(video_lbl)-0.25, 
                     y=ymax+0.05, yend=pct_correct)) + 
    geom_segment(data = d_scores_real4 %>% filter(post_dig_lit == "High"), ##right tip
                 aes(x=as.numeric(video_lbl)+0.25, xend=as.numeric(video_lbl)+0.25, 
                     y=ymax+0.05, yend=pct_correct)) + 
    ## comparisons -- p-values
    geom_text(data = diffs_bydiglit %>% 
                        filter(is_real) %>% 
                        mutate(post_dig_lit = "Moderate") %>% 
                        left_join(d_scores_real4), 
              aes(label=paste0(round(-diff*100, 1),"%",p.range), 
                  color=color, y=ymax+0.05, x=video_lbl,fontface=p.font), hjust=-0.1) + 
    scale_color_identity() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 1.1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    scale_fill_brewer(palette="YlOrRd", name="Digital literacy:") +
    coord_flip() +
    facet_grid(is_fake ~ .) +
    xlab("") + ylab("% of correct detections") +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "bottom",
              legend.text = element_text(size=10),
              legend.title = element_text(size=10),
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

pp4 <- cowplot::plot_grid(p_scores_fake4, p_scores_real4, nrow=2, rel_heights = c(1,1.4), align="v")

cowplot::save_plot("figures_exploratory/secondstage_byclip_bydiglit.pdf", plot=pp4, base_width=6.5, base_height=7)
if(SHOW_PDFS) system("open figures_exploratory/secondstage_byclip_bydiglit.pdf")

#####------------------------------------------------------#
##### Second-stage clip performance by subgroup: cognitive reflection ####
#####------------------------------------------------------#

scores_byCR <- dfsurvdat[,c("crt",nofake_vids, lowfake_vids, hifake_vids)] %>%
    mutate(crt=cut(crt, breaks=c(-1,0,.34,1.1), 
                   labels=c("Low", "Moderate", "High"))) %>%
    gather(key="video", value="response", -crt) %>%
    filter(!is.na(as.character(response))) %>%
    mutate(is_real=grepl("real", video)) %>%
    mutate(video=gsub("(_|\\.)\\d$", "", video)) %>%
    mutate(video=ifelse(video=="real_bidenfight", "real_biden_fight", video)) %>%
    mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
                          ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
    mutate(correct=replace_na(correct, 0)) %>%
    group_by(video,crt) %>%
    summarise(pct_correct=mean(correct, na.rm=T), .keep = "all") %>% 
    mutate(video_lbl=case_when(video == "fake_hilary2" ~ "Hillary Clinton\n(fake debate)",
                               video == "fake_obama_buzzfeed" ~ "Barack Obama\n(fake news announcement)",
                               video == "real_obama_missile" ~ "Barack Obama\n(Russian president hot mic)",
                               video == "fake_bernie1" ~ "Bernie Sanders\n(fake debate)",
                               video == "real_obama_smoking" ~ "Barack Obama\n(smoking hot mic)",
                               video == "real_warrenbeer" ~ "Elizabeth Warren\n(Instagram beer gaffe)",
                               video == "real_trump_soup" ~ 'Donald Trump\n("soup" press conference gaffe)',
                               video == "real_trump_apple" ~ "Donald Trump\n(Apple press conference gaffe)",
                               video == "real_biden_fight" ~ "Joe Biden\n(town hall 'push-up contest' gaffe)",
                               video == "fake_boris" ~ "Boris Johnson\n(fake Brexit announcement)",
                               video == "real_warrenliar" ~ "Elizabeth Warren\n(post-debate hot mic)",
                               video == "real_biden_stumble" ~ "Joe Biden\n(stutter gaffe)",
                               video == "real_trump_covid" ~ "Donald Trump\n(COVID-19 precautions announcement)",
                               video == "fake_trump_aids" ~ "Donald Trump\n(fake AIDS cure announcment)",
                               video == "fake_trump_resign" ~ "Donald Trump\n(fake resignation announcement)")) %>%
    as.data.frame()

diffs_byCRT <- dfsurvdat[,c("crt", nofake_vids, lowfake_vids, hifake_vids)] %>%
    mutate(crt=cut(crt, breaks=c(-1,0,.34,1.1), 
                   labels=c("Low", "Moderate", "High"))) %>%
    gather(key="video", value="response", -crt) %>%
    filter(!is.na(as.character(response))) %>%
    mutate(is_real=grepl("real", video)) %>%
    mutate(video=gsub("(_|\\.)\\d$", "", video)) %>%
    mutate(video=ifelse(video=="real_bidenfight", "real_biden_fight", video)) %>%
    mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
                          ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
    mutate(correct=replace_na(correct, 0)) %>%
    group_by(is_real, video) %>% summarise(n.d=sum(crt == "Low"),
                                  x.d=sum(crt == "Low" & correct),
                                  n.r=sum(crt == "High"),
                                  x.r=sum(crt == "High" & correct)) %>%
    group_by(is_real, video) %>%
    do(as.data.frame(z.prop.test(.$x.d, .$x.r, .$n.d, .$n.r)))

## BHq
alpha <- 0.05 ##original threshold
k <- nrow(diffs_byCRT) ##number of hypotheses 
r <- order(diffs_byCRT$p.value) ##ranks of p-values
alpha_adj <- (r*alpha)/k ##step-up 'tailored' thresholds
diffs_byCRT$color <- ifelse(diffs_byCRT$p.value < alpha_adj, "black", "grey50") ##step-up hypothesis tests 
diffs_byCRT$z_crit_adj <- qnorm(1 - (alpha_adj)/2) ##new critical values for CIs (asymptotic)
diffs_byCRT$p.range <- cut(diffs_byCRT$p.value, breaks=c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), labels=c("***","**","*",".",""))
diffs_byCRT$color <- ifelse(diffs_byCRT$diff < 0, "red", "yellow3")
diffs_byCRT$p.font <- ifelse(diffs_byCRT$p.range %in% c("***","**"), "bold", "plain")

d_scores_fake5 <- scores_byCR %>% mutate(is_fake=grepl("fake_", video)) %>% filter(is_fake) %>% 
    mutate(is_fake="Deepfake Videos") %>%
    arrange(desc(crt), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    group_by(video_lbl) %>% mutate(ymax=max(pct_correct)) %>% ungroup()

p_scores_fake5 <- d_scores_fake5 %>%
    ggplot(aes(x=video_lbl, y=pct_correct, fill=crt, label=crt)) + 
    geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
    ## comparisons -- brackets
    geom_segment(data = d_scores_fake5 %>% filter(crt == "Moderate"), ##horizontal
                 aes(x=as.numeric(video_lbl)-0.25, xend=as.numeric(video_lbl)+0.25, 
                     y=ymax+0.05, yend=ymax+0.05)) + 
    geom_segment(data = d_scores_fake5 %>% filter(crt == "Low"), ##left tip
                 aes(x=as.numeric(video_lbl)-0.25, xend=as.numeric(video_lbl)-0.25, 
                     y=ymax+0.05, yend=pct_correct)) + 
    geom_segment(data = d_scores_fake5 %>% filter(crt == "High"), ##right tip
                 aes(x=as.numeric(video_lbl)+0.25, xend=as.numeric(video_lbl)+0.25, 
                     y=ymax+0.05, yend=pct_correct)) + 
    ## comparisons -- p-values
    geom_text(data = diffs_byCRT %>% 
                        filter(!is_real) %>% 
                        mutate(crt = "Moderate") %>% 
                        left_join(d_scores_fake5), 
              aes(label=paste0(round(-diff*100, 1),"%",p.range), 
                  color=color, y=ymax+0.05, x=video_lbl, fontface=p.font), hjust=-0.1, size=3) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 1.1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    scale_fill_brewer(palette="PuRd") +
    scale_color_identity() + 
    coord_flip() +
    facet_grid(is_fake ~ .) +
    xlab("") + ylab("") +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

d_scores_real5 <- scores_byCR %>% mutate(is_real=grepl("real_", video)) %>% 
  filter(is_real) %>% select(-is_real) %>%
  mutate(is_fake="Authentic Videos") %>%
  group_by(video_lbl) %>% mutate(ymax=max(pct_correct)) %>% ungroup() %>% 
  arrange(desc(crt), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl))

p_scores_real5 <- d_scores_real5 %>% 
    arrange(desc(crt), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct, fill=crt, label=crt)) + 
    geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
    ## comparisons -- brackets
    geom_segment(data = d_scores_real5 %>% filter(crt == "Moderate"), ##horizontal
                 aes(x=as.numeric(video_lbl)-0.25, xend=as.numeric(video_lbl)+0.25, 
                     y=ymax+0.05, yend=ymax+0.05)) + 
    geom_segment(data = d_scores_real5 %>% filter(crt == "Low"), ##left tip
                 aes(x=as.numeric(video_lbl)-0.25, xend=as.numeric(video_lbl)-0.25, 
                     y=ymax+0.05, yend=pct_correct)) + 
    geom_segment(data = d_scores_real5 %>% filter(crt == "High"), ##right tip
                 aes(x=as.numeric(video_lbl)+0.25, xend=as.numeric(video_lbl)+0.25, 
                     y=ymax+0.05, yend=pct_correct)) + 
    ## comparisons -- p-values
    geom_text(data = diffs_byCRT %>% 
                        filter(is_real) %>% 
                        mutate(crt = "Moderate") %>% 
                        left_join(d_scores_real5), 
              aes(label=paste0(round(-diff*100, 1),"%",p.range), 
                  color=color, y=ymax+0.05, x=video_lbl, fontface=p.font), hjust=-0.1, size=3) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 1.1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    scale_color_identity() + 
    scale_fill_brewer(palette="PuRd", name="Cognitive reflection:") +
    coord_flip() +
    facet_grid(is_fake ~ .) +
    xlab("") + ylab("% of correct detections") +
    theme_linedraw2 + 
        theme(title = element_text(size=5),
              legend.position = "bottom",
              legend.text = element_text(size=10),
              legend.title = element_text(size=10),
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

pp5 <- cowplot::plot_grid(p_scores_fake5, p_scores_real5, nrow=2, rel_heights = c(1,1.4), align="v")

cowplot::save_plot("figures_exploratory/secondstage_byclip_byCR.pdf", plot=pp5, base_width=6.5, base_height=7)
if(SHOW_PDFS) system("open figures_exploratory/secondstage_byclip_byCR.pdf")

#####------------------------------------------------------#
##### Second-stage clip performance adjusted models ####
#####------------------------------------------------------#

vars <- c("PID", "meta_OS", "age_65", "educ", "crt", "post_dig_lit",
          "gender", "polknow", "internet_usage", "ambivalent_sexism")
dfsurvdat <- dfsurvdat %>% left_join(dat[c("StartDate", "weight")])
dfsurvdat$weight <- ifelse(dfsurvdat$weight < 1/1000, 1/1000, dfsurvdat$weight)

responses <- dfsurvdat[,c(vars, "weight", nofake_vids, lowfake_vids, hifake_vids)] %>%
    gather(key="video", value="response", -weight, -PID, -meta_OS, -age_65, -educ, -crt, -post_dig_lit, -gender, -polknow, -internet_usage, -ambivalent_sexism) %>%
    filter(!is.na(as.character(response))) %>%
    mutate(video=gsub("(_|\\.)\\d$", "", video)) %>%
    mutate(is_real=grepl("real", video)) %>%
    mutate(video=ifelse(video=="real_bidenfight", "real_biden_fight", video)) %>%
    mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
                          ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
    mutate(correct=replace_na(correct, 0)) %>%
    mutate(video_lbl=case_when(video == "fake_hilary2" ~ "Hillary Clinton\n(fake debate)",
                               video == "fake_obama_buzzfeed" ~ "Barack Obama\n(fake news announcement)",
                               video == "real_obama_missile" ~ "Barack Obama\n(Russian president hot mic)",
                               video == "fake_bernie1" ~ "Bernie Sanders\n(fake debate)",
                               video == "real_obama_smoking" ~ "Barack Obama\n(smoking hot mic)",
                               video == "real_warrenbeer" ~ "Elizabeth Warren\n(Instagram beer gaffe)",
                               video == "real_trump_soup" ~ 'Donald Trump\n("soup" press conference gaffe)',
                               video == "real_trump_apple" ~ "Donald Trump\n(Apple press conference gaffe)",
                               video == "real_biden_fight" ~ "Joe Biden\n(town hall 'push-up contest' gaffe)",
                               video == "fake_boris" ~ "Boris Johnson\n(fake Brexit announcement)",
                               video == "real_warrenliar" ~ "Elizabeth Warren\n(post-debate hot mic)",
                               video == "real_biden_stumble" ~ "Joe Biden\n(stutter gaffe)",
                               video == "real_trump_covid" ~ "Donald Trump\n(COVID-19 precautions announcement)",
                               video == "fake_trump_aids" ~ "Donald Trump\n(fake AIDS cure announcment)",
                               video == "fake_trump_resign" ~ "Donald Trump\n(fake resignation announcement)")) %>%
    mutate(is_logo=case_when(video == "fake_hilary2" ~ TRUE,
                             video == "fake_obama_buzzfeed" ~ FALSE,
                             video == "real_obama_missile" ~ TRUE,
                             video == "fake_bernie1" ~ TRUE,
                             video == "real_obama_smoking" ~ FALSE,
                             video == "real_warrenbeer" ~ TRUE,
                             video == "real_trump_soup" ~ TRUE,
                             video == "real_trump_apple" ~ TRUE,
                             video == "real_biden_fight" ~ FALSE,
                             video == "fake_boris" ~ FALSE,
                             video == "real_warrenliar" ~ TRUE,
                             video == "real_biden_stumble" ~ TRUE,
                             video == "real_trump_covid" ~ TRUE,
                             video == "fake_trump_aids" ~ TRUE,
                             video == "fake_trump_resign" ~ TRUE)) %>%
    mutate(is_real=grepl("real", video)) %>%
    mutate(is_trump=grepl("trump", video)) %>%
    mutate(is_obama=grepl("obama", video)) %>%
    mutate(is_warren=grepl("warren", video)) %>%
    mutate(is_biden=grepl("biden", video)) %>%
    as.data.frame()

mod <- lm(correct ~ PID + is_logo + meta_OS + age_65 + educ + crt + post_dig_lit + gender + polknow + internet_usage + ambivalent_sexism, data = responses); summary(mod)
mod <- lm(correct ~ PID*is_trump*is_real + is_logo + meta_OS + age_65 + educ + crt + post_dig_lit + gender + polknow + internet_usage + ambivalent_sexism, data = responses); summary(mod)
mod <- lm(correct ~ PID*is_obama*is_real + is_logo + meta_OS + age_65 + educ + crt + post_dig_lit + gender + polknow + internet_usage + ambivalent_sexism, data = responses); summary(mod)

## no adjustments
mod_df <- bind_rows(
  tidy(coeftest(lm(correct ~ PID, 
                   weight=weight,
                   data = responses[!responses$is_trump & responses$is_real,]), vcov=vcovHC)) %>% mutate(real="Authentic", person="Not Trump"),
  tidy(coeftest(lm(correct ~ PID, 
                   weight=weight,
                   data = responses[responses$is_trump & responses$is_real,]), vcov=vcovHC)) %>% mutate(real="Authentic", person="Trump"),
  tidy(coeftest(lm(correct ~ PID, 
                   # weight=weight,
                   data = responses[!responses$is_trump & !responses$is_real,]), vcov=vcovHC)) %>% mutate(real="Deepfake", person="Not Trump"),
  tidy(coeftest(lm(correct ~ PID, 
                   # weight=weight,
                   data = responses[responses$is_trump & !responses$is_real,]), vcov=vcovHC)) %>% mutate(real="Deepfake", person="Trump"),
  
  tidy(coeftest(lm(correct ~ post_dig_lit, 
                   # weight=weight,
                   data = responses[!responses$is_trump & responses$is_real,]), vcov=vcovHC)) %>% mutate(real="Authentic", person="Not Trump"),
  tidy(coeftest(lm(correct ~ post_dig_lit, 
                   # weight=weight,
                   data = responses[responses$is_trump & responses$is_real,]), vcov=vcovHC)) %>% mutate(real="Authentic", person="Trump"),
  tidy(coeftest(lm(correct ~ post_dig_lit, 
                   # weight=weight,
                   data = responses[!responses$is_trump & !responses$is_real,]), vcov=vcovHC)) %>% mutate(real="Deepfake", person="Not Trump"),
  tidy(coeftest(lm(correct ~ post_dig_lit, 
                   # weight=weight,
                   data = responses[responses$is_trump & !responses$is_real,]), vcov=vcovHC)) %>% mutate(real="Deepfake", person="Trump")
)


## normal adjustments
mod_df2 <- bind_rows(
  tidy(coeftest(lm(correct ~ PID + is_logo + meta_OS + age_65 + educ + crt + post_dig_lit + gender + polknow + internet_usage + ambivalent_sexism, 
                   weight=weight,
                   data = responses[!responses$is_trump & responses$is_real,]), vcov=vcovHC)) %>% mutate(real="Authentic", person="Not Trump"),
  tidy(coeftest(lm(correct ~ PID + is_logo + meta_OS + age_65 + educ + crt + post_dig_lit + gender + polknow + internet_usage + ambivalent_sexism, 
                   weight=weight,
                   data = responses[responses$is_trump & responses$is_real,]), vcov=vcovHC)) %>% mutate(real="Authentic", person="Trump"),
  tidy(coeftest(lm(correct ~ PID + meta_OS + age_65 + educ + crt + post_dig_lit + gender + polknow + internet_usage + ambivalent_sexism, 
                   weight=weight,
                   data = responses[!responses$is_trump & !responses$is_real,]), vcov=vcovHC)) %>% mutate(real="Deepfake", person="Not Trump"),
  tidy(coeftest(lm(correct ~ PID + is_logo + meta_OS + age_65 + educ + crt + post_dig_lit + gender + polknow + internet_usage + ambivalent_sexism, 
                   weight=weight,
                   data = responses[responses$is_trump & !responses$is_real,]), vcov=vcovHC)) %>% mutate(real="Deepfake", person="Trump")
)

## normal adjustments w/interaction for video
mod_df3 <- bind_rows(
  tidy(coeftest(lm(correct ~ PID*video_lbl + is_logo + meta_OS + age_65 + educ + crt + post_dig_lit*video_lbl + gender + polknow + internet_usage + ambivalent_sexism, 
                   weight=weight,
                   data = responses[!responses$is_trump & responses$is_real,]), vcov=vcovHC)) %>% mutate(real="Authentic", person="Not Trump"),
  tidy(coeftest(lm(correct ~ PID*video_lbl + is_logo + meta_OS + age_65 + educ + crt + post_dig_lit*video_lbl + gender + polknow + internet_usage + ambivalent_sexism, 
                   weight=weight,
                   data = responses[responses$is_trump & responses$is_real,]), vcov=vcovHC)) %>% mutate(real="Authentic", person="Trump"),
  tidy(coeftest(lm(correct ~ PID*video_lbl + meta_OS + is_logo + age_65 + educ + crt + post_dig_lit*video_lbl + gender + polknow + internet_usage + ambivalent_sexism, 
                   weight=weight,
                   data = responses[!responses$is_trump & !responses$is_real,]), vcov=vcovHC)) %>% mutate(real="Deepfake", person="Not Trump"),
  tidy(coeftest(lm(correct ~ PID*video_lbl + is_logo + meta_OS + age_65 + educ + crt + post_dig_lit*video_lbl + gender + polknow + internet_usage + ambivalent_sexism, 
                   weight=weight,
                   data = responses[responses$is_trump & !responses$is_real,]), vcov=vcovHC)) %>% mutate(real="Deepfake", person="Trump")
)


bind_rows(
  mod_df %>% mutate(`model:`="weighted diff-in-means"),
  mod_df2 %>% mutate(`model:`="weighted regression"),
  mod_df3 %>% mutate(`model:`="weighted regression (interactions)")
) %>% 
  # filter(term == "PIDRepublican" | term == "post_dig_lit") %>%
  filter(term == "PIDRepublican") %>%
  mutate(color=ifelse(p.value < 0.05 & estimate > 0, "black", ifelse(p.value < 0.05 & estimate < 0, "black", "grey50"))) %>% 
  mutate(term = ifelse(term == "PIDRepublican", "Republican\n(vs. Democrat)", "Digital literacy")) %>%
  ggplot(aes(x=term, y=estimate, ymax=estimate+1.96*std.error, ymin=estimate-1.96*std.error, color=color, group=`model:`)) + 
  geom_pointrange(aes(shape=`model:`), position=position_dodge(width=0.4)) + 
  geom_linerange(aes(ymax=estimate+1.65*std.error, ymin=estimate-1.65*std.error), position=position_dodge(width=0.4), size=1.25) + 
  geom_hline(yintercept=0, color="grey", alpha=0.5) + 
  scale_color_identity() + 
  scale_y_continuous(labels = function(x) ifelse(x > 0, paste0("+",round(x*100, 1),"%"),
                                                 ifelse(x < 0, paste0(round(x*100, 1),"%"), x))) + 
  xlab("") + ylab("Marginal probability of correct detection") + 
  facet_grid(person ~ real) +
  coord_flip() + 
  theme_linedraw2 +
  theme(legend.position="bottom") +
  guides(shape=guide_legend(nrow=1,byrow=TRUE, title.hjust=0))
ggsave("figures_exploratory/secondstage_obama_v_trump.pdf", width=8, height=4)
system("open figures_exploratory/secondstage_obama_v_trump.pdf")

#####------------------------------------------------------#
##### Correlations with distraction clip attitudes? ####
#####------------------------------------------------------#

#TODO

#####------------------------------------------------------#
##### Correlations b/t 'believe-fake' and 'did-happen'? ####
#####------------------------------------------------------#

## === Trump resigns ===
summary(lm(dfsurvdat$fake_vs_didnt_happen_1 ~ I(dfsurvdat$fake_trump_resign=="This video is not fake or doctored")))
# Coefficients:
#                                                                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                  9.9206     0.3136   31.64   <2e-16 ***
# I(dfsurvdat$fake_trump_resign == "This video is not fake or doctored")TRUE  21.6948     2.0343   10.66   <2e-16 ***
summary(lm(dfsurvdat$fake_vs_didnt_happen_1 ~ I(dfsurvdat$fake_trump_resign=="This video is fake or doctored")))
# Coefficients:
#                                                                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                             12.5349     0.3685   34.02   <2e-16 ***
# I(dfsurvdat$fake_trump_resign == "This video is fake or doctored")TRUE  -7.1720     0.6812  -10.53   <2e-16 ***

dfsurvdat %>% 
  filter(!is.na(factor(fake_trump_resign))) %>%
  mutate(fake_trump_resign = case_when(fake_trump_resign == "This video is fake or doctored" ~ "Believed that\nvideo is fake/doctored",
                                       fake_trump_resign == "This video is not fake or doctored" ~ "Believed that\nvideo is credible",
                                       fake_trump_resign == "I don't know" ~ "Unsure")) %>%
  ggplot(aes(x=fake_vs_didnt_happen_1)) + 
  xlab("Confidence that event took place") + ylab("") + 
  facet_grid(~ fake_trump_resign) +
  geom_density(color="black", fill="#53B400") + 
  theme_linedraw2 +
  theme(
    axis.text.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    strip.text = element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18)
  )
ggsave("figures_exploratory/secondstage_fake-vs-happen_trumpresign.pdf", width=9, height=3.25)

dfsurvdat %>% 
  filter(!is.na(factor(fake_trump_resign))) %>%
  mutate(fake_trump_resign = case_when(fake_trump_resign == "This video is fake or doctored" ~ "Believed that\nvideo is fake/doctored",
                                       fake_trump_resign == "This video is not fake or doctored" ~ "Believed that\nvideo is credible",
                                       fake_trump_resign == "I don't know" ~ "Unsure")) %>%
  ggplot(aes(x=fake_vs_didnt_happen_1, fill=PID)) + 
  xlab("Confidence that event took place") + ylab("") + 
  facet_grid(~ fake_trump_resign) +
  scale_fill_manual(values=c("blue","grey","red"), name="Party identification:") +
  # geom_histogram(color="black", alpha=0.8) + 
  geom_density(color="black", alpha=0.5) + 
  # scale_y_continuous(trans="log2", expand=c(0,0)) +
  # scale_y_continuous(breaks=c(0.0, 0.05, 0.1, 0.2, 0.9)) +
  scale_x_continuous(limits=c(1, 100)) +
  theme_linedraw2 +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    strip.text = element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18)
  )
ggsave("figures_exploratory/secondstage_fake-vs-happen_trumpresign_byPID.pdf", width=9, height=4.25)


## === Trump COVID precautions ===
dfsurvdat$real_trump_covid <- ifelse(is.na(factor(dfsurvdat$real_trump_covid)), 
                                     as.character(dfsurvdat$real_trump_covid_1),
                                     as.character(dfsurvdat$real_trump_covid))

summary(lm(dfsurvdat$fake_vs_didnt_happen_2 ~ I(dfsurvdat$real_trump_covid=="This video is not fake or doctored")))
# Coefficients:
#                                                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                39.9948     0.9112   43.89   <2e-16 ***
# I(dfsurvdat$real_trump_covid == "This video is not fake or doctored")TRUE  35.3524     1.1016   32.09   <2e-16 ***
summary(lm(dfsurvdat$fake_vs_didnt_happen_2 ~ I(dfsurvdat$real_trump_covid=="This video is fake or doctored")))
# Coefficients:
#                                                                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                            70.9244     0.5837  121.51   <2e-16 ***
# I(dfsurvdat$real_trump_covid == "This video is fake or doctored")TRUE -37.4844     1.3761  -27.24   <2e-16 ***

dfsurvdat %>% 
  filter(!is.na(factor(real_trump_covid))) %>%
  mutate(real_trump_covid = case_when(real_trump_covid == "This video is fake or doctored" ~ "Believed that\nvideo is fake/doctored",
                                      real_trump_covid == "This video is not fake or doctored" ~ "Believed that\nvideo is credible",
                                      real_trump_covid == "I don't know" ~ "Unsure")) %>%
  ggplot(aes(x=fake_vs_didnt_happen_2)) + 
  xlab("Confidence that event took place") + ylab("") + 
  facet_grid(~ real_trump_covid) +
  # scale_y_continuous(trans="log2", expand=c(0,0)) +
  geom_density(color="black", fill="#53B400") + 
  theme_linedraw2 +
  theme(
    axis.text.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    strip.text = element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18)
  )
ggsave("figures_exploratory/secondstage_fake-vs-happen_trumpcovid.pdf", width=9, height=3.25)


dfsurvdat %>% ## by partisanship
  filter(!is.na(factor(real_trump_covid))) %>%
  mutate(real_trump_covid = case_when(real_trump_covid == "This video is fake or doctored" ~ "Believed that\nvideo is fake/doctored",
                                      real_trump_covid == "This video is not fake or doctored" ~ "Believed that\nvideo is credible",
                                      real_trump_covid == "I don't know" ~ "Unsure")) %>%
  ggplot(aes(x=fake_vs_didnt_happen_2, fill=PID)) + 
  xlab("Confidence that event took place") + ylab("") + 
  facet_grid(~ real_trump_covid) +
  scale_fill_manual(values=c("blue","grey","red"), name="Party identification:") +
  geom_density(color="black", alpha=0.5) + 
  theme_linedraw2 +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    strip.text = element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18)
  )
ggsave("figures_exploratory/secondstage_fake-vs-happen_trumpcovid_byPID.pdf", width=9, height=4.25)

## === Obama Russian president hot mic ===
dfsurvdat$real_obama_missile <- ifelse(is.na(factor(dfsurvdat$real_obama_missile)), 
                                       as.character(dfsurvdat$real_obama_missile_1),
                                       as.character(dfsurvdat$real_obama_missile))

summary(lm(dfsurvdat$fake_vs_didnt_happen_3 ~ I(dfsurvdat$real_obama_missile=="This video is not fake or doctored")))
# Coefficients:
#                                                                             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                  46.0525     0.5151   89.40   <2e-16 ***
# I(dfsurvdat$real_obama_missile == "This video is not fake or doctored")TRUE  36.0184     1.5463   23.29   <2e-16 ***

summary(lm(dfsurvdat$fake_vs_didnt_happen_3 ~ I(dfsurvdat$real_obama_missile=="This video is fake or doctored")))
# Coefficients:
#                                                                         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                              51.1225     0.5488  93.147  < 2e-16 ***
# I(dfsurvdat$real_obama_missile == "This video is fake or doctored")TRUE  -7.4933     1.4504  -5.167 2.47e-07 ***

dfsurvdat %>% 
  filter(!is.na(factor(real_obama_missile))) %>%
  mutate(real_obama_missile = case_when(real_obama_missile == "This video is fake or doctored" ~ "Believed that\nvideo is fake/doctored",
                                      real_obama_missile == "This video is not fake or doctored" ~ "Believed that\nvideo is credible",
                                      real_obama_missile == "I don't know" ~ "Unsure")) %>%
  ggplot(aes(x=fake_vs_didnt_happen_3)) + 
  xlab("Confidence that event took place") + ylab("") + 
  facet_grid(~ real_obama_missile) +
  geom_density(color="black", fill="#53B400") + 
  theme_linedraw2 +
  theme(
    axis.text.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    strip.text = element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18)
  )
ggsave("figures_exploratory/secondstage_fake-vs-happen_obamamissile.pdf", width=9, height=3.25)

dfsurvdat %>% ## by partisanship
  filter(!is.na(factor(real_obama_missile))) %>%
  mutate(real_obama_missile = case_when(real_obama_missile == "This video is fake or doctored" ~ "Believed that\nvideo is fake/doctored",
                                      real_obama_missile == "This video is not fake or doctored" ~ "Believed that\nvideo is credible",
                                      real_obama_missile == "I don't know" ~ "Unsure")) %>%
  ggplot(aes(x=fake_vs_didnt_happen_3, fill=PID)) + 
  xlab("Confidence that event took place") + ylab("") + 
  facet_grid( ~ real_obama_missile) +
  geom_density(color="black", alpha=0.5) + 
  scale_fill_manual(values=c("blue","grey","red"), name="PID:") +
  theme_linedraw2 +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    strip.text = element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18)
  )
ggsave("figures_exploratory/secondstage_fake-vs-happen_obamamissile_byPID.pdf", width=9, height=4.25)

#####------------------------------------------------------#
##### Affect by believed vs. not ####
#####------------------------------------------------------#

t.test(dat$post_favor_Warren[dat$believed1_true==1],
       dat$post_favor_Warren[dat$believed1_true==0])

# dat %>%
#   filter(exp_1_prompt_control==T) %>%
#   filter(!is.na(believed1_true)) %>%
#   filter(!is.na(post_favor_Warren))
#   mutate(believed1_true = ifelse(believed1_true==1,"Credible","Not credible")) %>%
#   ggplot(aes(x=post_favor_Warren, fill=believed1_true, group=believed1_true)) +
#     geom_density(alpha = 0.5) +
#     xlab("Candidate affect thermometer") +
#     ylab("") +
#     scale_fill_manual(values = c("darkgreen", "red"), name="Found stimulus:") +
#     geom_vline(xintercept = mean(dat$post_favor_Warren[dat$believed1_true==1],na.rm=T), color="darkgreen", lty=2) +
#     geom_vline(xintercept = mean(dat$post_favor_Warren[dat$believed1_true==0],na.rm=T), color="red", lty=2) +
#     scale_x_continuous(breaks = c(0,25,50,75,100), labels = c("0°","25°","50°","75°","100°")) +
#     theme_linedraw2 +
#     theme(legend.position = "top",
#           axis.text.x = element_text(size=14),
#           axis.title = element_text(size=16))

dat %>%
  filter(exp_1_prompt_control==T) %>%
  filter(!is.na(believed1_true)) %>%
  filter(!is.na(post_favor_Warren)) %>%
  mutate(believed1_true = ifelse(believed1_true==1,"Credible","Not credible")) %>%
  ggplot(aes(x=believed1_true, y=post_favor_Warren)) +
    geom_boxplot() +
    # geom_jitter(width = 0.2, alpha = 0.2) + 
    scale_y_continuous(breaks = c(0,25,50,75,100), labels = c("0°","25°","50°","75°","100°")) +
    ylab("Candidate affect thermometer") + xlab("Credibility response") +
    theme_linedraw2 + 
    theme(title = element_text(size=5),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          strip.text = element_text(size=16),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14))
ggsave("figures_exploratory/firststage_affect_by_belief.pdf", width=9, height=4.25)
# system("open figures_exploratory/firststage_affect_by_belief.pdf")

#####------------------------------------------------------#
##### Affect by script ####
#####------------------------------------------------------#

script_lbls <- c("In-party\nincivility", "Out-party\nincivility", "Past\ncontroversy", "Novel\ncontroversy", "Political\ninsincerity")

# dat %>%
#   filter(!is.na(script)) %>%
#   filter(exp_1_prompt_control) %>%
#   ggplot(aes(x=post_favor_Warren)) +
#   geom_density(fill="grey", alpha = 0.5) +
#   ylab("Candidate affect thermometer") + xlab("Script") +
#   facet_grid(script ~ .) +
#   geom_vline(data = dat %>%
#                filter(!is.na(script)) %>%
#                filter(exp_1_prompt_control) %>%
#                group_by(script) %>%
#                summarise(post_favor_Warren_mean = mean(post_favor_Warren, na.rm=T),
#                          post_favor_Warren_sd = sd(post_favor_Warren, na.rm=T)/sqrt(n())),
#              aes(xintercept = post_favor_Warren_mean), lty=2) +
#   theme_linedraw2 + 
#   theme(title = element_text(size=5),
#         axis.text.x = element_text(size=12),
#         axis.text.y = element_text(size=12),
#         strip.text = element_text(size=16),
#         axis.title.x = element_text(size=14),
#         axis.title.y = element_text(size=14))

dat %>%
  filter(!is.na(script)) %>%
  filter(!is.na(post_favor_Warren)) %>%
  filter(exp_1_prompt_control==TRUE) %>%
  ggplot(aes(x=script, y=post_favor_Warren)) +
  geom_boxplot() +
  # geom_jitter(width = 0.2, alpha = 0.2) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), labels = c("0°","25°","50°","75°","100°")) +
  scale_x_discrete(labels = script_lbls) +
  ylab("Candidate affect thermometer") + xlab("Script") +
  theme_linedraw2 + 
  theme(title = element_text(size=5),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        strip.text = element_text(size=16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))
ggsave("figures_exploratory/firststage_affect_byscript.pdf", width=9, height=4.25)
# system("open figures_exploratory/firststage_affect_byscript.pdf")

dat %>%
  filter(!is.na(script)) %>%
  filter(!is.na(post_favor_Warren)) %>%
  filter(exp_1_prompt_control==TRUE) %>%
  with(., lm(post_favor_Warren ~ script)) %>%
  summary()

  
#####------------------------------------------------------#
##### Experiment 1: Non-parametric tests ####
#####------------------------------------------------------#

## deception
t.test(na.omit(dat$believed_true[dat$treat_fake_video == 1]), 
       na.omit(dat$believed_true[dat$treat_fake_audio == 1])) ##t = -1.9296, df = 1767.1, p-value = 0.05382, \delta = -0.119805

t.test(na.omit(dat$believed_true[dat$treat_fake_video == 1]), 
       na.omit(dat$believed_true[dat$treat_fake_text == 1])) ##t = -1.2151, df = 1761.7, p-value = 0.2245, \delta = -0.075769

t.test(na.omit(dat$believed_true[dat$treat_fake_video == 1]), 
       na.omit(dat$believed_true[dat$treat_skit == 1])) ##t = 8.4754, df = 1083.9, p-value < 2.2e-16, \delta = 0.653852

## affect
t.test(na.omit(dat$post_favor_Warren[dat$treat_fake_video == 1]), 
       na.omit(dat$post_favor_Warren[dat$treat_fake_audio == 1])) ##t = -1.647, df = 1802.3, p-value = 0.09974, \delta = -2.64796

t.test(na.omit(dat$post_favor_Warren[dat$treat_fake_video == 1]), 
       na.omit(dat$post_favor_Warren[dat$treat_fake_text == 1])) ##t = -1.8447, df = 1794.4, p-value = 0.06525, \delta = -2.94543

t.test(na.omit(dat$post_favor_Warren[dat$treat_fake_video == 1]), 
       na.omit(dat$post_favor_Warren[dat$treat_skit == 1])) ##t = -1.0868, df = 1793.6, p-value = 0.2773, \delta = -1.72203

t.test(na.omit(dat$post_favor_Warren[dat$treat_fake_video == 1]), 
       na.omit(dat$post_favor_Warren[dat$treat_attackad == 1])) ##t = -0.13861, df = 1787.2, p-value = 0.8898, \delta = -0.22528

t.test(na.omit(dat$post_favor_Warren[dat$treat_fake_video == 1]), 
       na.omit(dat$post_favor_Warren[dat$treat_control == 1])) ##t = -2.793, df = 1767.1, p-value = 0.005278, \delta = -4.53598

## info
t.test(dat$believed_true[dat$exp_1_prompt_info],
       dat$believed_true[dat$exp_1_prompt_control]) ##t = -6.8445, df = 4177.9, p-value = 8.79e-12, \delta = -0.284111

t.test(dat$believed_true[dat$exp_1_prompt_info & dat$treat_fake_video],
       dat$believed_true[dat$exp_1_prompt_control & dat$treat_fake_video]) ##t = -3.918, df = 853.94, p-value = 9.641e-05, \delta = -0.354745

## heterogeneity by age
t.test(dat$post_favor_Warren[dat$age_65 == ">65"],
       dat$post_favor_Warren[dat$age_65 == "<=65"]) ##t = -5.7333, df = 4698, p-value = 1.047e-08, \delta = -5.42

## heterogeneity by medium within age
t.test(dat$post_favor_Warren[dat$age_65 == ">65" & dat$treat_fake_video==1],
       dat$post_favor_Warren[dat$age_65 == ">65" & dat$treat_fake_text==1]) ##t = -1.4803, df = 771.99, p-value = 0.1392, \delta = -3.81

## heterogeneity by sexism
t.test(dat$believed_true[dat$ambivalent_sexism <= 2.33],
       dat$believed_true[dat$ambivalent_sexism > 3.66]) ##t = -7.9801, df = 1250.3, p-value = 3.284e-15, \delta = -0.558898

t.test(dat$post_favor_Warren[dat$ambivalent_sexism <= 2.33],
       dat$post_favor_Warren[dat$ambivalent_sexism > 3.66]) ##t = 17.772, df = 1525.8, p-value < 2.2e-16, \delta = -25.9583

## heterogeneity by medium within sexism
t.test(dat$believed_true[dat$ambivalent_sexism > 3.66 & dat$treat_fake_video==1],
       dat$believed_true[dat$ambivalent_sexism > 3.66 & dat$treat_fake_audio==1]) ##t = 1.122, df = 276.71, p-value = 0.2628, \delta = 0.180147

t.test(dat$post_favor_Warren[dat$ambivalent_sexism > 3.66 & dat$treat_fake_video==1],
       dat$post_favor_Warren[dat$ambivalent_sexism > 3.66 & dat$treat_fake_audio==1]) ##t = -2.0674, df = 277.91, p-value = 0.03962, \delta = -8.41421

## heterogeneity by PID
t.test(dat$believed_true[dat$PID=="Democrat"],
       dat$believed_true[dat$PID=="Republican"]) ##t = -13.771, df = 3621.7, p-value < 2.2e-16, \delta = -0.60564

t.test(dat$post_favor_Warren[dat$PID=="Democrat"],
       dat$post_favor_Warren[dat$PID=="Republican"]) ##t = 53.531, df = 4753, p-value < 2.2e-16, \delta = -42.58104

## heterogeneity by polknow
t.test(dat$believed_true[dat$polknow <= 0.5],
       dat$believed_true[dat$polknow > 0.5]) ##t = 0.10765, df = 1115.6, p-value = 0.9143, \delta = 0.005508

t.test(dat$post_favor_Warren[dat$polknow <= 0.5],
       dat$post_favor_Warren[dat$polknow > 0.5]) ##t = -3.6516, df = 1600.4, p-value = 0.000269, \delta = 3.85

#####------------------------------------------------------#
##### Experiment 2: Non-parametric tests ####
#####------------------------------------------------------#

## real vs. fake clips
t.test(scores$pct_correct[grepl("real_",scores$video)]*100, 
       scores$pct_correct[grepl("fake_",scores$video)]*100) ##t = 0.5702, df = 5.8213, p-value = 0.5899, \delta = 0.0720895

## logo vs. no logo clips
t.test(scores$pct_correct[scores$is_logo], 
       scores$pct_correct[!scores$is_logo]) ##t = 0.53689, df = 5.9601, p-value = 0.6108, \delta = 0.0603977

## obama missile
chisq.test(
table(as.character(dfsurvdat$real_obama_missile[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_obama_missile) != "I don't know"]),
      as.character(dfsurvdat$PID[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_obama_missile) != "I don't know"]))
) ##X-squared = 333.34, df = 1, p-value < 2.2e-16

## trump apple
chisq.test(
table(as.character(dfsurvdat$real_trump_apple[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_trump_apple) != "I don't know"]),
      as.character(dfsurvdat$PID[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_trump_apple) != "I don't know"]))
) ##X-squared = 75.155, df = 1, p-value < 2.2e-16

## trump covid
chisq.test(
table(as.character(dfsurvdat$real_trump_covid[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_trump_covid) != "I don't know"]),
      as.character(dfsurvdat$PID[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_trump_covid) != "I don't know"]))
) ##X-squared = 169.96, df = 1, p-value < 2.2e-16

