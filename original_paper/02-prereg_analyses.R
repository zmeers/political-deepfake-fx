# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Run through pre-registered analyses on deepfake studies.
# 
# Author: Soubhik Barari
# 
# Environment:
# - must use R 3.6
# 
# Runtime: <1 min
# 
# Input:
# - deepfake.Rdata:
#       `dat` object made from `deepfake_make_data`       
#
# Output:
# - figures/*
# - tables/*
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#####------------------------------------------------------#
##### Pre-amble ####
#####------------------------------------------------------#

rm(list=ls())

library(optparse)
library(tidyverse)
library(ggplot2)
library(broom)
library(stargazer)

setwd("~/Research_Group Dropbox/Soubhik Barari/Projects/repos/deepfakes_project")
load("deepfake.Rdata")

select <- dplyr::select

if (!file.exists("tables")) {
    system("mkdir tables")
}
if (!file.exists("figures")) {
    system("mkdir figures")
}

COVARS <- c("educ", "meta_OS", "age_65", "PID", "crt", "gender", "polknow", 
            "internet_usage", "ambivalent_sexism")

theme_linedraw2 <- theme_linedraw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(2, "lines"))

#####------------------------------------------------------#
##### Data ####
#####------------------------------------------------------#

arg_list <- list(     
    # make_option(c("--response_quality"), type="character", default="all", 
    #     help="Which quality of responses to condition on.",
    #     metavar="response_quality"),
    # make_option(c("--weight"), type="numeric", default=0,
    #             help="Use weights?",
    #             metavar="weight"),
    make_option(c("--show_pdfs"), type="numeric", default=0,
                help="Show PDFs in real time?",
                metavar="show_pdfs")
)
ARGS <- parse_args(OptionParser(option_list=arg_list))

SHOW_PDFS <- ARGS$show_pdfs

## last minute data cleaning
dat$lowq <- FALSE
dat$lowq[dat$quality_pretreat_duration_tooquick | dat$quality_pretreat_duration_tooslow | dat$quality_demographic_mismatch] <- TRUE
dat$internet_usage <- scales::rescale(dat$internet_usage)

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

#####------------------------------------------------------#
##### HELPERS ####
#####------------------------------------------------------#

coefviz <- function(df, ylab_="y", title_="") {
    df %>% 
        mutate(sig = as.factor(ifelse(estimate-1.96*std.error > 0 | estimate+1.96*std.error < 0, 
                                      "black", "gray"))) %>%
        ggplot(aes(x=term, 
                   y=estimate, 
                   ymin=estimate-1.96*std.error, 
                   ymax=estimate+1.96*std.error,
                   color=sig)) + 
        geom_pointrange(lwd=1) + 
        coord_flip() + 
        scale_color_identity() + 
        xlab("") + ylab(ylab_) +
        theme_linedraw2 + geom_hline(yintercept=0, lty=2, alpha=0.5) + 
        theme(
            legend.position = "none",
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=14)
        ) + ggtitle(title_)
}

groupcoefviz <- function(df, ylab_="y", title_="", nudge=0.05) {
    df %>% 
        mutate(sig = as.factor(ifelse(estimate-1.96*std.error > 0 | estimate+1.96*std.error < 0, 
                                      "black", "gray"))) %>%
        ggplot(aes(x=term, 
                   y=estimate, 
                   ymin=estimate-1.96*std.error, 
                   ymax=estimate+1.96*std.error,
                   group=model,
                   shape=model,
                   label=model,
                   colour=sig)) + 
        geom_pointrange(position=position_dodge(width=0.5), lwd=1) + 
        geom_text(aes(y=ifelse(estimate+1.96*std.error > 0, 
                               estimate-1.96*std.error-nudge,
                               estimate+1.96*std.error+nudge),
                  hjust=ifelse(estimate+1.96*std.error > 0, 
                               "right",
                               "left")),
                  position=position_dodge(width=0.5), 
                  vjust=0.5, colour="black") + 
        coord_flip() + 
        scale_shape_manual(values=c(15, 17)) +
        scale_color_identity() +
        xlab("") + ylab(ylab_) +
        theme_linedraw2 + geom_hline(yintercept=0, lty=2, alpha=0.5) + 
        theme(
            legend.position = "none",
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=14)
        ) + ggtitle(title_)
}

weighted.sd <- function(x, w, na.rm = FALSE) {
    if (na.rm) {
        w <- w[i <- !is.na(x)]
        x <- x[i]
    }
    sum.w <- sum(w)
    sqrt((sum(w*x^2) * sum.w - sum(w*x)^2) / (sum.w^2 - sum(w^2)))
}

#####------------------------------------------------------#
##### H1: Deepfakes more deceptive than text/audio/skit (no-warn cohort) ####
#####------------------------------------------------------#

n <- dat %>% 
    filter(!(treat%in%c("ad","control")), !is.na(treat), exp_1_prompt_control==T, !is.na(believed_true)) %>% nrow()

### non-parametric tests
t.test(na.omit(dat$believed_true[dat$treat_fake_video == 1]), 
       na.omit(dat$believed_true[dat$treat_fake_audio == 1])) ##t = -1.9296, df = 1767.1, p-value = 0.05382, \delta = -0.119805

t.test(na.omit(dat$believed_true[dat$treat_fake_video == 1]), 
       na.omit(dat$believed_true[dat$treat_fake_text == 1])) ##t = -1.2151, df = 1761.7, p-value = 0.2245, \delta = -0.075769

t.test(na.omit(dat$believed_true[dat$treat_fake_video == 1]), 
       na.omit(dat$believed_true[dat$treat_skit == 1])) ##t = 8.4754, df = 1083.9, p-value < 2.2e-16, \delta = 0.653852

### descriptive plots
dat %>% 
    mutate(treat = as.character(treat)) %>%
    filter(!(treat%in%c("ad","control","skit")), !is.na(treat), exp_1_prompt_control==T) %>%
    mutate(treat = replace(treat, treat == "text", "text")) %>%
    mutate(treat = replace(treat, treat == "audio", "audio")) %>%
    mutate(treat = replace(treat, treat == "video", "video")) %>%
    mutate(treat = replace(treat, treat == "skit", "skit")) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=4)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=4)) %>%
    group_by(treat) %>% 
    summarise(y=weighted.mean(believed_true,weight,na.rm=T), 
              ymax=weighted.mean(believed_true,weight,na.rm=T)+1.96*weighted.sd(believed_true,weight,na.rm=T)/sqrt(n()),
              ymin=weighted.mean(believed_true,weight,na.rm=T)-1.96*weighted.sd(believed_true,weight,na.rm=T)/sqrt(n())) %>%
    ggplot(aes(x=treat, y=y, ymin=ymin, ymax=ymax)) + 
    geom_bar(stat="identity") + ylim(c(0, 5)) + 
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    xlab("Scandal clipping") + ylab("Credibility confidence") +
    ggtitle(paste0("n=",n)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=14)
        )
ggsave(file = "figures/firststage_deception.pdf", width=5, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_deception.pdf")

dat %>% 
    mutate(treat = as.character(treat)) %>%
    filter(!(treat%in%c("ad","control","skit")), !is.na(treat), exp_1_prompt_control==T) %>%
    mutate(treat = replace(treat, treat == "text", "text")) %>%
    mutate(treat = replace(treat, treat == "audio", "audio")) %>%
    mutate(treat = replace(treat, treat == "video", "video")) %>%
    mutate(treat = replace(treat, treat == "skit", "skit")) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=4)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=4)) %>%
    group_by(treat) %>% 
    summarise(y=weighted.mean(believed1_true,weight,na.rm=T)) %>%
    ggplot(aes(x=treat, y=y)) + 
    geom_bar(stat="identity") +
    geom_text(aes(label=scales::percent(round(y,2), accuracy=1), y=y+0.03)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    xlab("Scandal clipping") + ylab("% somewhat/strongly confident in credibility") +
    ggtitle(paste0("n=",n)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=8)
        )
ggsave(file = "figures/firststage_deception_binary.pdf", width=5, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_deception_binary.pdf")

## difference-in-means
(h1.m <- lm(believed_true ~ treat, dat %>% filter(treat != "ad",treat != "skit",exp_1_prompt_control==T))); summary(h1.m);
(h1.m.wt <- lm(believed_true ~ treat, dat %>% filter(treat != "ad",treat != "skit",exp_1_prompt_control==T), weights=weight)); summary(h1.m.wt); 
(h1.m.hq <- lm(believed_true ~ treat, dat %>% filter(treat != "ad",treat != "skit",exp_1_prompt_control==T, !lowq))); summary(h1.m.hq); 

tidy(h1.m) %>%
    filter(term != "(Intercept)", term != "treatad") %>%
    mutate(term = replace(term, term == "treattext", "text")) %>%
    mutate(term = replace(term, term == "treataudio", "audio")) %>%
    mutate(term = replace(term, term == "treatskit", "skit")) %>%
    mutate(term = fct_relevel(term, "audio", after=1)) %>%
    coefviz(ylab = "Effect on credibility confidence (relative to fake video)", title = "no other controls")
ggsave(file = "figures/firststage_treatfx.pdf", width=7, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_treatfx.pdf")

(h1.m.bin <- lm(believed1_true ~ treat, dat %>% filter(exp_1_prompt_control==T,treat != "ad",treat != "skit"))); summary(h1.m.bin); 
(h1.m.bin.wt <- lm(believed1_true ~ treat, dat %>% filter(exp_1_prompt_control==T,treat != "ad",treat != "skit"), weights=weight)); summary(h1.m.bin.wt); 
(h1.m.bin.hq <- lm(believed1_true ~ treat, dat %>% filter(exp_1_prompt_control==T,treat != "ad",treat != "skit",!lowq))); summary(h1.m.bin.hq); 

tidy(h1.m.bin) %>%
    filter(term != "(Intercept)", term != "treatad", term != "treatskit") %>%
    mutate(term = replace(term, term == "treattext", "text")) %>%
    mutate(term = replace(term, term == "treataudio", "audio")) %>%
    mutate(term = fct_relevel(term, "audio", after=1)) %>%
    coefviz(ylab = "Effect on credibility confidence (relative to fake video)", title = "no other controls")
ggsave(file = "figures/firststage_treatfx_binary.pdf", width=7, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_treatfx_binary.pdf")


## adjustments
(h1.m.adj <- lm(believed_true ~ treat + meta_OS + age_65 + educ + crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat),treat != "ad",treat != "skit",exp_1_prompt_control==T))); summary(h1.m.adj);
(h1.m.adj.wt <- lm(believed_true ~ treat + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat),treat != "ad",treat != "skit",exp_1_prompt_control==T), weights=weight)); summary(h1.m.adj.wt);
(h1.m.adj.hq <- lm(believed_true ~ treat + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat),treat != "ad",treat != "skit",exp_1_prompt_control==T,!lowq))); summary(h1.m.adj.hq);
(h1.m.adj.wt.hq <- lm(believed_true ~ treat + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat),treat != "ad",treat != "skit",exp_1_prompt_control==T,!lowq), weights=weight)); summary(h1.m.adj.wt.hq);

tidy(h1.m.adj) %>%
    filter(grepl("treat", term)) %>%
    filter(term != "(Intercept)", term != "treatad", term != "treatskit") %>%
    mutate(term = replace(term, term == "treattext", "text")) %>%
    mutate(term = replace(term, term == "treataudio", "audio")) %>%
    mutate(term = replace(term, term == "treatskit", "skit")) %>%
    mutate(term = fct_relevel(term, "audio", after=1)) %>%
    coefviz(ylab = "Effect on credibility confidence (relative to fake video)", title = "adjusted for controls")
ggsave(file = "figures/firststage_treatfx_controlled.pdf", width=7, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_treatfx_controlled.pdf")

(h1.m.bin.adj <- lm(believed1_true ~ treat + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat),treat != "ad",treat != "skit",exp_1_prompt_control==T))); summary(h1.m.bin.adj);
(h1.m.bin.adj.wt <- lm(believed1_true ~ treat + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat),treat != "ad",treat != "skit",exp_1_prompt_control==T), weights=weight)); summary(h1.m.bin.adj.wt);
(h1.m.bin.adj.hq <- lm(believed1_true ~ treat + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat),treat != "ad",treat != "skit",exp_1_prompt_control==T,!lowq))); summary(h1.m.bin.adj.hq);
(h1.m.bin.adj.wt.hq <- lm(believed1_true ~ treat + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat),treat != "ad",treat != "skit",exp_1_prompt_control==T,!lowq), weights=weight)); summary(h1.m.bin.adj.wt.hq);

tidy(h1.m.bin.adj) %>%
    filter(grepl("treat", term), term != "treatad") %>%
    filter(term != "(Intercept)", term != "treatad") %>%
    mutate(term = replace(term, term == "treattext", "text")) %>%
    mutate(term = replace(term, term == "treataudio", "audio")) %>%
    mutate(term = replace(term, term == "treatskit", "skit")) %>%
    mutate(term = fct_relevel(term, "audio", after=1)) %>%
    coefviz(ylab = "Effect on credibility confidence (relative to fake video)", title = "adjusted for controls")
ggsave(file = "figures/firststage_treatfx_binary_controlled.pdf", width=7, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_treatfx_binary_controlled.pdf")


### combined coefficients plots
tidy(h1.m) %>% mutate(model="without controls") %>%
    bind_rows(tidy(h1.m.adj) %>% mutate(model="with controls")) %>%
    filter(grepl("treat", term), term != "treatad", term != "treatskit") %>%
    mutate(term = replace(term, term == "treattext", "text")) %>%
    mutate(term = replace(term, term == "treataudio", "audio")) %>%
    mutate(term = fct_relevel(term, "audio", after=1)) %>%
    groupcoefviz(ylab = "Effect on credibility confidence (relative to fake video)",
                 nudge = 0.01)
ggsave(file = "figures/firststage_treatfx_controlled2.pdf", width=7, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_treatfx_controlled2.pdf")

tidy(h1.m.bin) %>% mutate(model="without controls") %>%
    bind_rows(tidy(h1.m.bin.adj) %>% mutate(model="with controls")) %>%
    filter(grepl("treat", term), term != "treatad", term != "treatskit") %>%
    mutate(term = replace(term, term == "treattext", "text")) %>%
    mutate(term = replace(term, term == "treataudio", "audio")) %>%
    mutate(term = replace(term, term == "treatskit", "skit")) %>%
    mutate(term = fct_relevel(term, "audio", after=1)) %>%
    groupcoefviz(ylab = "Effect on credibility confidence (relative to fake video)",
                 nudge = 0.005)
ggsave(file = "figures/firststage_treatfx_binary_controlled2.pdf", width=7, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_treatfx_binary_controlled2.pdf")


### regression tables
h1.m.df <- bind_rows(
    tidy(h1.m),
    tidy(h1.m.wt),
    tidy(h1.m.hq),
    tidy(h1.m.adj),
    tidy(h1.m.adj.wt),
    tidy(h1.m.adj.hq),
    tidy(h1.m.adj.wt.hq)
)
h1.m.df$p.value.adj <- (h1.m.df$p.value/order(h1.m.df$p.value))*nrow(h1.m.df) ##BHq corrections

stargazer(h1.m,
          h1.m.wt,
          h1.m.hq,
          h1.m.adj,
          h1.m.adj.wt,
          h1.m.adj.hq,
          h1.m.adj.wt.hq,
          header = FALSE,
          apply.p = function(p) { h1.m.df$p.value.adj[h1.m.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$"),
          notes.append = FALSE,
          no.space=TRUE,
          digits=2,
          table.layout ="=d#-t-a-s=n",
          notes.align = "l",
          title="\\textbf{Models of Credibility Confidence in Incidental Exposure Experiment}",
          add.lines = list(c("Weighted?", "", "\\checkmark", "", "", "\\checkmark","","\\checkmark"),
                           c("Low-Quality Dropped?","","","\\checkmark","","","\\checkmark","\\checkmark")),
          covariate.labels = c("Audio",
                               "Text",
                               # "Skit",
                               # "Attack Ad",
                               "On Mobile",
                               "Age 65+",
                               "High School", "College", "Postgrad",
                               "Independent PID", "Republican PID",
                               "C.R.",
                               "Male",
                               "Political Knowledge",
                               "Internet Usage",
                               "Ambivalent Sexism"),
          dep.var.labels = c("\\normalsize Confidence that clipping was credible [1-5]"),
          omit.stat=c("f", "ser"),
          column.sep.width = "1pt",
          font.size = "footnotesize",
          style = "apsr",
          label = "firststage_deception",
          out="tables/firststage_deception.tex")

h1.m.bin.df <- bind_rows(
    tidy(h1.m.bin),
    tidy(h1.m.bin.wt),
    tidy(h1.m.bin.hq),
    tidy(h1.m.bin.adj),
    tidy(h1.m.bin.adj.wt),
    tidy(h1.m.bin.adj.hq),
    tidy(h1.m.bin.adj.wt.hq)
)
h1.m.bin.df$p.value.adj <- (h1.m.bin.df$p.value/order(h1.m.bin.df$p.value))*nrow(h1.m.bin.df) ##BHq corrections

stargazer(h1.m.bin,
          h1.m.bin.wt,
          h1.m.bin.hq,
          h1.m.bin.adj,
          h1.m.bin.adj.wt,
          h1.m.bin.adj.hq,
          h1.m.bin.adj.wt.hq,
          header = FALSE,
          no.space=TRUE,
          digits=2,
          table.layout ="=d#-t-a-s=n",
          notes.align = "l",
          title="\\textbf{Models of Binarized Credibility Confidence of Scandal Clipping in Incidental Exposure Experiment}",
          apply.p = function(p) { h1.m.bin.df$p.value.adj[h1.m.bin.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$"),
          notes.append = FALSE,
          add.lines = list(c("Weighted?", "", "\\checkmark", "", "", "\\checkmark","","\\checkmark"),
                           c("Low-Quality Dropped?","","","\\checkmark","","","\\checkmark","\\checkmark")),
          covariate.labels = c("Audio",
                               "Text",
                               # "Skit",
                               # "Attack Ad",
                               "On Mobile",
                               "Age 65+",
                               "High School", "College", "Postgrad",
                               "Independent PID", "Republican PID",
                               "C.R.",
                               "Male",
                               "Political Knowledge",
                               "Internet Usage",
                               "Ambivalent Sexism"),
          dep.var.labels = c("\\normalsize Somewhat/strongly confident clipping was credible"),
          omit.stat=c("f", "ser"),
          column.sep.width = "1pt",
          font.size = "footnotesize",
          label = "firststage_deception2",
          style = "apsr",
          out="tables/firststage_deception2.tex")


#####------------------------------------------------------#
##### H2: Deepfakes make target more unfavorable than text/audio/skit  ####
#####------------------------------------------------------#

## @SB: no difference in medium in unfavorability effect
n <- dat %>%
    filter(!is.na(treat), !is.na(post_favor_Warren)) %>%
    filter(exp_1_prompt_control==T) %>% nrow()

### non-parametric tests
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

### descriptive plots
dat %>%
    filter(!is.na(treat)) %>%
    filter(exp_1_prompt_control==T) %>%
    group_by(treat) %>% 
    summarise(y=weighted.mean(post_favor_Warren,weight,na.rm=T), 
              ymax=weighted.mean(post_favor_Warren,weight,na.rm=T)+1.96*weighted.sd(post_favor_Warren,weight,na.rm=T)/sqrt(n()),
              ymin=weighted.mean(post_favor_Warren,weight,na.rm=T)-1.96*weighted.sd(post_favor_Warren,weight,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    mutate(treat = as.character(treat)) %>%
    mutate(treat = replace(treat, treat == "text", "text")) %>%
    mutate(treat = replace(treat, treat == "audio", "audio")) %>%
    mutate(treat = replace(treat, treat == "video", "video")) %>%
    mutate(treat = replace(treat, treat == "skit", "skit")) %>%
    mutate(treat = replace(treat, treat == "ad", "attack ad")) %>%
    mutate(treat = fct_relevel(as.factor(treat), "control", after=0)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "attack ad", after=1)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=5)) %>%
    ggplot(aes(x=treat, y=y, ymin=ymin, ymax=ymax)) + 
    geom_bar(stat="identity") +
    scale_y_continuous(limits=c(30,50),oob = scales::rescale_none) +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    geom_vline(xintercept=2.5, lty=2, size=.5) +
    xlab("             Reference stimuli                                 Fake scandal clipping") + ylab("Candidate affect thermometer") +
    ggtitle(paste0("n=",n)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.x = element_text(size=14, hjust=0),
            axis.title.y = element_text(size=14)
        )  
ggsave("figures/firststage_feelings.pdf", width=8, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_feelings.pdf")

### regression tables
(h2.m <- lm(post_favor_Warren ~ treat, dat)); summary(h2.m);
(h2.m.wt <- lm(post_favor_Warren ~ treat, dat, weights=weight)); summary(h2.m.wt);
(h2.m.hq <- lm(post_favor_Warren ~ treat, dat %>% filter(!lowq))); summary(h2.m.hq);
(h2.m.adj <- lm(post_favor_Warren ~ treat + exp_1_prompt + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat)); summary(h2.m.adj);
(h2.m.adj.wt <- lm(post_favor_Warren ~ treat + exp_1_prompt + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat, weights=weight)); summary(h2.m.adj.wt);
(h2.m.adj.hq <- lm(post_favor_Warren ~ treat + exp_1_prompt + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat%>%filter(!lowq))); summary(h2.m.adj.hq);
(h2.m.adj.hq.wt <- lm(post_favor_Warren ~ treat + exp_1_prompt + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat%>%filter(!lowq), weights=weight)); summary(h2.m.adj.hq.wt);

h2.m.df <- bind_rows(
    tidy(h2.m),
    tidy(h2.m.wt),
    tidy(h2.m.hq),
    tidy(h2.m.adj),
    tidy(h2.m.adj.wt),
    tidy(h2.m.adj.hq),
    tidy(h2.m.adj.hq.wt)
)
h2.m.df$p.value.adj <- (h2.m.df$p.value/order(h2.m.df$p.value))*nrow(h2.m.df) ##BHq corrections

stargazer(h2.m, 
          h2.m.wt,
          h2.m.hq,
          h2.m.adj,
          h2.m.adj.wt,
          h2.m.adj.hq,
          h2.m.adj.hq.wt,
          header = FALSE,
          no.space=TRUE,
          digits=2,
          table.layout ="=d#-t-a-s=n",
          notes.align = "l",
          apply.p = function(p) { h2.m.df$p.value.adj[h2.m.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$",
                     "Reference category for clip type is \\texttt{control}."),
          notes.append = FALSE,
          title="\\textbf{Models of Scandal Target Affect in Incidental Exposure Experiment}",
          # omit = "response_wave_ID",
          add.lines = list(c("Weighted?", "", "\\checkmark", "", "", "\\checkmark","","\\checkmark"),
                           c("Low-Quality Dropped?","","","\\checkmark","","","\\checkmark","\\checkmark")),
          covariate.labels = c("Video",
                               "Audio",
                               "Text",
                               "Skit",
                               "Attack Ad",
                               "Information",
                               "On Mobile",
                               "Age 65+",
                               "High School", "College", "Postgrad",
                               "Independent PID", "Republican PID",
                               "C.R.",
                               "Male",
                               "Political Knowledge",
                               "Internet Usage",
                               "Ambivalent Sexism"),
          dep.var.labels = c("\\normalsize Elizabeth Warren Affect Thermometer"),
          omit.stat=c("f", "ser"),
          column.sep.width = "-2pt",
          font.size = "footnotesize",
          style = "apsr",
          label = "firststage_feelings",
          out="tables/firststage_feelings.tex")

### manipulation/placebo checks
(h2.m.adj.part <- lm(post_favor_Warren ~ treat + response_wave_ID + PID*crt, dat)); summary(h2.m.adj.part);

plcbs_df <- tidy(h2.m) %>% mutate(model="Without controls") %>%
    bind_rows(tidy(h2.m.adj.part) %>% mutate(model="PID controls")) %>%
    bind_rows(tidy(h2.m.adj) %>% mutate(model="Full controls")) %>%
    filter(grepl("treat", term)) %>%
    filter(term != "(Intercept)") %>%
    mutate(term = replace(term, term == "treattext", "text")) %>%
    mutate(term = replace(term, term == "treataudio", "audio")) %>%
    mutate(term = replace(term, term == "treatad", "ad")) %>%
    mutate(term = replace(term, term == "treatvideo", "video")) %>%
    mutate(term = replace(term, term == "treatskit", "skit")) %>%
    mutate(term = fct_relevel(term, "audio", after=1))
plcbs_df$target <- "Elizabeth Warren"

for (favor in c("post_favor_Biden","post_favor_Klobuchar","post_favor_Sanders","post_favor_Bloomberg")) {
    dat$post_favor_ <- dat[[favor]]
    
    (h2.plcb.m <- lm(post_favor_ ~ treat, dat, weights=weight)); summary(h2.plcb.m);
    (h2.plcb.m.adj.part <- lm(post_favor_ ~ treat + response_wave_ID + PID*crt, dat, weights=weight)); summary(h2.plcb.m.adj.part);
    (h2.plcb.m.adj <- lm(post_favor_ ~ treat + response_wave_ID + exp_1_prompt + meta_OS + age_65 + educ + PID*crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat, weights=weight)); summary(h2.plcb.m.adj);
    
    plcb_df <- tidy(h2.plcb.m) %>% mutate(model="Without controls") %>%
        bind_rows(tidy(h2.plcb.m.adj.part) %>% mutate(model="PID controls")) %>%
        bind_rows(tidy(h2.plcb.m.adj) %>% mutate(model="Full controls")) %>%
        filter(grepl("treat", term)) %>%
        filter(term != "(Intercept)") %>%
        mutate(term = replace(term, term == "treattext", "text")) %>%
        mutate(term = replace(term, term == "treataudio", "audio")) %>%
        mutate(term = replace(term, term == "treatad", "ad")) %>%
        mutate(term = replace(term, term == "treatvideo", "video")) %>%
        mutate(term = replace(term, term == "treatskit", "skit")) %>%
        mutate(term = fct_relevel(term, "audio", after=1))
    plcb_df$target <- favor
    plcbs_df <- bind_rows(plcbs_df, plcb_df)
}
plcbs_df %>% 
    mutate(target = replace(target, target == "post_favor_Sanders", "Bernie Sanders")) %>% 
    mutate(target = replace(target, target == "post_favor_Klobuchar", "Amy Klobuchar")) %>% 
    mutate(target = replace(target, target == "post_favor_Bloomberg", "Michael Bloomberg")) %>% 
    mutate(target = replace(target, target == "post_favor_Biden", "Joe Biden")) %>% 
    mutate(target = fct_relevel(target, "Joe Biden", "Michael Bloomberg", "Bernie Sanders", "Amy Klobuchar", "Elizabeth Warren")) %>%
    mutate(model = fct_relevel(model, "without controls", "PID controls", "full controls")) %>%
    mutate(sig = as.factor(ifelse(estimate-1.96*std.error > 0 | estimate+1.96*std.error < 0, 
                                  "black", "gray"))) %>%
    ggplot(aes(x=target, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, group=term, color=sig)) +
    geom_pointrange(position = position_dodge(width=0.5), size=1) +
    geom_text(aes(label = term, y = estimate+1.96*std.error+2), position = position_dodge(width=0.5), color="black") +
    coord_flip() +z
    geom_hline(yintercept=0, lty=2, alpha=0.5) + 
    geom_vline(xintercept=4.5, lty=1, alpha=0.5) +
    ylab("Effect on affect thermometer") + xlab("Affect thermometer target") +
    facet_wrap(. ~ model) + 
    scale_color_identity() +
    theme_linedraw2 + 
        theme(
            legend.position = "none",
            axis.text.x = element_text(size=20),
            axis.text.y = element_text(size=20),
            strip.text = element_text(size=26),
            panel.spacing = unit(2, "lines"),
            axis.title.x = element_text(size=24),
            axis.title.y = element_text(size=24)
        )
ggsave("figures/firststage_feelings_placebo.pdf", width=15.5, height=8)
if(SHOW_PDFS) system("open figures/firststage_feelings_placebo.pdf")

## other general affective responses
## @SB: skit was found to be most funny, least informative, and most offensive (ad also found offensive)
dat %>%
    filter(!is.na(treat), treat != "control") %>%
    filter(exp_1_prompt_control==T) %>%
    group_by(treat) %>% 
    summarise(believed_funny1=weighted.mean(believed_funny1,weight,na.rm=T),
              believed_true1=weighted.mean(believed1_true,weight,na.rm=T),
              believed_informative1=weighted.mean(believed_informative1,weight,na.rm=T),
              believed_offensive1=weighted.mean(believed_offensive1,weight,na.rm=T)) %>%
    ungroup() %>%
    gather(key="belief", value="%", "believed_true1", "believed_funny1", "believed_informative1", "believed_offensive1") %>%
    mutate(treat = as.character(treat)) %>%
    mutate(treat = replace(treat, treat == "text", "text")) %>%
    mutate(treat = replace(treat, treat == "audio", "audio")) %>%
    mutate(treat = replace(treat, treat == "video", "video")) %>%
    mutate(treat = replace(treat, treat == "skit", "skit")) %>%
    mutate(treat = replace(treat, treat == "ad", "ad")) %>%
    mutate(belief = gsub("believed_|1", "", belief)) %>%
    mutate(belief = replace(belief, belief == "true", "real")) %>%
    filter(!(belief == "real")) %>%
    mutate(belief = stringr::str_to_title(belief)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "ad", after=0)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=5)) %>%
    mutate(belief = fct_relevel(as.factor(belief), "real", after=0)) %>%
    ggplot(aes(x=treat, y=`%`)) + 
    facet_grid(~ belief, scales="free_x") +
    geom_text(aes(label=scales::percent(round(`%`,2), accuracy=1), y=`%`+0.05)) +
    geom_bar(stat="identity") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    geom_vline(aes(xintercept=ifelse(belief != "real", 1.5, -1)), lty=2, size=.5, alpha=0.8) +
    ylab("% somewhat/strongly believe clipping was...") + xlab("") +
    theme_linedraw2 + 
        theme(
            axis.text.x = element_text(size=10),
            axis.text.y = element_text(size=12),
            strip.text = element_text(size=18),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=8)
        )
ggsave("figures/firststage_belief_other.pdf", width=8, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_belief_other.pdf") ## this is technically not pre-registered


(m <- lm(believed_funny ~ treat, dat %>% filter(!is.na(treat)), weights=weight)); summary(m); 
(m <- lm(believed_informative ~ treat, dat %>% filter(!is.na(treat)), weights=weight)); summary(m); 
(m <- lm(believed_offensive ~ treat, dat %>% filter(!is.na(treat)), weights=weight)); summary(m); 

(m <- lm(believed_funny ~ treat + response_wave_ID + meta_OS + age_65 + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat)), weights=weight)); summary(m); 
(m <- lm(believed_informative ~ treat + response_wave_ID + meta_OS + age_65 + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat)), weights=weight)); summary(m); 
(m <- lm(believed_offensive ~ treat + response_wave_ID + meta_OS + age_65 + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat)), weights=weight)); summary(m); 


#####------------------------------------------------------#
##### H3: Deepfake salience effect on media trust/FPR ####
#####------------------------------------------------------#

## 3a/I: information prompt decreases media trust
## (overall, offline, online-only, social media)

## @SB: no effect
(h3aI.m <- lm(post_media_trust ~ exp_1_prompt, dat%>%filter(!lowq)%>%filter(treat!="skit",treat!="ad"))); summary(h3aI.m); 
(h3aI.m.adj <- lm(post_media_trust ~ exp_1_prompt + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat%>%filter(!lowq)%>%filter(treat!="skit",treat!="ad"))); summary(h3aI.m.adj); 
(h3aI.m.wt <- lm(post_media_trust ~ exp_1_prompt, dat%>%filter(!lowq)%>%filter(treat!="skit",treat!="ad"), weights=weight)); summary(h3aI.m.wt); 
(h3aI.m.adj.wt <- lm(post_media_trust ~ exp_1_prompt + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat%>%filter(!lowq)%>%filter(treat!="skit",treat!="ad"), weights=weight)); summary(h3aI.m.adj.wt); 
(h3aI.m.hq <- lm(post_media_trust ~ exp_1_prompt, dat%>%filter(!lowq)%>%filter(treat!="skit",treat!="ad"))); summary(h3aI.m.hq); 
(h3aI.m.adj.hq <- lm(post_media_trust ~ exp_1_prompt + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat%>%filter(!lowq)%>%filter(treat!="skit",treat!="ad"))); summary(h3aI.m.adj.hq); 
(h3aI.m.hq.wt <- lm(post_media_trust ~ exp_1_prompt, dat%>%filter(!lowq)%>%filter(treat!="skit",treat!="ad"), weights=weight)); summary(h3aI.m.hq); 
(h3aI.m.adj.hq.wt <- lm(post_media_trust ~ exp_1_prompt + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat%>%filter(!lowq)%>%filter(treat!="skit",treat!="ad"), weights=weight)); summary(h3aI.m.adj.hq); 

(h3aI.m.onl <- lm(post_media_trust1 ~ exp_1_prompt, dat%>%filter(!lowq)%>%filter(treat!="skit",treat!="ad"))); summary(h3aI.m.onl); 
(h3aI.m.onl.adj <- lm(post_media_trust1 ~ exp_1_prompt + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat%>%filter(!lowq)%>%filter(treat!="skit",treat!="ad"))); summary(h3aI.m.onl.adj); 

(h3aI.m.off <- lm(post_media_trust2 ~ exp_1_prompt, dat%>%filter(!lowq)%>%filter(treat!="skit",treat!="ad"))); summary(h3aI.m.off); 
(h3aI.m.off.adj <- lm(post_media_trust2 ~ exp_1_prompt + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat%>%filter(!lowq)%>%filter(treat!="skit",treat!="ad"))); summary(h3aI.m.off.adj); 

(h3aI.m.soc <- lm(post_media_trust3 ~ exp_1_prompt, dat%>%filter(!lowq)%>%filter(treat!="skit",treat!="ad"))); summary(h3aI.m.soc); 
(h3aI.m.soc.adj <- lm(post_media_trust3 ~ exp_1_prompt + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat%>%filter(!lowq)%>%filter(treat!="skit",treat!="ad"))); summary(h3aI.m.soc.adj); 


### regression tables

h3aI.m.df <- bind_rows(
    tidy(h3aI.m),
    tidy(h3aI.m.adj),
    tidy(h3aI.m.wt),
    tidy(h3aI.m.adj.wt),
    tidy(h3aI.m.hq),
    tidy(h3aI.m.adj.hq),
    tidy(h3aI.m.hq.wt),
    tidy(h3aI.m.adj.hq.wt)
)
h3aI.m.df$p.value.adj <- (h3aI.m.df$p.value/order(h3aI.m.df$p.value))*nrow(h3aI.m.df) ##BHq corrections

stargazer(h3aI.m,
          h3aI.m.wt,
          h3aI.m.hq,
          h3aI.m.adj,
          h3aI.m.adj.wt,
          h3aI.m.adj.hq,
          h3aI.m.adj.hq.wt,
          header = FALSE,
          apply.p = function(p) { h3aI.m.df$p.value.adj[h3aI.m.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$",
                     "Respondents in \\texttt{skit} and \\texttt{ad} conditions are excluded."),
          no.space = TRUE,
          digits=2,
          table.layout ="=ld#-t-a-s=n",
          notes.append = FALSE,
          notes.align = "l",
          # notes = c("$^{*}$p $<$ 0.1; $^{**}$p $<$ .05; $^{***}$p $<$ .01"),
          title="\\textbf{Models of Information Provision and Media Trust in Incidental Exposure Experiment}",
          add.lines = list(c("Weighted?", "", "\\checkmark", "", "", "\\checkmark","","\\checkmark"),
                           c("Low-Quality Dropped?","","","\\checkmark","","","\\checkmark","\\checkmark"),
                           c("Controls?","","","","\\checkmark","\\checkmark","\\checkmark","\\checkmark")),    
          omit = c("response_wave_ID", COVARS),
          # notes = c("\\textit{Notes}: Same controls used as in previous models."),
          covariate.labels = c("Information"),
          dep.var.labels = c("\\normalsize Trust in Media (Combined Index)"),
          omit.stat=c("f", "ser"),
          column.sep.width = "-2pt",
          font.size = "footnotesize",
          # style = "apsr",
          label = "firststage_mediatrust",
          out="tables/firststage_mediatrust.tex") ##NEED TO MANUALLY ADJUST TO MATCH FORMAT OF REST

h3aI.m.df2 <- bind_rows(
    tidy(h3aI.m.onl),
    tidy(h3aI.m.onl.adj),
    tidy(h3aI.m.off),
    tidy(h3aI.m.off.adj),
    tidy(h3aI.m.soc),
    tidy(h3aI.m.soc.adj),
    tidy(h3aI.m),
    tidy(h3aI.m.adj)
)
h3aI.m.df2$p.value.adj <- (h3aI.m.df2$p.value/order(h3aI.m.df2$p.value))*nrow(h3aI.m.df2) ##BHq corrections

stargazer(h3aI.m.onl,
          h3aI.m.onl.adj,
          h3aI.m.off,
          h3aI.m.off.adj,
          h3aI.m.soc,
          h3aI.m.soc.adj,
          h3aI.m, 
          h3aI.m.adj,
          header = FALSE,
          apply.p = function(p) { h3aI.m.df$p.value.adj[h3aI.m.df$p.value == p][1] },
          notes =  c("$^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$",
                     "Respondents in \\texttt{skit} and \\texttt{ad} conditions are excluded."),
          no.space = TRUE,
          digits=2,
          table.layout ="=ld#-t-a-s=n",
          notes.append = FALSE,
          notes.align = "l",
          # notes = c("$^{*}$p $<$ 0.1; $^{**}$p $<$ .05; $^{***}$p $<$ .01"),
          title="\\textbf{Models of Information Provision and Media Trust Across Sources in Incidental Exposure Experiment}",
          add.lines = list(c("Controls?","","\\checkmark","","\\checkmark","","\\checkmark","","\\checkmark")),    
          omit = c("response_wave_ID", COVARS),
          covariate.labels = c("Information"),
          dep.var.caption  = "\\normalsize Trust in...",
          dep.var.labels = c("Offline Media", "Online Media", "Social Media", "Combined Index"),
          omit.stat=c("f", "ser"),
          column.sep.width = "-2pt",
          font.size = "footnotesize",
          # style = "apsr",
          out="tables/firststage_mediatrust2.tex") ##NEED TO MANUALLY ADJUST TO MATCH FORMAT OF REST


## 3a/II: seeing and recognizing a deepfake decreases media trust

## @SB: believing scandal is true decreases trust in media ... but mostly
##      offline media, NOT online media; may not be moving social media trust 
##      becauase there's a floor there -- no heterogeneity by medium

(h3aII.m <- lm(post_media_trust ~ believed_true*I(treat=="video"), dat %>% filter(!is.na(treat), treat%in%c("audio","video","text","control"), exp_1_prompt_control))); summary(h3aII.m); 
(h3aII.m1 <- lm(post_media_trust1 ~ believed_true*I(treat=="video"), dat %>% filter(!is.na(treat), treat%in%c("audio","video","text","control"), exp_1_prompt_control))); summary(h3aII.m1); 
(h3aII.m2 <- lm(post_media_trust2 ~ believed_true*I(treat=="video"), dat %>% filter(!is.na(treat), treat%in%c("audio","video","text","control"), exp_1_prompt_control))); summary(h3aII.m2); 
(h3aII.m3 <- lm(post_media_trust3 ~ believed_true*I(treat=="video"), dat %>% filter(!is.na(treat), treat%in%c("audio","video","text","control"), exp_1_prompt_control))); summary(h3aII.m3); 

(h3aII.m.adj <- lm(post_media_trust ~ believed_true*I(treat=="video")  + response_wave_ID + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat), treat%in%c("audio","video","text","control"), exp_1_prompt_control))); summary(h3aII.m.adj); 
(h3aII.m.adj.wt <- lm(post_media_trust ~ believed_true*I(treat=="video")  + response_wave_ID + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat), treat%in%c("audio","video","text","control"), exp_1_prompt_control), weights=weight)); summary(h3aII.m.adj.wt); 
(h3aII.m.adj.hq <- lm(post_media_trust ~ believed_true*I(treat=="video")  + response_wave_ID + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat), treat%in%c("audio","video","text","control"), exp_1_prompt_control, !lowq))); summary(h3aII.m.adj.hq); 
(h3aII.m.adj.hq.wt <- lm(post_media_trust ~ believed_true*I(treat=="video")  + response_wave_ID + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat), treat%in%c("audio","video","text","control"), exp_1_prompt_control, !lowq), weights=weight)); summary(h3aII.m.adj.hq.wt); 
(h3aII.m.adj.b1 <- lm(post_media_trust ~ believed_true*I(treat=="video")  + response_wave_ID + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat), treat%in%c("audio","video","text","control"), exp_1_prompt_control) %>% mutate(believed_true=believed1_true))); summary(h3aII.m.adj.b1); 
(h3aII.m.adj.hq.wt.b1 <- lm(post_media_trust ~ believed_true*I(treat=="video")  + response_wave_ID + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat), treat%in%c("audio","video","text","control"), exp_1_prompt_control) %>% mutate(believed_true=believed1_true,!lowq), weights=weight)); summary(h3aII.m.adj.hq.wt.b1); 

(h3aII.m1.adj <- lm(post_media_trust1 ~ believed_true*I(treat=="video") + response_wave_ID + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat), treat%in%c("audio","video","text","control"), exp_1_prompt_control))); summary(h3aII.m1.adj); 
(h3aII.m1.adj.b1 <- lm(post_media_trust1 ~ believed_true*I(treat=="video") + response_wave_ID + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat), treat%in%c("audio","video","text","control"), exp_1_prompt_control) %>% mutate(believed_true=believed1_true))); summary(h3aII.m1.adj.b1); 
(h3aII.m2.adj <- lm(post_media_trust2 ~ believed_true*I(treat=="video") + response_wave_ID + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat), treat%in%c("audio","video","text","control"), exp_1_prompt_control))); summary(h3aII.m2.adj); 
(h3aII.m2.adj.b1 <- lm(post_media_trust2 ~ believed_true*I(treat=="video") + response_wave_ID + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat), treat%in%c("audio","video","text","control"), exp_1_prompt_control) %>% mutate(believed_true=believed1_true))); summary(h3aII.m2.adj.b1); 
(h3aII.m3.adj <- lm(post_media_trust3 ~ believed_true*I(treat=="video") + response_wave_ID + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat), treat%in%c("audio","video","text","control"), exp_1_prompt_control))); summary(h3aII.m3.adj); 
(h3aII.m3.adj.b1 <- lm(post_media_trust3 ~ believed_true*I(treat=="video") + response_wave_ID + meta_OS + age_65 + educ + PID + crt + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat), treat%in%c("audio","video","text","control"), exp_1_prompt_control) %>% mutate(believed_true=believed1_true))); summary(h3aII.m3.adj.b1); 


### regression tables

h3aII.m.df <- bind_rows(
    tidy(h3aII.m),
    tidy(h3aII.m.adj),
    tidy(h3aII.m.adj.b1),
    tidy(h3aII.m.adj.wt),
    tidy(h3aII.m.adj.hq),
    tidy(h3aII.m.adj.hq.wt),
    tidy(h3aII.m.adj.hq.wt.b1)
)
h3aII.m.df$p.value.adj <- (h3aII.m.df$p.value/order(h3aII.m.df$p.value))*nrow(h3aII.m.df) ##BHq corrections

stargazer(h3aII.m,
          h3aII.m.adj,
          h3aII.m.adj.b1,
          h3aII.m.adj.wt,
          h3aII.m.adj.hq,
          h3aII.m.adj.hq.wt,
          h3aII.m.adj.hq.wt.b1,
          header = FALSE,
          no.space = TRUE,
          digits=2,
          table.layout ="=ld#-t-a-s=n",
          notes.append = FALSE,
          apply.p = function(p) { h3aII.m.df$p.value.adj[h3aII.m.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$",
                     "Respondents in \\texttt{skit} and \\texttt{ad} conditions are excluded."),
          notes.align = "l",
            add.lines = list(
                             c("Weighted?","","","","\\checkmark","","\\checkmark","\\checkmark"),
                             c("Low-Quality Dropped?","","","","","\\checkmark","\\checkmark","\\checkmark"),
                             c("Controls?","","\\checkmark","\\checkmark","\\checkmark","\\checkmark","\\checkmark","\\checkmark"),
                             c("Credibility Binarized?","","","\\checkmark","","","","\\checkmark")),    
          title="\\textbf{Models of Deepfake Exposure, Credibility, and Media Trust Across Sources in Incidental Exposure Experiment}",
          omit = c(COVARS, "response_wave_ID"),
          covariate.labels = c("Credible",
                               "Video",
                               "Credible x Video"),
          dep.var.labels  = "\\normalsize Trust in Media (Combined Index)",
          omit.stat=c("f", "ser"),
          column.sep.width = "-2pt",
          font.size = "footnotesize",
          style = "apsr",
          out="tables/firststage_mediatrust3.tex") ##NEED TO MANUALLY ADJUST TO MATCH FORMAT OF REST

h3aII.m.df2 <- bind_rows(
    tidy(h3aII.m1.adj),
    tidy(h3aII.m1.adj.b1),
    tidy(h3aII.m2.adj),
    tidy(h3aII.m2.adj.b1),
    tidy(h3aII.m3.adj),
    tidy(h3aII.m3.adj.b1),
    tidy(h3aII.m.adj)
)
h3aII.m.df2$p.value.adj <- (h3aII.m.df2$p.value/order(h3aII.m.df2$p.value))*nrow(h3aII.m.df2) ##BHq corrections


stargazer(h3aII.m1.adj,
          h3aII.m1.adj.b1,
          h3aII.m2.adj,
          h3aII.m2.adj.b1, 
          h3aII.m3.adj,
          h3aII.m3.adj.b1,
          h3aII.m.adj,
          h3aII.m.adj.b1,
          header = FALSE,
          no.space = TRUE,
          digits=2,
          table.layout ="=ld#-t-a-s=n",
          apply.p = function(p) { h3aII.m.df2$p.value.adj[h3aII.m.df2$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$",
                     "Respondents in \\texttt{skit} and \\texttt{ad} conditions are excluded."),
          notes.append = FALSE,
          notes.align = "l",
            add.lines = list(c("Controls?","\\checkmark","\\checkmark","\\checkmark","\\checkmark","\\checkmark","\\checkmark","\\checkmark","\\checkmark"),
                             c("Credibility Binarized?","", "\\checkmark","","\\checkmark","","\\checkmark","","\\checkmark")),    
          title="\\textbf{Models of Deepfake Exposure, Credibility, and Media Trust Across Sources in Incidental Exposure Experiment}",
          omit = c(COVARS, "response_wave_ID"),
          covariate.labels = c("Credibility",
                               "Video",
                               "Credibility x Video"),
          dep.var.caption  = "\\normalsize Trust in...",
          dep.var.labels = c("Offline Media", "Online Media", "Social Media", "Combined Index"),
          omit.stat=c("f", "ser"),
          column.sep.width = "-2pt",
          font.size = "footnotesize",
          # style = "apsr",
          out="tables/firststage_mediatrust4.tex") ##NEED TO MANUALLY ADJUST TO MATCH FORMAT OF REST


## 3b/III: debrief increases false detection rate of deepfakes
## @SB: additional debrief definitely increases FDR; environment/mode itself also massively
##      increases FPR (i.e. low/no deepfakes)
(m <- lm(exp_2_pct_false_fake ~ exp_2_after_debrief, dat, weights=weight)); summary(m); 
(m <- lm(exp_2_pct_false_fake ~ exp_2_after_debrief + exp_2, dat, weights=weight)); summary(m); 
(m <- lm(exp_2_pct_false_fake ~ exp_2_after_debrief + exp_2 + exp_2_prompt + age_65 + educ + PID + I(gender=="Male") + polknow + internet_usage, dat, weights=weight)); summary(m); 


## 3b/IV: information prompts increase false detection rate of deepfakes
## @SB: no effect of information prompt on FDR
(m <- lm(exp_2_pct_false_fake ~ exp_1_prompt, dat, weights=weight)); summary(m); 
(m <- lm(exp_2_pct_false_fake ~ exp_1_prompt + exp_2, dat, weights=weight)); summary(m); 
(m <- lm(exp_2_pct_false_fake ~ exp_1_prompt + exp_2 + exp_2_after_debrief + age_65 + educ + PID + gender + polknow + internet_usage, dat, weights=weight)); summary(m); 

### (see final section for plots and regression tables)

#####------------------------------------------------------#
##### H4: Heterogeneity in deception effect by info ####
#####------------------------------------------------------#

n0 <- dat %>% 
    filter(exp_1_prompt_control==T, !is.na(believed_true)) %>% nrow()
n1 <- dat %>% 
    filter(exp_1_prompt_control==F, !is.na(believed_true)) %>% nrow()

### non-parametric tests
t.test(dat$believed_true[dat$exp_1_prompt_info],
       dat$believed_true[dat$exp_1_prompt_control]) ##t = -6.8445, df = 4177.9, p-value = 8.79e-12, \delta = -0.284111

t.test(dat$believed_true[dat$exp_1_prompt_info & dat$treat_fake_video],
       dat$believed_true[dat$exp_1_prompt_control & dat$treat_fake_video]) ##t = -3.918, df = 853.94, p-value = 9.641e-05, \delta = -0.354745


### visualise just by info condition
dat %>% 
    filter(!is.na(exp_1_prompt)) %>%
    group_by(exp_1_prompt) %>%
    summarise(y=weighted.mean(believed_true,weight,na.rm=T),
              ymin=weighted.mean(believed_true,weight,na.rm=T)-1.96*weighted.sd(believed_true,weight,na.rm=T)/sqrt(n()),
              ymax=weighted.mean(believed_true,weight,na.rm=T)+1.96*weighted.sd(believed_true,weight,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    mutate(exp_1_prompt = as.character(exp_1_prompt)) %>%
    mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "info", "Information")) %>%
    mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "control", "No information")) %>%
    mutate(exp_1_prompt = fct_relevel(as.factor(exp_1_prompt), "No information", after=0)) %>%
    ggplot(aes(x=exp_1_prompt, y=y, ymin=ymin, ymax=ymax)) +
    geom_bar(stat="identity") +
    ylab("Credibility confidence") + xlab("") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    ggtitle(paste0("n_control=",n0," | n_info=",n1)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            strip.text = element_text(size=16),
            axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=14)
        )
ggsave("figures/firststage_belief_byinfo.pdf", width=4, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_belief_byinfo.pdf")

dat %>% 
    filter(!is.na(exp_1_prompt)) %>%
    group_by(exp_1_prompt) %>%
    summarise(y=weighted.mean(believed1_true,weight,na.rm=T)) %>%
    ungroup() %>%
    mutate(exp_1_prompt = as.character(exp_1_prompt)) %>%
    mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "info", "Information")) %>%
    mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "control", "No information")) %>%
    mutate(exp_1_prompt = fct_relevel(as.factor(exp_1_prompt), "No information", after=0)) %>%
    ggplot(aes(x=exp_1_prompt, y=y)) +
    geom_bar(stat="identity") + xlab("") +
    ylab("% somewhat/strongly confident in credibility") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    ggtitle(paste0("n_control=",n0," | n_info=",n1)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            strip.text = element_text(size=16),
            axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=8)
        )
ggsave("figures/firststage_belief_byinfo_binary.pdf", width=4, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_belief_byinfo_binary.pdf")

### visualise by info condition and medium
dat.viz <- dat %>% 
    filter(!is.na(exp_1_prompt)) %>%
    filter(!is.na(treat), treat != "ad", treat != "control", treat != "skit") %>%
    group_by(exp_1_prompt, treat) %>%
    summarise(y=weighted.mean(believed_true,weight,na.rm=T),
              ymin=weighted.mean(believed_true,weight,na.rm=T)-1.96*weighted.sd(believed_true,weight,na.rm=T)/sqrt(n()),
              ymax=weighted.mean(believed_true,weight,na.rm=T)+1.96*weighted.sd(believed_true,weight,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    mutate(exp_1_prompt = as.character(exp_1_prompt)) %>%
    mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "info", "Information")) %>%
    mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "control", "No information")) %>%
    mutate(exp_1_prompt = fct_relevel(as.factor(exp_1_prompt), "No information", after=0)) %>%
    mutate(treat = as.character(treat)) %>%
    mutate(treat = replace(treat, treat == "text", "text")) %>%
    mutate(treat = replace(treat, treat == "audio", "audio")) %>%
    mutate(treat = replace(treat, treat == "video", "video")) %>%
    mutate(treat = replace(treat, treat == "skit", "skit")) %>%
    mutate(treat = replace(treat, treat == "ad", "attack ad")) %>%
    mutate(treat = fct_relevel(as.factor(treat), "control", after=0)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "attack ad", after=1)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=5))

dat.viz %>%
    ggplot(aes(x=treat, y=y, ymin=ymin, ymax=ymax)) +
    geom_bar(stat="identity") +
    ylab("Credibility confidence") + xlab("Scandal clipping") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    scale_y_continuous(limits=c(2,3.75),oob = scales::rescale_none) +
    facet_wrap(~exp_1_prompt) +
    geom_hline(data = dat.viz %>% group_by(exp_1_prompt) %>% summarise(yint=mean(y)),
               aes(yintercept = yint), size=1, lty=1, color="red") +
    ggtitle(paste0("n_control=",n0," | n_info=",n1)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            strip.text = element_text(size=16),
            axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=14)
        )
ggsave("figures/firststage_belief_byinfo2.pdf", width=8, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_belief_byinfo2.pdf")

dat.viz <- dat %>% 
    filter(!is.na(exp_1_prompt)) %>%
    filter(!is.na(treat), treat != "ad", treat != "control", treat != "skit") %>%
    group_by(exp_1_prompt, treat) %>%
    summarise(y=weighted.mean(believed1_true,weight,na.rm=T)) %>%
    ungroup() %>%
    mutate(exp_1_prompt = as.character(exp_1_prompt)) %>%
    mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "info", "Information")) %>%
    mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "control", "No information")) %>%
    mutate(exp_1_prompt = fct_relevel(as.factor(exp_1_prompt), "No information", after=0)) %>%
    mutate(treat = as.character(treat)) %>%
    mutate(treat = replace(treat, treat == "text", "text")) %>%
    mutate(treat = replace(treat, treat == "audio", "audio")) %>%
    mutate(treat = replace(treat, treat == "video", "video"))


dat.viz %>%
    ggplot(aes(x=treat, y=y)) +
    geom_bar(stat="identity") +
    ylab("% somewhat/strongly confident in credibility") + xlab("Scandal clipping") +
    # scale_y_continuous(limits=c(2,3.75),oob = scales::rescale_none) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    facet_wrap(~exp_1_prompt) +
    geom_text(aes(label=scales::percent(round(y,2), accuracy=1), y=y+0.04)) +
    geom_hline(data = dat.viz %>% group_by(exp_1_prompt) %>% summarise(yint=mean(y)),
               aes(yintercept = yint), size=1, lty=1, color="red") +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            strip.text = element_text(size=16),
            axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=8)
        )
ggsave("figures/firststage_belief_byinfo_binary2.pdf", width=8, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_belief_byinfo_binary2.pdf")


## @SB: EXTREMELY consistent negative effect on deception
# (h4.full.m <- lm(believed_true ~ exp_1_prompt*treat, dat, weights=weight)); summary(h4.full.m); 
# (h4.full.m.adj <- lm(believed_true ~  exp_1_prompt*treat + response_wave_ID +meta_OS + age_65 + educ + PID*crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat, weights=weight)); summary(h4.full.m.adj);

(h4.full.m <- lm(believed_true ~  exp_1_prompt*treat, dat%>%filter(treat != "ad", treat != "skit"))); summary(h4.full.m); 
(h4.full.m.wt <- lm(believed_true ~  exp_1_prompt*treat, dat%>%filter(treat != "ad", treat != "skit"), weights=weight)); summary(h4.full.m.wt); 
(h4.full.m.hq <- lm(believed_true ~  exp_1_prompt*treat, dat%>%filter(!lowq,treat != "ad", treat != "skit"))); summary(h4.full.m.hq); 

(h4.full.m.adj <- lm(believed_true ~ exp_1_prompt*treat +meta_OS + age_65 + educ + PID*crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat%>%filter(treat != "ad", treat != "skit"))); summary(h4.full.m.adj);
(h4.full.m.adj.wt <- lm(believed_true ~ exp_1_prompt*treat +meta_OS + age_65 + educ + PID*crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat%>%filter(treat != "ad", treat != "skit"), weights=weight)); summary(h4.full.m.adj.wt);
(h4.full.m.adj.hq <- lm(believed_true ~ exp_1_prompt*treat +meta_OS + age_65 + educ + PID*crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat%>%filter(!lowq,treat != "ad", treat != "skit"))); summary(h4.full.m.adj.hq);
(h4.full.m.adj.wt.hq <- lm(believed_true ~ exp_1_prompt*treat +meta_OS + age_65 + educ + PID*crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat%>%filter(!lowq,treat != "ad", treat != "skit"), weights=weight)); summary(h4.full.m.adj.wt.hq);

h4.full.m.df <- bind_rows(
    tidy(h4.full.m),
    tidy(h4.full.m.wt),
    tidy(h4.full.m.hq),
    tidy(h4.full.m.adj),
    tidy(h4.full.m.adj.wt),
    tidy(h4.full.m.adj.hq),
    tidy(h4.full.m.adj.wt.hq)
) 
h4.full.m.df$p.value.adj <- (h4.full.m.df$p.value/order(h4.full.m.df$p.value))*nrow(h4.full.m.df) ##BHq corrections


stargazer(h4.full.m,
          h4.full.m.wt,
          h4.full.m.hq,
          h4.full.m.adj,
          h4.full.m.adj.wt,
          h4.full.m.adj.hq,
          h4.full.m.adj.wt.hq,
          header = FALSE,
          no.space=TRUE,
          digits=2,
          table.layout ="=d#-t-a-s=n",
          apply.p = function(p) { h4.full.m.df$p.value.adj[h4.full.m.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$"),
          notes.append = FALSE,
          notes.align = "l",
          title="\\textbf{Models of Information Provision and Credibility Confidence of Clipping in Incidental Exposure Experiment}",
          covariate.labels = c(
              "Information", "Audio", "Text",# "Skit", 
              # "Attack Ad",
              "Info x Audio", "Info x Text"#, "Info x Skit"
              # "Info x Ad"
          ),
          omit = c(COVARS, "response_wave_ID"),
          dep.var.labels = c("\\normalsize Confidence that clipping was credible [1-5]"),
          add.lines = list(c("Weighted?", "", "\\checkmark", "", "", "\\checkmark","","\\checkmark"),
                           c("Low-Quality Dropped?","","","\\checkmark","","","\\checkmark","\\checkmark"),
                           c("Controls?","","","","\\checkmark","\\checkmark","\\checkmark","\\checkmark")),
          omit.stat=c("f", "ser"),
          column.sep.width = "1pt",
          font.size = "footnotesize",
          style = "apsr",
          label="firststage_deception_byinfo",
          out="tables/firststage_deception_byinfo.tex")

(h4.full.m.bin <- lm(believed1_true ~  exp_1_prompt*treat, dat%>%filter(treat != "ad", treat != "skit"))); summary(h4.full.m.bin); 
(h4.full.m.bin.wt <- lm(believed1_true ~  exp_1_prompt*treat, dat%>%filter(treat != "ad", treat != "skit"), weights=weight)); summary(h4.full.m.bin.wt); 
(h4.full.m.bin.hq <- lm(believed1_true ~  exp_1_prompt*treat, dat%>%filter(!lowq,treat != "ad", treat != "skit"))); summary(h4.full.m.bin.hq); 

(h4.full.m.bin.adj <- lm(believed1_true ~ exp_1_prompt*treat +meta_OS + age_65 + educ + PID*crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat%>%filter(treat != "ad", treat != "skit"))); summary(h4.full.m.bin.adj);
(h4.full.m.bin.adj.wt <- lm(believed1_true ~ exp_1_prompt*treat +meta_OS + age_65 + educ + PID*crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat%>%filter(treat != "ad", treat != "skit"), weights=weight)); summary(h4.full.m.bin.adj.wt);
(h4.full.m.bin.adj.hq <- lm(believed1_true ~ exp_1_prompt*treat +meta_OS + age_65 + educ + PID*crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat%>%filter(!lowq,treat != "ad", treat != "skit"))); summary(h4.full.m.bin.adj.hq);
(h4.full.m.bin.adj.wt.hq <- lm(believed1_true ~ exp_1_prompt*treat +meta_OS + age_65 + educ + PID*crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat%>%filter(!lowq,treat != "ad", treat != "skit"), weights=weight)); summary(h4.full.m.bin.adj.wt.hq);

h4.full.m.bin.df <- bind_rows(
    tidy(h4.full.m.bin),
    tidy(h4.full.m.bin.wt),
    tidy(h4.full.m.bin.hq),
    tidy(h4.full.m.bin.adj),
    tidy(h4.full.m.bin.adj.wt),
    tidy(h4.full.m.bin.adj.hq),
    tidy(h4.full.m.bin.adj.wt.hq)
)
h4.full.m.bin.df$p.value.adj <- (h4.full.m.bin.df$p.value/order(h4.full.m.bin.df$p.value))*nrow(h4.full.m.bin.df) ##BHq corrections

stargazer(h4.full.m.bin,
          h4.full.m.bin.wt,
          h4.full.m.bin.hq,
          h4.full.m.bin.adj,
          h4.full.m.bin.adj.wt,
          h4.full.m.bin.adj.hq,
          h4.full.m.bin.adj.wt.hq,
          header = FALSE,
          no.space=TRUE,
          digits=2,
          table.layout ="=d#-t-a-s=n",
          apply.p = function(p) { h4.full.m.bin.df$p.value.adj[h4.full.m.bin.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$"),
          notes.append = FALSE,
          notes.align = "l",
          title="\\textbf{Models of Information Provision and Binarized Credibility Confidence of Clipping in Incidental Exposure Experiment}",
          covariate.labels = c(
              "Information", "Audio", "Text",# "Skit", 
              # "Attack Ad",
              "Info x Audio", "Info x Text"#, "Info x Skit"
              # "Info x Ad"
          ),
          omit = c(COVARS, "response_wave_ID"),
          dep.var.labels = c("\\normalsize Somewhat/strongly confident clipping was credible"),
          add.lines = list(c("Weighted?", "", "\\checkmark", "", "", "\\checkmark","","\\checkmark"),
                           c("Low-Quality Dropped?","","","\\checkmark","","","\\checkmark","\\checkmark"),
                           c("Controls?","","","","\\checkmark","\\checkmark","\\checkmark","\\checkmark")),
          omit.stat=c("f", "ser"),
          column.sep.width = "1pt",
          font.size = "footnotesize",
          style = "apsr",
          label="firststage_deception_byinfo2",
          out="tables/firststage_deception_byinfo2.tex")



#####------------------------------------------------------#
##### H5: Heterogeneity in deception effect by cognition ####
#####------------------------------------------------------#

n <- dat %>% 
    filter(!is.na(treat), treat != "ad", treat != "control", treat != "skit", exp_1_prompt_control==T, !is.na(crt), !is.na(believed_true)) %>% nrow()
n.low <- dat %>%
    mutate(crt=cut(crt, breaks=c(-1,0,.34,1.1), 
                   labels=c("Low c.r.", "Moderate c.r.", "High c.r."))) %>%
    filter(!is.na(treat), treat != "ad", treat != "control", treat != "skit", exp_1_prompt_control==T, !is.na(crt), !is.na(believed_true)) %>%
    filter(crt == "Low c.r.") %>% nrow()
n.mod <- dat %>%
    mutate(crt=cut(crt, breaks=c(-1,0,.34,1.1), 
                   labels=c("Low c.r.", "Moderate c.r.", "High c.r."))) %>%
    filter(!is.na(treat), treat != "ad", treat != "control", treat != "skit", exp_1_prompt_control==T, !is.na(crt), !is.na(believed_true)) %>%
    filter(crt == "Moderate c.r.") %>% nrow()
n.high <- dat %>%
    mutate(crt=cut(crt, breaks=c(-1,0,.34,1.1), 
                   labels=c("Low c.r.", "Moderate c.r.", "High c.r."))) %>%
    filter(!is.na(treat), treat != "ad", treat != "control", treat != "skit", exp_1_prompt_control==T, !is.na(crt), !is.na(believed_true)) %>%
    filter(crt == "High c.r.") %>% nrow()

## descriptive plots
### visualize by medium
dat.viz <- dat %>% 
    filter(!is.na(treat), treat != "ad", treat != "control", treat != "skit", exp_1_prompt_control==T) %>%
    # mutate(crt=cut(crt, breaks=c(-1,0,.34,.67,1.1), 
    #                              labels=c("C.R. = 0/3", "C.R. = 1/3", "C.R. = 2/3", "C.R. = 3/3"))) %>%
    mutate(crt=cut(crt, breaks=c(-1,0,.34,1.1), 
                                 labels=c("Low c.r.", "Moderate c.r.", "High c.r."))) %>%
    filter(!is.na(crt), !is.nan(crt)) %>%
    group_by(treat, crt) %>%
    summarise(y=weighted.mean(believed_true,weight,na.rm=T),
              ymin=weighted.mean(believed_true,weight,na.rm=T)-1.96*weighted.sd(believed_true,weight,na.rm=T)/sqrt(n()),
              ymax=weighted.mean(believed_true,weight,na.rm=T)+1.96*weighted.sd(believed_true,weight,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    mutate(treat = as.character(treat))

dat.viz %>%
    ggplot(aes(x=treat, y=y, ymin=ymin, ymax=ymax)) +
    geom_bar(stat="identity") +
    ylab("Credibility confidence") + xlab("Scandal clipping") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    scale_y_continuous(limits=c(1,4),oob = scales::rescale_none) +
    facet_grid(~crt) +
    geom_hline(data = dat.viz %>% group_by(crt) %>% summarise(yint=mean(y)),
               aes(yintercept = yint), size=1, lty=1, color="red") +
    ggtitle(paste0("n=",n," | n_low=",n.low," | n_mod=",n.mod," | n_high=",n.high)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            strip.text = element_text(size=16),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16)
        )
ggsave("figures/firststage_belief_byC.R..pdf", width=8, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_belief_byC.R..pdf")

dat.viz <- dat %>% 
    filter(!is.na(treat), treat != "ad", treat != "control", exp_1_prompt_control==T) %>%
    # mutate(crt=cut(crt, breaks=c(-1,0,.34,.67,1.1), 
    #                              labels=c("C.R. = 0/3", "C.R. = 1/3", "C.R. = 2/3", "C.R. = 3/3"))) %>%
    mutate(crt=cut(crt, breaks=c(-1,0,.34,1.1), 
                                 labels=c("Low c.r.", "Moderate c.r.", "High c.r."))) %>%
    filter(!is.na(crt), !is.nan(crt)) %>%
    group_by(treat, crt) %>%
    summarise(y=weighted.mean(believed1_true,weight,na.rm=T)) %>%
    ungroup() %>%
    mutate(treat = as.character(treat))

dat.viz %>%
    ggplot(aes(x=treat, y=y)) +
    geom_bar(stat="identity") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    ylab("% somewhat/strongly confident in credibility") + xlab("Scandal clipping") +
    facet_grid(~crt) +
    geom_text(aes(label=scales::percent(round(y,2), accuracy=1), y=y+0.04)) +
    geom_hline(data = dat.viz %>% group_by(crt) %>% summarise(yint=mean(y)),
               aes(yintercept = yint), size=1, lty=1, color="red") +
    ggtitle(paste0("n=",n," | n_low=",n.low," | n_mod=",n.mod," | n_high=",n.high)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            strip.text = element_text(size=16),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=8)
        )
ggsave("figures/firststage_belief_byC.R._binary.pdf", width=8, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_belief_byC.R._binary.pdf")

### regression tables

## @SB: no effect of C.R.
(h5m <- lm(believed_true ~ treat*crt, dat %>% filter(!is.na(treat), treat != "ad", treat !="skit"))); summary(h5m); 
(h5m.wt <- lm(believed_true ~ treat*crt, dat %>% filter(!is.na(treat), treat != "ad", treat !="skit"), weights=weight)); summary(h5m.wt); 
(h5m.hq <- lm(believed_true ~ treat*crt, dat %>% filter(!is.na(treat), treat != "ad", treat !="skit", !lowq))); summary(h5m.hq); 
(h5m.adj <- lm(believed_true ~ treat*crt + exp_1_prompt + meta_OS + age_65 + educ + PID + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat), treat != "ad", treat !="skit"))); summary(h5m.adj); 
(h5m.adj.wt <- lm(believed_true ~ treat*crt + exp_1_prompt + meta_OS + age_65 + educ + PID + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat), treat != "ad", treat !="skit"), weights=weight)); summary(h5m.adj.wt); 
(h5m.adj.hq <- lm(believed_true ~ treat*crt + exp_1_prompt + meta_OS + age_65 + educ + PID + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat), treat != "ad", treat !="skit", !lowq))); summary(h5m.adj.hq); 
(h5m.adj.hq.wt <- lm(believed_true ~ treat*crt + exp_1_prompt + meta_OS + age_65 + educ + PID + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat), treat != "ad", treat !="skit", !lowq), weights=weight)); summary(h5m.adj.hq.wt); 

h5m.df <- bind_rows(
    tidy(h5m),
    tidy(h5m.wt),
    tidy(h5m.hq),
    tidy(h5m.adj),
    tidy(h5m.adj.wt),
    tidy(h5m.adj.hq),
    tidy(h5m.adj.hq.wt)
)
h5m.df$p.value.adj <- (h5m.df$p.value/order(h5m.df$p.value))*nrow(h5m.df) ##BHq corrections

stargazer(h5m,
          h5m.wt,
          h5m.hq,
          h5m.adj,
          h5m.adj.wt,
          h5m.adj.hq,
          h5m.adj.hq.wt,
          header = FALSE,
          no.space=TRUE,
          apply.p = function(p) { h5m.df$p.value.adj[h5m.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$"),
          notes.append = FALSE,
          digits=2,
          table.layout ="=d#-t-a-s=n",
          notes.align = "l",
          title="\\textbf{Models of Cognitive Reflection and Credibility Confidence of Clipping in Incidental Exposure Experiment}",
          dep.var.labels = c("\\normalsize Confidence that clipping was credible [1-5]"),
          covariate.labels = c("Audio","Text",
                               "C.R.",
                               "C.R. x Audio",
                               "C.R. x Text"),
          omit = c(COVARS[COVARS != "crt"], "response_wave", "exp_1_promptinfo"),
          add.lines = list(c("Weighted?", "", "\\checkmark", "", "", "\\checkmark","","\\checkmark"),
                           c("Low-Quality Dropped?","","","\\checkmark","","","\\checkmark","\\checkmark"),
                           c("Controls?","","","","\\checkmark","\\checkmark","\\checkmark","\\checkmark")),
          omit.stat=c("f", "ser"),
          column.sep.width = "1pt",
          font.size = "footnotesize",
          style = "apsr",
          label = "firststage_deception_byC.R.",
          out="tables/firststage_deception_byC.R..tex")


(h5m.b1 <- lm(believed1_true ~ treat*crt, dat %>% filter(!is.na(treat), treat != "ad", treat !="skit"))); summary(h5m.b1); 
(h5m.wt.b1 <- lm(believed1_true ~ treat*crt, dat %>% filter(!is.na(treat), treat != "ad", treat !="skit"), weights=weight)); summary(h5m.wt.b1); 
(h5m.hq.b1 <- lm(believed1_true ~ treat*crt, dat %>% filter(!is.na(treat), treat != "ad", treat !="skit", !lowq))); summary(h5m.hq.b1); 
(h5m.adj.b1 <- lm(believed1_true ~ treat*crt + exp_1_prompt + meta_OS + age_65 + educ + PID + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat), treat !="skit", treat != "ad"))); summary(h5m.adj.b1); 
(h5m.adj.wt.b1 <- lm(believed1_true ~ treat*crt + exp_1_prompt + meta_OS + age_65 + educ + PID + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat), treat !="skit", treat != "ad"), weights=weight)); summary(h5m.adj.wt.b1); 
(h5m.adj.hq.b1 <- lm(believed1_true ~ treat*crt + exp_1_prompt + meta_OS + age_65 + educ + PID + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat), treat !="skit", treat != "ad", !lowq))); summary(h5m.adj.hq.b1); 
(h5m.adj.hq.wt.b1 <- lm(believed1_true ~ treat*crt + exp_1_prompt + meta_OS + age_65 + educ + PID + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat), treat !="skit", treat != "ad", !lowq), weights=weight)); summary(h5m.adj.hq.wt.b1); 

h5m.b1.df <- bind_rows(
    tidy(h5m.b1),
    tidy(h5m.wt.b1),
    tidy(h5m.hq.b1),
    tidy(h5m.adj.b1),
    tidy(h5m.adj.wt.b1),
    tidy(h5m.adj.hq.b1),
    tidy(h5m.adj.hq.wt.b1)
)
h5m.b1.df$p.value.adj <- (h5m.b1.df$p.value/order(h5m.b1.df$p.value))*nrow(h5m.b1.df) ##BHq corrections

stargazer(h5m.b1,
          h5m.wt.b1,
          h5m.hq.b1,
          h5m.adj.b1,
          h5m.adj.wt.b1,
          h5m.adj.hq.b1,
          h5m.adj.hq.wt.b1,
          header = FALSE,
          no.space=TRUE,
          apply.p = function(p) { h5m.b1.df$p.value.adj[h5m.b1.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$"),
          notes.append = FALSE,
          digits=2,
          table.layout ="=d#-t-a-s=n",
          notes.align = "l",
          title="\\textbf{Models of Cognitive Reflection and Binarized Credibility Confidence of Clipping in Incidental Exposure Experiment}",
          dep.var.labels = c("\\normalsize Somewhat/strongly confident clipping was credible"),
          covariate.labels = c("Audio","Text",
                               "C.R.",
                               "C.R. x Audio",
                               "C.R. x Text"),
          omit = c(COVARS[COVARS != "crt"], "response_wave", "exp_1_promptinfo"),
          add.lines = list(c("Weighted?", "", "\\checkmark", "", "", "\\checkmark","","\\checkmark"),
                           c("Low-Quality Dropped?","","","\\checkmark","","","\\checkmark","\\checkmark"),
                           c("Controls?","","","","\\checkmark","\\checkmark","\\checkmark","\\checkmark")),
          omit.stat=c("f", "ser"),
          column.sep.width = "1pt",
          font.size = "footnotesize",
          style = "apsr",
          label="firststage_deception_byC.R.2",
          out="tables/firststage_deception_byC.R.2.tex")

#####------------------------------------------------------#
##### H6a: Heterogeneities in deception by partisanship ####
#####------------------------------------------------------#

n <- dat %>% 
    filter(!is.na(treat), treat != "ad", treat != "control", treat != "skit", exp_1_prompt_control==T, !is.na(PID), PID !="N/A", !is.na(believed_true)) %>% nrow()
n.dem <- dat %>% 
    filter(!is.na(treat), treat != "ad", treat != "control", treat != "skit", exp_1_prompt_control==T, !is.na(PID), PID =="Democrat", !is.na(believed_true)) %>% nrow()
n.ind <- dat %>% 
    filter(!is.na(treat), treat != "ad", treat != "control", treat != "skit", exp_1_prompt_control==T, !is.na(PID), PID =="Independent", !is.na(believed_true)) %>% nrow()
n.rep <- dat %>% 
    filter(!is.na(treat), treat != "ad", treat != "control", treat != "skit", exp_1_prompt_control==T, !is.na(PID), PID =="Republican", !is.na(believed_true)) %>% nrow()

## descriptive plots
### visualise just by PID
dat %>% 
    filter(exp_1_prompt_control==T) %>%
    filter(!is.na(PID), PID != "N/A") %>%
    group_by(PID) %>%
    summarise(y=weighted.mean(believed_true,weight,na.rm=T),
              ymin=weighted.mean(believed_true,weight,na.rm=T)-1.96*weighted.sd(believed_true,weight,na.rm=T)/sqrt(n()),
              ymax=weighted.mean(believed_true,weight,na.rm=T)+1.96*weighted.sd(believed_true,weight,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    mutate(PID = as.character(PID)) %>%
    mutate(PID = replace(PID, PID == "Democrat", "Democrats")) %>%
    mutate(PID = replace(PID, PID == "Independent", "Independents")) %>%
    mutate(PID = replace(PID, PID == "Republican", "Republicans")) %>%
    ggplot(aes(x=PID, y=y, ymin=ymin, ymax=ymax)) +
    geom_bar(stat="identity") +
    ylab("Credibility confidence") + xlab("") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    ggtitle(paste0("n=",n," | n_dem=",n.dem," | n_ind=",n.ind," | n_rep=",n.rep)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("figures/firststage_belief_byPID.pdf", width=4.75, height=4.5)
if(SHOW_PDFS) system("open figures/firststage_belief_byPID.pdf")

dat %>% 
    filter(exp_1_prompt_control==T) %>%
    filter(PID != "N/A") %>%
    group_by(PID) %>%
    summarise(y=weighted.mean(believed1_true,weight,na.rm=T)) %>%
    ungroup() %>%
    mutate(PID = as.character(PID)) %>%
    mutate(PID = replace(PID, PID == "Democrat", "Democrats")) %>%
    mutate(PID = replace(PID, PID == "Independent", "Independents")) %>%
    mutate(PID = replace(PID, PID == "Republican", "Republicans")) %>%
    ggplot(aes(x=PID, y=y)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=scales::percent(round(y,2), accuracy=1), y=y+0.02)) +
    ylab("% somewhat/strongly confident in credibility") + xlab("") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    ggtitle(paste0("n=",n," | n_dem=",n.dem," | n_ind=",n.ind," | n_rep=",n.rep)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=11)
        )
ggsave("figures/firststage_belief_byPID_binary.pdf", width=4.75, height=4.5)
if(SHOW_PDFS) system("open figures/firststage_belief_byPID_binary.pdf")

### visualise by PID and medium
dat.viz <- dat %>% 
    filter(exp_1_prompt_control==T) %>%
    filter(!is.na(treat), treat != "ad", treat != "control", treat !="skit") %>%
    group_by(PID, treat) %>%
    summarise(y=weighted.mean(believed_true,weight,na.rm=T),
              ymin=weighted.mean(believed_true,weight,na.rm=T)-1.96*weighted.sd(believed_true,weight,na.rm=T)/sqrt(n()),
              ymax=weighted.mean(believed_true,weight,na.rm=T)+1.96*weighted.sd(believed_true,weight,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    mutate(treat = as.character(treat)) %>%
    mutate(PID = as.character(PID)) %>%
    mutate(PID = replace(PID, PID == "Democrat", "Democrats")) %>%
    mutate(PID = replace(PID, PID == "Independent", "Independents")) %>%
    mutate(PID = replace(PID, PID == "Republican", "Republicans")) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=5))

dat.viz %>%
    ggplot(aes(x=treat, y=y, ymin=ymin, ymax=ymax)) +
    geom_bar(stat="identity") +
    ylab("Credibility confidence") + xlab("Scandal clipping") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    scale_y_continuous(limits=c(2,4.5),oob = scales::rescale_none) +
    facet_wrap(~PID) +
    geom_hline(data = dat.viz %>% group_by(PID) %>% summarise(yint=mean(y)),
               aes(yintercept = yint), size=1, lty=1, color="red") +
    ggtitle(paste0("n=",n," | n_dem=",n.dem," | n_ind=",n.ind," | n_rep=",n.rep)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            strip.text = element_text(size=18),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16)
        )
ggsave("figures/firststage_belief_byPID2.pdf", width=8, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_belief_byPID2.pdf")

dat.viz <- dat %>% 
    filter(exp_1_prompt_control==T) %>%
    filter(!is.na(treat), treat != "ad", treat != "control", treat !="skit") %>%
    group_by(PID, treat) %>%
    summarise(y=weighted.mean(believed1_true,weight,na.rm=T)) %>%
    ungroup() %>%
    mutate(treat = as.character(treat)) %>%
    mutate(PID = as.character(PID)) %>%
    mutate(PID = replace(PID, PID == "Democrat", "Democrats")) %>%
    mutate(PID = replace(PID, PID == "Independent", "Independents")) %>%
    mutate(PID = replace(PID, PID == "Republican", "Republicans")) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=5))

dat.viz %>%
    ggplot(aes(x=treat, y=y)) +
    geom_bar(stat="identity") +
    ylab("% somewhat/strongly confident in credibility") + xlab("Scandal clipping") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    geom_text(aes(label=scales::percent(round(y,2), accuracy=1), y=y+0.04)) +
    facet_wrap(~PID) +
    geom_hline(data = dat.viz %>% group_by(PID) %>% summarise(yint=mean(y)),
               aes(yintercept = yint), size=1, lty=1, color="red") +
    ggtitle(paste0("n=",n," | n_dem=",n.dem," | n_ind=",n.ind," | n_rep=",n.rep)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            strip.text = element_text(size=18),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=11)
        )
ggsave("figures/firststage_belief_byPID_binary2.pdf", width=8, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_belief_byPID_binary2.pdf")


### regression tables

## @SB: HUGE effects of partisanship in all cases, but no heterogeneity by video type or C.R.
(h6a <- lm(believed_true ~ I(PID=="Republican")*treat*crt, dat %>% filter(treat != "ad", treat !="skit"))); summary(h6a); 
(h6a.wt <- lm(believed_true ~ I(PID=="Republican")*treat*crt, dat %>% filter(treat != "ad", treat !="skit"), weights=weight)); summary(h6a.wt); 
(h6a.hq <- lm(believed_true ~ I(PID=="Republican")*treat*crt, dat %>% filter(treat != "ad", treat !="skit", !lowq))); summary(h6a.hq); 
(h6a.adj <- lm(believed_true ~ I(PID=="Republican")*treat*crt + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", treat !="skit"))); summary(h6a.adj); 
(h6a.adj.wt <- lm(believed_true ~ I(PID=="Republican")*treat*crt + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", treat !="skit"), weights=weight)); summary(h6a.adj.wt); 
(h6a.adj.hq <- lm(believed_true ~ I(PID=="Republican")*treat*crt + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", treat !="skit", !lowq))); summary(h6a.adj.hq); 
(h6a.adj.hq.wt <- lm(believed_true ~ I(PID=="Republican")*treat*crt + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", treat !="skit", !lowq), weights=weight)); summary(h6a.adj.hq.wt); 

h6a.df <- bind_rows(
    tidy(h6a),
    tidy(h6a.wt),
    tidy(h6a.hq),
    tidy(h6a.adj),
    tidy(h6a.adj.wt),
    tidy(h6a.adj.hq),
    tidy(h6a.adj.hq.wt)
)
h6a.df$p.value.adj <- (h6a.df$p.value/order(h6a.df$p.value))*nrow(h6a.df) ##BHq corrections


stargazer(h6a,
          h6a.wt,
          h6a.hq,
          h6a.adj,
          h6a.adj.wt,
          h6a.adj.hq,
          h6a.adj.hq.wt,
          apply.p = function(p) { h6a.df$p.value.adj[h6a.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$",
                     "PID is pooled to Republican/Not Republican for brevity. PID interacted with C.R. to test",
                     "possible mechanism of motivated reasoning (pre-registered), although, as a reviewer pointed out,",
                     "this is not a sufficient test of a motivated reasoning mechanism by itself."),
          notes.append = FALSE,
          header = FALSE,
          no.space=TRUE,
          digits=2,
          table.layout ="=d#-t-a-s=n",
          notes.align = "l",
          title="\\textbf{Models of Partisan Group Identity and Credibility Confidence of Clipping in Incidental Exposure Experiment}",
          # notes = c("\\textit{Notes}: PID is pooled to Republican/Not Republican for brevity; reference category for medium is Video."),
          covariate.labels = c("Repub PID",
                               "Audio","Text",
                               "C.R.",
                               "Repub x Audio",
                               "Repub x Text",
                               "C.R. x Repub",
                               "Audio x C.R.",
                               "Text x C.R.",
                               "Repub x Audio x C.R.",
                               "Repub x Text x C.R."),
          omit = c("response_wave", COVARS[!(COVARS %in% c("PID","crt"))], "exp_1"),
          dep.var.labels = c("\\normalsize Confidence that clipping was credible [1-5]"),
          add.lines = list(c("Weighted?", "", "\\checkmark", "", "", "\\checkmark","","\\checkmark"),
                           c("Low-Quality Dropped?","","","\\checkmark","","","\\checkmark","\\checkmark"),
                           c("Controls?","","","","\\checkmark","\\checkmark","\\checkmark","\\checkmark")),
          omit.stat=c("f", "ser"),
          column.sep.width = "1pt",
          font.size = "footnotesize",
          style = "apsr",
          label="firststage_deception_byPID",
          out="tables/firststage_deception_byPID.tex")

(h6a.b1 <- lm(believed1_true ~ I(PID=="Republican")*treat*crt, dat %>% filter(treat != "ad", treat != "skit"))); summary(h6a.b1); 
(h6a.wt.b1 <- lm(believed1_true ~ I(PID=="Republican")*treat*crt, dat %>% filter(treat != "ad", treat != "skit"), weights=weight)); summary(h6a.wt.b1); 
(h6a.hq.b1 <- lm(believed1_true ~ I(PID=="Republican")*treat*crt, dat %>% filter(treat != "ad", treat != "skit", !lowq))); summary(h6a.hq.b1); 
(h6a.adj.b1 <- lm(believed1_true ~ I(PID=="Republican")*treat*crt + response_wave_ID + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", treat != "skit"))); summary(h6a.adj.b1); 
(h6a.adj.wt.b1 <- lm(believed1_true ~ I(PID=="Republican")*treat*crt + response_wave_ID + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", treat != "skit"), weights=weight)); summary(h6a.adj.wt.b1); 
(h6a.adj.hq.b1 <- lm(believed1_true ~ I(PID=="Republican")*treat*crt + response_wave_ID + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", treat != "skit", !lowq))); summary(h6a.adj.hq.b1); 
(h6a.adj.hq.wt.b1 <- lm(believed1_true ~ I(PID=="Republican")*treat*crt + response_wave_ID + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", treat != "skit", !lowq), weights=weight)); summary(h6a.adj.hq.wt.b1); 

h6a.b1.df <- bind_rows(
    tidy(h6a.b1),
    tidy(h6a.wt.b1),
    tidy(h6a.hq.b1),
    tidy(h6a.adj.b1),
    tidy(h6a.adj.wt.b1),
    tidy(h6a.adj.hq.b1),
    tidy(h6a.adj.hq.wt.b1)
)
h6a.b1.df$p.value.adj <- (h6a.b1.df$p.value/order(h6a.b1.df$p.value))*nrow(h6a.b1.df) ##BHq corrections


stargazer(h6a.b1,
          h6a.wt.b1,
          h6a.hq.b1,
          h6a.adj.b1,
          h6a.adj.wt.b1,
          h6a.adj.hq.b1,
          h6a.adj.hq.wt.b1,
          apply.p = function(p) { h6a.b1.df$p.value.adj[h6a.b1.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$"),
          notes.append = FALSE,
          header = FALSE,
          no.space=TRUE,
          digits=2,
          table.layout ="=d#-t-a-s=n",
          notes.align = "l",
          title="\\textbf{Models of Partisan Group Identity and Binarized Credibility Confidence of Clipping in Incidental Exposure Experiment}",
          # notes = c("\\textit{Notes}: PID is pooled to Republican/Not Republican for brevity; reference category for medium is Video."),
          covariate.labels = c("Repub PID",
                               "Audio","Text",
                               "C.R.",
                               "Repub x Audio",
                               "Repub x Text",
                               "C.R. x Repub",
                               "Audio x C.R.",
                               "Text x C.R.",
                               "Repub x Audio x C.R.",
                               "Repub x Text x C.R."),
          omit = c("response_wave", COVARS[!(COVARS %in% c("PID","crt"))], "exp_1"),
          dep.var.labels = c("\\normalsize Somewhat/strongly confident clipping was credible"),
          add.lines = list(c("Weighted?", "", "\\checkmark", "", "", "\\checkmark","","\\checkmark"),
                           c("Low-Quality Dropped?","","","\\checkmark","","","\\checkmark","\\checkmark"),
                           c("Controls?","","","","\\checkmark","\\checkmark","\\checkmark","\\checkmark")),
          omit.stat=c("f", "ser"),
          column.sep.width = "1pt",
          font.size = "footnotesize",
          style = "apsr",
          label="firststage_deception_byPID2",
          out="tables/firststage_deception_byPID2.tex")

#####------------------------------------------------------#
##### H6b: Heterogeneities in favorability by partisanship ####
#####------------------------------------------------------#

n <- dat %>% 
    filter(!is.na(treat), exp_1_prompt_control==T, !is.na(PID), PID !="N/A", !is.na(post_favor_Warren)) %>% nrow()
n.dem <- dat %>% 
    filter(!is.na(treat), exp_1_prompt_control==T, !is.na(PID), PID =="Democrat", !is.na(post_favor_Warren)) %>% nrow()
n.ind <- dat %>% 
    filter(!is.na(treat), exp_1_prompt_control==T, !is.na(PID), PID =="Independent", !is.na(post_favor_Warren)) %>% nrow()
n.rep <- dat %>% 
    filter(!is.na(treat),exp_1_prompt_control==T, !is.na(PID), PID =="Republican", !is.na(post_favor_Warren)) %>% nrow()

## descriptive plots

### visualise by PID and medium
dat.viz <- dat %>% 
    filter(!is.na(treat)) %>%
    filter(exp_1_prompt_control == T) %>%
    group_by(PID, treat) %>%
    summarise(y=weighted.mean(post_favor_Warren,weight,na.rm=T),
              ymin=weighted.mean(post_favor_Warren,weight,na.rm=T)-1.96*weighted.sd(post_favor_Warren,weight,na.rm=T)/sqrt(n()),
              ymax=weighted.mean(post_favor_Warren,weight,na.rm=T)+1.96*weighted.sd(post_favor_Warren,weight,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    mutate(PID = as.character(PID)) %>%
    mutate(PID = replace(PID, PID == "Democrat", "Democrats")) %>%
    mutate(PID = replace(PID, PID == "Independent", "Independents")) %>%
    mutate(PID = replace(PID, PID == "Republican", "Republicans")) %>%
    mutate(treat = as.character(treat)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "control", after=0)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "ad", after=1)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=5))
    
dat.viz %>%
    ggplot(aes(x=treat, y=y, ymin=ymin, ymax=ymax)) +
    geom_bar(stat="identity") +
    ylab("feeling thermometer") + xlab("") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    geom_hline(data = dat.viz %>% group_by(PID) %>% summarise(yint=mean(y)),
               aes(yintercept = yint), size=1, lty=1, color="red") +
    geom_vline(xintercept=2.5, size=0.5, lty=2) +
    facet_wrap(~PID) + 
    ggtitle(paste0("n=",n," | n_dem=",n.dem," | n_ind=",n.ind," | n_rep=",n.rep)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=11),
            axis.text.y = element_text(size=12),
            strip.text = element_text(size=18),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16)
        )
ggsave("figures/firststage_feelings_byPID.pdf", width=8, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_feelings_byPID.pdf")


## regression tables

## @SB: HUGE effects of partisanship, some effects that C.R. x Republican partisanship is 
##      driving down favorability (actual "motivated" reasoning)

(h6b.b1 <- lm(post_favor_Warren ~ I(PID=="Republican")*treat*crt, dat %>% filter(!is.na(treat)))); summary(h6b.b1); 
(h6b.wt.b1 <- lm(post_favor_Warren ~ I(PID=="Republican")*treat*crt, dat %>% filter(!is.na(treat)), weights=weight)); summary(h6b.wt.b1); 
(h6b.hq.b1 <- lm(post_favor_Warren ~ I(PID=="Republican")*treat*crt, dat %>% filter(!is.na(treat), !lowq))); summary(h6b.hq.b1); 
(h6b.adj.b1 <- lm(post_favor_Warren ~ I(PID=="Republican")*treat*crt + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat)))); summary(h6b.adj.b1); 
(h6b.adj.wt.b1 <- lm(post_favor_Warren ~ I(PID=="Republican")*treat*crt + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat)), weights=weight)); summary(h6b.adj.wt.b1); 
(h6b.adj.hq.b1 <- lm(post_favor_Warren ~ I(PID=="Republican")*treat*crt + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat), !lowq))); summary(h6b.adj.hq.b1); 
(h6b.adj.hq.wt.b1 <- lm(post_favor_Warren ~ I(PID=="Republican")*treat*crt + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat), !lowq), weights=weight)); summary(h6b.adj.hq.wt.b1); 

h6b.b1.df <- bind_rows(
    tidy(h6b.b1),
    tidy(h6b.wt.b1),
    tidy(h6b.hq.b1),
    tidy(h6b.adj.b1),
    tidy(h6b.adj.wt.b1),
    tidy(h6b.adj.hq.b1),
    tidy(h6b.adj.hq.wt.b1)
)
h6b.b1.df$p.value.adj <- (h6b.b1.df$p.value/order(h6b.b1.df$p.value))*nrow(h6b.b1.df) ##BHq corrections


stargazer(h6b.b1,
          h6b.wt.b1,
          h6b.hq.b1,
          h6b.adj.b1,
          h6b.adj.wt.b1,
          h6b.adj.hq.b1,
          h6b.adj.hq.wt.b1,
          header = FALSE,
          apply.p = function(p) { h6b.b1.df$p.value.adj[h6b.b1.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$"),
          notes.append = FALSE,
          no.space=TRUE,
          digits=2,
          table.layout ="=d#-t-a-s=n",
          notes.align = "l",
          title="\\textbf{Models of Partisan Group Identity and Scandal Target Affect in Incidental Exposure Experiment}",
          covariate.labels = c("Repub PID",
                               "Video", "Audio","Text","Skit", "Attack Ad",
                               "C.R.",
                               "Repub x Video",
                               "Repub x Audio",
                               "Repub x Text",
                               "Repub x Skit",
                               "Repub x Ad",
                               "C.R. x Repub",
                               "Video x C.R.",
                               "Audio x C.R.",
                               "Text x C.R.",
                               "Skit x C.R.",
                               "Ad x C.R.",
                               "Repub x Video x C.R.",
                               "Repub x Audio x C.R.",
                               "Repub x Text x C.R.",
                               "Repub x Skit x C.R.",
                               "Repub x Ad x C.R."),
          add.lines = list(c("Weighted?", "", "\\checkmark", "", "", "\\checkmark","","\\checkmark"),
                           c("Low-Quality Dropped?","","","\\checkmark","","","\\checkmark","\\checkmark"),
                           c("Controls?","","","","\\checkmark","\\checkmark","\\checkmark","\\checkmark")),
          omit = c("response_wave", COVARS[!(COVARS %in% c("PID","crt"))], "exp_1"),         
          dep.var.labels = c("\\normalsize Elizabeth Warren Feeling Thermometer"),
          omit.stat=c("f", "ser"),
          column.sep.width = "1pt",
          font.size = "scriptsize",
          style = "apsr",
          label="firststage_feelings_byPID",
          out="tables/firststage_feelings_byPID.tex")

#####------------------------------------------------------#
##### H7a: Heterogeneities in deception by ambivalent sexism ####
#####------------------------------------------------------#

n <- dat %>% 
    filter(!is.na(treat), treat != "ad", treat != "control", treat != "skit", exp_1_prompt_control==T, !is.na(believed_true)) %>%
    mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                 labels=c("low", "moderate", "high"))) %>%
    filter(!is.na(ambivalent_sexism), !is.nan(ambivalent_sexism)) %>%
    nrow()
n.low <- dat %>%
    filter(!is.na(treat), treat != "ad", treat != "control", treat != "skit", exp_1_prompt_control==T, !is.na(believed_true)) %>%
    mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                 labels=c("low", "moderate", "high"))) %>%
    filter(!is.na(ambivalent_sexism), !is.nan(ambivalent_sexism)) %>%
    filter(ambivalent_sexism == "low") %>%
    nrow()
n.mod <- dat %>%
    filter(!is.na(treat), treat != "ad", treat != "control", treat != "skit", exp_1_prompt_control==T, !is.na(believed_true)) %>%
    mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                 labels=c("low", "moderate", "high"))) %>%
    filter(!is.na(ambivalent_sexism), !is.nan(ambivalent_sexism)) %>%
    filter(ambivalent_sexism == "moderate") %>%
    nrow()
n.high <- dat %>%
    filter(!is.na(treat), treat != "ad", treat != "control", treat != "skit", exp_1_prompt_control==T, !is.na(believed_true)) %>%
    mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                 labels=c("low", "moderate", "high"))) %>%
    filter(!is.na(ambivalent_sexism), !is.nan(ambivalent_sexism)) %>%
    filter(ambivalent_sexism == "high") %>%
    nrow()

## descriptive plots

### visualise just by sexism
dat %>% 
    filter(exp_1_prompt_control==T) %>%
    mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                 labels=c("low", "moderate", "high"))) %>%
    filter(!is.na(ambivalent_sexism), !is.nan(ambivalent_sexism)) %>%
    group_by(ambivalent_sexism) %>%
    summarise(y=weighted.mean(believed_true,weight,na.rm=T),
              ymin=weighted.mean(believed_true,weight,na.rm=T)-1.96*weighted.sd(believed_true,weight,na.rm=T)/sqrt(n()),
              ymax=weighted.mean(believed_true,weight,na.rm=T)+1.96*weighted.sd(believed_true,weight,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    ggplot(aes(x=ambivalent_sexism, y=y, ymin=ymin, ymax=ymax)) +
    geom_bar(stat="identity") +
    ylab("Credibility confidence") + xlab("level of ambivalent sexism") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    ggtitle(paste0("n=",n," | n_low=",n.low," | n_mod=",n.mod," | n_high=",n.high)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16)
        )
ggsave("figures/firststage_belief_bysexism.pdf", width=4, height=4.5)
if(SHOW_PDFS) system("open figures/firststage_belief_bysexism.pdf")

dat %>% 
    filter(exp_1_prompt_control==T) %>%
    mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                 labels=c("low", "moderate", "high"))) %>%
    filter(!is.na(ambivalent_sexism), !is.nan(ambivalent_sexism)) %>%
    group_by(ambivalent_sexism) %>%
    summarise(y=weighted.mean(believed1_true,weight,na.rm=T)) %>%
    ungroup() %>%
    ggplot(aes(x=ambivalent_sexism, y=y)) +
    geom_bar(stat="identity") +
    ylab("% somewhat/strongly confident in credibility") + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    xlab("level of ambivalent sexism") +
    ggtitle(paste0("n=",n," | n_low=",n.low," | n_mod=",n.mod," | n_high=",n.high)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("figures/firststage_belief_bysexism_binary.pdf", width=4, height=4.5)
if(SHOW_PDFS) system("open figures/firststage_belief_bysexism_binary.pdf")

### visualise by sexism and medium
dat.viz <- dat %>% 
    filter(exp_1_prompt_control==T) %>%
    filter(!is.na(treat), treat != "ad", treat != "control", treat != "skit") %>%
    mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                 labels=c("low\nsexism", "moderate\nsexism", "high\nsexism"))) %>%
    filter(!is.na(ambivalent_sexism), !is.nan(ambivalent_sexism)) %>%
    group_by(treat, ambivalent_sexism) %>%
    summarise(y=weighted.mean(believed_true,weight,na.rm=T),
              ymin=weighted.mean(believed_true,weight,na.rm=T)-1.96*weighted.sd(believed_true,weight,na.rm=T)/sqrt(n()),
              ymax=weighted.mean(believed_true,weight,na.rm=T)+1.96*weighted.sd(believed_true,weight,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    mutate(treat = as.character(treat)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=5))

dat.viz %>%
    ggplot(aes(x=treat, y=y, ymin=ymin, ymax=ymax)) +
    geom_bar(stat="identity") +
    ylab("Credibility confidence") + 
    xlab("") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    scale_y_continuous(limits=c(1,4),oob = scales::rescale_none) +
    facet_wrap(~ambivalent_sexism) +
    geom_hline(data = dat.viz %>% group_by(ambivalent_sexism) %>% summarise(yint=mean(y)),
               aes(yintercept = yint), size=1, lty=1, color="red") +
    ggtitle(paste0("n=",n," | n_low=",n.low," | n_mod=",n.mod," | n_high=",n.high)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            strip.text = element_text(size=18),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16)
        )
ggsave("figures/firststage_belief_bysexism2.pdf", width=8, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_belief_bysexism2.pdf")


dat.viz <- dat %>% 
    filter(exp_1_prompt_control==T) %>%
    filter(!is.na(treat), treat != "ad", treat != "control", treat != "skit") %>%
    mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                 labels=c("low sexism", "moderate sexism", "high sexism"))) %>%
    filter(!is.na(ambivalent_sexism), !is.nan(ambivalent_sexism)) %>%
    group_by(treat, ambivalent_sexism) %>%
    summarise(y=weighted.mean(believed1_true,weight,na.rm=T)) %>%
    ungroup() %>%
    mutate(treat = as.character(treat)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=5))

dat.viz %>%
    ggplot(aes(x=treat, y=y)) +
    geom_bar(stat="identity") +
    ylab("% somewhat/strongly confident in credibility") + 
    xlab("") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    facet_wrap(~ambivalent_sexism) +
    geom_text(aes(label=scales::percent(round(y,2), accuracy=1), y=y+0.04)) +
    geom_hline(data = dat.viz %>% group_by(ambivalent_sexism) %>% summarise(yint=mean(y)),
               aes(yintercept = yint), size=1, lty=1, color="red") +
    ggtitle(paste0("n=",n," | n_low=",n.low," | n_mod=",n.mod," | n_high=",n.high)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            strip.text = element_text(size=18),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16)
        )
ggsave("figures/firststage_belief_bysexism_binary2.pdf", width=8, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_belief_bysexism_binary2.pdf")



## regression tables
## @SB: no evidence of interaction effects
(h7a <- lm(believed_true ~ ambivalent_sexism*treat, dat %>% filter(treat != "ad", treat != "skit"))); summary(h7a); 
(h7a.wt <- lm(believed_true ~ ambivalent_sexism*treat, dat %>% filter(treat != "ad", treat != "skit"), weights=weight)); summary(h7a.wt); 
(h7a.hq <- lm(believed_true ~ ambivalent_sexism*treat, dat %>% filter(treat != "ad", treat != "skit", !lowq))); summary(h7a.hq); 
(h7a.adj <- lm(believed_true ~ ambivalent_sexism*treat + PID + crt + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", treat != "skit"))); summary(h7a.adj); 
(h7a.adj.wt <- lm(believed_true ~ ambivalent_sexism*treat + PID + crt + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", treat != "skit"), weights=weight)); summary(h7a.adj.wt); 
(h7a.adj.hq <- lm(believed_true ~ ambivalent_sexism*treat + PID + crt + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", treat != "skit", !lowq))); summary(h7a.adj.hq); 
(h7a.adj.hq.wt <- lm(believed_true ~ ambivalent_sexism*treat + PID + crt + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", treat != "skit", !lowq), weights=weight)); summary(h7a.adj.hq.wt); 

h7a.df <- bind_rows(
    tidy(h7a),
    tidy(h7a.wt),
    tidy(h7a.hq),
    tidy(h7a.adj),
    tidy(h7a.adj.wt),
    tidy(h7a.adj.hq),
    tidy(h7a.adj.hq.wt)
)
h7a.df$p.value.adj <- (h7a.df$p.value/order(h7a.df$p.value))*nrow(h7a.df) ##BHq corrections


stargazer(h7a,
          h7a.wt,
          h7a.hq,
          h7a.adj,
          h7a.adj.wt,
          h7a.adj.hq,
          h7a.adj.hq.wt,
          header = FALSE,
          apply.p = function(p) { h7a.df$p.value.adj[h7a.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$"),
          notes.append = FALSE,
          no.space=TRUE,
          digits=2,
          table.layout ="=d#-t-a-s=n",
          notes.align = "l",
          title="\\textbf{Models of Ambivalent Sexism and Credibility Confidence in Scandal Clipping in Incidental Exposure Experiment}",
          covariate.labels = c("Ambivalent Sexism",
                               "Audio","Text",
                               "A.S. x Audio",
                               "A.S. x Text"),
          omit = c("response_wave", COVARS[!(COVARS %in% c("ambivalent_sexism"))], "exp_1"),
          dep.var.labels = c("\\normalsize Confidence that clipping was credible [1-5]"),
          add.lines = list(c("Weighted?", "", "\\checkmark", "", "", "\\checkmark","","\\checkmark"),
                           c("Low-Quality Dropped?","","","\\checkmark","","","\\checkmark","\\checkmark"),
                           c("Controls?","","","","\\checkmark","\\checkmark","\\checkmark","\\checkmark")),
          omit.stat=c("f", "ser"),
          column.sep.width = "1pt",
          font.size = "footnotesize",
          style = "apsr",
          label="firststage_deception_bysexism",
          out="tables/firststage_deception_bysexism.tex")

(h7a.b1 <- lm(believed1_true ~ ambivalent_sexism*treat, dat %>% filter(treat != "ad", treat != "skit"))); summary(h7a.b1); 
(h7a.wt.b1 <- lm(believed1_true ~ ambivalent_sexism*treat, dat %>% filter(treat != "ad", treat != "skit"), weights=weight)); summary(h7a.wt.b1); 
(h7a.hq.b1 <- lm(believed1_true ~ ambivalent_sexism*treat, dat %>% filter(treat != "ad", treat != "skit", !lowq))); summary(h7a.hq.b1); 
(h7a.adj.b1 <- lm(believed1_true ~ ambivalent_sexism*treat + PID + crt + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", treat != "skit"))); summary(h7a.adj.b1); 
(h7a.adj.wt.b1 <- lm(believed1_true ~ ambivalent_sexism*treat + PID + crt + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", treat != "skit"), weights=weight)); summary(h7a.adj.wt.b1); 
(h7a.adj.hq.b1 <- lm(believed1_true ~ ambivalent_sexism*treat + PID + crt + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", treat != "skit", !lowq))); summary(h7a.adj.hq.b1); 
(h7a.adj.hq.wt.b1 <- lm(believed1_true ~ ambivalent_sexism*treat + PID + crt + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", treat != "skit", !lowq), weights=weight)); summary(h7a.adj.hq.wt.b1); 

h7a.b1.df <- bind_rows(
    tidy(h7a.b1),
    tidy(h7a.wt.b1),
    tidy(h7a.hq.b1),
    tidy(h7a.adj.b1),
    tidy(h7a.adj.wt.b1),
    tidy(h7a.adj.hq.b1),
    tidy(h7a.adj.hq.wt.b1)
)
h7a.b1.df$p.value.adj <- (h7a.b1.df$p.value/order(h7a.b1.df$p.value))*nrow(h7a.b1.df) ##BHq corrections


stargazer(h7a.b1,
          h7a.wt.b1,
          h7a.hq.b1,
          h7a.adj.b1,
          h7a.adj.wt.b1,
          h7a.adj.hq.b1,
          h7a.adj.hq.wt.b1,
          header = FALSE,
          apply.p = function(p) { h7a.b1.df$p.value.adj[h7a.b1.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$"),
          notes.append = FALSE,
          no.space=TRUE,
          digits=2,
          table.layout ="=d#-t-a-s=n",
          notes.align = "l",
          title="\\textbf{Models of Ambivalent Sexism and Binarized Credibility Confidence in Scandal Clipping in Incidental Exposure Experiment}",
          covariate.labels = c("Ambivalent Sexism",
                               "Audio","Text",
                               "A.S. x Audio",
                               "A.S. x Text"),
          omit = c("response_wave", COVARS[!(COVARS %in% c("ambivalent_sexism"))], "exp_1"),
          dep.var.labels = c("\\normalsize Somewhat/strongly confident clipping was credible"),
          add.lines = list(c("Weighted?", "", "\\checkmark", "", "", "\\checkmark","","\\checkmark"),
                           c("Low-Quality Dropped?","","","\\checkmark","","","\\checkmark","\\checkmark"),
                           c("Controls?","","","","\\checkmark","\\checkmark","\\checkmark","\\checkmark")),
          omit.stat=c("f", "ser"),
          column.sep.width = "1pt",
          font.size = "footnotesize",
          style = "apsr",
          label="firststage_deception_bysexism2",
          out="tables/firststage_deception_bysexism2.tex")

## manipulation checks

## @SB: (manipulation check) Ambivalent sexism seems to predict 
##      Warren being underfavored relative to men, but less so for Klobuchar;
##      weirdly positive interaction with Republican PID


h7_plcbs_df <- data.frame()

for (favor in c("post_favor_Warren","post_favor_Biden","post_favor_Klobuchar","post_favor_Sanders","post_favor_Bloomberg")) {
    dat$post_favor_ <- dat[[favor]]
    
    (h7.plcb.m <- lm(post_favor_ ~ treat + ambivalent_sexism + PID, dat, weights=weight)); summary(h7.plcb.m);
    (h7.plcb.m.adj.part.D <- lm(post_favor_ ~ treat + response_wave_ID + crt + ambivalent_sexism, dat %>% filter(PID=="Democrat"), weights=weight)); summary(h7.plcb.m.adj.part.D);
    (h7.plcb.m.adj.part.R <- lm(post_favor_ ~ treat + response_wave_ID + crt + ambivalent_sexism, dat %>% filter(PID=="Republican"), weights=weight)); summary(h7.plcb.m.adj.part.R);

    (h7.plcb.m.adj <- lm(post_favor_ ~ treat + response_wave_ID + exp_1_prompt + meta_OS + age_65 + educ + crt + ambivalent_sexism + I(gender=="Male") + polknow + internet_usage + PID, dat, weights=weight)); summary(h7.plcb.m.adj);
    (h7.plcb.m.adj.D <- lm(post_favor_ ~ treat + response_wave_ID + exp_1_prompt + meta_OS + age_65 + educ + crt + ambivalent_sexism + I(gender=="Male") + polknow + internet_usage, dat %>% filter(PID=="Democrat"), weights=weight)); summary(h7.plcb.m.adj.D);
    (h7.plcb.m.adj.R <- lm(post_favor_ ~ treat + response_wave_ID + exp_1_prompt + meta_OS + age_65 + educ + crt + ambivalent_sexism + I(gender=="Male") + polknow + internet_usage, dat %>% filter(PID=="Republican"), weights=weight)); summary(h7.plcb.m.adj.R);
    
    plcb_df <- tidy(h7.plcb.m) %>% mutate(model="Without controls") %>%
        bind_rows(tidy(h7.plcb.m.adj) %>% mutate(model="Full controls")) %>%
        filter(grepl("ambivalent_sexism", term)) %>%
        mutate(term = fct_relevel(term, "audio", after=1))    
    
    ## doing for each PID separately is misleading -- strong floor fx for Repub
    # plcb_df <- tidy(h7.plcb.m) %>% mutate(model="Without controls") %>%
    #     bind_rows(tidy(h7.plcb.m.adj.part.D) %>% mutate(model="PID controls") %>% mutate(term=paste0(term,"D"))) %>%
    #     bind_rows(tidy(h7.plcb.m.adj.part.R) %>% mutate(model="PID controls") %>% mutate(term=paste0(term,"R"))) %>%
    #     bind_rows(tidy(h7.plcb.m.adj.D) %>% mutate(model="Full controls") %>% mutate(term=paste0(term,"D"))) %>%
    #     bind_rows(tidy(h7.plcb.m.adj.R) %>% mutate(model="Full controls") %>% mutate(term=paste0(term,"R"))) %>%
    #     filter(grepl("ambivalent_sexism", term)) %>%
    #     mutate(term = fct_relevel(term, "audio", after=1))
    
    plcb_df$target <- favor
    h7_plcbs_df <- bind_rows(h7_plcbs_df, plcb_df)
}
h7_plcbs_df %>% 
    mutate(target = replace(target, target == "post_favor_Warren", "Elizabeth Warren")) %>% 
    mutate(target = replace(target, target == "post_favor_Sanders", "Bernie Sanders")) %>% 
    mutate(target = replace(target, target == "post_favor_Klobuchar", "Amy Klobuchar")) %>% 
    mutate(target = replace(target, target == "post_favor_Bloomberg", "Michael Bloomberg")) %>% 
    mutate(target = replace(target, target == "post_favor_Biden", "Joe Biden")) %>% 
    mutate(target = fct_relevel(target, "Joe Biden", "Michael Bloomberg", "Bernie Sanders", "Amy Klobuchar", "Elizabeth Warren")) %>%
    # mutate(model = fct_relevel(model, "Without controls", "PID controls", "Full controls")) %>%
    mutate(model = fct_relevel(model, "Without controls", "Full controls")) %>%
    mutate(term = as.character(term)) %>%
    mutate(term = replace(term, term == "ambivalent_sexism", "all")) %>%
    mutate(term = replace(term, term == "ambivalent_sexismD", "dem.")) %>%
    mutate(term = replace(term, term == "ambivalent_sexismR", "repub.")) %>%
    mutate(sig = as.factor(ifelse(estimate-1.96*std.error > 0 | estimate+1.96*std.error < 0, 
                                  1, 0.8))) %>%
    ggplot(aes(x=target, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, group=term, alpha=sig, color=term)) +
    geom_pointrange(position = position_dodge(width=0.7), size=1) +
    # geom_text(aes(label = term, y = estimate+1.96*std.error+2.5), position = position_dodge(width=0.7), color="black", size=6) +
    coord_flip() +
    geom_hline(yintercept=0, lty=2, alpha=0.5) + 
    geom_vline(xintercept=4.5, lty=1, alpha=0.5) +
    ylab("Effect of ambivalent sexism on affect thermometer") + xlab("Affect thermometer target") +
    facet_wrap(. ~ model) + 
    scale_color_manual(values=c("black","blue","red")) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            legend.position = "none",
            axis.text.x = element_text(size=20),
            axis.text.y = element_text(size=20),
            strip.text = element_text(size=26),
            panel.spacing = unit(2, "lines"),
            axis.title.x = element_text(size=24),
            axis.title.y = element_text(size=24)
        )
ggsave("figures/firststage_sexism_placebo.pdf", width=15.5, height=6)
if(SHOW_PDFS) system("open figures/firststage_sexism_placebo.pdf")

#####------------------------------------------------------#
##### H7b: Heterogeneities in favorability by ambivalent sexism ####
#####------------------------------------------------------#

n <- dat %>% 
    filter(!is.na(treat), exp_1_prompt_control==T, !is.na(post_favor_Warren)) %>%
    mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                 labels=c("low", "moderate", "high"))) %>%
    filter(!is.na(ambivalent_sexism), !is.nan(ambivalent_sexism)) %>%
    nrow()
n.low <- dat %>%
    filter(!is.na(treat), exp_1_prompt_control==T, !is.na(post_favor_Warren)) %>%
    mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                 labels=c("low", "moderate", "high"))) %>%
    filter(!is.na(ambivalent_sexism), !is.nan(ambivalent_sexism)) %>%
    filter(ambivalent_sexism == "low") %>%
    nrow()
n.mod <- dat %>%
    filter(!is.na(treat), exp_1_prompt_control==T, !is.na(post_favor_Warren)) %>%
    mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                 labels=c("low", "moderate", "high"))) %>%
    filter(!is.na(ambivalent_sexism), !is.nan(ambivalent_sexism)) %>%
    filter(ambivalent_sexism == "moderate") %>%
    nrow()
n.high <- dat %>%
    filter(!is.na(treat), exp_1_prompt_control==T, !is.na(post_favor_Warren)) %>%
    mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                 labels=c("low", "moderate", "high"))) %>%
    filter(!is.na(ambivalent_sexism), !is.nan(ambivalent_sexism)) %>%
    filter(ambivalent_sexism == "high") %>%
    nrow()

## descriptive plots
dat.viz <- dat %>% 
    filter(!is.na(treat)) %>%
    filter(exp_1_prompt_control == T) %>%
    mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                 labels=c("low sexism", "moderate sexism", "high sexism"))) %>%
    filter(!is.na(ambivalent_sexism), !is.nan(ambivalent_sexism)) %>%
    group_by(ambivalent_sexism, treat) %>%
    summarise(y=weighted.mean(post_favor_Warren,weight,na.rm=T),
              ymin=weighted.mean(post_favor_Warren,weight,na.rm=T)-1.96*weighted.sd(post_favor_Warren,weight,na.rm=T)/sqrt(n()),
              ymax=weighted.mean(post_favor_Warren,weight,na.rm=T)+1.96*weighted.sd(post_favor_Warren,weight,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    mutate(treat = as.character(treat)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "control", after=0)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "ad", after=1)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=5))
    
dat.viz %>%
    ggplot(aes(x=treat, y=y, ymin=ymin, ymax=ymax)) +
    geom_bar(stat="identity") +
    ylab("feeling thermometer") + xlab("") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    geom_hline(data = dat.viz %>% group_by(ambivalent_sexism) %>% summarise(yint=mean(y)),
               aes(yintercept = yint), size=1, lty=1, color="red") +
    geom_vline(xintercept=2.5, size=0.5, lty=2) +
    facet_wrap(~ambivalent_sexism) + 
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=11),
            axis.text.y = element_text(size=12),
            strip.text = element_text(size=18),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16)
        )
ggsave("figures/firststage_feelings_bysexism.pdf", width=8, height=3.05)
if(SHOW_PDFS) system("open figures/firststage_feelings_bysexism.pdf")

## regression tables
(h7b.b1 <- lm(post_favor_Warren ~ ambivalent_sexism*treat, dat %>% filter(treat != "ad"))); summary(h7b.b1); 
(h7b.wt.b1 <- lm(post_favor_Warren ~ ambivalent_sexism*treat, dat %>% filter(treat != "ad"), weights=weight)); summary(h7b.wt.b1); 
(h7b.hq.b1 <- lm(post_favor_Warren ~ ambivalent_sexism*treat, dat %>% filter(treat != "ad", !lowq))); summary(h7b.hq.b1); 
(h7b.adj.b1 <- lm(post_favor_Warren ~ ambivalent_sexism*treat + PID + crt + response_wave_ID + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad"))); summary(h7b.adj.b1); 
(h7b.adj.wt.b1 <- lm(post_favor_Warren ~ ambivalent_sexism*treat + PID + crt + response_wave_ID + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad"), weights=weight)); summary(h7b.adj.wt.b1); 
(h7b.adj.hq.b1 <- lm(post_favor_Warren ~ ambivalent_sexism*treat + PID + crt + response_wave_ID + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", !lowq))); summary(h7b.adj.hq.b1); 
(h7b.adj.hq.wt.b1 <- lm(post_favor_Warren ~ ambivalent_sexism*treat + PID + crt + response_wave_ID + exp_1_prompt + meta_OS + age_65 + educ + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(treat != "ad", !lowq), weights=weight)); summary(h7b.adj.hq.wt.b1); 

h7b.b1.df <- bind_rows(
    tidy(h7b.b1),
    tidy(h7b.wt.b1),
    tidy(h7b.hq.b1),
    tidy(h7b.adj.b1),
    tidy(h7b.adj.wt.b1),
    tidy(h7b.adj.hq.b1),
    tidy(h7b.adj.hq.wt.b1)
)
h7b.b1.df$p.value.adj <- (h7b.b1.df$p.value/order(h7b.b1.df$p.value))*nrow(h7b.b1.df) ##BHq corrections

stargazer(h7b.b1,
          h7b.wt.b1,
          h7b.hq.b1,
          h7b.adj.b1,
          h7b.adj.wt.b1,
          h7b.adj.hq.b1,
          h7b.adj.hq.wt.b1,
          header = FALSE,
          no.space=TRUE,
          apply.p = function(p) { h7b.b1.df$p.value.adj[h7b.b1.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$"),
          notes.append = FALSE,
          digits=2,
          table.layout ="=d#-t-a-s=n",
          notes.align = "l",
          title="\\textbf{Models of Ambivalent Sexism and Scandal Target Affect in Incidental Exposure Experiment}",
          covariate.labels = c("Ambivalent Sexism",
                               "Video","Audio","Text","Skit",
                               "A.S. x Video",
                               "A.S. x Audio",
                               "A.S. x Text",
                               "A.S. x Skit"),
          omit = c("response_wave", COVARS[!(COVARS %in% c("ambivalent_sexism"))], "exp_1"),
          dep.var.labels = c("\\normalsize Elizabeth Warren Feeling Thermometer"),
          add.lines = list(c("Weighted?", "", "\\checkmark", "", "", "\\checkmark","","\\checkmark"),
                           c("Low-Quality Dropped?","","","\\checkmark","","","\\checkmark","\\checkmark"),
                           c("Controls?","","","","\\checkmark","\\checkmark","\\checkmark","\\checkmark")),
          omit.stat=c("f", "ser"),
          column.sep.width = "1pt",
          font.size = "scriptsize",
          style = "apsr",
          label="firststage_feelings_bysexism",
          out="tables/firststage_feelings_bysexism.tex")

#####------------------------------------------------------#
##### H8 and H9: Accuracy salience/diglit and detection accuracy ####
#####------------------------------------------------------#

## regression tables

## @SB: accuracy prompts don't increase accuracy
(h8.m <- lm(exp_2_pct_correct ~ exp_2_prompt_accuracy, dat, weights=weight)); summary(h8.m); 

## @SB: statistically significant and positive effect of digital literacy
##      on performance
(h9.m <- lm(exp_2_pct_correct ~ post_dig_lit, dat)); summary(h9.m); 
(h89.corr.adj <- lm(exp_2_pct_correct ~ post_dig_lit + exp_2_prompt_accuracy + exp_2_before_debrief + exp_1_prompt_info + polknow + internet_usage +
                              exp_2 + age_65 + educ + I(PID=="Republican")*crt + ambivalent_sexism, dat%>%mutate(exp_2_before_debrief=!exp_2_after_debrief))); summary(h89.corr.adj);
(h89.corr.adj.wt <- lm(exp_2_pct_correct ~ post_dig_lit + exp_2_prompt_accuracy + exp_2_before_debrief + exp_1_prompt_info + polknow + internet_usage +
                              exp_2 + age_65 + educ + I(PID=="Republican")*crt + ambivalent_sexism, dat%>%mutate(exp_2_before_debrief=!exp_2_after_debrief), weights=weight)); summary(h89.corr.adj);
(h89.corr.adj.hq <- lm(exp_2_pct_correct ~ post_dig_lit + exp_2_prompt_accuracy + exp_2_before_debrief + exp_1_prompt_info + polknow + internet_usage +
                              exp_2 + age_65 + educ + I(PID=="Republican")*crt + ambivalent_sexism, dat%>%mutate(exp_2_before_debrief=!exp_2_after_debrief)%>%filter(!lowq))); summary(h89.corr.adj);
(h89.corr.adj.hq.wt <- lm(exp_2_pct_correct ~ post_dig_lit + exp_2_prompt_accuracy + exp_2_before_debrief + exp_1_prompt_info + polknow + internet_usage +
                              exp_2 + age_65 + educ + I(PID=="Republican")*crt + ambivalent_sexism, dat%>%mutate(exp_2_before_debrief=!exp_2_after_debrief)%>%filter(!lowq), weights=weight)); summary(h89.corr.adj);

h89.corr.df <- bind_rows(
    tidy(h9.m),
    tidy(h89.corr.adj),
    tidy(h89.corr.adj.wt),
    tidy(h89.corr.adj.hq),
    tidy(h89.corr.adj.hq.wt)
)
h89.corr.df$p.value.adj <- (h89.corr.df$p.value/order(h89.corr.df$p.value))*nrow(h89.corr.df) ##BHq corrections

var_order <- names(coefficients(h89.corr.adj))[2:16]
stargazer(h8.m,
          h9.m,
          h89.corr.adj,
          h89.corr.adj.wt,
          h89.corr.adj.hq,
          h89.corr.adj.hq.wt,
          header = FALSE,
          no.space=TRUE,
          apply.p = function(p) { h89.corr.df$p.value.adj[h89.corr.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$"),
          notes.append = FALSE,
          digits=2,
          table.layout ="=d#-t-a-s=n",
          notes.align = "l",
          title="\\textbf{Predictors of Detection Task Accuracy}",
          omit = c("response_wave_ID"), 
          dep.var.labels = c("\\normalsize Deepfake Detection Accuracy (\\% Correctly Classified)"),
          omit.stat=c("f", "ser"),
          order = var_order,
          covariate.labels = c(
            "Digital Literacy",
            "Accuracy Prime",
            "Exp 1 Debrief",
            "Exp 1 Information",
            "Political Knowledge",
            "Internet Usage",
            "Low-fake Env.",
            "No-fake Env.",
            "Age 65+",
            "High School",
            "College",
            "Postgrad",
            "C.R.",
            "C.R. x Republican",
            "Ambivalent Sexism",
            "Republican"
          ),
          add.lines = list(c("Weighted?", "", "", "", "\\checkmark", "","\\checkmark"),
                           c("Low-Quality Dropped?", "", "", "", "","\\checkmark","\\checkmark")),
          column.sep.width = "1pt",
          font.size = "footnotesize",
          style = "apsr",
          label="secondstage_accuracy",
          out="tables/secondstage_accuracy.tex")

## regression tables on FPR
(h8.m.fpr <- lm(exp_2_pct_false_fake ~ exp_2_prompt_accuracy, dat, weights=weight)); summary(h8.m.fpr); 
(h9.m.fpr <- lm(exp_2_pct_false_fake ~ post_dig_lit, dat)); summary(h9.m.fpr); 
(h89.corr.adj.fpr <- lm(exp_2_pct_false_fake ~ post_dig_lit + exp_2_prompt_accuracy + exp_2_before_debrief + exp_1_prompt_info + polknow + internet_usage +
                              exp_2 + age_65 + educ + I(PID=="Republican")*crt + ambivalent_sexism, dat%>%mutate(exp_2_before_debrief=!exp_2_after_debrief))); summary(h89.corr.adj.fpr);
(h89.corr.adj.wt.fpr <- lm(exp_2_pct_false_fake ~ post_dig_lit + exp_2_prompt_accuracy + exp_2_before_debrief + exp_1_prompt_info + polknow + internet_usage +
                              exp_2 + age_65 + educ + I(PID=="Republican")*crt + ambivalent_sexism, dat%>%mutate(exp_2_before_debrief=!exp_2_after_debrief), weights=weight)); summary(h89.corr.adj.fpr);
(h89.corr.adj.hq.fpr <- lm(exp_2_pct_false_fake ~ post_dig_lit + exp_2_prompt_accuracy + exp_2_before_debrief + exp_1_prompt_info + polknow + internet_usage +
                              exp_2 + age_65 + educ + I(PID=="Republican")*crt + ambivalent_sexism, dat%>%mutate(exp_2_before_debrief=!exp_2_after_debrief)%>%filter(!lowq))); summary(h89.corr.adj.fpr);
(h89.corr.adj.hq.wt.fpr <- lm(exp_2_pct_false_fake ~ post_dig_lit + exp_2_prompt_accuracy + exp_2_before_debrief + exp_1_prompt_info + polknow + internet_usage +
                              exp_2 + age_65 + educ + I(PID=="Republican")*crt + ambivalent_sexism, dat%>%mutate(exp_2_before_debrief=!exp_2_after_debrief)%>%filter(!lowq), weights=weight)); summary(h89.corr.adj.fpr);

h89.fpr.df <- bind_rows(
    tidy(h8.m.fpr),
    tidy(h9.m.fpr),
    tidy(h89.corr.adj.fpr),
    tidy(h89.corr.adj.wt.fpr),
    tidy(h89.corr.adj.hq.fpr),
    tidy(h89.corr.adj.hq.wt.fpr)
)
h89.fpr.df$p.value.adj <- (h89.fpr.df$p.value/order(h89.fpr.df$p.value))*nrow(h89.fpr.df) ##BHq corrections

var_order <- names(coefficients(h89.corr.adj.fpr))[2:16]
stargazer(h8.m.fpr,
          h9.m.fpr,
          h89.corr.adj.fpr,
          h89.corr.adj.wt.fpr,
          h89.corr.adj.hq.fpr,
          h89.corr.adj.hq.wt.fpr,
          header = FALSE,
          no.space=TRUE,
          apply.p = function(p) { h89.fpr.df$p.value.adj[h89.fpr.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$"),
          notes.append = FALSE,
          digits=2,
          table.layout ="=d#-t-a-s=n",
          notes.align = "l",
          title="\\textbf{Predictors of Detection Task False Positive Rate (FPR)}",
          omit = c("response_wave_ID"), 
          dep.var.labels = c("\\normalsize Detection FPR (\\% Real Videos Classified as Deepfakes)"),
          omit.stat=c("f", "ser"),
          order = var_order,
          covariate.labels = c(
            "Digital Literacy",
            "Accuracy Prime",
            "Exp 1 Debrief",
            "Exp 1 Information",
            "Political Knowledge",
            "Internet Usage",
            "Low-fake Env.",
            "No-fake Env.",
            "Age 65+",
            "High School",
            "College",
            "Postgrad",
            "C.R.",
            "C.R. x Republican",
            "Ambivalent Sexism",
            "Republican"
          ),
          add.lines = list(c("Weighted?", "", "", "", "\\checkmark", "","\\checkmark"),
                           c("Low-Quality Dropped?", "", "", "", "","\\checkmark","\\checkmark")),
          column.sep.width = "1pt",
          font.size = "footnotesize",
          style = "apsr",
          label="secondstage_fpr",
          out="tables/secondstage_fpr.tex")

## regression tables on FNR
(h8.m.fnr <- lm(exp_2_pct_false_real ~ exp_2_prompt_accuracy, dat, weights=weight)); summary(h8.m.fnr); 
(h9.m.fnr <- lm(exp_2_pct_false_real ~ post_dig_lit, dat)); summary(h9.m.fnr); 
(h89.corr.adj.fnr <- lm(exp_2_pct_false_real ~ post_dig_lit + exp_2_prompt_accuracy + exp_2_before_debrief + exp_1_prompt_info + polknow + internet_usage +
                              exp_2 + age_65 + educ + I(PID=="Republican")*crt + ambivalent_sexism, dat%>%mutate(exp_2_before_debrief=!exp_2_after_debrief))); summary(h89.corr.adj.fnr);
(h89.corr.adj.wt.fnr <- lm(exp_2_pct_false_real ~ post_dig_lit + exp_2_prompt_accuracy + exp_2_before_debrief + exp_1_prompt_info + polknow + internet_usage +
                              exp_2 + age_65 + educ + I(PID=="Republican")*crt + ambivalent_sexism, dat%>%mutate(exp_2_before_debrief=!exp_2_after_debrief), weights=weight)); summary(h89.corr.adj.fnr);
(h89.corr.adj.hq.fnr <- lm(exp_2_pct_false_real ~ post_dig_lit + exp_2_prompt_accuracy + exp_2_before_debrief + exp_1_prompt_info + polknow + internet_usage +
                              exp_2 + age_65 + educ + I(PID=="Republican")*crt + ambivalent_sexism, dat%>%mutate(exp_2_before_debrief=!exp_2_after_debrief)%>%filter(!lowq))); summary(h89.corr.adj.fnr);
(h89.corr.adj.hq.wt.fnr <- lm(exp_2_pct_false_real ~ post_dig_lit + exp_2_prompt_accuracy + exp_2_before_debrief + exp_1_prompt_info + polknow + internet_usage +
                              exp_2 + age_65 + educ + I(PID=="Republican")*crt + ambivalent_sexism, dat%>%mutate(exp_2_before_debrief=!exp_2_after_debrief)%>%filter(!lowq), weights=weight)); summary(h89.corr.adj.fnr);

h89.fnr.df <- bind_rows(
    tidy(h8.m.fnr),
    tidy(h9.m.fnr),
    tidy(h89.corr.adj.fnr),
    tidy(h89.corr.adj.wt.fnr),
    tidy(h89.corr.adj.hq.fnr),
    tidy(h89.corr.adj.hq.wt.fnr)
)
h89.fnr.df$p.value.adj <- (h89.fnr.df$p.value/order(h89.fnr.df$p.value))*nrow(h89.fnr.df) ##BHq corrections

var_order <- names(coefficients(h89.corr.adj.fnr))[2:16]
stargazer(h8.m.fnr,
          h9.m.fnr,
          h89.corr.adj.fnr,
          h89.corr.adj.wt.fnr,
          h89.corr.adj.hq.fnr,
          h89.corr.adj.hq.wt.fnr,
          header = FALSE,
          no.space=TRUE,
          apply.p = function(p) { h89.fnr.df$p.value.adj[h89.fnr.df$p.value == p][1] },
          notes =  c("\\textit{Notes:} $^{.}$ $p \\cdot r/K < .1$ * $p \\cdot r/K < .05$ ** $p \\cdot r/K < .01$ *** $p \\cdot r/K < .001$"),
          notes.append = FALSE,
          digits=2,
          table.layout ="=d#-t-a-s=n",
          notes.align = "l",
          title="\\textbf{Predictors of Detection Task False Negative Rate (FNR)}",
          omit = c("response_wave_ID"), 
          dep.var.labels = c("\\normalsize Detection FNR (\\% Deepfakes Classified as Real Videos)"),
          omit.stat=c("f", "ser"),
          order = var_order,
          covariate.labels = c(
            "Digital Literacy",
            "Accuracy Prime",
            "Exp 1 Debrief",
            "Exp 1 Information",
            "Political Knowledge",
            "Internet Usage",
            "Low-fake Env.",
            "No-fake Env.",
            "Age 65+",
            "High School",
            "College",
            "Postgrad",
            "Republican",
            "C.R.",
            "Ambivalent Sexism",
            "C.R. x Republican"
          ),
          add.lines = list(c("Weighted?", "", "", "", "\\checkmark", "","\\checkmark"),
                           c("Low-Quality Dropped?", "", "", "", "","\\checkmark","\\checkmark")),
          column.sep.width = "1pt",
          font.size = "footnotesize",
          style = "apsr",
          label="secondstage_fnr",
          out="tables/secondstage_fnr.tex")

#####------------------------------------------------------#
##### Additional second-stage plots ####
#####------------------------------------------------------#

## Second stage hypotheses are a bit scattered so collecting
## all the plots for them here

## === Error decomposition ===
n <- dat %>% filter(!is.na(exp_2), !is.na(exp_2_pct_correct)) %>% nrow()
n.no <- dat %>% filter(!is.na(exp_2), !is.na(exp_2_pct_correct), exp_2=="nofake") %>% nrow()
n.lo <- dat %>% filter(!is.na(exp_2), !is.na(exp_2_pct_correct), exp_2=="lofake") %>% nrow()
n.hi <- dat %>% filter(!is.na(exp_2), !is.na(exp_2_pct_correct), exp_2=="hifake") %>% nrow()


dat %>%
    dplyr::select(exp_2, exp_2_pct_correct, exp_2_pct_false_fake, exp_2_pct_false_real) %>%
    tidyr::gather(key="metric",value="val", exp_2_pct_correct, exp_2_pct_false_fake, exp_2_pct_false_real) %>%
    filter(!is.na(exp_2)) %>%
    group_by(exp_2, metric) %>%
    summarise(
        y=mean(val,na.rm=T),
        sd=sd(val,na.rm=T)/sqrt(n())
    ) %>%
    ungroup() %>%
    mutate(metric = as.character(metric)) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_correct", "accuracy")) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_false_fake", "false positive rate")) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_false_real", "false negative rate")) %>%
    mutate(exp_2 = as.character(exp_2)) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "lofake", "low\nfake")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "hifake", "high\nfake")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "nofake", "no\nfake")) %>%
    mutate(exp_2 = fct_relevel(as.factor(exp_2), "high\nfake", after=0)) %>%
    mutate(y = replace(y, metric == "false negative rate" & exp_2 == "no\nfake", 0.004)) %>%
    mutate(sd = replace(sd, metric == "false negative rate" & exp_2 == "no\nfake", 0)) %>%
    ggplot(aes(x=exp_2, y=y, ymin=y-1.96*sd, ymax=y+1.96*sd)) +
    facet_grid(.~ metric, scales="free_x", space="free_x") +
    geom_bar(stat="identity") + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) + 
    ylab("rate") + xlab("detection environment") +
    ggtitle(paste0("n=",n," | n_no=",n.no," | n_lo=",n.lo," | no_hi=",n.hi)) +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            strip.text = element_text(size=18),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16)
        )
ggsave("figures/secondstage_detectionbyenv.pdf", width=8, height=3.05)
if(SHOW_PDFS) system("open figures/secondstage_detectionbyenv.pdf")

dat %>%
    dplyr::select(exp_2, exp_2_pct_correct, exp_2_pct_false_fake, exp_2_pct_false_real) %>%
    tidyr::gather(key="metric",value="val", exp_2_pct_correct, exp_2_pct_false_fake, exp_2_pct_false_real) %>%
    filter(!is.na(exp_2)) %>%
    group_by(exp_2, metric) %>%
    summarise(
        y=mean(val,na.rm=T),
        sd=sd(val,na.rm=T)/sqrt(n())
    ) %>%
    ungroup() %>%
    mutate(metric = as.character(metric)) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_correct", "Acc.")) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_false_fake", "FPR")) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_false_real", "FNR")) %>%
    mutate(exp_2 = as.character(exp_2)) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "lofake", "low-fake\nenvironment")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "hifake", "high-fake\nenvironment")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "nofake", "no-fake\nenvironment")) %>%
    mutate(exp_2 = fct_relevel(as.factor(exp_2), "high-fake\nenvironment", after=0)) %>%
    mutate(y = replace(y, metric == "FNR" & exp_2 == "no-fake\nenvironment", 0.004)) %>%
    mutate(sd = replace(sd, metric == "FNR" & exp_2 == "no-fake\nenvironment", 0)) %>%
    ggplot(aes(x=metric, y=y, ymin=y-1.96*sd, ymax=y+1.96*sd)) +
    facet_grid(.~ exp_2, scales="free_x", space="free_x") +
    geom_bar(stat="identity") + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) + 
    ggtitle(paste0("n=",n," | n_no=",n.no," | n_lo=",n.lo," | no_hi=",n.hi)) +
    ylab("rate") + xlab("detection metrics") +
    theme_linedraw2 + 
        theme(
            title = element_text(size=5),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            strip.text = element_text(size=14),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16)
        )
ggsave("figures/secondstage_detectionbyenv2.pdf", width=6, height=3.05)
if(SHOW_PDFS) system("open figures/secondstage_detectionbyenv2.pdf")


## === Basic linear models ===
(m1 <- lm(exp_2_pct_correct ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                              exp_2 + response_wave_ID + age_65 + educ + PID + internet_usage + ambivalent_sexism, dat, weights=weight)); summary(m1);
(m2 <- lm(exp_2_pct_false_fake ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                                 exp_2 + response_wave_ID + age_65 + educ + PID + internet_usage + ambivalent_sexism, dat, weights=weight)); summary(m2);
(m3 <- lm(exp_2_pct_false_real ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                                 exp_2 + response_wave_ID + age_65 + educ + PID + internet_usage + ambivalent_sexism, dat, weights=weight)); summary(m3);

dat.viz1 <- bind_rows(tidy(m1) %>% mutate(metric="Accuracy"), 
                      tidy(m2) %>% mutate(metric="False Positive Rate"),
                      tidy(m3) %>% mutate(metric="False Negative Rate"))

terms2viz <- c("post_dig_lit", "exp_2_prompt_accuracyTRUE", "exp_1_prompt_infoTRUE", "exp_2_after_debrief", "polknow", "crt", "PIDRepublican")
dat.viz1 %>% 
    filter(term %in% terms2viz) %>%
    mutate(term = replace(term, term == "post_dig_lit", "Digital literacy")) %>%
    mutate(term = replace(term, term == "exp_2_after_debrief", "Debriefed before task")) %>%
    mutate(term = replace(term, term == "exp_2_prompt_accuracyTRUE", "Received accuracy prime")) %>%
    mutate(term = replace(term, term == "exp_1_prompt_infoTRUE", "Received information")) %>%
    mutate(term = replace(term, term == "polknow", "Political knowledge")) %>%
    mutate(term = replace(term, term == "PIDRepublican", "Republican")) %>%
    mutate(term = replace(term, term == "crt", "Cognitive reflection")) %>%
    group_by(term, metric) %>% mutate(overall_avg=mean(estimate)) %>% ungroup() %>% arrange(metric, overall_avg) %>% mutate(term=as_factor(term)) %>%
    mutate(sig = as.factor(ifelse(estimate-1.96*std.error > 0 | estimate+1.96*std.error < 0, "black", "grey"))) %>%
    ggplot(aes(x=term, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, col=sig)) + 
    geom_pointrange(position=position_dodge(width=0.2), lwd=1) + 
    xlab("") + ylab("effect on detection metric (%)") +
    facet_wrap(~ metric, scales = "free_x") +
    geom_hline(yintercept=0, lty=2, alpha=0.5) +
    scale_color_identity() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    coord_flip() +
    theme_linedraw2 +
        theme(
            legend.position = "none",
            axis.text.x = element_text(size=11),
            axis.text.y = element_text(size=14),
            strip.text = element_text(size=18),
            panel.spacing = unit(2, "lines"),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("figures/secondstage_treatfx.pdf", width=10, height=4)
if(SHOW_PDFS) system("open figures/secondstage_treatfx.pdf")


## === Linear models interacted with environment ===
dat.viz2 <- data.frame()
for (e in levels(dat$exp_2)) {
    (m1 <- lm(exp_2_pct_correct ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                                 response_wave_ID + age_65 + educ + PID + internet_usage + ambivalent_sexism, dat %>% filter(exp_2 %in% e), weights=weight)); summary(m1);
    (m2 <- lm(exp_2_pct_false_fake ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                                 response_wave_ID + age_65 + educ + PID + internet_usage + ambivalent_sexism, dat %>% filter(exp_2 %in% e), weights=weight)); summary(m2);
    if (e != "nofake") {
        (m3 <- lm(exp_2_pct_false_real ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                                         response_wave_ID + age_65 + educ + PID + internet_usage + ambivalent_sexism, dat %>% filter(exp_2 %in% e))); summary(m3);
    }
    dat.viz2 <- bind_rows(dat.viz2, tidy(m1) %>% mutate(metric="Accuracy", exp_2=e)) 
    dat.viz2 <- bind_rows(dat.viz2, tidy(m2) %>% mutate(metric="False Positive Rate", exp_2=e))
    if (e != "nofake") {
        dat.viz2 <- bind_rows(dat.viz2, tidy(m3) %>% mutate(metric="False Negative Rate", exp_2=e)) 
    }
}

terms2viz <- c("post_dig_lit", "exp_2_prompt_accuracyTRUE", "exp_1_prompt_infoTRUE", "exp_2_after_debrief", "polknow", "crt", "PIDRepublican")
dat.viz2 %>% 
    filter(term %in% terms2viz) %>%
    mutate(term = replace(term, term == "post_dig_lit", "Digital literacy")) %>%
    mutate(term = replace(term, term == "exp_2_after_debrief", "Debriefed before task")) %>%
    mutate(term = replace(term, term == "exp_2_prompt_accuracyTRUE", "Received accuracy prime")) %>%
    mutate(term = replace(term, term == "exp_1_prompt_infoTRUE", "Received information")) %>%
    mutate(term = replace(term, term == "polknow", "Political knowledge")) %>%
    mutate(term = replace(term, term == "PIDRepublican", "Republican")) %>%
    mutate(term = replace(term, term == "crt", "Cognitive reflection")) %>%
    group_by(term, metric) %>% mutate(overall_avg=mean(estimate)) %>% ungroup() %>% arrange(metric, overall_avg) %>% mutate(term=as_factor(term)) %>%
    mutate(exp_2 = as.character(exp_2)) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "lofake", "low-fake")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "hifake", "high-fake")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "nofake", "no-fake")) %>%
    mutate(exp_2 = fct_relevel(as.factor(exp_2), "no-fake", after=0)) %>%
    mutate(exp_2 = fct_relevel(as.factor(exp_2), "high-fake", after=2)) %>%
    mutate(sig = as.factor(ifelse(estimate-1.96*std.error > 0 | estimate+1.96*std.error < 0, 1, 0.8))) %>%
    ggplot(aes(x=term, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=exp_2, alpha=sig)) + 
    geom_pointrange(position=position_dodge(width=0.7), lwd=1) + 
    xlab("") + ylab("effect on detection metric (%)") +
    facet_wrap(~ metric, scales = "free_x") +
    geom_hline(yintercept=0, lty=2, alpha=0.5) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    scale_alpha_discrete(guide=FALSE) + 
    scale_color_manual(values=c("black", "blue", "red"), name="Environment:") +
    coord_flip() +
    theme_linedraw2 +
        theme(
            axis.text.x = element_text(size=11),
            axis.text.y = element_text(size=14),
            strip.text = element_text(size=18),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.spacing = unit(2, "lines"),
            legend.position = "bottom",
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("figures/secondstage_treatfx_byenv.pdf", width=11, height=4)
if(SHOW_PDFS) system("open figures/secondstage_treatfx_byenv.pdf")

#####------------------------------------------------------#
##### Cleanup ####
#####------------------------------------------------------#

suffix <- ""

# if (ARGS$weight == 1 & "weight" %in% colnames(dat)) {
#     suffix <- paste0(suffix,"_wt")
# }
if (ARGS$response_quality == "low") {
    suffix <- paste0(suffix,"_lowq")
}
if (ARGS$response_quality == "high") {
    suffix <- paste0(suffix,"_highq")
}

if (suffix != "") {
    system(sprintf("mv tables tables%s", suffix))
    system(sprintf("mv figures figures%s", suffix))
}
