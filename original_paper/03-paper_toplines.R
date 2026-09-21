# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Replicate main figures in paper.
# 
# Author: Soubhik Barari
# 
# Environment:
# - must use R 3.6
# 
# Input:
# - code/deepfake.Rdata
#
# Output:
# - figures/*
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

rm(list=ls())
setwd("~/Research_Group Dropbox/Soubhik Barari/Projects/repos/deepfakes_project")
load("code/deepfake.RData")

select <- dplyr::select

library(latex2exp)
library(optparse)
library(equivalence)
library(cowplot)
library(tidyverse)
library(ggplot2)
library(ggsignif)
library(broom)
library(stargazer)

#####------------------------------------------------------#
##### Setup ####
#####------------------------------------------------------#

polknow_quantile <- quantile(dat$polknow, probs=c(0,0.5,1), include.lowest=T)
polknow_quantile[1] <- -Inf
polknow_quantile[3] <- Inf

diglit_quantile <- quantile(dat$post_dig_lit, probs=c(0,0.33,0.66,1),na.rm=T,include.lowest=T)
diglit_quantile[1] <- -Inf
diglit_quantile[4] <- Inf

sexism_quantile <- quantile(dat$ambivalent_sexism, probs=c(0,0.33,0.66,1),na.rm=T,include.lowest=T)
sexism_quantile[1] <- -Inf
sexism_quantile[4] <- Inf

dat <- dat %>% mutate(polknow=cut(polknow, breaks=polknow_quantile, 
                                  labels=c("Less knowledge", "More knowledge")))

dat <- dat %>% mutate(crt=cut(crt, breaks=c(-1,0,.34,1.1),
                              labels=c("Low c.r.", "Moderate c.r.", "High c.r."))) 

dat <- dat %>% mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=sexism_quantile,
                                            labels=c("Low a.s.", "Moderate a.s.", "High a.s.")))
    
dat <- dat %>% mutate(post_dig_lit=cut(post_dig_lit, breaks=diglit_quantile, 
                                       labels=c("Low d.l.", "Moderate d.l.","High d.l.")))

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

theme_linedraw2 <- theme_linedraw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(2, "lines"))

#####------------------------------------------------------#
##### Experiment 1: deception ####
#####------------------------------------------------------#

colorscheme <- scale_fill_manual(values=c("#C77CFF","#00BFC4","#7CAE00","#F8766D"))

####++distribution of baseline####
dat1 <- dat %>% 
    filter(treat != "ad", treat != "control", treat != "skit") %>%
    filter(!is.na(believed_true)) %>% ##remove the few NAs we have, remove skit conditions not assigned to Qs
    mutate(treat = fct_relevel(treat, "video","audio","text")) %>%
    group_by(treat) %>% mutate(n_cond=n()) %>%
    group_by(treat, believed_true) %>% summarise(n=n(), n_cond=first(n_cond)) %>% 
    mutate(pct=(n/n_cond)) %>%
    group_by(treat) %>% 
    arrange(-believed_true) %>%
    mutate(cum_pct=((cumsum(pct)-lag(cumsum(pct)))/2)+lag(cumsum(pct))) %>%
    mutate(cum_pct=ifelse(is.na(cum_pct), pct/2, cum_pct)) %>%
    mutate(panel="All\n") %>%
    mutate(believed_true=as_factor((believed_true))) %>% 
    as.data.frame()

panel1 <- bind_rows(dat1) %>%
    arrange(desc(row_number())) %>%
    mutate(treat = as_factor(treat)) %>%
    ggplot(aes(x=treat, y=pct, fill=believed_true)) +
    geom_bar(stat="identity", color="black") +
    geom_text(aes(y=cum_pct, label=paste0(round(pct, 2)*100,"%")), size=4) + 
    scale_fill_brewer(type="seq", palette = "RdYlGn", name = "Clipping of candidate is fake or doctored:", 
                      labels=((c("Strongly agree","Somewhat agree","Neither agree/disagree","Somewhat disagree","Strongly disagree")))) + 
    geom_vline(xintercept=2.5, lwd=1.5, color="black") +
    # facet_grid(panel ~ .) + 
    scale_y_continuous(labels=scales::percent_format(1)) +
    scale_x_discrete(labels=c("Warren Text","Warren Audio","Warren Video")) +
    ylab("") + 
    xlab("") + 
    theme_minimal() + 
    coord_flip() + 
    ggtitle("Credibility: Baselines") + 
    theme(title = element_text(size=5),
          plot.title = element_text(size=20, hjust=0.5, face="bold"),
          # legend.position = "bottom",
          legend.position = c(.5,-0.25), legend.direction = "horizontal",
          legend.title = element_text(size=12),
          legend.box="horizontal",
          axis.title.y = element_text(size=16),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=16),
          axis.text.x = element_blank(),
          plot.margin = unit(c(0,0,4.1,0), "lines"),
          axis.title.x = element_text(size=14, hjust=0)) +
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5, 
                               label.position = "bottom", reverse = TRUE))

e.epsilon.l <- -sd(dat$believed_true, na.rm=T)/2
e.epsilon.u <- sd(dat$believed_true, na.rm=T)/2

ylims <- ylim(c(e.epsilon.l, e.epsilon.u))

####++marginal fx (overall)####
dat.all <- dat %>% 
    filter(treat != "ad", treat != "skit") %>%
    mutate(treat = fct_relevel(treat,"text","audio","video")) %>%
    # mutate(believed_true = scale(believed_true)) %>%
    # mutate(believed_true = 5 - believed_true) %>%
    do(tidy(lm(believed_true ~ treat, data = .))) %>%
    filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>%
    mutate(panel = "All\n") %>%
    mutate(lab=toupper(substr(term, 1, 1))) %>%
    # mutate(term = paste0("fake ",term)) %>%
    mutate(color = ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, "grey50", "black"))
    
## BHq adjustments
alpha <- 0.05 ##original threshold
k <- nrow(dat.all) ##number of hypotheses 
r <- order(dat.all$p.value) ##ranks of p-values
alpha_adj <- (r*alpha)/k ##step-up 'tailored' thresholds
dat.all$color <- ifelse(dat.all$p.value < alpha_adj, "black", "grey50") ##step-up hypothesis tests 
dat.all$z_crit_adj <- qnorm(1 - (alpha_adj)/2) ##new critical values for CIs (asymptotic)

## equivalence tests
etest1 <- tost(dat$believed_true[dat$treat == "text"], 
              dat$believed_true[dat$treat == "audio"], epsilon=e.epsilon.l)
etest2 <- tost(dat$believed_true[dat$treat == "text"], 
              dat$believed_true[dat$treat == "video"], epsilon=e.epsilon.u)

dat.all[1,"tost.lo"] <- etest1$tost.interval[1]
dat.all[1,"tost.hi"] <- etest1$tost.interval[2]
dat.all[2,"tost.lo"] <- etest2$tost.interval[1]
dat.all[2,"tost.hi"] <- etest2$tost.interval[2]

panel1.fx.all <- dat.all %>%
    ggplot(aes(x=term, y=estimate, fill=color, folor=color)) +
    ## null reference points
    geom_hline(yintercept=0.0, lty=1, color="grey", size=1) + 
    geom_hline(yintercept=e.epsilon.u, lty=1, color="green", size=1, alpha=0.5) + 
    geom_hline(yintercept=e.epsilon.l, lty=1, color="green", size=1, alpha=0.5) + 
    ## CIs
    geom_linerange(aes(ymin=-tost.lo, ymax=-tost.hi, color=ifelse(tost.lo < e.epsilon.l | tost.hi > e.epsilon.u, "green", "red")), alpha=0.5, size=13) + 
    geom_linerange(aes(ymin=estimate-z_crit_adj*std.error, ymax=estimate+z_crit_adj*std.error, color=color), size=1) +
    geom_linerange(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=color), size=2) +
    ## labels
    geom_label(aes(label=term, y=estimate, fill=color, color="white"), fontface = "bold", size=4) +
    scale_shape_manual(values=c(21,22,24)) +
    # annotate("text", x=Inf, y=-0.27, label="assuming null of no diff.", vjust=6.1, color="darkgrey", fontface="bold") + 
    # annotate("segment", y=0.25, x=1.42, yend=0.1, xend=2.1, color="darkgrey") +
    # annotate("text", x=Inf, y=0.25, label=TeX("\\textbf{assuming null of $\\pm 0.5$ diff.}"), vjust=3, color="red", alpha=0.5) + 
    # annotate("segment", y=-0.25, x=2.4, yend=-0.1, xend=2.1, color="red", alpha=0.5) +
    scale_fill_identity() + 
    scale_color_identity() + 
    ylab("") + 
    xlab("") + 
    coord_flip() + 
    theme_linedraw() + 
    ylims +
    facet_grid(panel ~ .) + 
    ggtitle("Credibility: Effects") + 
    theme(title = element_text(size=5),
          plot.title = element_text(size=20, hjust=0.5, face="bold"),
          legend.position = "none",
          legend.title = element_text(size=12),
          legend.box="horizontal",
          axis.title.y = element_text(size=16),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=16),
          axis.text.x = element_blank(),
          # plot.margin = unit(c(0,0,6,0), "lines"),
          # axis.ticks.x = element_blank(),
          axis.title.x = element_text(size=14, hjust=0))


####++marginal fx (by group)####
dat.bygroups <- bind_rows(
    ## by intervention group
    dat %>% filter(treat != "ad", treat !=" control", treat != "skit", !is.na(treat)) %>%
        # mutate(believed_true = 5 - believed_true) %>%
        mutate(exp_1_prompt = ifelse(exp_1_prompt=="info", "Information","No information")) %>%
        group_by(exp_1_prompt) %>%
        mutate(treat = fct_relevel(treat,"text","audio","video")) %>%
        # mutate(believed_true = scale(believed_true)) %>%
        do(tidy(lm(believed_true ~ treat, data = .))) %>%
        mutate(category = "By\nIntervention") %>%
        filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=exp_1_prompt),
    ## by party
    dat %>% filter(!is.na(PID)) %>% group_by(PID) %>% filter(treat != "ad", treat !=" control", treat != "skit", !is.na(treat)) %>%
        # mutate(believed_true = scale(believed_true)) %>%
        # mutate(believed_true = 5 - believed_true) %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat, "text","audio","video")) %>%
        do(tidy(lm(believed_true ~ treat, data = .))) %>% ungroup() %>%
        mutate(category = "By Partisanship") %>%
        filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=PID),
    ## by knowledge
     dat %>%
        # mutate(believed_true = scale(believed_true)) %>%
        # mutate(believed_true = 5 - believed_true) %>%
        filter(!is.nan(polknow), !is.na(polknow)) %>%
        group_by(polknow) %>% filter(treat != "ad", treat !=" control", !is.na(treat), treat != "skit") %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat, "text","audio","video")) %>%
        do(tidy(lm(believed_true ~ treat, data = .))) %>% ungroup() %>%
        mutate(category = "By Political\nKnowledge") %>%
        filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=polknow),
    ## by age
    dat %>% filter(!is.na(age_65)) %>% group_by(age_65) %>% filter(treat != "ad", treat !=" control", treat != "skit", !is.na(treat)) %>%
        # mutate(believed_true = scale(believed_true)) %>%
        # mutate(believed_true = 5 - believed_true) %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat, "text","audio","video")) %>%
        do(tidy(lm(believed_true ~ treat, data = .))) %>% ungroup() %>%
        mutate(category = "By Age\nGroup") %>%
        filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=age_65),
    ## by CRT
    dat %>% filter(!is.na(crt)) %>%
        # mutate(believed_true = scale(believed_true)) %>%
        # mutate(believed_true = 5 - believed_true) %>%
        group_by(crt) %>% filter(treat != "ad", treat !=" control", treat != "skit", !is.na(treat)) %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat, "text","audio","video")) %>%
        do(tidy(lm(believed_true ~ treat, data = .))) %>% ungroup() %>%
        mutate(category = "By Cognitive\nReflection") %>%
        filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=crt),
    ## by sexism
    dat %>%
        # mutate(believed_true = scale(believed_true)) %>%
        # mutate(believed_true = 5 - believed_true) %>%
        filter(!is.nan(ambivalent_sexism), !is.na(ambivalent_sexism)) %>%
        group_by(ambivalent_sexism) %>% filter(treat != "ad", treat !=" control", !is.na(treat), treat != "skit") %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat, "text","audio","video")) %>%
        do(tidy(lm(believed_true ~ treat, data = .))) %>% ungroup() %>%
        mutate(category = "By Ambivalent\nSexism") %>%
        filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=ambivalent_sexism)
) %>% mutate(color = ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, "grey50", "black"))

## BHq adjustments
alpha <- 0.05 ##original threshold
k <- nrow(dat.bygroups) ##number of hypotheses 
r <- order(dat.bygroups$p.value) ##ranks of p-values
alpha_adj <- (r*alpha)/k ##step-up 'tailored' thresholds
dat.bygroups$color <- ifelse(dat.bygroups$p.value < alpha_adj, "black", "grey50") ##step-up hypothesis tests 
dat.bygroups$z_crit_adj <- qnorm(1 - (alpha_adj)/2) ##new critical values for CIs (asymptotic)

## equivalence tests
### by intervention group
etest1 <- tost(dat$believed_true[dat$treat == "text" & dat$exp_1_prompt == "info"], 
              dat$believed_true[dat$treat == "audio" & dat$exp_1_prompt == "info"], epsilon=1)
etest2 <- tost(dat$believed_true[dat$treat == "text" & dat$exp_1_prompt == "info"], 
              dat$believed_true[dat$treat == "video" & dat$exp_1_prompt == "info"], epsilon=1)
etest3 <- tost(dat$believed_true[dat$treat == "text" & dat$exp_1_prompt != "info"], 
              dat$believed_true[dat$treat == "audio" & dat$exp_1_prompt != "info"], epsilon=1)
etest4 <- tost(dat$believed_true[dat$treat == "text" & dat$exp_1_prompt != "info"], 
              dat$believed_true[dat$treat == "video" & dat$exp_1_prompt != "info"], epsilon=1)

dat.bygroups[1,"tost.lo"] <- etest1$tost.interval[1]
dat.bygroups[1,"tost.hi"] <- etest1$tost.interval[2]
dat.bygroups[2,"tost.lo"] <- etest2$tost.interval[1]
dat.bygroups[2,"tost.hi"] <- etest2$tost.interval[2]
dat.bygroups[3,"tost.lo"] <- etest3$tost.interval[1]
dat.bygroups[3,"tost.hi"] <- etest3$tost.interval[2]
dat.bygroups[4,"tost.lo"] <- etest4$tost.interval[1]
dat.bygroups[4,"tost.hi"] <- etest4$tost.interval[2]

### by party
etest5 <- tost(dat$believed_true[dat$treat == "text" & dat$PID == "Democrat"], 
               dat$believed_true[dat$treat == "audio" & dat$PID == "Democrat"], epsilon=1)
etest6 <- tost(dat$believed_true[dat$treat == "text" & dat$PID == "Democrat"], 
               dat$believed_true[dat$treat == "video" & dat$PID == "Democrat"], epsilon=1)
etest7 <- tost(dat$believed_true[dat$treat == "text" & dat$PID == "Independent"], 
               dat$believed_true[dat$treat == "audio" & dat$PID == "Independent"], epsilon=1)
etest8 <- tost(dat$believed_true[dat$treat == "text" & dat$PID == "Independent"], 
               dat$believed_true[dat$treat == "video" & dat$PID == "Independent"], epsilon=1)
etest9 <- tost(dat$believed_true[dat$treat == "text" & dat$PID == "Republican"], 
               dat$believed_true[dat$treat == "audio" & dat$PID == "Republican"], epsilon=1)
etest10 <- tost(dat$believed_true[dat$treat == "text" & dat$PID == "Republican"], 
                dat$believed_true[dat$treat == "video" & dat$PID == "Republican"], epsilon=1)

dat.bygroups[5,"tost.lo"] <- etest5$tost.interval[1]
dat.bygroups[5,"tost.hi"] <- etest5$tost.interval[2]
dat.bygroups[6,"tost.lo"] <- etest6$tost.interval[1]
dat.bygroups[6,"tost.hi"] <- etest6$tost.interval[2]
dat.bygroups[7,"tost.lo"] <- etest7$tost.interval[1]
dat.bygroups[7,"tost.hi"] <- etest7$tost.interval[2]
dat.bygroups[8,"tost.lo"] <- etest8$tost.interval[1]
dat.bygroups[8,"tost.hi"] <- etest8$tost.interval[2]
dat.bygroups[9,"tost.lo"] <- etest9$tost.interval[1]
dat.bygroups[9,"tost.hi"] <- etest9$tost.interval[2]
dat.bygroups[10,"tost.lo"] <- etest10$tost.interval[1]
dat.bygroups[10,"tost.hi"] <- etest10$tost.interval[2]

### by knowledge
etest11 <- tost(dat$believed_true[dat$treat == "text" & dat$polknow == "Less knowledge"], 
               dat$believed_true[dat$treat == "audio" & dat$polknow == "Less knowledge"], epsilon=1)
etest12 <- tost(dat$believed_true[dat$treat == "text" & dat$polknow == "Less knowledge"], 
               dat$believed_true[dat$treat == "video" & dat$polknow == "Less knowledge"], epsilon=1)
etest13 <- tost(dat$believed_true[dat$treat == "text" & dat$polknow == "More knowledge"], 
               dat$believed_true[dat$treat == "audio" & dat$polknow == "More knowledge"], epsilon=1)
etest14 <- tost(dat$believed_true[dat$treat == "text" & dat$polknow == "More knowledge"], 
               dat$believed_true[dat$treat == "video" & dat$polknow == "More knowledge"], epsilon=1)

dat.bygroups[11,"tost.lo"] <- etest11$tost.interval[1]
dat.bygroups[11,"tost.hi"] <- etest11$tost.interval[2]
dat.bygroups[12,"tost.lo"] <- etest12$tost.interval[1]
dat.bygroups[12,"tost.hi"] <- etest12$tost.interval[2]
dat.bygroups[13,"tost.lo"] <- etest13$tost.interval[1]
dat.bygroups[13,"tost.hi"] <- etest13$tost.interval[2]
dat.bygroups[14,"tost.lo"] <- etest14$tost.interval[1]
dat.bygroups[14,"tost.hi"] <- etest14$tost.interval[2]

### by age
etest15 <- tost(dat$believed_true[dat$treat == "text" & dat$age_65 == "<=65"], 
               dat$believed_true[dat$treat == "audio" & dat$age_65 == "<=65"], epsilon=1)
etest16 <- tost(dat$believed_true[dat$treat == "text" & dat$age_65 == "<=65"], 
               dat$believed_true[dat$treat == "video" & dat$age_65 == "<=65"], epsilon=1)
etest17 <- tost(dat$believed_true[dat$treat == "text" & dat$age_65 == ">65"], 
               dat$believed_true[dat$treat == "audio" & dat$age_65 == ">65"], epsilon=1)
etest18 <- tost(dat$believed_true[dat$treat == "text" & dat$age_65 == ">65"], 
               dat$believed_true[dat$treat == "video" & dat$age_65 == ">65"], epsilon=1)

dat.bygroups[15,"tost.lo"] <- etest15$tost.interval[1]
dat.bygroups[15,"tost.hi"] <- etest15$tost.interval[2]
dat.bygroups[16,"tost.lo"] <- etest16$tost.interval[1]
dat.bygroups[16,"tost.hi"] <- etest16$tost.interval[2]
dat.bygroups[17,"tost.lo"] <- etest17$tost.interval[1]
dat.bygroups[17,"tost.hi"] <- etest17$tost.interval[2]
dat.bygroups[18,"tost.lo"] <- etest18$tost.interval[1]
dat.bygroups[18,"tost.hi"] <- etest18$tost.interval[2]


### by CRT
etest19 <- tost(dat$believed_true[dat$treat == "text" & dat$crt == "Low c.r."], 
               dat$believed_true[dat$treat == "audio" & dat$crt == "Low c.r."], epsilon=1)
etest20 <- tost(dat$believed_true[dat$treat == "text" & dat$crt == "Low c.r."], 
               dat$believed_true[dat$treat == "video" & dat$crt == "Low c.r."], epsilon=1)
etest21 <- tost(dat$believed_true[dat$treat == "text" & dat$crt == "Moderate c.r."], 
               dat$believed_true[dat$treat == "audio" & dat$crt == "Moderate c.r."], epsilon=1)
etest22 <- tost(dat$believed_true[dat$treat == "text" & dat$crt == "Moderate c.r."], 
               dat$believed_true[dat$treat == "video" & dat$crt == "Moderate c.r."], epsilon=1)
etest23 <- tost(dat$believed_true[dat$treat == "text" & dat$crt == "High c.r."], 
               dat$believed_true[dat$treat == "audio" & dat$crt == "High c.r."], epsilon=1)
etest24 <- tost(dat$believed_true[dat$treat == "text" & dat$crt == "High c.r."], 
               dat$believed_true[dat$treat == "video" & dat$crt == "High c.r."], epsilon=1)

dat.bygroups[19,"tost.lo"] <- etest19$tost.interval[1]
dat.bygroups[19,"tost.hi"] <- etest19$tost.interval[2]
dat.bygroups[20,"tost.lo"] <- etest20$tost.interval[1]
dat.bygroups[20,"tost.hi"] <- etest20$tost.interval[2]
dat.bygroups[21,"tost.lo"] <- etest21$tost.interval[1]
dat.bygroups[21,"tost.hi"] <- etest21$tost.interval[2]
dat.bygroups[22,"tost.lo"] <- etest22$tost.interval[1]
dat.bygroups[22,"tost.hi"] <- etest22$tost.interval[2]
dat.bygroups[23,"tost.lo"] <- etest23$tost.interval[1]
dat.bygroups[23,"tost.hi"] <- etest23$tost.interval[2]
dat.bygroups[24,"tost.lo"] <- etest24$tost.interval[1]
dat.bygroups[24,"tost.hi"] <- etest24$tost.interval[2]

### by sexism
etest25 <- tost(dat$believed_true[dat$treat == "text" & dat$ambivalent_sexism == "Low a.s."], 
               dat$believed_true[dat$treat == "audio" & dat$ambivalent_sexism == "Low a.s."], epsilon=1)
etest26 <- tost(dat$believed_true[dat$treat == "text" & dat$ambivalent_sexism == "Low a.s."], 
               dat$believed_true[dat$treat == "video" & dat$ambivalent_sexism == "Low a.s."], epsilon=1)
etest27 <- tost(dat$believed_true[dat$treat == "text" & dat$ambivalent_sexism == "Moderate a.s."], 
               dat$believed_true[dat$treat == "audio" & dat$ambivalent_sexism == "Moderate a.s."], epsilon=1)
etest28 <- tost(dat$believed_true[dat$treat == "text" & dat$ambivalent_sexism == "Moderate a.s."], 
               dat$believed_true[dat$treat == "video" & dat$ambivalent_sexism == "Moderate a.s."], epsilon=1)
etest29 <- tost(dat$believed_true[dat$treat == "text" & dat$ambivalent_sexism == "High a.s."], 
               dat$believed_true[dat$treat == "audio" & dat$ambivalent_sexism == "High a.s."], epsilon=1)
etest30 <- tost(dat$believed_true[dat$treat == "text" & dat$ambivalent_sexism == "High a.s."], 
               dat$believed_true[dat$treat == "video" & dat$ambivalent_sexism == "High a.s."], epsilon=1)

dat.bygroups[25,"tost.lo"] <- etest25$tost.interval[1]
dat.bygroups[25,"tost.hi"] <- etest25$tost.interval[2]
dat.bygroups[26,"tost.lo"] <- etest26$tost.interval[1]
dat.bygroups[26,"tost.hi"] <- etest26$tost.interval[2]
dat.bygroups[27,"tost.lo"] <- etest27$tost.interval[1]
dat.bygroups[27,"tost.hi"] <- etest27$tost.interval[2]
dat.bygroups[28,"tost.lo"] <- etest28$tost.interval[1]
dat.bygroups[28,"tost.hi"] <- etest28$tost.interval[2]
dat.bygroups[29,"tost.lo"] <- etest29$tost.interval[1]
dat.bygroups[29,"tost.hi"] <- etest29$tost.interval[2]
dat.bygroups[30,"tost.lo"] <- etest30$tost.interval[1]
dat.bygroups[30,"tost.hi"] <- etest30$tost.interval[2]

####++marginal fx viz (by group)####
panel1.fx.bygroups <- dat.bygroups %>%
    filter(term == "video") %>%
    # mutate(term = paste0("fake ",term)) %>%
    arrange(desc(row_number())) %>%
    mutate(group=as_factor(group)) %>%
    mutate(group = fct_relevel(group, "Low a.s.", "Moderate a.s.")) %>%
    mutate(group = fct_relevel(group, "Low c.r.", "Moderate c.r.")) %>%
    mutate(lab=toupper(substr(trimws(term), 1, 1))) %>%
    ggplot(aes(x=group, y=estimate, fill=color, group=term)) +
    geom_hline(yintercept=0.0, lty=1, color="grey", size=1) + 
    geom_hline(yintercept=e.epsilon.u, lty=1, color="green", size=1, alpha=0.5) + 
    geom_hline(yintercept=e.epsilon.l, lty=1, color="green", size=1, alpha=0.5) + 
    geom_linerange(aes(ymin=-tost.lo, ymax=-tost.hi, color=ifelse(tost.lo < e.epsilon.l | tost.hi > e.epsilon.u, "green", "red")), alpha=0.5, size=13,
                   position=position_dodge(width=0.8)) + 
    geom_linerange(aes(shape=term, ymin=estimate-z_crit_adj*std.error, ymax=estimate+z_crit_adj*std.error, color=color, fill=color), 
                    size=1, position=position_dodge(width=0.8)) +
    geom_linerange(aes(shape=term, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=color, fill=color), 
                    size=2, position=position_dodge(width=0.8)) + 
    geom_label(aes(label=term, y=estimate, fill=color, color="white"), fontface = "bold", size=6, position=position_dodge(width=0.8)) +
    facet_grid(category ~ ., space="free", scales="free") +
    scale_x_discrete(expand=c(0.2,0.2)) +
    # scale_x_discrete(labels=c("control\ngroup","information\nrecipients")) +
    ylab("Mean change in credibility [1-5]\nrelative to fake text") + 
    xlab("") + coord_flip() + theme_linedraw() +
    scale_shape_manual(values=c(21,22)) +
    scale_y_continuous(breaks=c(e.epsilon.l, -.5, -.25, 0, .25, .5, e.epsilon.u),
                       labels=c(TeX("$-0.5\\sigma$"), "-.50", "-.25", "0", "+.25", "+.50", TeX("$+0.5\\sigma$")), 
                       limits=c(e.epsilon.l, e.epsilon.u)) + 
    geom_vline(xintercept=1.5, lty=2, color="darkgray") + 
    geom_vline(xintercept=2.5, lty=2, color="darkgray") + 
    scale_fill_identity() + scale_color_identity() + 
    theme(title = element_text(size=5),
          legend.position = "none",
          legend.title = element_text(size=14),
          legend.box="horizontal",
          axis.title.y = element_text(size=16),
          # axis.ticks.y = element_blank(),
          axis.text.y = element_text(size=14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=13),
          axis.text.x = element_text(size=14),
          # plot.margin = unit(c(0,0,6,0), "lines"),
          # axis.ticks.x = element_blank(),
          axis.title.x = element_text(size=16, hjust=0.5))

####++combined####
pp <- cowplot::plot_grid(panel1, 
                         panel1.fx.all, 
                         panel1.fx.bygroups, 
                         rel_heights = c(2,1.4,8),
                         nrow=3, align="v", axis="lr")
cowplot::save_plot("figures/topline_exp1_a.pdf", plot = pp, base_height=14, base_width=8)
system("open figures/topline_exp1_a.pdf")

#####------------------------------------------------------#
##### Experiment 1: affect ####
#####------------------------------------------------------#

colorscheme <- scale_fill_manual(values=c("black","white","#C77CFF","#00BFC4","#7CAE00","#F8766D"))

####++distribution of baseline####
panel1.b <- dat %>% filter(!is.na(treat)) %>%
    mutate(treat = fct_relevel(treat, "control","ad","skit","text","audio","video")) %>%
    group_by(treat) %>% summarise(estimate=mean(post_favor_Warren, na.rm=T), 
                                  std.error=sd(post_favor_Warren, na.rm=T)/sqrt(n())) %>%
    mutate(panel="All\n") %>%
    ggplot(aes(x=treat, 
               y=estimate,
               fill=estimate)) +
    geom_bar(stat="identity", color="black") +
    coord_flip() +
    geom_linerange(aes(ymin=estimate-1.96*std.error,
                       ymax=estimate+1.96*std.error), size=0.75) + 
    scale_fill_gradient2(low = "red", high = "green", mid = "yellow", midpoint = 50) + 
    scale_y_continuous(breaks = c(0,10,20,30,40,50), 
                       labels = paste0(c(0,10,20,30,40,50),'°')) + 
    
    geom_vline(xintercept=5.5, lwd=1.5, color="black") +
    scale_x_discrete(labels=c("No Stimuli","Warren Ad","Warren Skit",
                              "Warren Text","Warren Audio","Warren Video")) + 
    ylab("Candidate affect thermometer") + 
    xlab("") + 
    theme_minimal() + 
    coord_flip() + 
    ggtitle("Affect: Baselines") + 
    theme(title = element_text(size=5),
          plot.title = element_text(size=20, hjust=0.5, face="bold"),
          legend.position = "none",
          axis.title.y = element_text(size=16),
          # axis.ticks.y = element_line(),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=16),
          axis.text.x = element_text(size=12),
          # plot.margin = unit(c(0,0,4.5,0), "lines"),
          axis.ticks.x = element_line(),
          axis.title.x = element_text(size=14, hjust=0.5))


####++marginal fx (overall)####
dat.b.fx.all <- dat %>% filter(!is.na(treat)) %>%
    mutate(treat = fct_relevel(treat,"control","ad","skit","text","audio","video")) %>%
    # mutate(believed_true = scale(believed_true)) %>%
    do(tidy(lm(post_favor_Warren ~ treat, data = .))) %>%
    filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>%
    mutate(panel = "All\n") %>%
    # mutate(term = ifelse(
    #     term %in% c("video","text","audio"), paste0("fake ",term),term
    # )) %>%
    mutate(term = fct_relevel(term,"ad","skit","text","audio","video")) %>%
    mutate(color = ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, "grey50", "black"))


## BHq adjustments
alpha <- 0.05 ##original threshold
k <- nrow(dat.b.fx.all) ##number of hypotheses 
r <- order(dat.b.fx.all$p.value) ##ranks of p-values
alpha_adj <- (r*alpha)/k ##step-up 'tailored' thresholds
dat.b.fx.all$color <- ifelse(dat.b.fx.all$p.value < alpha_adj, "black", "grey50") ##step-up hypothesis tests 
dat.b.fx.all$z_crit_adj <- qnorm(1 - (alpha_adj)/2) ##new critical values for CIs (asymptotic)
    
e.epsilon.l <- 0-sd(dat$post_favor_Warren,na.rm=T)/2
e.epsilon.u <- 0+sd(dat$post_favor_Warren,na.rm=T)/2

## equivalence tests
etest1 <- tost(dat$post_favor_Warren[dat$treat == "ad"], 
               dat$post_favor_Warren[dat$treat == "control"], epsilon=e.epsilon.u)
etest2 <- tost(dat$post_favor_Warren[dat$treat == "skit"], 
               dat$post_favor_Warren[dat$treat == "control"], epsilon=e.epsilon.u)
etest3 <- tost(dat$post_favor_Warren[dat$treat == "text"], 
               dat$post_favor_Warren[dat$treat == "control"], epsilon=e.epsilon.u)
etest4 <- tost(dat$post_favor_Warren[dat$treat == "audio"], 
               dat$post_favor_Warren[dat$treat == "control"], epsilon=e.epsilon.u)
etest5 <- tost(dat$post_favor_Warren[dat$treat == "video"], 
               dat$post_favor_Warren[dat$treat == "control"], epsilon=e.epsilon.u)

dat.b.fx.all[1,"tost.lo"] <- etest1$tost.interval[1]
dat.b.fx.all[1,"tost.hi"] <- etest1$tost.interval[2]
dat.b.fx.all[2,"tost.lo"] <- etest2$tost.interval[1]
dat.b.fx.all[2,"tost.hi"] <- etest2$tost.interval[2]
dat.b.fx.all[3,"tost.lo"] <- etest3$tost.interval[1]
dat.b.fx.all[3,"tost.hi"] <- etest3$tost.interval[2]
dat.b.fx.all[4,"tost.lo"] <- etest4$tost.interval[1]
dat.b.fx.all[4,"tost.hi"] <- etest4$tost.interval[2]
dat.b.fx.all[5,"tost.lo"] <- etest5$tost.interval[1]
dat.b.fx.all[5,"tost.hi"] <- etest5$tost.interval[2]

panel1.b.fx.all <- dat.b.fx.all %>%
    ggplot(aes(x=term, y=estimate, fill=color)) +
    geom_hline(yintercept=0.0, lty=1, color="grey", size=1) + 
    geom_hline(yintercept=e.epsilon.u, lty=1, color="green", size=1, alpha=0.5) + 
    geom_hline(yintercept=e.epsilon.l, lty=1, color="green", size=1, alpha=0.5) + 
    geom_linerange(aes(ymin=tost.lo, ymax=tost.hi), color="red", alpha=0.5, size=10, position=position_dodge(width=0.5)) + 
    geom_linerange(aes(ymin=estimate-z_crit_adj*std.error, ymax=estimate+z_crit_adj*std.error, color=color), size=1) +
    geom_linerange(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=color), size=2) +
    geom_label(aes(label=term, y=estimate, color="white"), fontface = "bold", size=4) +
    scale_fill_identity() + scale_color_identity() + 
    ylab("") + 
    xlab("") + 
    coord_flip() + 
    theme_linedraw() + 
    ylim(c(e.epsilon.l,e.epsilon.u)) + 
    facet_grid(panel ~ .) + 
    ggtitle("Affect: Effects") + 
    theme(title = element_text(size=5),
          plot.title = element_text(size=20, hjust=0.5, face="bold"),
          legend.position = "none",
          legend.title = element_text(size=14),
          legend.box="horizontal",
          axis.title.y = element_text(size=16),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=16),
          axis.text.x = element_blank(),
          # plot.margin = unit(c(0,0,6,0), "lines"),
          # axis.ticks.x = element_blank(),
          axis.title.x = element_text(size=14, hjust=0))


####++marginal fx (by group)####
dat.b.bygroups <- bind_rows(
    ## by intervention group
    dat %>% filter(!is.na(treat), treat != "text") %>%
        mutate(exp_1_prompt = ifelse(exp_1_prompt=="info", "Information","No information")) %>%
        group_by(exp_1_prompt) %>%
        mutate(treat = fct_relevel(treat,"control","ad","skit","audio","video")) %>%
        # mutate(believed_true = scale(believed_true)) %>%
        do(tidy(lm(post_favor_Warren ~ treat, data = .))) %>%
        mutate(category = "By\nIntervention") %>%
        filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=exp_1_prompt),
    ## by party
    dat %>% filter(!is.na(PID)) %>% group_by(PID) %>% filter(!is.na(treat), treat != "text") %>%
        # mutate(believed_true = scale(believed_true)) %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat,"control","ad","skit","audio","video")) %>%
        do(tidy(lm(post_favor_Warren ~ treat, data = .))) %>% ungroup() %>%
        mutate(category = "By Partisan\nAffiliation") %>%
        filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=PID),
    ## by age
    dat %>% filter(!is.na(age_65)) %>% group_by(age_65) %>% filter(!is.na(treat), treat != "text") %>%
        # mutate(believed_true = scale(believed_true)) %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat,"control","ad","skit","audio","video")) %>%
        do(tidy(lm(post_favor_Warren ~ treat, data = .))) %>% ungroup() %>%
        mutate(category = "By Age\nGroup") %>%
        filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=age_65),
    ## by knowledge
    dat %>% 
        # mutate(believed_true = scale(believed_true)) %>%
        filter(!is.nan(polknow), !is.na(polknow)) %>%
        group_by(polknow) %>% filter(!is.na(treat), treat != "text") %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat,"control","ad","skit","audio","video")) %>%
        do(tidy(lm(post_favor_Warren ~ treat, data = .))) %>% ungroup() %>%
        mutate(category = "By Political\nKnowledge") %>%
        filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=polknow),
    ## by CRT
    dat %>% filter(!is.na(crt)) %>%
        # mutate(believed_true = scale(believed_true)) %>%
        group_by(crt) %>% filter(!is.na(treat), treat != "text") %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat,"control","ad","skit","audio","video")) %>%
        do(tidy(lm(post_favor_Warren ~ treat, data = .))) %>% ungroup() %>%
        mutate(category = "By Cognitive\nReflection") %>%
        filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=crt),
    ## by sexism
    dat %>%
        # mutate(believed_true = scale(believed_true)) %>%
        filter(!is.nan(ambivalent_sexism), !is.na(ambivalent_sexism)) %>%
        group_by(ambivalent_sexism) %>% filter(!is.na(treat), treat != "text") %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat,"control","ad","skit","audio","video")) %>%
        do(tidy(lm(post_favor_Warren ~ treat, data = .))) %>% ungroup() %>%
        mutate(category = "By Ambivalent\nSexism") %>%
        filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=ambivalent_sexism)
) %>% mutate(color = ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, "grey50", "black")) %>%
    # mutate(term = ifelse(
    #     term %in% c("video","text","audio"), paste0("fake ",term),term
    # )) %>%
    mutate(term = fct_relevel(term,"ad","skit","audio","video"))

## BHq adjustments
alpha <- 0.05 ##original threshold
k <- nrow(dat.b.bygroups) ##number of hypotheses 
r <- order(dat.b.bygroups$p.value) ##ranks of p-values
alpha_adj <- (r*alpha)/k ##step-up 'tailored' thresholds
dat.b.bygroups$color <- ifelse(dat.b.bygroups$p.value < alpha_adj, "black", "grey50") ##step-up hypothesis tests 
dat.b.bygroups$z_crit_adj <- qnorm(1 - (alpha_adj)/2) ##new critical values for CIs (asymptotic)
   
## equivalence tests
i <- 1
### by intervention group
etestX <- tost(dat$post_favor_Warren[dat$treat == "ad" & dat$exp_1_prompt == "info"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$exp_1_prompt == "info"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "skit" & dat$exp_1_prompt == "info"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$exp_1_prompt == "info"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "audio" & dat$exp_1_prompt == "info"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$exp_1_prompt == "info"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "video" & dat$exp_1_prompt == "info"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$exp_1_prompt == "info"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;


etestX <- tost(dat$post_favor_Warren[dat$treat == "ad" & dat$exp_1_prompt != "info"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$exp_1_prompt != "info"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "skit" & dat$exp_1_prompt != "info"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$exp_1_prompt != "info"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "audio" & dat$exp_1_prompt != "info"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$exp_1_prompt != "info"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "video" & dat$exp_1_prompt != "info"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$exp_1_prompt != "info"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;


### by party
etestX <- tost(dat$post_favor_Warren[dat$treat == "ad" & dat$PID == "Democrat"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$PID == "Democrat"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "skit" & dat$PID == "Democrat"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$PID == "Democrat"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "audio" & dat$PID == "Democrat"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$PID == "Democrat"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "video" & dat$PID == "Democrat"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$PID == "Democrat"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;


etestX <- tost(dat$post_favor_Warren[dat$treat == "ad" & dat$PID == "Independent"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$PID == "Independent"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "skit" & dat$PID == "Independent"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$PID == "Independent"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "audio" & dat$PID == "Independent"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$PID == "Independent"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "video" & dat$PID == "Independent"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$PID == "Independent"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;


etestX <- tost(dat$post_favor_Warren[dat$treat == "ad" & dat$PID == "Republican"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$PID == "Republican"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "skit" & dat$PID == "Republican"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$PID == "Republican"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "audio" & dat$PID == "Republican"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$PID == "Republican"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "video" & dat$PID == "Republican"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$PID == "Republican"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;


### by age
etestX <- tost(dat$post_favor_Warren[dat$treat == "ad" & dat$age_65 == "<=65"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$age_65 == "<=65"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "skit" & dat$age_65 == "<=65"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$age_65 == "<=65"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "audio" & dat$age_65 == "<=65"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$age_65 == "<=65"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "video" & dat$age_65 == "<=65"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$age_65 == "<=65"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;


etestX <- tost(dat$post_favor_Warren[dat$treat == "ad" & dat$age_65 == ">65"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$age_65 == ">65"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "skit" & dat$age_65 == ">65"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$age_65 == ">65"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "audio" & dat$age_65 == ">65"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$age_65 == ">65"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "video" & dat$age_65 == ">65"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$age_65 == ">65"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;


### by knowledge
etestX <- tost(dat$post_favor_Warren[dat$treat == "ad" & dat$polknow == "Less knowledge"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$polknow == "Less knowledge"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "skit" & dat$polknow == "Less knowledge"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$polknow == "Less knowledge"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "audio" & dat$polknow == "Less knowledge"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$polknow == "Less knowledge"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "video" & dat$polknow == "Less knowledge"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$polknow == "Less knowledge"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;


etestX <- tost(dat$post_favor_Warren[dat$treat == "ad" & dat$polknow == "More knowledge"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$polknow == "More knowledge"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "skit" & dat$polknow == "More knowledge"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$polknow == "More knowledge"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "audio" & dat$polknow == "More knowledge"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$polknow == "More knowledge"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "video" & dat$polknow == "More knowledge"], 
               dat$post_favor_Warren[dat$treat == "control" & dat$polknow == "More knowledge"], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;


### by CRT
etestX <- tost(dat$post_favor_Warren[dat$treat == "ad" & dat$crt == "Low c.r."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$crt == "Low c.r."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "skit" & dat$crt == "Low c.r."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$crt == "Low c.r."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "audio" & dat$crt == "Low c.r."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$crt == "Low c.r."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "video" & dat$crt == "Low c.r."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$crt == "Low c.r."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;


etestX <- tost(dat$post_favor_Warren[dat$treat == "ad" & dat$crt == "Moderate c.r."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$crt == "Moderate c.r."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "skit" & dat$crt == "Moderate c.r."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$crt == "Moderate c.r."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "audio" & dat$crt == "Moderate c.r."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$crt == "Moderate c.r."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "video" & dat$crt == "Moderate c.r."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$crt == "Moderate c.r."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;


etestX <- tost(dat$post_favor_Warren[dat$treat == "ad" & dat$crt == "High c.r."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$crt == "High c.r."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "skit" & dat$crt == "High c.r."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$crt == "High c.r."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "audio" & dat$crt == "High c.r."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$crt == "High c.r."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "video" & dat$crt == "High c.r."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$crt == "High c.r."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;


### by sexism
etestX <- tost(dat$post_favor_Warren[dat$treat == "ad" & dat$ambivalent_sexism == "Low a.s."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$ambivalent_sexism == "Low a.s."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "skit" & dat$ambivalent_sexism == "Low a.s."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$ambivalent_sexism == "Low a.s."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "audio" & dat$ambivalent_sexism == "Low a.s."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$ambivalent_sexism == "Low a.s."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "video" & dat$ambivalent_sexism == "Low a.s."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$ambivalent_sexism == "Low a.s."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;


etestX <- tost(dat$post_favor_Warren[dat$treat == "ad" & dat$ambivalent_sexism == "Moderate a.s."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$ambivalent_sexism == "Moderate a.s."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "skit" & dat$ambivalent_sexism == "Moderate a.s."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$ambivalent_sexism == "Moderate a.s."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "audio" & dat$ambivalent_sexism == "Moderate a.s."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$ambivalent_sexism == "Moderate a.s."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "video" & dat$ambivalent_sexism == "Moderate a.s."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$ambivalent_sexism == "Moderate a.s."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;


etestX <- tost(dat$post_favor_Warren[dat$treat == "ad" & dat$ambivalent_sexism == "High a.s."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$ambivalent_sexism == "High a.s."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "skit" & dat$ambivalent_sexism == "High a.s."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$ambivalent_sexism == "High a.s."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "audio" & dat$ambivalent_sexism == "High a.s."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$ambivalent_sexism == "High a.s."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;
etestX <- tost(dat$post_favor_Warren[dat$treat == "video" & dat$ambivalent_sexism == "High a.s."], 
               dat$post_favor_Warren[dat$treat == "control" & dat$ambivalent_sexism == "High a.s."], epsilon=e.epsilon.u)
dat.b.bygroups[i,"tost.lo"] <- etestX$tost.interval[1]
dat.b.bygroups[i,"tost.hi"] <- etestX$tost.interval[2]; i <- i + 1;

####++marginal fx viz (by group)####
panel1.b.fx.bygroups <- dat.b.bygroups %>%
    filter(term == "video") %>%
    # filter(category == "By Partisan\nAffiliation") %>%
    arrange(desc(row_number())) %>%
    mutate(group=as_factor(group)) %>%
    mutate(group = fct_relevel(group, "Low a.s.", "Moderate a.s.")) %>%
    mutate(group = fct_relevel(group, "Low c.r.", "Moderate c.r.")) %>%
    ggplot(aes(x=group, y=estimate, fill=color, group=term)) +
    geom_hline(yintercept=0, lty=1, color="grey", size=1) + 
    geom_hline(yintercept=e.epsilon.u, lty=1, color="green", size=1, alpha=0.5) + 
    geom_hline(yintercept=e.epsilon.l, lty=1, color="green", size=1, alpha=0.5) + 
    geom_linerange(aes(ymin=tost.lo, ymax=tost.hi), color="red", alpha=0.5, 
                   size=13, position=position_dodge(width=0.85)) + 
    geom_linerange(aes(ymin=estimate-z_crit_adj*std.error, ymax=estimate+z_crit_adj*std.error, color=color), 
                   size=1, position=position_dodge(width=0.85)) +
    geom_linerange(aes(ymin=estimate-1.65*std.error, ymax=estimate+1.65*std.error, color=color), 
                   size=2, position=position_dodge(width=0.85)) +
    geom_label(aes(label=term, y=estimate, color="white"), fontface = "bold", 
               size=6, position=position_dodge(width=0.85)) +
    facet_grid(category ~ ., space="free", scales="free") +
    scale_x_discrete(expand=c(0.2,0.2)) +
    scale_y_continuous(breaks=c(e.epsilon.l, -10, 0, 10, e.epsilon.u), 
                       labels=c(TeX("-$0.5\\sigma$"), "-10°", "0°", "+10°", TeX("+$0.5\\sigma$")),
                       limits=c(e.epsilon.l,e.epsilon.u)) +
    # scale_x_discrete(labels=c("control\ngroup","information\nrecipients")) +
    ylab("Mean change in affect [1-100]\nrelative to no stimuli") + 
    xlab("") + coord_flip() + theme_linedraw() +
    geom_vline(xintercept=1.5, lty=2, color="darkgray") + 
    geom_vline(xintercept=2.5, lty=2, color="darkgray") + 
    scale_shape_manual(values=c(21,22,23,24,25)) +
    scale_fill_identity() + scale_color_identity() + 
    theme(title = element_text(size=5),
          legend.position = "none",
          legend.title = element_text(size=14),
          legend.box="horizontal",
          axis.title.y = element_text(size=16),
          # axis.ticks.y = element_blank(),
          axis.text.y = element_text(size=14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=14),
          axis.text.x = element_text(size=14),
          # plot.margin = unit(c(0,0,6,0), "lines"),
          # axis.ticks.x = element_blank(),
          axis.title.x = element_text(size=16, hjust=0.5))

####++combined####
pp2 <- cowplot::plot_grid(panel1.b, panel1.b.fx.all, panel1.b.fx.bygroups, 
                         rel_heights = c(2,1.4,8),
                         nrow=3, align="v", axis="lr")
cowplot::save_plot("figures/topline_exp1_b.pdf", plot = pp2, base_height=14, base_width=8)
system("open figures/topline_exp1_b.pdf")

#####------------------------------------------------------#
##### Experiment 1: combined ####
#####------------------------------------------------------#

pp.A <- cowplot::plot_grid(panel1, 
                           panel1.fx.all, 
                           panel1.fx.bygroups, 
                           rel_heights = c(2.5,1.25,8),
                           nrow=3, align="v", axis="lr")

pp.B <- cowplot::plot_grid(panel1.b + theme(plot.margin = unit(c(0,0,3,0), "lines")), 
                           panel1.b.fx.all, 
                           panel1.b.fx.bygroups, 
                           rel_heights = c(2.5,1.25,8),
                           nrow=3, align="v", axis="lr")

pp.A_B <- cowplot::plot_grid(pp.A, pp.B, 
                             ncol=2, align="h", rel_widths = c(1,1))
cowplot::save_plot("figures/topline_exp1.pdf", plot = pp.A_B, base_height=14.8, base_width=12)
system("open figures/topline_exp1.pdf")

#####------------------------------------------------------#
##### Experiment 1: all baselines ####
#####------------------------------------------------------#

dat1.means.all.a <- dat %>% filter(treat != "ad", treat != "control", treat != "skit") %>%
    filter(!is.na(believed_true)) %>% ##remove the few NAs we have, remove skit conditions not assigned to Qs
    group_by(treat) %>% summarise(mean=mean(believed_true, na.rm=T), se=sd(believed_true, na.rm=T)/sqrt(n())) %>% 
    mutate(category="All\n") %>%
    as.data.frame()

dat1.means.bygroup.a <- bind_rows(
    ## by intervention group
    dat %>% 
        filter(treat != "ad", treat != "control", treat != "skit", !is.na(treat)) %>%
        mutate(exp_1_prompt = ifelse(exp_1_prompt=="info", "Information","No information")) %>%
        group_by(exp_1_prompt, treat) %>%
        summarise(mean=mean(believed_true, na.rm=T), se=sd(believed_true, na.rm=T)/sqrt(n())) %>% 
        mutate(category = "By\nIntervention") %>%
        rename(group=exp_1_prompt),
    ## by party
    dat %>% 
        filter(!is.na(PID)) %>% 
        filter(treat != "ad", treat != "control", treat != "skit", !is.na(treat)) %>%
        group_by(PID, treat) %>%
        summarise(mean=mean(believed_true, na.rm=T), se=sd(believed_true, na.rm=T)/sqrt(n())) %>% 
        mutate(category = "By Partisanship") %>%
        rename(group=PID),
    ## by age
    dat %>% 
        filter(!is.na(age_65)) %>% group_by(age_65) %>% 
        filter(treat != "ad", treat != "control", treat != "skit", !is.na(treat)) %>%
        group_by(age_65, treat) %>%
        summarise(mean=mean(believed_true, na.rm=T), se=sd(believed_true, na.rm=T)/sqrt(n())) %>% 
        mutate(category = "By Age\nGroup") %>%
        rename(group=age_65),
    ## by knowledge
    dat %>% 
        filter(!is.nan(polknow), !is.na(polknow)) %>%
        filter(treat != "ad", treat != "control", treat != "skit", !is.na(treat)) %>%
        group_by(polknow, treat) %>%
        summarise(mean=mean(believed_true, na.rm=T), se=sd(believed_true, na.rm=T)/sqrt(n())) %>% 
        mutate(category = "By Political\nKnowledge") %>%
        rename(group=polknow),
    ## by CRT
    dat %>% 
        filter(!is.nan(crt), !is.na(crt)) %>%
        filter(treat != "ad", treat != "control", treat != "skit", !is.na(treat)) %>%
        group_by(crt, treat) %>%
        summarise(mean=mean(believed_true, na.rm=T), se=sd(believed_true, na.rm=T)/sqrt(n())) %>% 
        mutate(category = "By Cognitive\nReflection") %>%
        rename(group=crt),
    ## by sexism
    dat %>%
        filter(!is.nan(ambivalent_sexism), !is.na(ambivalent_sexism)) %>%
        filter(treat != "ad", treat != "control", treat != "skit", !is.na(treat)) %>%
        group_by(ambivalent_sexism, treat) %>%
        summarise(mean=mean(believed_true, na.rm=T), se=sd(believed_true, na.rm=T)/sqrt(n())) %>% 
        mutate(category = "By Ambivalent\nSexism") %>%
        rename(group=ambivalent_sexism)
)

dat1.means.all.b <- dat %>% 
    filter(!is.na(believed_true)) %>% ##remove the few NAs we have, remove skit conditions not assigned to Qs
    group_by(treat) %>% summarise(mean=mean(post_favor_Warren, na.rm=T), se=sd(post_favor_Warren, na.rm=T)/sqrt(n())) %>% 
    mutate(category="All\n") %>%
    as.data.frame()

dat1.means.bygroup.b <- bind_rows(
    ## by intervention group
    dat %>% 
        mutate(exp_1_prompt = ifelse(exp_1_prompt=="info", "Information","No information")) %>%
        group_by(exp_1_prompt, treat) %>%
        summarise(mean=mean(post_favor_Warren, na.rm=T), se=sd(post_favor_Warren, na.rm=T)/sqrt(n())) %>% 
        mutate(category = "By\nIntervention") %>%
        rename(group=exp_1_prompt),
    ## by party
    dat %>% 
        filter(!is.na(PID)) %>% 
        group_by(PID, treat) %>%
        summarise(mean=mean(post_favor_Warren, na.rm=T), se=sd(post_favor_Warren, na.rm=T)/sqrt(n())) %>% 
        mutate(category = "By Partisan\nAffiliation") %>%
        rename(group=PID),
    ## by age
    dat %>% 
        filter(!is.na(age_65)) %>% group_by(age_65) %>% 
        group_by(age_65, treat) %>%
        summarise(mean=mean(post_favor_Warren, na.rm=T), se=sd(post_favor_Warren, na.rm=T)/sqrt(n())) %>% 
        mutate(category = "By Age\nGroup") %>%
        rename(group=age_65),
    ## by knowledge
    dat %>% 
        filter(!is.nan(polknow), !is.na(polknow)) %>%
        group_by(polknow, treat) %>%
        summarise(mean=mean(post_favor_Warren, na.rm=T), se=sd(post_favor_Warren, na.rm=T)/sqrt(n())) %>% 
        mutate(category = "By Political\nKnowledge") %>%
        rename(group=polknow),
    ## by CRT
    dat %>% 
        filter(!is.nan(crt), !is.na(crt)) %>%
        group_by(crt, treat) %>%
        summarise(mean=mean(post_favor_Warren, na.rm=T), se=sd(post_favor_Warren, na.rm=T)/sqrt(n())) %>% 
        mutate(category = "By Cognitive\nReflection") %>%
        rename(group=crt),
    ## by sexism
    dat %>%
        filter(!is.nan(ambivalent_sexism), !is.na(ambivalent_sexism)) %>%
        group_by(ambivalent_sexism, treat) %>%
        summarise(mean=mean(post_favor_Warren, na.rm=T), se=sd(post_favor_Warren, na.rm=T)/sqrt(n())) %>% 
        mutate(category = "By Ambivalent\nSexism") %>%
        rename(group=ambivalent_sexism)
)

baselines1.viz.a <- bind_rows(
  dat1.means.all.a %>% mutate(panel = "Credibility") %>% mutate(group=""),
  dat1.means.bygroup.a %>% mutate(panel = "Credibility")
) %>%
    filter(treat == "video") %>%
    arrange(desc(row_number())) %>%
    mutate(group=as_factor(group)) %>%
    mutate(group = fct_relevel(group, "Low a.s.", "Moderate a.s.")) %>%
    mutate(group = fct_relevel(group, "Low c.r.", "Moderate c.r.")) %>%
    ggplot(aes(x=group, y=mean, group=category)) + 
    geom_bar(aes(fill=mean), stat="identity", color="black") + 
    geom_text(aes(x=group, y=0.5, label=round(mean, 2), group=category), 
              size=3.5, color="white", fontface="bold") + 
    geom_linerange(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), size=1.5) + 
    facet_grid(category ~ panel, space="free_y", scales="free") +
    xlab("") + ylab("Mean credibility [1-5] of fake video") + 
    ylim(c(0, 5)) +
    theme_linedraw() + coord_flip() + 
    theme(title = element_text(size=5),
          legend.position = "none",
          legend.title = element_text(size=14),
          legend.box="horizontal",
          axis.title.y = element_text(size=16),
          # axis.ticks.y = element_blank(),
          axis.text.y = element_text(size=14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=10),
          strip.text.x = element_text(size=16),
          axis.text.x = element_text(size=14),
          # plot.margin = unit(c(0,0,6,0), "lines"),
          # axis.ticks.x = element_blank(),
          axis.title.x = element_text(size=16, hjust=0.5))

baselines1.viz.b <- bind_rows(
  dat1.means.all.b %>% mutate(panel = "Affect") %>% mutate(group=""),
  dat1.means.bygroup.b %>% mutate(panel = "Affect")
) %>%
    filter(treat == "video") %>%
    arrange(desc(row_number())) %>%
    mutate(group=as_factor(group)) %>%
    mutate(group = fct_relevel(group, "Low a.s.", "Moderate a.s.")) %>%
    mutate(group = fct_relevel(group, "Low c.r.", "Moderate c.r.")) %>%
    ggplot(aes(x=group, y=mean, group=category)) + 
    geom_bar(aes(fill=mean), stat="identity", color="black") + 
    geom_text(aes(x=group, y=15, label=round(mean, 2), group=category), 
              size=3.5, color="white", fontface="bold") + 
    geom_linerange(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), size=1.5) + 
    facet_grid(category ~ panel, space="free_y", scales="free") +
    xlab("") + ylab("Mean affect towards Warren [1-100]\nafter fake video") + 
    ylim(c(0, 100)) +
    theme_linedraw() + coord_flip() + 
    theme(title = element_text(size=5),
          legend.position = "none",
          legend.title = element_text(size=14),
          legend.box="horizontal",
          axis.title.y = element_text(size=16),
          # axis.ticks.y = element_blank(),
          axis.text.y = element_text(size=14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=10),
          strip.text.x = element_text(size=16),
          axis.text.x = element_text(size=14),
          # plot.margin = unit(c(0,0,6,0), "lines"),
          # axis.ticks.x = element_blank(),
          axis.title.x = element_text(size=16, hjust=0.5))

baslines1.viz <- cowplot::plot_grid(baselines1.viz.a + theme(strip.text.y = element_blank()), 
                                    baselines1.viz.b + theme(axis.text.y=element_blank()), 
                                    ncol=2, align="h", rel_widths = c(1,1))
cowplot::save_plot("figures/topline_exp1_baselines.pdf", plot = baslines1.viz, base_height=8, base_width=12)
system("open figures/topline_exp1_baselines.pdf")

#####------------------------------------------------------#
##### Experiment 2 ####
#####------------------------------------------------------#

d <- dat %>%
    gather(key="metric", value="val", exp_2_pct_correct, exp_2_pct_false_fake, exp_2_pct_false_real) %>%
    mutate(metric = as.character(metric)) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_correct", "Accuracy")) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_false_fake", "False Positive Rate")) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_false_real", "False Negative Rate")) %>%
    mutate(exp_2 = as.character(exp_2)) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "lofake", "low-fake")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "hifake", "high-fake")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "nofake", "no-fake"))

####++distribution of baseline####
dat2.means <- d %>% filter(!is.na(exp_2)) %>%
    # filter(metric != "False Negative Rate") %>%
    group_by(exp_2, metric) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
    filter(!is.na(std.error)) %>% mutate(type="All", group=exp_2)
    
plot2.means1 <- dat2.means %>%
    mutate(panel = "By Environment")  %>% filter(metric == "Accuracy") %>%
    ggplot(aes(x=exp_2, y=estimate)) + 
    geom_bar(aes(fill=estimate), stat="identity", color="black") + 
    geom_text(aes(x=group, y=0.15, label=paste0(round(estimate*100, 2),"%"), color="white")) + scale_color_identity() +
    geom_linerange(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), size=2) +
    coord_flip() +
    scale_y_continuous(label=scales::percent_format(acc=1)) +
    scale_x_discrete(labels=c("High-fake", "Low-fake", "No-fake")) + 
    # scale_fill_gradient2(high="darkblue", mid="grey", low="lightblue", midpoint=0.5) + 
    facet_grid(panel ~ metric) + 
    theme_linedraw() +
    xlab("") + ylab("Baseline performance") +
    theme(title = element_text(size=5),
          plot.title = element_text(size=20, hjust=0.5, face="bold"),
          legend.position = "none",
          # legend.position = c(.5,-0.25), legend.direction = "horizontal",
          legend.title = element_text(size=14),
          legend.box="horizontal",
          axis.title.y = element_text(size=16, hjust=0.5),
          # axis.ticks.y = element_line(),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=14),
          axis.text.x = element_text(size=12),
          # plot.margin = unit(c(0,0,4.5,0), "lines"),
          axis.ticks.x = element_line(),
          axis.title.x = element_text(size=14, hjust=0.5))

plot2.means2 <- dat2.means %>%
    mutate(panel = "By Environment")  %>% filter(metric == "False Negative Rate") %>%
    bind_rows(data.frame(exp_2="no-fake", metric="False Negative Rate", estimate=NA, std.error=0, group="no-fake", panel="By Environment", type="All")) %>%
    ggplot(aes(x=exp_2, y=estimate)) + 
    geom_bar(aes(fill=estimate), stat="identity", color="black") + 
    geom_text(aes(x=group, y=0.09, label=paste0(round(estimate*100, 2),"%"), color="white")) + scale_color_identity() +
    geom_linerange(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), size=2) +
    coord_flip() +
    scale_y_continuous(label=scales::percent_format(acc=1)) +
    scale_x_discrete(labels=c("High-fake", "Low-fake", "No-fake")) + 
    # scale_fill_gradient2(high="darkblue", mid="grey", low="lightblue", midpoint=0.5) + 
    facet_grid(panel ~ metric) + 
    theme_linedraw() +
    xlab("") + ylab("") +
    theme(title = element_text(size=5),
          plot.title = element_text(size=20, hjust=0.5, face="bold"),
          legend.position = "none",
          # legend.position = c(.5,-0.25), legend.direction = "horizontal",
          legend.title = element_text(size=14),
          legend.box="horizontal",
          axis.title.y = element_text(size=16, hjust=0.5),
          # axis.ticks.y = element_line(),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=14),
          axis.text.x = element_text(size=12),
          # plot.margin = unit(c(0,0,4.5,0), "lines"),
          axis.ticks.x = element_line(),
          axis.title.x = element_text(size=14, hjust=0.5))    
    
plot2.means3 <- dat2.means %>%
    mutate(panel = "By Environment")  %>% filter(metric == "False Positive Rate") %>%
    ggplot(aes(x=exp_2, y=estimate)) + 
    geom_bar(aes(fill=estimate), stat="identity", color="black") + 
    geom_text(aes(x=group, y=0.07, label=paste0(round(estimate*100, 2),"%"), color="white")) + scale_color_identity() +
    geom_linerange(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), size=2) +
    coord_flip() +
    scale_y_continuous(label=scales::percent_format(acc=1)) +
    scale_x_discrete(labels=c("High-fake", "Low-fake", "No-fake")) + 
    # scale_fill_gradient2(high="darkblue", mid="grey", low="lightblue", midpoint=0.5) + 
    facet_grid(panel ~ metric) + 
    theme_linedraw() +
    xlab("") + ylab("") +
    theme(title = element_text(size=5),
          plot.title = element_text(size=20, hjust=0.5, face="bold"),
          legend.position = "none",
          # legend.position = c(.5,-0.25), legend.direction = "horizontal",
          legend.title = element_text(size=14),
          legend.box="horizontal",
          axis.title.y = element_text(size=16, hjust=0.5),
          # axis.ticks.y = element_line(),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=14),
          axis.text.x = element_text(size=12),
          # plot.margin = unit(c(0,0,4.5,0), "lines"),
          axis.ticks.x = element_line(),
          axis.title.x = element_text(size=14, hjust=0.5))

####++distribution of baseline (by groups)####
dd <- bind_rows(
    # ##intervention
    # d %>% filter(!is.na(exp_2)) %>%
    #     filter(metric != "False Negative Rate") %>%
    #     mutate(exp_2=factor(exp_2, levels=c("no-fake","low-fake","high-fake"))) %>%
    #     group_by(metric, exp_1_prompt) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
    #     mutate(exp_1_prompt = as.character(exp_1_prompt)) %>%
    #     mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "control", "No information")) %>%
    #     mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "info", "Information")) %>%
    #     rename(group=exp_1_prompt) %>% mutate(panel="By Intervention\nSubgroups")
    # ,
    d %>% filter(!is.na(exp_2)) %>%
        # filter(metric != "False Negative Rate") %>%
        mutate(exp_2=factor(exp_2, levels=c("no-fake","low-fake","high-fake"))) %>%
        group_by(metric, exp_2_after_debrief) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
        mutate(exp_2_after_debrief = as.character(exp_2_after_debrief)) %>%
        mutate(exp_2_after_debrief = replace(exp_2_after_debrief, exp_2_after_debrief == "0", "Debriefed before")) %>%
        mutate(exp_2_after_debrief = replace(exp_2_after_debrief, exp_2_after_debrief == "1", "Debriefed after")) %>%
        rename(group=exp_2_after_debrief) %>% mutate(panel="By Intervention\nSubgroups")
    ,
    d %>% filter(!is.na(exp_2)) %>%
        # filter(metric != "False Negative Rate") %>%
        mutate(exp_2=factor(exp_2, levels=c("no-fake","low-fake","high-fake"))) %>%
        group_by(metric, exp_2_prompt) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
        mutate(exp_2_prompt = as.character(exp_2_prompt)) %>%
        mutate(exp_2_prompt = replace(exp_2_prompt, exp_2_prompt == "control", "No accuracy prime")) %>%
        mutate(exp_2_prompt = replace(exp_2_prompt, exp_2_prompt == "accuracy", "Accuracy prime")) %>%
        rename(group=exp_2_prompt) %>% mutate(panel="By Intervention\nSubgroups")
    ,
    ## knowledge
    d %>% filter(!is.na(polknow)) %>%
        # filter(metric != "False Negative Rate") %>%
        mutate(exp_2=factor(exp_2, levels=c("no-fake","low-fake","high-fake"))) %>%
        group_by(metric, polknow) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
        rename(group=polknow) %>% mutate(panel="By Political\nKnowledge")
    ,
    ## dig-lit
    d %>% filter(!is.na(post_dig_lit)) %>%
        # filter(metric != "False Negative Rate") %>%
        mutate(exp_2=factor(exp_2, levels=c("no-fake","low-fake","high-fake"))) %>%
        group_by(metric, post_dig_lit) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
        rename(group=post_dig_lit) %>% mutate(panel="By Digital\nLiteracy")
    ,
    ## CR
    d %>% filter(!is.na(crt)) %>%
        # filter(metric != "False Negative Rate") %>%
        mutate(exp_2=factor(exp_2, levels=c("no-fake","low-fake","high-fake"))) %>%
        group_by(metric, crt) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
        rename(group=crt) %>% mutate(panel="By Cognitive\nReflection")
    ,
    ## PID
    d %>% filter(!is.na(PID)) %>%
        # filter(metric != "False Negative Rate") %>%
        mutate(exp_2=factor(exp_2, levels=c("no-fake","low-fake","high-fake"))) %>%
        group_by(metric, PID) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
        rename(group=PID) %>% mutate(panel="By Partisan\nAffiliation")
) %>% mutate(color = ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, "grey50", "black"))

dat2.means.bygroups <- dd %>% 
    as.data.frame() %>%
    arrange(desc(row_number())) %>%
    mutate(group=as_factor(group)) %>%
    as.data.frame() %>%
    arrange(desc(row_number())) %>%
    mutate(group=as_factor(group)) 
  
plot2.means.bygroups1 <- dat2.means.bygroups %>% filter(metric == "Accuracy") %>%
    ggplot(aes(x=group, y=estimate)) + 
    geom_bar(aes(fill=estimate), stat="identity", color="black") + 
    geom_linerange(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), size=2) +
    coord_flip() +
    geom_vline(aes(xintercept=1.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=2.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=3.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=4.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=5.5), lty=2, color="grey", alpha=0.8) +
    # geom_vline(data = dd %>% filter(panel=="By Intervention\nSubgroups"), aes(xintercept=4.5), lty=1, size=1, color="black", alpha=1) +
    geom_vline(data = dd %>% filter(metric == "Accuracy") %>% filter(panel=="By Intervention\nSubgroups"), aes(xintercept=2.5), lty=1, size=1, color="black", alpha=1) +
    geom_text(aes(x=group, y=0.07, label=paste0(round(estimate*100, 2),"%"), color="white")) + scale_color_identity() +
    scale_y_continuous(label=scales::percent_format(acc=1)) +
    # scale_fill_gradient2(high="darkblue", mid="blue", low="lightblue", midpoint=mean(dat2.means.bygroups$estimate[dat2.means.bygroups$metric == "Accuracy"])) + 
    facet_grid(panel ~ metric, scales="free", space="free_y") + 
    theme_linedraw() +
    xlab("") + ylab("")
    
plot2.means.bygroups2 <- dat2.means.bygroups %>% filter(metric == "False Negative Rate") %>% 
    ggplot(aes(x=group, y=estimate)) + 
    geom_bar(aes(fill=estimate), stat="identity", color="black") + 
    geom_linerange(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), size=2) +
    coord_flip() +
    geom_vline(aes(xintercept=1.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=2.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=3.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=4.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=5.5), lty=2, color="grey", alpha=0.8) +
    # geom_vline(data = dd %>% filter(panel=="By Intervention\nSubgroups"), aes(xintercept=4.5), lty=1, size=1, color="black", alpha=1) +
    geom_vline(data = dd %>% filter(metric == "False Negative Rate") %>% filter(panel=="By Intervention\nSubgroups"), aes(xintercept=2.5), lty=1, size=1, color="black", alpha=1) +
    geom_text(aes(x=group, y=0.09, label=paste0(round(estimate*100, 2),"%"), color="white")) + scale_color_identity() +
    scale_y_continuous(label=scales::percent_format(acc=1)) +
    # scale_fill_gradient2(high="darkblue", mid="blue", low="lightblue", midpoint=mean(dat2.means.bygroups$estimate[dat2.means.bygroups$metric == "False Negative Rate"])) + 
    facet_grid(panel ~ metric, scales="free", space="free_y") + 
    theme_linedraw() +
    xlab("") + ylab("")

plot2.means.bygroups3 <- dat2.means.bygroups %>% filter(metric == "False Positive Rate") %>%
    ggplot(aes(x=group, y=estimate)) + 
    geom_bar(aes(fill=estimate), stat="identity", color="black") + 
    geom_linerange(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), size=2) +
    coord_flip() +
    geom_vline(aes(xintercept=1.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=2.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=3.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=4.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=5.5), lty=2, color="grey", alpha=0.8) +
    # geom_vline(data = dd %>% filter(panel=="By Intervention\nSubgroups"), aes(xintercept=4.5), lty=1, size=1, color="black", alpha=1) +
    geom_vline(data = dd %>% filter(metric == "False Positive Rate") %>% filter(panel=="By Intervention\nSubgroups"), aes(xintercept=2.5), lty=1, size=1, color="black", alpha=1) +
    geom_text(aes(x=group, y=0.09, label=paste0(round(estimate*100, 2),"%"), color="white")) + scale_color_identity() +
    scale_y_continuous(label=scales::percent_format(acc=1), limits = c(0, 0.3)) + 
    # scale_fill_gradient2(high="darkblue", mid="blue", low="lightblue", midpoint=mean(dat2.means.bygroups$estimate[dat2.means.bygroups$metric == "False Positive Rate"])) + 
    facet_grid(panel ~ metric, scales="free", space="free_y") + 
    theme_linedraw() +
    xlab("") + ylab("")

legend.means.bygroups <- theme(title = element_text(size=5),
                               plot.title = element_text(size=20, hjust=0.5, face="bold"),
                               legend.position = "none",
                               legend.box="horizontal",
                               axis.title.y = element_text(size=16, hjust=0.5),
                               # axis.ticks.y = element_line(),
                               axis.text.y = element_text(size=12),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               strip.text = element_text(size=14),
                               axis.text.x = element_text(size=12),
                               # plot.margin = unit(c(0,0,4.5,0), "lines"),
                               axis.ticks.x = element_line(),
                               axis.title.x = element_text(size=14, hjust=0.5))
    
    
    
####++combined####
library(gridExtra)
pp2 <- grid.arrange(grobs=list(plot2.means1 + theme(axis.text.x = element_blank(),
                                                    strip.text.y = element_blank(), 
                                                    plot.margin = margin(l=1.9, r=0.1, t=0.2, b=0.2, unit="cm"),
                                                    strip.text.x = element_text(size=12)) + ylab(""), 
                               plot2.means2 + theme(axis.text.x = element_blank(), 
                                                    axis.text.y = element_blank(),
                                                    strip.text.y = element_blank(),
                                                    # plot.margin = margin(b=-4, l=-80, r=-80, unit="cm"),
                                                    strip.text.x = element_text(size=12)) + ylab(""),
                               plot2.means3 + theme(axis.text.x = element_blank(), 
                                                    axis.text.y = element_blank(),
                                                    # strip.text.y = element_blank(), 
                                                    strip.text.x = element_text(size=12)) + ylab(""),
                               plot2.means.bygroups1 + legend.means.bygroups + theme(strip.text.x = element_blank(), 
                                                                                     strip.text.y = element_blank()), 
                               plot2.means.bygroups2 + legend.means.bygroups + theme(strip.text.x = element_blank(),
                                                                                     axis.text.y = element_blank(),
                                                                                     strip.text.y = element_blank()),
                               plot2.means.bygroups3 + legend.means.bygroups + theme(strip.text.x = element_blank(),
                                                                                     axis.text.y = element_blank(),
                                                                                     strip.text.y = element_text(size=10))),
                    ncol=3, nrow=2, heights=c(1,3.25), widths=c(1.65,1,1))
ggsave("figures/topline_exp2.pdf", plot = pp2, height=10, width=8)
system("open figures/topline_exp2.pdf")


#####------------------------------------------------------#
##### Experiment 2: marginal fx of traits ####
#####------------------------------------------------------#

rm(dat)
load("code/deepfake.RData")

####++weighted adjusted models####
(m1 <- lm(exp_2_pct_correct ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                              exp_2 + response_wave_ID + age_65 + educ + PID + internet_usage + ambivalent_sexism, dat, weights=weight)); summary(m1);
(m2 <- lm(exp_2_pct_false_fake ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                                 exp_2 + response_wave_ID + age_65 + educ + PID + internet_usage + ambivalent_sexism, dat, weights=weight)); summary(m2);
(m3 <- lm(exp_2_pct_false_real ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                                 exp_2 + response_wave_ID + age_65 + educ + PID + internet_usage + ambivalent_sexism, dat, weights=weight)); summary(m3);

dat.viz1 <- bind_rows(tidy(m1) %>% mutate(metric="Accuracy"), 
                      tidy(m2) %>% mutate(metric="False Positive Rate"),
                      tidy(m3) %>% mutate(metric="False Negative Rate"))

####++weighted univariate####
dat.viz <- bind_rows(
tidy(lm(exp_2_pct_correct ~ post_dig_lit, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="Accuracy"),
tidy(lm(exp_2_pct_correct ~ exp_2_prompt_accuracy, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="Accuracy"),
tidy(lm(exp_2_pct_correct ~ exp_1_prompt_info, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="Accuracy"),
tidy(lm(exp_2_pct_correct ~ exp_2_after_debrief, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="Accuracy"),
tidy(lm(exp_2_pct_correct ~ polknow, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="Accuracy"),
tidy(lm(exp_2_pct_correct ~ post_dig_lit, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="Accuracy"),
tidy(lm(exp_2_pct_correct ~ crt, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="Accuracy"),
tidy(lm(exp_2_pct_correct ~ exp_2, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="Accuracy"),
tidy(lm(exp_2_pct_correct ~ age_65, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="Accuracy"),
tidy(lm(exp_2_pct_correct ~ educ, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="Accuracy"),
tidy(lm(exp_2_pct_correct ~ PID, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="Accuracy"),
tidy(lm(exp_2_pct_correct ~ ambivalent_sexism, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="Accuracy"),
tidy(lm(exp_2_pct_false_fake ~ post_dig_lit, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Positive Rate"),
tidy(lm(exp_2_pct_false_fake ~ exp_2_prompt_accuracy, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Positive Rate"),
tidy(lm(exp_2_pct_false_fake ~ exp_1_prompt_info, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Positive Rate"),
tidy(lm(exp_2_pct_false_fake ~ exp_2_after_debrief, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Positive Rate"),
tidy(lm(exp_2_pct_false_fake ~ polknow, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Positive Rate"),
tidy(lm(exp_2_pct_false_fake ~ post_dig_lit, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Positive Rate"),
tidy(lm(exp_2_pct_false_fake ~ crt, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Positive Rate"),
tidy(lm(exp_2_pct_false_fake ~ exp_2, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Positive Rate"),
tidy(lm(exp_2_pct_false_fake ~ age_65, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Positive Rate"),
tidy(lm(exp_2_pct_false_fake ~ educ, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Positive Rate"),
tidy(lm(exp_2_pct_false_fake ~ PID, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Positive Rate"),
tidy(lm(exp_2_pct_false_fake ~ ambivalent_sexism, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Positive Rate"),
tidy(lm(exp_2_pct_false_real ~ post_dig_lit, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Negative Rate"),
tidy(lm(exp_2_pct_false_real ~ exp_2_prompt_accuracy, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Negative Rate"),
tidy(lm(exp_2_pct_false_real ~ exp_1_prompt_info, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Negative Rate"),
tidy(lm(exp_2_pct_false_real ~ exp_2_after_debrief, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Negative Rate"),
tidy(lm(exp_2_pct_false_real ~ polknow, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Negative Rate"),
tidy(lm(exp_2_pct_false_real ~ post_dig_lit, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Negative Rate"),
tidy(lm(exp_2_pct_false_real ~ crt, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Negative Rate"),
tidy(lm(exp_2_pct_false_real ~ exp_2, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Negative Rate"),
tidy(lm(exp_2_pct_false_real ~ age_65, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Negative Rate"),
tidy(lm(exp_2_pct_false_real ~ educ, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Negative Rate"),
tidy(lm(exp_2_pct_false_real ~ PID, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Negative Rate"),
tidy(lm(exp_2_pct_false_real ~ ambivalent_sexism, dat, weights=weight)) %>% mutate(model="weighted univariate", metric="False Negative Rate")
) %>% 
    bind_rows(dat.viz1 %>% mutate(model="weighted multivariate")) %>%
    filter(!grepl("Intercept", term))

terms2viz <- c("post_dig_lit", "exp_2_prompt_accuracyTRUE", #"exp_1_prompt_infoTRUE", 
               "exp_2_after_debrief", "polknow", "crt", "PIDRepublican", "exp_2lofake", "exp_2nofake")

## BHq adjustments
alpha <- 0.05 ##original threshold
k <- nrow(dat.viz) ##number of hypotheses 
r <- order(dat.viz$p.value) ##ranks of p-values
alpha_adj <- (r*alpha)/k ##step-up 'tailored' thresholds
dat.viz$color <- ifelse(dat.viz$p.value < alpha_adj, "black", "grey50") ##step-up hypothesis tests 
dat.viz$z_crit_adj <- qnorm(1 - (alpha_adj)/2) ##new critical values for CIs (asymptotic)


dat.viz %>%
    filter(term %in% terms2viz) %>%
    mutate(term = replace(term, term == "post_dig_lit", "Digital literacy")) %>%
    mutate(term = replace(term, term == "exp_2lofake", "Low-fake feed")) %>%
    mutate(term = replace(term, term == "exp_2nofake", "No-fake feed")) %>%
    mutate(term = replace(term, term == "exp_2_after_debrief", "Debriefed before task")) %>%
    mutate(term = replace(term, term == "exp_2_prompt_accuracyTRUE", "Accuracy prime")) %>%
    # mutate(term = replace(term, term == "exp_1_prompt_infoTRUE", "Information")) %>%
    mutate(term = replace(term, term == "polknow", "Political knowledge")) %>%
    mutate(term = replace(term, term == "PIDRepublican", "Republican")) %>%
    mutate(term = replace(term, term == "crt", "Cognitive reflection")) %>%
    arrange(desc(model), metric, estimate) %>% 
    as.data.frame() %>%
    mutate(term = as_factor(term)) %>%
    ggplot(aes(x=term, y=estimate, group=model, color=color)) + 
    ## null benchmark: 0
    geom_hline(yintercept=0, lty=1, size=1.5, color="gray") +
    ## null benchmark: acc, +0.5sd
    geom_hline(data=dat.viz %>% filter(metric == "Accuracy"), 
               aes(yintercept=sd(dat$exp_2_pct_correct, na.rm=T)*0.5), color="green", alpha=0.5) +
    geom_text(data=dat.viz %>% filter(metric == "Accuracy") %>% head(1), 
              aes(y=sd(dat$exp_2_pct_correct, na.rm=T)*0.5, x=-Inf, label=TeX("+$.5\\sigma$", output="character")), 
              parse = TRUE, hjust=-0.1, vjust=-0.2, color="darkgreen", alpha=0.5, angle=90) +
    ## null benchmark: fpr, +0.5sd
    geom_hline(data=dat.viz %>% filter(metric == "False Positive Rate"), 
               aes(yintercept=sd(dat$exp_2_pct_false_fake, na.rm=T)*0.5), color="green", alpha=0.5) +
    geom_text(data=dat.viz %>% filter(metric == "False Positive Rate") %>% head(1), 
              aes(y=sd(dat$exp_2_pct_false_fake, na.rm=T)*0.5, x=-Inf, label=TeX("+$.5\\sigma$", output="character")), 
              parse = TRUE, hjust=-0.1, vjust=-0.2, color="darkgreen", alpha=0.5, angle=90) +
    ## null benchmark: fpr, -0.5sd
    geom_hline(data=dat.viz %>% filter(metric == "False Positive Rate"), 
               aes(yintercept=-sd(dat$exp_2_pct_false_fake, na.rm=T)*0.5), color="green", alpha=0.5) +
    geom_text(data=dat.viz %>% filter(metric == "False Positive Rate") %>% head(1), 
              aes(y=-sd(dat$exp_2_pct_false_fake, na.rm=T)*0.5, x=-Inf, label=TeX("-$.5\\sigma$", output="character")), 
              parse = TRUE, hjust=-0.1, vjust=-0.2, color="darkgreen", alpha=0.5, angle=90) +
    ## null benchmark: fnr, -0.5sd
    geom_hline(data=dat.viz %>% filter(metric == "False Negative Rate"), 
               aes(yintercept=-sd(dat$exp_2_pct_false_real, na.rm=T)*0.5), color="green", alpha=0.5) +
    geom_text(data=dat.viz %>% filter(metric == "False Negative Rate") %>% head(1), 
              aes(y=-sd(dat$exp_2_pct_false_real, na.rm=T)*0.5, x=-Inf, label=TeX("-$.5\\sigma$", output="character")), 
              parse = TRUE, hjust=-0.1, vjust=-0.2, color="darkgreen", alpha=0.5, angle=90) +
    ## coefficients
    geom_linerange(aes(ymin=estimate-z_crit_adj*std.error, ymax=estimate+z_crit_adj*std.error, color=color),
                    position=position_dodge(width=0.5), lwd=0.75) + 
    geom_linerange(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=color),
                    position=position_dodge(width=0.5), lwd=1.25) + 
    geom_point(aes(shape=model), position=position_dodge(width=0.5), size=2, fill="white") +
    xlab("") + ylab("Predicted marginal effect on performance (%)") +
    facet_wrap(~ metric, scales = "free_x") +
    ## variable separators
    geom_vline(xintercept=2.5, lty=2, alpha=0.5) +
    geom_vline(xintercept=4.5, lty=2, alpha=0.5) +
    geom_vline(xintercept=8.5, lty=2, alpha=0.5) +
    scale_y_continuous(labels = function(x) ifelse(x > 0, paste0("+",round(x*100, 1),"%"),
                                                   ifelse(x < 0, paste0(round(x*100, 1),"%"), x))) + 
    scale_color_identity() + 
    scale_shape_manual(values=c(22,24), name="model:") + 
    coord_flip() +
    theme_linedraw() +
        theme(
            axis.text.x = element_text(size=11),
            axis.text.y = element_text(size=14),
            strip.text = element_text(size=18),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.spacing = unit(2, "lines"),
            legend.position = "bottom",
            legend.text = element_text(size=12),
            legend.title = element_text(size=14),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("figures/secondstage_treatfx.pdf", width=11, height=4)
system("open figures/secondstage_treatfx.pdf")

#####------------------------------------------------------#
##### Experiment 2: performance by clip (exploratory) ####
#####------------------------------------------------------#

dfsurvdat$PID_main <- as.character(dfsurvdat$PID_main)
dfsurvdat$PID_leaners <- as.character(dfsurvdat$PID_leaners)
dfsurvdat$PID[dfsurvdat$PID_main=="Democrat"|dfsurvdat$PID_leaners=="Democrat"] <- "Democrat"
dfsurvdat$PID[dfsurvdat$PID_main=="Republican"|dfsurvdat$PID_leaners=="Republican"] <- "Republican"
dfsurvdat$PID[dfsurvdat$PID_main=="Independent"&!(dfsurvdat$PID_leaners %in% c("Democrat","Republican"))] <- "Independent"
dfsurvdat$PID[is.na(dfsurvdat$PID)] <- "N/A"
dfsurvdat$PID <- factor(dfsurvdat$PID, levels=c("N/A","Democrat","Independent","Republican"))

scores_byPID <- dfsurvdat[,c("PID", nofake_vids, lowfake_vids, hifake_vids)] %>%
    gather(key="video", value="response", -PID) %>%
    filter(!is.na(as.character(response))) %>%
    mutate(is_real=grepl("real", video)) %>%
    mutate(video=gsub("(_|\\.)\\d$", "", video)) %>%
    mutate(video=ifelse(video=="real_bidenfight", "real_biden_fight", video)) %>%
    mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
                          ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
    mutate(correct=replace_na(correct, 0)) %>%
    group_by(PID, video) %>%
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
                               video == "fake_trump_aids" ~ "Donald Trump\n(fake AIDS cure announcement)",
                               video == "fake_trump_resign" ~ "Donald Trump\n(fake resignation announcement)")) %>%
    as.data.frame()


diffs_byPID <- dfsurvdat[,c("PID", nofake_vids, lowfake_vids, hifake_vids)] %>%
    gather(key="video", value="response", -PID) %>%
    filter(!is.na(as.character(response))) %>%
    mutate(is_real=grepl("real", video)) %>%
    mutate(video=gsub("(_|\\.)\\d$", "", video)) %>%
    mutate(video=ifelse(video=="real_bidenfight", "real_biden_fight", video)) %>%
    mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
                          ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
    mutate(correct=replace_na(correct, 0)) %>%
    group_by(is_real, video) %>% summarise(n.d=sum(PID == "Democrat"),
                                  x.d=sum(PID == "Democrat" & correct),
                                  n.r=sum(PID == "Republican"),
                                  x.r=sum(PID == "Republican" & correct)) %>%
    group_by(is_real, video) %>%
    do(as.data.frame(z.prop.test(.$x.d, .$x.r, .$n.d, .$n.r)))
    
## BHq
alpha <- 0.05 ##original threshold
k <- nrow(diffs_byPID) ##number of hypotheses 
r <- order(diffs_byPID$p.value) ##ranks of p-values
alpha_adj <- (r*alpha)/k ##step-up 'tailored' thresholds
diffs_byPID$color <- ifelse(diffs_byPID$p.value < alpha_adj, "black", "grey50") ##step-up hypothesis tests 
diffs_byPID$z_crit_adj <- qnorm(1 - (alpha_adj)/2) ##new critical values for CIs (asymptotic)
diffs_byPID$p.range <- cut(diffs_byPID$p.value, breaks=c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf), labels=c("***","**","*",".",""))
diffs_byPID$color <- ifelse(diffs_byPID$diff < 0, "red", "blue")
diffs_byPID$p.font <- ifelse(diffs_byPID$p.range %in% c("***","**"), "bold", "plain")

d_scores_fake2 <- scores_byPID %>% mutate(is_fake=grepl("fake_", video)) %>% filter(is_fake) %>% 
    mutate(is_fake="Deepfake Videos") %>%
    arrange(desc(PID), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    mutate(PID=case_when(PID == "Democrat" ~ "D",
                         PID == "Republican" ~ "R",
                         PID == "Independent" ~ "I")) %>% as.data.frame() %>%
    group_by(video_lbl) %>% mutate(ymax=max(pct_correct)) %>% ungroup()

p_scores_fake2 <- d_scores_fake2 %>%
  ggplot(aes(x=video_lbl, y=pct_correct, fill=PID, label=PID)) + 
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
  geom_text(aes(x=video_lbl, y=0.07, label=paste0(round(pct_correct*100, 0),"%")), 
            size=3, color="white", fontface="bold", position=position_dodge(width=0.8)) + 
  ## comparisons -- brackets
  geom_segment(data = d_scores_fake2 %>% filter(PID == "I"), ##horizontal
               aes(x=as.numeric(video_lbl)-0.25, xend=as.numeric(video_lbl)+0.25, 
                   y=ymax+0.05, yend=ymax+0.05)) + 
  geom_segment(data = d_scores_fake2 %>% filter(PID == "D"), ##left tip
               aes(x=as.numeric(video_lbl)-0.25, xend=as.numeric(video_lbl)-0.25, 
                   y=ymax+0.05, yend=pct_correct)) + 
  geom_segment(data = d_scores_fake2 %>% filter(PID == "R"), ##right tip
               aes(x=as.numeric(video_lbl)+0.25, xend=as.numeric(video_lbl)+0.25, 
                   y=ymax+0.05, yend=pct_correct)) + 
  ## comparisons -- p-values
  geom_text(data = diffs_byPID %>% 
              filter(!is_real) %>% 
              mutate(PID = "I") %>% 
              left_join(d_scores_fake2), 
            aes(label=paste0(round(-diff*100, 1),"%",p.range), 
                color=color, y=ymax+0.05, x=video_lbl,fontface=p.font), hjust=-0.1) + 
  scale_color_identity() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks=c(0, 0.5, 1),
                     limits=c(0, 1.2)) + 
  scale_x_discrete(expand=c(-0.1, 0)) +
  scale_fill_manual(values=c("blue","grey","red")) +
  coord_flip() +
  facet_grid(is_fake ~ .) +
  xlab("") + ylab("") +
  theme_linedraw() + 
  theme(title = element_text(size=5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        strip.text = element_text(size=16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

d_scores_real2 <- scores_byPID %>% mutate(is_real=grepl("real_", video)) %>% filter(is_real) %>% select(-is_real) %>%
  mutate(is_fake="Authentic Videos") %>%
  mutate(PID=case_when(PID == "Democrat" ~ "D",
                       PID == "Republican" ~ "R",
                       PID == "Independent" ~ "I")) %>%
  group_by(video_lbl) %>% mutate(ymax=max(pct_correct)) %>% ungroup() %>% 
  arrange(desc(PID), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl))

p_scores_real2 <- d_scores_real2 %>%
  ggplot(aes(x=video_lbl, y=pct_correct, fill=PID, label=PID)) + 
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
  geom_text(aes(x=video_lbl, y=0.08, label=paste0(round(pct_correct*100, 0),"%")), size=3, color="white", fontface="bold", position=position_dodge(width=0.8)) + 
  ## comparisons -- brackets
  geom_segment(data = d_scores_real2 %>% filter(PID == "I"), ##horizontal
               aes(x=as.numeric(video_lbl)-0.25, xend=as.numeric(video_lbl)+0.25, 
                   y=ymax+0.05, yend=ymax+0.05)) + 
  geom_segment(data = d_scores_real2 %>% filter(PID == "D"), ##left tip
               aes(x=as.numeric(video_lbl)-0.25, xend=as.numeric(video_lbl)-0.25, 
                   y=ymax+0.05, yend=pct_correct)) + 
  geom_segment(data = d_scores_real2 %>% filter(PID == "R"), ##right tip
               aes(x=as.numeric(video_lbl)+0.25, xend=as.numeric(video_lbl)+0.25, 
                   y=ymax+0.05, yend=pct_correct)) + 
  ## comparisons -- p-values
  geom_text(data = diffs_byPID %>% 
              filter(is_real) %>% 
              mutate(PID = "I") %>% 
              left_join(d_scores_real2), 
            aes(label=paste0(round(-diff*100, 1),"%",p.range), 
                color=color, y=ymax+0.05, x=video_lbl, fontface=p.font), hjust=-0.1, size=3) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks=c(0, 0.5, 1),
                     limits=c(0, 1.2)) + 
  scale_x_discrete(expand=c(-0.1, 0)) +
  scale_fill_manual(values=c("blue","grey","red"), name="PID:") +
  scale_color_identity() + 
  coord_flip() +
  facet_grid(is_fake ~ .) +
  xlab("") + ylab("% of correct detections") +
  theme_linedraw() + 
  theme(title = element_text(size=5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        strip.text = element_text(size=16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

pp2 <- cowplot::plot_grid(p_scores_fake2, p_scores_real2, nrow=2, rel_heights = c(1,1.5), align="v")
pp2

cowplot::save_plot("figures/secondstage_byclip_byPID.pdf", plot=pp2, base_width=6.5, base_height=7)
system("open figures/secondstage_byclip_byPID.pdf")

## error by congenial vs. not 
mean(dat$exp_2_pct_correct[dat$PID == "Democrat"], na.rm=T) ##0.5418522 (~4 clips)
mean(dat$exp_2_pct_correct.congenial[dat$PID == "Democrat"], na.rm=T) ##0.7491396 (~6 clips)

mean(dat$exp_2_pct_correct[dat$PID == "Republican"], na.rm=T) ##0.604164 (~5 clips)
mean(dat$exp_2_pct_correct.congenial[dat$PID == "Republican"], na.rm=T) ##0.686099 (~6 clips)

## P(fake|real) by congenial vs. not 
mean(dat$exp_2_pct_false_fake[dat$PID == "Democrat"], na.rm=T) ##0.2431774 (~2 clips)
mean(dat$exp_2_pct_false_fake.congenial[dat$PID == "Democrat"], na.rm=T) ##0.1372881 (~1 clips)

mean(dat$exp_2_pct_false_fake[dat$PID == "Republican"], na.rm=T) ##0.1839299 (~1 clips)
mean(dat$exp_2_pct_false_fake.congenial[dat$PID == "Republican"], na.rm=T) ##0.1786401 (~1 clips)

## P(real|fake) by congenial vs. not 
mean(dat$exp_2_pct_false_real[dat$PID == "Democrat"], na.rm=T) ##0.3470769 (~2 clips)
mean(dat$exp_2_pct_false_real.congenial[dat$PID == "Democrat"], na.rm=T) ##0.06982249 (~1 clips)

mean(dat$exp_2_pct_false_real[dat$PID == "Republican"], na.rm=T) ##0.3307591 (~1 clips)
mean(dat$exp_2_pct_false_real.congenial[dat$PID == "Republican"], na.rm=T) ##0.1036988 (~1 clips)





