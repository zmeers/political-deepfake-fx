# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Conduct sensitivity tests for some pre-registered analyses.
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

library(tidyverse)
library(ggplot2)
library(broom)

rm(list=ls())

theme_linedraw2 <- theme_linedraw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(2, "lines"))

# system("mv deepfake.RData deepfake.bak.RData")

#####------------------------------------------------------#
##### Different thresholds for stage-2 missing responses ####
#####------------------------------------------------------#

dat.viz <- data.frame()

for (i in 1:8) {
    cat(i)
    system("rm deepfake.RData")
    system(sprintf("Rscript 00-deepfake_make_data.R --idtask_NA_threshold %d", i))
    load("deepfake.RData")
    
    dat <- dat[as.numeric(dat$quality) == 2,] 
    
    ## === Basic linear models ===
    (m1 <- lm(exp_2_pct_correct ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                                  exp_2 + response_wave_ID + agegroup + Ethnicity + educ + PID + internet_usage + ambivalent_sexism, dat)); summary(m1);
    (m2 <- lm(exp_2_pct_false_fake ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                                     exp_2 + response_wave_ID + agegroup + Ethnicity + educ + PID + internet_usage + ambivalent_sexism, dat)); summary(m2);
    (m3 <- lm(exp_2_pct_false_real ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                                     exp_2 + response_wave_ID + agegroup + Ethnicity + educ + PID + internet_usage + ambivalent_sexism, dat)); summary(m3);
    
    dat.viz.i <- bind_rows(tidy(m1) %>% mutate(metric="Accuracy"), 
                           tidy(m2) %>% mutate(metric="False Positive Rate"),
                           tidy(m3) %>% mutate(metric="False Negative Rate"))
    dat.viz.i$threshold <- i
    dat.viz <- bind_rows(dat.viz, dat.viz.i)
}

dat.viz %>% 
    filter(term %in% c("post_dig_lit", "exp_2_prompt_accuracyTRUE", "exp_1_prompt_infoTRUE", "exp_2_after_debrief", "polknow", "crt")) %>%
    mutate(term = replace(term, term == "post_dig_lit", "Digital literacy")) %>%
    mutate(term = replace(term, term == "exp_2_after_debrief", "Debriefed before task")) %>%
    mutate(term = replace(term, term == "exp_2_prompt_accuracyTRUE", "Accuracy prime")) %>%
    mutate(term = replace(term, term == "exp_1_prompt_infoTRUE", "Information")) %>%
    mutate(term = replace(term, term == "polknow", "Political knowledge")) %>%
    mutate(term = replace(term, term == "PIDRepublican", "Republican")) %>%
    mutate(term = replace(term, term == "crt", "Cognitive reflection")) %>%
    group_by(term, metric) %>% mutate(overall_avg=mean(estimate)) %>% ungroup() %>% arrange(metric, overall_avg) %>% mutate(term=as_factor(term)) %>%
    mutate(sig = as.factor(ifelse(estimate-1.96*std.error > 0 | estimate+1.96*std.error < 0, "black", "grey"))) %>%
    mutate(threshold = paste("<", threshold)) %>%
    ggplot(aes(x=term, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, col=sig,
               group=threshold, label=threshold)) + 
    geom_pointrange(position=position_dodge(width=0.95), size=1) + 
    xlab("") + ylab("Effect on detection metric (%)") +
    facet_wrap(~ metric, scales = "free_x") +
    geom_text(aes(y=estimate-1.96*std.error-0.02, size=.25), position=position_dodge(width=0.95)) +
    geom_hline(yintercept=0, lty=2, alpha=0.5) +
    scale_color_identity() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    coord_flip() +
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
ggsave("figures/secondstage_treatfx_threshold.pdf", width=14, height=9)
system("open figures/secondstage_treatfx_threshold.pdf")
