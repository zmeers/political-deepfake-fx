# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Determine sensitivity of pre-registered analyses to differential
# attrition across treatment conditions by (1) re-weighting treatment conditions
# to parity and (2) conducting Manski extreme bounds analyses.
#
# Note: excluding all registered analyses/hypotheses that don't 
#       involve the stage 1 video/text/audio,etc. condition.
# 
# Author: Soubhik Barari
# 
# Environment:
# - must use R 3.6
#
# Input:
# - deepfakes.RData
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(magrittr)
library(latex2exp)
library(ggplot2)
library(tidyverse)

if (!("dat" %in% ls())) {
    load("deepfake.Rdata")
    dat$lowq <- FALSE
    dat$lowq[dat$quality_pretreat_duration_tooquick | dat$quality_pretreat_duration_tooslow | dat$quality_demographic_mismatch] <- TRUE
    dat$internet_usage <- scales::rescale(dat$internet_usage)
}

#####------------------------------------------------------#
##### HELPERS ####
#####------------------------------------------------------#

UpwtVideoFunc <- function(dat_, lm_specif, term_ = "treatvideo") {
    ## (1) re-compute estimates with treat condition weights
    ## (2) up-weight video condition and re-compute estimates setting outcomes
    ##     to two most extreme values
    out_col <- all.vars(lm_specif)[1]
    e.epsilon.l <- -sd(dat_[[out_col]], na.rm=T)/2
    e.epsilon.u <- sd(dat_[[out_col]], na.rm=T)/2

    lm_orig <- lm(lm_specif, 
                  data = dat_ %>%
                      filter(treat %in% c("text","video")) %>%
                      mutate(treat = factor(treat, levels = c("text","video"))))
    lm_upwt <- lm(lm_specif,
                  data = dat_ %>%
                      filter(treat %in% c("text","video")) %>%
                      mutate(treat = factor(treat, levels = c("text","video"))),
                  weights = dat_ %>%
                      filter(treat %in% c("text","video")) %>%
                      mutate(n_all=n()) %>%
                      group_by(treat) %>%
                      mutate(p_score=1/(n()/n_all)) %>%
                      pull(p_score))
    lm_sharp.L <- lm(lm_specif,
                     data = bind_rows(
                         ## original sample
                         dat_ %>%
                            filter(treat %in% c("text","video")),
                         ## video condition up-sampled with worst outcome
                         dat_ %>%
                             filter(treat %in% c("text","video")) %T>% {
                                 video_n <<- with(., sum(treat == "video",na.rm=T))
                                 text_n <<- with(., sum(treat == "text",na.rm=T))
                             } %>%
                             sample_n(size = text_n-video_n) %>%
                             mutate(treat = "video") %>%
                             mutate(mutate(across(all_of(out_col), ~ min(dat[[out_col]],na.rm=T))))
                     ) %>% mutate(treat = factor(treat, levels = c("text", "video"))))
    lm_sharp.U <- lm(lm_specif, 
                     data = bind_rows(
                         ## original sample
                         dat_ %>%
                             filter(treat %in% c("text","video")),
                         ## video condition up-sampled with worst outcome
                         dat_ %>%
                             filter(treat %in% c("text","video")) %T>% {
                                 video_n <<- with(., sum(treat == "video",na.rm=T))
                                 text_n <<- with(., sum(treat == "text",na.rm=T))
                             } %>%
                             sample_n(size = text_n-video_n) %>%
                             mutate(treat = "video") %>%
                             mutate(mutate(across(all_of(out_col), ~ max(dat[[out_col]],na.rm=T))))
                     ) %>% mutate(treat = factor(treat, levels = c("text", "video"))))
    
    out_df <- bind_rows(
        bind_cols(data.frame(type="Original"), tidy(lm_orig) %>% filter(term == term_)),
        bind_cols(data.frame(type="Weighted"), tidy(lm_upwt) %>% filter(term == term_)),
        bind_cols(data.frame(type="Lower Bound"), tidy(lm_sharp.L) %>% filter(term == term_)),
        bind_cols(data.frame(type="Upper Bound"), tidy(lm_sharp.U) %>% filter(term == term_))
    )
    out_df$video_n <- video_n
    out_df$text_n <- text_n
    out_df$upper <- e.epsilon.u
    out_df$lower <- e.epsilon.l
    
    out_plot <- out_df %>%
        mutate(type = factor(type, levels = c("Original", "Weighted", "Lower Bound", "Upper Bound"))) %>%
        ggplot(aes(y=type, x=estimate, xmin=estimate-2*std.error, xmax=estimate+2*std.error)) +
            geom_pointrange() + 
            theme_linedraw() +
            ylab("") + 
            xlab("Estimate") +
            geom_vline(xintercept = 0, lty = 2, alpha = 0.5) +
            geom_vline(xintercept = e.epsilon.l, alpha = 0.5, size = 1, color = "green") +
            geom_vline(xintercept = e.epsilon.u, alpha = 0.5, size = 1, color = "green") +
            scale_x_continuous(breaks=c(e.epsilon.l, e.epsilon.l/2, 0, e.epsilon.u/2, e.epsilon.u),
                               labels=c(TeX("-$0.5\\sigma$"), round(e.epsilon.l/2,1), 0, round(e.epsilon.u/2,1), TeX("+$0.5\\sigma$")),
                               limits=c(min(c(e.epsilon.l,min(out_df$estimate - 2*out_df$std.error))),
                                        max(c(e.epsilon.u,max(out_df$estimate + 2*out_df$std.error))))) +
            theme(legend.position = "none") +
            theme(strip.text = element_text(size=16),
                  panel.grid.minor = element_blank(),
                  panel.grid = element_line(color = col_grid))
    
    out <- list(out_df, out_plot)
    return(out)
}

#####------------------------------------------------------#
##### H1: Deepfakes more deceptive than text/audio/skit (no-warn cohort) ####
#####------------------------------------------------------#

h1_specif <- formula(believed_true ~ treat)
dat_ <- dat
h1_out <- UpwtVideoFunc(dat_, h1_specif)
h1_plot <- h1_out[[2]] + 
    facet_wrap(~ "H1: Credibility ~ Video")

#####------------------------------------------------------#
##### H2: Deepfakes make target more unfavorable than text/audio/skit  ####
#####------------------------------------------------------#

h2_specif <- formula(post_favor_Warren ~ treat)
dat_ <- dat
h2_out <- UpwtVideoFunc(dat_, h2_specif)
h2_plot <- h2_out[[2]] + 
    facet_wrap(~ "H2: Favorability ~ Video")

#####------------------------------------------------------#
##### H4: Heterogeneity in deception effect by info ####
#####------------------------------------------------------#

h4_specif <- formula(believed_true ~ treat*exp_1_prompt_info)
dat_ <- dat
h4_out <- UpwtVideoFunc(dat_, h4_specif, term_ = "treatvideo:exp_1_prompt_infoTRUE")
h4_plot <- h4_out[[2]] + 
    xlab("Estimate (interaction term)") +
    facet_wrap(~ "H4: Credibility ~ Video x Info")

#####------------------------------------------------------#
##### H5: Heterogeneity in deception effect by cognition ####
#####------------------------------------------------------#

h5_specif <- formula(believed_true ~ treat*crt)
dat_$crt <- dat$crt > .34
h5_out <- UpwtVideoFunc(dat_, lm_specif = h5_specif, term_ = "treatvideo:crtTRUE")
h5_plot <- h5_out[[2]] + 
    xlab("Estimate (discretized interaction term)") +
    facet_wrap(~ "H5: Credibility ~ Video x C.R.")

#####------------------------------------------------------#
##### H6a: Heterogeneities in deception by partisanship ####
#####------------------------------------------------------#

h6a_specif <- formula(believed_true ~ treat*PID*crt)
h6a_out <- UpwtVideoFunc(dat_, lm_specif = h6a_specif, term_ = "treatvideo:PIDRepublican:crtTRUE")
h6a_out[[2]] + 
    xlab("Estimate (discretized interaction term)")

h6a_specif2 <- formula(believed_true ~ treat*PID)
h6a_out2 <- UpwtVideoFunc(dat_, lm_specif = h6a_specif, term_ = "treatvideo:PIDRepublican")
h6a_out2[[2]] + 
    xlab("Estimate (discretized interaction term)")

h6a_plot <- bind_rows(
    h6a_out[[1]] %>% mutate(specif = "Video x PID x CRT"),
    h6a_out2[[1]] %>% mutate(specif = "Video x PID")
) %>%
    mutate(type = factor(type, levels = c("Original", "Weighted", "Lower Bound", "Upper Bound"))) %>%
    ggplot(aes(y=type, x=estimate, xmin=estimate-2*std.error, xmax=estimate+2*std.error, colour=specif)) +
        geom_pointrange(position = position_dodge(0.5)) + 
        theme_linedraw() +
        ylab("") + 
        xlab("Estimate") +
        geom_vline(xintercept = 0, lty = 2, alpha = 0.5) +
        geom_vline(xintercept = h6a_out[[1]]$lower[1], alpha = 0.5, size = 1, color = "green") +
        geom_vline(xintercept = h6a_out[[1]]$upper[1], alpha = 0.5, size = 1, color = "green") +
        scale_x_continuous(breaks=c(h6a_out[[1]]$lower[1], h6a_out[[1]]$lower[1]/2, 0, h6a_out[[1]]$upper[1]/2, h6a_out[[1]]$upper[1]),
                           labels=c(TeX("-$0.5\\sigma$"), round(h6a_out[[1]]$lower[1]/2,1), 0, round(h6a_out[[1]]$upper[1]/2,1), TeX("+$0.5\\sigma$")),
                           limits=c(min(c(h6a_out[[1]]$lower[1],min(h6a_out[[1]]$estimate - 2*h6a_out[[1]]$std.error))),
                                    max(c(h6a_out[[1]]$upper[1],max(h6a_out[[1]]$estimate + 2*h6a_out[[1]]$std.error))))) +
        scale_color_manual(values = c("black", "darkgrey"), name = "specification:") +
        annotate("text", x=-.15, y=0.65, label = "Video x PID", color = "black") +
        annotate("text", x=-.15, y=1.35, label = "Video x PID x CRT", color = "darkgrey") +
        theme(legend.position = "none") +
        theme(strip.text = element_text(size=16),
              panel.grid.minor = element_blank(),
              panel.grid = element_line(color = col_grid)) +
        facet_wrap(~ "H6a: Credibility ~ Video x PID (x C.R.)")

#####------------------------------------------------------#
##### H6b: Heterogeneities in favorability by partisanship ####
#####------------------------------------------------------#

h6b_specif <- formula(post_favor_Warren ~ treat*PID*crt)
h6b_out <- UpwtVideoFunc(dat_, lm_specif = h6b_specif, term_ = "treatvideo:PIDRepublican:crtTRUE")
h6b_out[[2]] + 
    xlab("Estimate (discretized interaction term)")

h6b_specif2 <- formula(post_favor_Warren ~ treat*PID)
h6b_out2 <- UpwtVideoFunc(dat_, lm_specif = h6b_specif2, term_ = "treatvideo:PIDRepublican")
h6b_out2[[2]] + 
    xlab("Estimate (discretized interaction term)")

h6b_plot <- bind_rows(
    h6b_out[[1]] %>% mutate(specif = "Video x PID x CRT"),
    h6b_out2[[1]] %>% mutate(specif = "Video x PID")
) %>%
    mutate(type = factor(type, levels = c("Original", "Weighted", "Lower Bound", "Upper Bound"))) %>%
    ggplot(aes(y=type, x=estimate, xmin=estimate-2*std.error, xmax=estimate+2*std.error, colour=specif)) +
        geom_pointrange(position = position_dodge(0.5)) + 
        theme_linedraw() +
        ylab("") + 
        xlab("Estimate") +
        geom_vline(xintercept = 0, lty = 2, alpha = 0.5) +
        geom_vline(xintercept = h6b_out[[1]]$lower[1], alpha = 0.5, size = 1, color = "green") +
        geom_vline(xintercept = h6b_out[[1]]$upper[1], alpha = 0.5, size = 1, color = "green") +
        scale_x_continuous(breaks=c(h6b_out[[1]]$lower[1], h6b_out[[1]]$lower[1]/2, 0, h6b_out[[1]]$upper[1]/2, h6b_out[[1]]$upper[1]),
                           labels=c(TeX("-$0.5\\sigma$"), round(h6b_out[[1]]$lower[1]/2,1), 0, round(h6b_out[[1]]$upper[1]/2,1), TeX("+$0.5\\sigma$")),
                           limits=c(min(c(h6b_out[[1]]$lower[1],min(h6b_out[[1]]$estimate - 2*h6b_out[[1]]$std.error))),
                                    max(c(h6b_out[[1]]$upper[1],max(h6b_out[[1]]$estimate + 2*h6b_out[[1]]$std.error))))) +
        scale_color_manual(values = c("black", "darkgrey"), name = "specification:") +
        annotate("text", x=-.15, y=0.65, label = "Video x PID", color = "black") +
        annotate("text", x=-.15, y=1.35, label = "Video x PID x CRT", color = "darkgrey") +
        theme(legend.position = "none") +
        theme(strip.text = element_text(size=16),
              panel.grid.minor = element_blank(),
              panel.grid = element_line(color = col_grid)) +
        facet_wrap(~ "H6b: Favorability ~ Video x PID (x C.R.)")

#####------------------------------------------------------#
##### H7a: Heterogeneities in deception by ambivalent sexism ####
#####------------------------------------------------------#

as_q <- quantile(dat$ambivalent_sexism, probs=c(0,0.33,0.66,1), na.rm=T)

h7a_specif <- formula(believed_true ~ treat*ambivalent_sexism)
dat_$ambivalent_sexism <- dat$ambivalent_sexism > as_q[3]
h7a_out <- UpwtVideoFunc(dat_, lm_specif = h7a_specif, term_ = "treatvideo:ambivalent_sexismTRUE")
h7a_plot <- h7a_out[[2]] + 
    xlab("Estimate (discretized interaction term)") +
    facet_wrap(~ "H7a: Credibility ~ Video x A.S.")

#####------------------------------------------------------#
##### H7b: Heterogeneities in favorability by ambivalent sexism ####
#####------------------------------------------------------#

as_q <- quantile(dat$ambivalent_sexism, probs=c(0,0.33,0.66,1), na.rm=T)

h7b_specif <- formula(post_favor_Warren ~ treat*ambivalent_sexism)
dat_$ambivalent_sexism <- dat$ambivalent_sexism > as_q[3]
h7b_out <- UpwtVideoFunc(dat_, lm_specif = h7b_specif, term_ = "treatvideo:ambivalent_sexismTRUE")
h7b_plot <- h7b_out[[2]] + 
    xlab("Estimate (discretized interaction term)") +
    facet_wrap(~ "H7b: Favorability ~ Video x A.S.")

#####------------------------------------------------------#
##### SAVE ####
#####------------------------------------------------------#

pp <- cowplot::plot_grid(h1_plot,
                         h2_plot,
                         h4_plot,
                         h5_plot + theme(strip.text = element_text(size=14)),
                         h6a_plot + theme(strip.text = element_text(size=12)),
                         h6b_plot + theme(strip.text = element_text(size=11)),
                         h7a_plot + theme(strip.text = element_text(size=14)),
                         h7b_plot + theme(strip.text = element_text(size=14)),
                         ncol = 4)
cowplot::save_plot(pp, filename = "figures/bounds_prereg.pdf", limitsize = F,
                   base_width = 16, base_height = 5)
system("open figures/bounds_prereg.pdf")
