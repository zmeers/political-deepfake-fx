# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Determine whether our observed sample is actually powered to detect
# the equivalence bounds specified in the paper.
#
# Note: using only a subset of pre-registered specifications
#       and treatment contrasts (mostly video vs. text) but results
#       broadly extend to others.
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

samp_fracs <- c(0.10,
                0.50,
                1.00,
                1.50,
                2.00)
n_draws <- 100

if (!("dat" %in% ls())) {
    load("deepfake.Rdata")
    dat$lowq <- FALSE
    dat$lowq[dat$quality_pretreat_duration_tooquick | dat$quality_pretreat_duration_tooslow | dat$quality_demographic_mismatch] <- TRUE
    dat$internet_usage <- scales::rescale(dat$internet_usage)
}
library(tidyverse)
library(broom)
library(latex2exp)
library(ggplot2)

col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255)

#####------------------------------------------------------#
##### HELPERS ####
#####------------------------------------------------------#

PowerFunc <- function(dat, draw_func) {
    ## re-sample dataset at different proportions and calculate power to reject null
    power_df <- data.frame()
    pbar <- utils::txtProgressBar(min=0, max=length(samp_fracs), style=3)
    i <- 0
    for (samp_frac in samp_fracs) {
        i <- i + 1
        utils::setTxtProgressBar(pbar, i)
        draws_df <- data.frame()
        for (draw in 1:n_draws) {
            draw_out <- draw_func(dat, samp_frac)
            draws_df <- bind_rows(draws_df,
                                  draw_out)
        }
        if ("type" %in% colnames(draws_df)) {
            for (t in unique(draws_df[["type"]])) {
                power_df <- bind_rows(power_df,
                                      data.frame(
                                          samp_frac=samp_frac,
                                          samp_n=mean(draws_df$n[draws_df$type==t]),
                                          mean_ests=mean(draws_df$est[draws_df$type==t]),
                                          frac_ests_sig=mean(draws_df$p.val[draws_df$type==t] < 0.01),
                                          type=t
                                      ))
            }
        } else {
            power_df <- bind_rows(power_df,
                                  data.frame(
                                      samp_frac=samp_frac,
                                      samp_n=mean(draws_df$n),
                                      mean_ests=mean(draws_df$est),
                                      frac_ests_sig=mean(draws_df$p.val < 0.01)
                                  ))
        }
    }
    close(pbar)
    if ("type" %in% colnames(draws_df)) {
        power_plot <- power_df %>%
            mutate(t = as.numeric(factor(type))) %>%
            mutate(vj = -0.5 + (t-1)*1) %>%
            ggplot(aes(x=samp_frac, y=frac_ests_sig, fill=type)) +
            geom_line(aes(color=type, lty=type)) +
            geom_point(aes(color=type)) +
            geom_text(aes(x=samp_frac, label = paste("n =", ceiling(samp_n)), y=-Inf, vjust=vj, color=type), angle=90, hjust=-0.1) +
            scale_x_continuous(labels = scales::percent_format(2), breaks=unique(power_df$samp_frac)) +
            scale_y_continuous(limits=c(0, 1)) +
            geom_vline(xintercept = 1, lty = 2, alpha = 0.5) +
            xlab("% of observed sample size") +
            ylab("Power to reject null") + 
            theme_linedraw() +
            theme(legend.position = "none") +
            theme(strip.text = element_text(size=16),
                  panel.grid.minor = element_blank(),
                  panel.grid = element_line(color = col_grid))
    } else {
        power_plot <- ggplot(power_df, aes(x=samp_frac, y=frac_ests_sig)) +
            geom_line() +
            geom_point() +
            geom_text(aes(x=samp_frac, label = paste("n =", ceiling(samp_n)), y=-Inf), angle=90, vjust=0.5, hjust=-0.1) +
            scale_x_continuous(labels = scales::percent_format(2), breaks=unique(power_df$samp_frac)) +
            scale_y_continuous(limits=c(0, 1)) +
            geom_vline(xintercept = 1, lty = 2, alpha = 0.5) +
            xlab("% of observed sample size") +
            ylab("Power to reject null") + 
            theme_linedraw() + 
            theme(strip.text = element_text(size=16),
                  panel.grid.minor = element_blank(),
                  panel.grid = element_line(color = col_grid))
    }
    
    return(list(power_df, power_plot))
}

#####------------------------------------------------------#
##### H1: Deepfakes more deceptive than text/audio/skit (no-warn cohort) ####
#####------------------------------------------------------#

## original specif
h1.m <- lm(believed_true ~ treat,
           data = dat %>% 
               filter(treat != "ad",treat != "skit") %>%
               mutate(treat = factor(treat, levels = c("text","video","audio"))))
summary(h1.m)

## specif w/diff sample size
h1_func <- function(dat, samp_frac) {
    tau <- sd(dat$believed_true, na.rm=T)/2 ## upper equivalence bound
    lm_obj <- lm(believed_true ~ treat, 
                 data = bind_rows(
                     ## baseline condition
                     dat %>% 
                         filter(treat == "text") %>%
                         sample_frac(samp_frac/2, replace=TRUE)
                     ,
                     ## re-sampled treatment condition with sim fx
                     dat %>%
                         filter(treat == "text") %>%
                         mutate(treat = "video") %>%
                         sample_frac(samp_frac/2, replace=TRUE) %>%
                         mutate(believed_true = believed_true + tau)
                 ) %>% mutate(treat = factor(treat, levels = c("text","video"))))
    
    draw_df <- data.frame(
        n=nrow(lm_obj$model),
        p.val=tidy(lm_obj)$p.value[2],
        est=tidy(lm_obj)$estimate[2]
    )
    return(draw_df)
}
h1_out <- PowerFunc(dat, h1_func)
h1_plot <- h1_out[[2]] + 
    facet_wrap(~ "H1: Credibility ~ Video")

#####------------------------------------------------------#
##### H2: Deepfakes make target more unfavorable than text/audio/skit  ####
#####------------------------------------------------------#

## original specif
h2.m <- lm(post_favor_Warren ~ treat,
           data = dat)
summary(h2.m)

## specif w/diff sample size
h2_func <- function(dat, samp_frac) {
    tau <- sd(dat$believed_true, na.rm=T)/2 ## upper equivalence bound
    lm_obj <- lm(believed_true ~ treat, 
                 data = bind_rows(
                     ## baseline condition
                     dat %>% 
                         filter(treat == "text") %>%
                         sample_frac(samp_frac/2, replace=TRUE)
                     ,
                     ## re-sampled treatment condition with sim fx
                     dat %>%
                         filter(treat == "text") %>%
                         mutate(treat = "video") %>%
                         sample_frac(samp_frac/2, replace=TRUE) %>%
                         mutate(believed_true = believed_true + tau)
                 ) %>% mutate(treat = factor(treat, levels = c("text","video"))))
    
    draw_df <- data.frame(
        n=nrow(lm_obj$model),
        p.val=tidy(lm_obj)$p.value[2],
        est=tidy(lm_obj)$estimate[2]
    )
    return(draw_df)
}
h2_out <- PowerFunc(dat, h2_func)
h2_plot <- h2_out[[2]] + 
    facet_wrap(~ "H2: Favorability ~ Video")

#####------------------------------------------------------#
##### H3: Deepfake salience effect on media trust/FPR ####
#####------------------------------------------------------#

## original specif
h3aI.m <- lm(post_media_trust ~ exp_1_prompt,
             data = dat %>% 
                 filter(!lowq,treat!="skit",treat!="ad"))
summary(h3aI.m)

## specif w/diff sample size
h3aI_func <- function(dat, samp_frac) {
    tau <- sd(dat$post_media_trust, na.rm=T)/2 ## upper equivalence bound
    lm_obj <- lm(post_media_trust ~ exp_1_prompt, 
                 data = bind_rows(
                     ## baseline condition
                     dat %>% 
                         filter(!lowq) %>%
                         sample_frac(samp_frac/2, replace=TRUE)
                     ,
                     ## re-sampled treatment condition with sim fx
                     dat %>%
                         filter(!lowq) %>%
                         mutate(exp_1_prompt = "info") %>%
                         sample_frac(samp_frac/2, replace=TRUE) %>%
                         mutate(post_media_trust = post_media_trust + tau)
                 ) %>% mutate(exp_1_prompt = factor(exp_1_prompt, levels = c("control","info"))))
    
    draw_df <- data.frame(
        n=nrow(lm_obj$model),
        p.val=tidy(lm_obj)$p.value[2],
        est=tidy(lm_obj)$estimate[2]
    )
    return(draw_df)
}
h3aI_out <- PowerFunc(dat, h3aI_func)
h3aI_plot <- h3aI_out[[2]] + 
    facet_wrap(~ "H3a: Trust ~ Salience")

#####------------------------------------------------------#
##### H4: Heterogeneity in deception effect by info ####
#####------------------------------------------------------#

## specif w/diff sample size
h4_func <- function(dat, samp_frac) {
    tau <- sd(dat$believed_true, na.rm=T)/2 ## upper equivalence bound
    lm_obj <- lm(believed_true ~ treat, 
                 data = bind_rows(
                     ## baseline condition
                     dat %>% 
                         filter(treat == "text", exp_1_prompt_control==TRUE) %>%
                         sample_frac(samp_frac/2, replace=TRUE)
                     ,
                     ## re-sampled treatment condition with sim fx
                     dat %>%
                         filter(treat == "text", exp_1_prompt_control==TRUE) %>%
                         mutate(treat = "video") %>%
                         sample_frac(samp_frac/2, replace=TRUE) %>%
                         mutate(believed_true = believed_true + tau)
                 ) %>% mutate(treat = factor(treat, levels = c("text","video"))))
    
    lm_obj2 <- lm(believed_true ~ treat, 
                  data = bind_rows(
                      ## baseline condition
                      dat %>% 
                          filter(treat == "text", exp_1_prompt_control==FALSE) %>%
                          sample_frac(samp_frac/2, replace=TRUE)
                      ,
                      ## re-sampled treatment condition with sim fx
                      dat %>%
                          filter(treat == "text", exp_1_prompt_control==FALSE) %>%
                          mutate(treat = "video") %>%
                          sample_frac(samp_frac/2, replace=TRUE) %>%
                          mutate(believed_true = believed_true + tau)
                  ) %>% mutate(treat = factor(treat, levels = c("text","video"))))
    
    draw_df <- bind_rows(
        data.frame(
            n=nrow(lm_obj$model),
            p.val=tidy(lm_obj)$p.value[2],
            est=tidy(lm_obj)$estimate[2],
            type="No Info"
        ),
        data.frame(
            n=nrow(lm_obj2$model),
            p.val=tidy(lm_obj2)$p.value[2],
            est=tidy(lm_obj2)$estimate[2],
            type="Info"
        )
    )
    return(draw_df)
}
h4_out <- PowerFunc(dat, h4_func)
h4_plot <- h4_out[[2]] + 
    annotate("text", x=0.6, y=0.65, label="No Info", color="grey") +
    annotate("text", x=0.2, y=0.65, label="Info", color="black") +
    scale_color_manual(values=c("grey","black"), name="") +
    scale_fill_manual(values=c("grey","black"), name="") +
    scale_linetype_manual(values=c(2,1), name="") +
    facet_wrap(~ "H4: Credibility ~ Video x Info")

#####------------------------------------------------------#
##### H5: Heterogeneity in deception effect by cognition ####
#####------------------------------------------------------#

## modified original specif for low CR cell only
h5.m <- lm(believed_true ~ treat, 
           data = dat %>% 
               filter(!is.na(treat), treat != "ad", treat !="skit", crt < .34))
summary(h5.m)

## specif w/diff sample size
h5_func <- function(dat, samp_frac) {
    tau <- sd(dat$believed_true, na.rm=T)/2 ## upper equivalence bound
    lm_obj <- lm(believed_true ~ treat, 
                 data = bind_rows(
                     ## baseline condition
                     dat %>% 
                         filter(treat == "text", crt < .34) %>%
                         sample_frac(samp_frac/2, replace=TRUE)
                     ,
                     ## re-sampled treatment condition with sim fx
                     dat %>%
                         filter(treat == "text", crt < .34) %>%
                         mutate(treat = "video") %>%
                         sample_frac(samp_frac/2, replace=TRUE) %>%
                         mutate(believed_true = believed_true + tau)
                 ) %>% mutate(treat = factor(treat, levels = c("text","video"))))
    
    draw_df <- data.frame(
        n=nrow(lm_obj$model),
        p.val=tidy(lm_obj)$p.value[2],
        est=tidy(lm_obj)$estimate[2]
    )
    return(draw_df)
}
h5_out <- PowerFunc(dat, h5_func)
h5_plot <- h5_out[[2]] + 
    facet_wrap(~ "H5: Credibility ~ Video x C.R.")

#####------------------------------------------------------#
##### H6a: Heterogeneities in deception by partisanship ####
#####------------------------------------------------------#

## modified original specif for cognitively reflective Republicans only
h6a <- lm(believed_true ~ treat, 
          data = dat %>% 
              filter(treat != "ad", treat != "skit", crt > .34, PID == "Republican"))
summary(h6a)

## specif w/diff sample size
h6a_func <- function(dat, samp_frac) {
    tau <- sd(dat$believed_true, na.rm=T)/2 ## upper equivalence bound
    
    ## high cognition Republicans
    lm_obj <- lm(believed_true ~ treat, 
                 data = bind_rows(
                     ## baseline condition
                     dat %>% 
                         filter(treat == "text", crt > .34, PID == "Republican") %>%
                         sample_frac(samp_frac/2, replace=TRUE)
                     ,
                     ## re-sampled treatment condition with sim fx
                     dat %>%
                         filter(treat == "text", crt > .34, PID == "Republican") %>%
                         mutate(treat = "video") %>%
                         sample_frac(samp_frac/2, replace=TRUE) %>%
                         mutate(believed_true = believed_true + tau)
                 ) %>% mutate(treat = factor(treat, levels = c("text","video"))))
    
    ## all Republicans
    lm_obj2 <- lm(believed_true ~ treat,
                  data = bind_rows(
                      ## baseline condition
                      dat %>%
                          filter(treat == "text", PID == "Republican") %>%
                          sample_frac(samp_frac/2, replace=TRUE)
                      ,
                      ## re-sampled treatment condition with sim fx
                      dat %>%
                          filter(treat == "text", PID == "Republican") %>%
                          mutate(treat = "video") %>%
                          sample_frac(samp_frac/2, replace=TRUE) %>%
                          mutate(believed_true = believed_true + tau)
                  ) %>% mutate(treat = factor(treat, levels = c("text","video"))))
    
    draw_df <- bind_rows(
        data.frame(
            n=nrow(lm_obj$model),
            p.val=tidy(lm_obj)$p.value[2],
            est=tidy(lm_obj)$estimate[2],
            type="High C.R. Republicans"
        ),
        data.frame(
            n=nrow(lm_obj2$model),
            p.val=tidy(lm_obj2)$p.value[2],
            est=tidy(lm_obj2)$estimate[2],
            type="All Republicans"
        )
    )
    return(draw_df)
}

h6a_out <- PowerFunc(dat, h6a_func)
h6a_plot <- h6a_out[[2]] + 
    annotate("text", x=1.4, y=0.95, label="All Republicans", color="red") +
    annotate("text", x=1.4, y=0.6, label="High C.R.\nRepublicans", color="salmon1") +
    scale_color_manual(values=c("red","salmon1"), name="") +
    scale_fill_manual(values=c("red","salmon1"), name="") +
    scale_linetype_manual(values=c(1,2), name="") + 
    facet_wrap(~ "H6a: Credibility ~ Video x PID")

#####------------------------------------------------------#
##### H6b: Heterogeneities in favorability by partisanship ####
#####------------------------------------------------------#

## modified original specif for cognitively reflective Republicans only
h6b <- lm(post_favor_Warren ~ treat,
          data = dat %>% 
              filter(!is.na(treat), crt > .34, PID == "Republican"))
summary(h6b)

## specif w/diff sample size
h6b_func <- function(dat, samp_frac) {
    tau <- sd(dat$post_favor_Warren, na.rm=T)/2 ## upper equivalence bound
    
    ## high cognition Republicans
    lm_obj <- lm(post_favor_Warren ~ treat, 
                 data = bind_rows(
                     ## baseline condition
                     dat %>% 
                         filter(treat == "text", crt > .34, PID == "Republican") %>%
                         sample_frac(samp_frac/2, replace=TRUE)
                     ,
                     ## re-sampled treatment condition with sim fx
                     dat %>%
                         filter(treat == "text", crt > .34, PID == "Republican") %>%
                         mutate(treat = "video") %>%
                         sample_frac(samp_frac/2, replace=TRUE) %>%
                         mutate(post_favor_Warren = post_favor_Warren + tau)
                 ) %>% mutate(treat = factor(treat, levels = c("text","video"))))
    
    ## all Republicans
    lm_obj2 <- lm(post_favor_Warren ~ treat,
                  data = bind_rows(
                      ## baseline condition
                      dat %>%
                          filter(treat == "text", PID == "Republican") %>%
                          sample_frac(samp_frac/2, replace=TRUE)
                      ,
                      ## re-sampled treatment condition with sim fx
                      dat %>%
                          filter(treat == "text", PID == "Republican") %>%
                          mutate(treat = "video") %>%
                          sample_frac(samp_frac/2, replace=TRUE) %>%
                          mutate(post_favor_Warren = post_favor_Warren + tau)
                  ) %>% mutate(treat = factor(treat, levels = c("text","video"))))
    
    draw_df <- bind_rows(
        data.frame(
            n=nrow(lm_obj$model),
            p.val=tidy(lm_obj)$p.value[2],
            est=tidy(lm_obj)$estimate[2],
            type="High C.R. Republicans"
        ),
        data.frame(
            n=nrow(lm_obj2$model),
            p.val=tidy(lm_obj2)$p.value[2],
            est=tidy(lm_obj2)$estimate[2],
            type="All Republicans"
        )
    )
    return(draw_df)
}

h6b_out <- PowerFunc(dat, h6b_func)
h6b_plot <- h6b_out[[2]] + 
    annotate("text", x=1.4, y=0.95, label="All Republicans", color="red") +
    annotate("text", x=1.4, y=0.6, label="High C.R.\nRepublicans", color="salmon1") +
    scale_color_manual(values=c("red","salmon1"), name="") +
    scale_fill_manual(values=c("red","salmon1"), name="") +
    scale_linetype_manual(values=c(1,2), name="") +
    facet_wrap(~ "H6b: Favorability ~ Video x PID x C.R.")

#####------------------------------------------------------#
##### H7a: Heterogeneities in deception by ambivalent sexism ####
#####------------------------------------------------------#

as_q <- quantile(dat$ambivalent_sexism, probs=c(0,0.33,0.66,1), na.rm=T)

## modified original specif for high a.s. only
h7a <- lm(believed_true ~ treat, 
          data = dat %>%
              filter(!is.na(treat), ambivalent_sexism > as_q[3]))

## specif w/diff sample size
h7a_func <- function(dat, samp_frac) {
    tau <- sd(dat$believed_true, na.rm=T)/2 ## upper equivalence bound
    
    ## high a.s.
    lm_obj <- lm(believed_true ~ treat, 
                 data = bind_rows(
                     ## baseline condition
                     dat %>% 
                         filter(treat == "text", ambivalent_sexism > as_q[3]) %>%
                         sample_frac(samp_frac/2, replace=TRUE)
                     ,
                     ## re-sampled treatment condition with sim fx
                     dat %>%
                         filter(treat == "text", ambivalent_sexism > as_q[3]) %>%
                         mutate(treat = "video") %>%
                         sample_frac(samp_frac/2, replace=TRUE) %>%
                         mutate(believed_true = believed_true + tau)
                 ) %>% mutate(treat = factor(treat, levels = c("text","video"))))
    
    ## moderate a.s.
    lm_obj2 <- lm(believed_true ~ treat, 
                  data = bind_rows(
                      ## baseline condition
                      dat %>% 
                          filter(treat == "text", ambivalent_sexism > as_q[2], ambivalent_sexism < as_q[3]) %>%
                          sample_frac(samp_frac/2, replace=TRUE)
                      ,
                      ## re-sampled treatment condition with sim fx
                      dat %>%
                          filter(treat == "text", ambivalent_sexism > as_q[2], ambivalent_sexism < as_q[3]) %>%
                          mutate(treat = "video") %>%
                          sample_frac(samp_frac/2, replace=TRUE) %>%
                          mutate(believed_true = believed_true + tau)
                  ) %>% mutate(treat = factor(treat, levels = c("text","video"))))
    
    draw_df <- bind_rows(
        data.frame(
            n=nrow(lm_obj$model),
            p.val=tidy(lm_obj)$p.value[2],
            est=tidy(lm_obj)$estimate[2],
            type="High A.S."
        ),
        data.frame(
            n=nrow(lm_obj2$model),
            p.val=tidy(lm_obj2)$p.value[2],
            est=tidy(lm_obj2)$estimate[2],
            type="Moderate A.S."
        )
    )
    return(draw_df)
}
h7a_out <- PowerFunc(dat, h7a_func)
h7a_plot <- h7a_out[[2]] + 
    annotate("text", x=1.4, y=0.83, label="Moderate A.S.", color="blue") +
    annotate("text", x=0.6, y=0.95, label="High A.S.", color="skyblue") +
    scale_color_manual(values=c("blue","skyblue"), name="") +
    scale_fill_manual(values=c("blue","skyblue"), name="") +
    scale_linetype_manual(values=c(1,2), name="") +
    facet_wrap(~ "H7a: Credibility ~ Video x A.S.")

#####------------------------------------------------------#
##### H7b: Heterogeneities in favorability by ambivalent sexism ####
#####------------------------------------------------------#

as_q <- quantile(dat$ambivalent_sexism, probs=c(0,0.33,0.66,1), na.rm=T)

## modified original specif for high a.s. only
h7b <- lm(post_favor_Warren ~ treat, 
          data = dat %>%
              filter(!is.na(treat), ambivalent_sexism > as_q[3]))

## specif w/diff sample size
h7b_func <- function(dat, samp_frac) {
    tau <- sd(dat$post_favor_Warren, na.rm=T)/2 ## upper equivalence bound
    
    ## high a.s.
    lm_obj <- lm(post_favor_Warren ~ treat, 
                 data = bind_rows(
                     ## baseline condition
                     dat %>% 
                         filter(treat == "text", ambivalent_sexism > as_q[3]) %>%
                         sample_frac(samp_frac/2, replace=TRUE)
                     ,
                     ## re-sampled treatment condition with sim fx
                     dat %>%
                         filter(treat == "text", ambivalent_sexism > as_q[3]) %>%
                         mutate(treat = "video") %>%
                         sample_frac(samp_frac/2, replace=TRUE) %>%
                         mutate(post_favor_Warren = post_favor_Warren + tau)
                 ) %>% mutate(treat = factor(treat, levels = c("text","video"))))
    
    ## moderate a.s.
    lm_obj2 <- lm(post_favor_Warren ~ treat, 
                  data = bind_rows(
                      ## baseline condition
                      dat %>% 
                          filter(treat == "text", ambivalent_sexism > as_q[2], ambivalent_sexism < as_q[3]) %>%
                          sample_frac(samp_frac/2, replace=TRUE)
                      ,
                      ## re-sampled treatment condition with sim fx
                      dat %>%
                          filter(treat == "text", ambivalent_sexism > as_q[2], ambivalent_sexism < as_q[3]) %>%
                          mutate(treat = "video") %>%
                          sample_frac(samp_frac/2, replace=TRUE) %>%
                          mutate(post_favor_Warren = post_favor_Warren + tau)
                  ) %>% mutate(treat = factor(treat, levels = c("text","video"))))
    
    draw_df <- bind_rows(
        data.frame(
            n=nrow(lm_obj$model),
            p.val=tidy(lm_obj)$p.value[2],
            est=tidy(lm_obj)$estimate[2],
            type="High A.S."
        ),
        data.frame(
            n=nrow(lm_obj2$model),
            p.val=tidy(lm_obj2)$p.value[2],
            est=tidy(lm_obj2)$estimate[2],
            type="Moderate A.S."
        )
    )
    return(draw_df)
}
h7b_out <- PowerFunc(dat, h7b_func)
h7b_plot <- h7b_out[[2]] + 
    annotate("text", x=1.4, y=0.83, label="Moderate A.S.", color="blue") +
    annotate("text", x=0.6, y=0.95, label="High A.S.", color="skyblue") +
    scale_color_manual(values=c("blue","skyblue"), name="") +
    scale_fill_manual(values=c("blue","skyblue"), name="") +
    scale_linetype_manual(values=c(1,2), name="") +
    facet_wrap(~ "H7b: Favorability ~ Video x A.S.")

#####------------------------------------------------------#
##### H8: Accuracy salience and detection accuracy ####
#####------------------------------------------------------#

## original specif
h8.m <- lm(exp_2_pct_correct ~ exp_2_prompt_accuracy,
           data = dat)
summary(h8.m)

## specif w/diff sample size
h8_func <- function(dat, samp_frac) {
    tau <- sd(dat$exp_2_pct_correct, na.rm=T)/2 ## upper equivalence bound
    lm_obj <- lm(exp_2_pct_correct ~ exp_2_prompt_accuracy, 
                 data = bind_rows(
                     ## baseline condition
                     dat %>% 
                         filter(exp_2_prompt_accuracy == FALSE) %>%
                         sample_frac(samp_frac/2, replace=TRUE)
                     ,
                     ## re-sampled treatment condition with sim fx
                     dat %>%
                         filter(exp_2_prompt_accuracy == FALSE) %>%
                         mutate(exp_2_prompt_accuracy = TRUE) %>%
                         sample_frac(samp_frac/2, replace=TRUE) %>%
                         mutate(exp_2_pct_correct = exp_2_pct_correct + tau)
                 ))
    
    draw_df <- data.frame(
        n=nrow(lm_obj$model),
        p.val=tidy(lm_obj)$p.value[2],
        est=tidy(lm_obj)$estimate[2]
    )
    return(draw_df)
}
h8_out <- PowerFunc(dat, h8_func)
h8_plot <- h8_out[[2]] +
    facet_wrap(~ "H8: Acc ~ Salience")

#####------------------------------------------------------#
##### H9: Diglit and detection accuracy ####
#####------------------------------------------------------#

## original specif
h9.m <- lm(exp_2_pct_correct ~ post_dig_lit,
           data = dat)
summary(h9.m)

## specif w/diff sample size
h9_func <- function(dat, samp_frac) {
    tau <- sd(dat$exp_2_pct_correct, na.rm=T)/2 ## upper equivalence bound
    lm_obj <- lm(exp_2_pct_correct ~ post_dig_lit, 
                 data = bind_rows(
                     dat %>%
                         sample_frac(samp_frac, replace=TRUE) %>%
                         mutate(post_dig_lit = post_dig_lit*tau)
                 ))
    
    draw_df <- data.frame(
        n=nrow(lm_obj$model),
        p.val=tidy(lm_obj)$p.value[2],
        est=tidy(lm_obj)$estimate[2]
    )
    return(draw_df)
}
h9_out <- PowerFunc(dat, h9_func)
h9_plot <- h9_out[[2]] +
    facet_wrap(~ "H9: Acc ~ Digital Literacy")

#####------------------------------------------------------#
##### SAVE ####
#####------------------------------------------------------#

pp <- cowplot::plot_grid(h1_plot + xlab(""), 
                         h2_plot + xlab("") + ylab(""), 
                         h3aI_plot + xlab("") + ylab(""),
                         h4_plot + xlab("") + ylab(""),
                         h5_plot + xlab(""),
                         h6a_plot + xlab("") + ylab(""),
                         h6b_plot + xlab("") + ylab("") + theme(strip.text = element_text(size=12)),
                         h7a_plot + xlab("") + ylab("") + theme(strip.text = element_text(size=15)),
                         h7b_plot + theme(strip.text = element_text(size=15)),
                         h8_plot + ylab(""),
                         h9_plot + ylab(""))
cowplot::save_plot(pp, filename = "figures/power_prereg.pdf",
                   base_width = 14.5, base_height = 8)
system("open figures/power_prereg.pdf")
