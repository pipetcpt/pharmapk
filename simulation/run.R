library(mrgsolve)
library(tidyverse)
library(ggpubr)
library(rlang)
library(httpgd)
mod <- mread_cache('simulation/TMDD.cpp')
mod <- param(mod, list(KINT = 0.003,
                       KOFF = 0.001,
                       KEL = 0.0015,
                       KPT = 0,
                       KTP = 0,
                       KDEG = 0.0089,
                       R0 = 12.36,
                       VC = 0.04,
                       KON = 0.01
))

#### Change of dose  ####

amt_vec <- c(1, 5, 10, 15, 20)

dose_change_plot <- mod %>%
  data_set(as_data_set(data.frame(amt = amt_vec, ID = 1:5, cmt = 2) %>% mutate(dose = amt))) %>%
  carry_out(dose) %>%
  zero_re() %>%
  mrgsim(end = 1800) %>%
  as.data.frame() %>%
  mutate(dose = as.factor(dose)) %>%
  filter(CP > 1E-8) %>%
  ggplot(aes(x = time, y = CP, col = dose)) +
  geom_line() +
  #scale_y_continuous(trans = 'log10') +
  theme_bw() +
  labs(x = "Time (hr)",
       y = "Concentration (nM/L)",
       col = "Dose (nM)")

dose_change_plot_log <- dose_change_plot + scale_y_continuous(trans = 'log10')

ggarrange(dose_change_plot, dose_change_plot_log, common.legend = TRUE, labels = c("A", "B"))
ggsave("media-07/DOSE.png", width = 6, height = 4, unit = "in")
#### Change of parameter ####

param_change <- function(PAR, SEQ, TIME = 800) {
  PAR2 <- as_name(enquo(PAR))
  idata <- data.frame(ID = 1:5) %>% mutate(!!PAR2 := SEQ)
  p1 <- mod %>%
    idata_set(idata) %>%
    data_set(as_data_set(data.frame(amt = 1, ID = 1:5, cmt = 2) %>% mutate(dose = amt))) %>%
    mrgsim(end = TIME) %>%
    as.data.frame() %>%
    left_join(idata, by = "ID") %>%
    mutate(!!PAR2 := as.factor({{PAR}})) %>%
    filter(CP > 1E-5) %>%
    ggplot(aes(x = time, y = CP, col = {{PAR}})) +
    geom_line() +
    theme_bw() +
    labs(x = "Time (hr)",
         y = "Concentration (nM/L)",
         col = as_label(enquo(PAR))
    )
  p2 <- p1 + scale_y_continuous(trans = "log10")
  ggarrange(p1, p2, common.legend = TRUE, labels = c("A", "B"))
}

param_change_plot <- param_change(KINT, c(0.0001, 0.001, 0.1, 1, 10))
param_change_plot_log <- param_change_plot + scale_y_continuous(trans = 'log10')


param_change(R0, seq(1, 20, length.out = 5), 100)
ggsave('media-07/R0.png', width = 6, height = 4, unit = "in")
param_change(KEL, seq(0.00015, 0.01, length.out = 5), 300)
ggsave('media-07/KEL.png', width = 6, height = 4, unit = "in")
param_change(KDEG, seq(0.00015, 0.01, length.out = 5), 300)
ggsave('media-07/KDEG.png', width = 6, height = 4, unit = "in")
param_change(KINT, seq(0.00015, 0.01, length.out = 5), 1000)
ggsave('media-07/KINT.png', width = 6, height = 4, unit = "in")
param_change(KON, seq(0.00015, 0.01, length.out = 5), 300)
ggsave('media-07/KON.png', width = 6, height = 4, unit = "in")
param_change(KOFF, seq(0.00015, 0.01, length.out = 5), 300)
ggsave("media-07/KOFF.png", width = 6, height = 4, unit = "in")





amt_vec <- c(1, 5, 10, 15, 20)

dose_change_plot <- mod %>%
  data_set(as_data_set(data.frame(amt = amt_vec, ID = 1:5, cmt = 2) %>% mutate(dose = amt))) %>%
  carry_out(dose) %>%
  mrgsim(end = 1800) %>%
  as.data.frame() %>%
  mutate(dose = as.factor(dose)) %>%
  filter(CP > 1E-8) %>%
  ggplot(aes(x = time, y = CP, col = dose)) +
  geom_line() +
  # scale_y_continuous(trans = 'log10') +
  theme_bw() +
  labs(
    x = "Time (hr)",
    y = "Concentration (nM/L)",
    col = "Dose (nM)"
  )


mod %>%
  ev(amt = 10, cmt = 2) %>%
  mrgsim(nid = 5, end = 1800) %>%
  as.data.frame() %>%
  filter(CP > 1E-8) %>%
  ggplot(aes(x = time, y = CP, col = as.factor(ID))) +
  geom_line() +
  scale_y_continuous(trans = 'log10') +
  theme_bw() +
  labs(
    x = "Time (hr)",
    y = "Concentration (nM/L)",
    col = "Dose (nM)"
  )
init(mod)
mod2 <- mread_cache('simulation/TMDD2.cpp')
e1 <- ev(amt = 10, cmt = 2)
e2 <- ev(amt = 200, time = 800, cmt = 4, tinf = 50)
c(e1, e2)

sample <- 24 * c(1, 2, 4, 10, 14, 21, 28, 35, 49, 63, 77)
doseinfo1 <- expand.grid(ID = 1:8, amt = 10, cmt = 2, evid = 1, tinf = 0, time = 0)
doseinfo2 <- as.data.frame(ID = 1:4, amt = rep(10000, 4), time = c(800, 830, 860, 890), cmt = rep(4, 4), tinf = rep(1000, 4), evid = rep(1, 4))
mod %>%
  data_set(rbind(doseinfo1, doseinfo2) %>% arrange(ID)) %>%
  mrgsim(end = 2000) %>%
  as.data.frame() %>%
  filter(CP > 1E-8, time %in% sample) %>%
  ggplot(aes(x = time, y = CP, col = as.factor(ID))) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(trans = "log10") +
  theme_bw() +
  labs(
    x = "Time (hr)",
    y = "Concentration (nM/L)",
    col = "Dose (nM)"
  )
init(mod2)
