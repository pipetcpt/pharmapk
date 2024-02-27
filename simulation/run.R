library(mrgsolve)
library(tidyverse)
library(ggpubr)
library(rlang)
library(httpgd)
library(ggthemes)
library(ggsci)
library(ggpubr)
mod <- mread_cache('simulation/TMDD.cpp')
mod <- param(mod, list(KINT = 0.003,
                       KOFF = 0.001,
                       KEL = 0.0005,
                       KPT = 0,
                       KTP = 0,
                       KDEG = 0.0089,
                       R0 = 20.36,
                       VC = 0.04,
                       KON = 0.002
))
#### Change of dose  ####

amt_vec <- c(10, 15, 20, 25)
amt_vec2 <- c(3, 5, 10, 15)
dose_change_plotA <- mod %>%
  data_set(as_data_set(data.frame(amt = amt_vec, ID = 1:4, cmt = 2) %>% mutate(dose = amt))) %>%
  carry_out(dose) %>%
  zero_re() %>%
  mrgsim(end = 3500) %>%
  as.data.frame() %>%
  mutate(dose = as.factor(dose)) %>%
  filter(CP > 1E-8) %>%
  ggplot(aes(x = time, y = CP, col = dose)) +
  geom_line() +
  labs(x = "Time (hr)",
       y = "Concentration (nM/L)",
       col = "Dose (nM)")+
  scale_y_continuous(limits = c(0.05, 1000), breaks = c( 0.1, 1, 10, 100, 1000), trans = "log10") +
  scale_x_continuous(limits = c(0, 3000)) +
  scale_color_npg() +
  theme_bw()

plot_grid(dose_change_plotA, dose_change_plotA, labels = c("A", "B"), label_size = 18)
dose_change_plotA
ggsave('media-07/TMDD_dose.png', width = 7.5, height = 6, unit = "in")
dose_change_plot_log <- dose_change_plot + scale_y_continuous(trans = 'log10')

dose_change_plot_log
ggarrange(dose_change_plot_log, dose_change_plot, common.legend = TRUE, labels = c("A", "B"))
ggsave("media-07/DOSEA.png", width = 6, height = 4, unit = "in")

#### Change of parameter ####
library(scales)
NotFancy <- function(l) {
  l <- format(l, scientific = FALSE)
  parse(text = l)
}

param_change <- function(PAR, SEQ, TIME = 800) {
  PAR2 <- as_name(enquo(PAR))
  idata <- data.frame(ID = 1:5) %>% mutate(!!PAR2 := SEQ)
  p1 <- mod %>%
    idata_set(idata) %>%
    data_set(as_data_set(data.frame(amt = 5, ID = 1:5, cmt = 2) %>% mutate(dose = amt))) %>%
    mrgsim(end = TIME) %>%
    as.data.frame() %>%
    left_join(idata, by = "ID") %>%
    mutate(!!PAR2 := as.factor({{PAR}})) %>%
    filter(CP > 1E-5) %>%
    ggplot(aes(x = time, y = CP, col = {{PAR}})) +
    geom_line() +
    labs(x = "Time (hr)",
         y = "Concentration (nM/L)",
         col = paste0(as_label(enquo(PAR)), " (nM)")) +
    theme_bw() +
    scale_color_npg(labels = c("0.0005", "0.001", "0.005", "0.01", "0.05")) +
    scale_y_continuous(breaks = c(0, 50, 100), limits = c(0, 130)) 
  p1
  #p2 <- p1 + scale_y_continuous(trans = "log10")
  #ggarrange(p1, p2, common.legend = TRUE, labels = c("A", "B"))
}

param_change_plot <- param_change(R0, c(5, 10, 20, 50, 100), 50)

param_change_plot
param_change(R0, seq(1, 20, length.out = 5), 100)
ggsave('media-07/R0.png', width = 7.5, height = 6, unit = "in")
param_change(KEL, seq(0.00015, 0.01, length.out = 5), 300)
ggsave('media-07/KEL.png', width = 6, height = 4, unit = "in")
param_change(KDEG, c(0.00015, 0.0015, 0.015, 0.15, 1.5), 300)
ggsave('media-07/KDEG.png', width = 6, height = 4, unit = "in")
param_change(KINT, c(0.0005, 0.001, 0.005, 0.01, 0.05), 1000) + scale_y_continuous(trans = "log10") + labs(color = "Kec (nM)")
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

mod2 <- param(mod2, list(
  KINT = 0.003,
  KOFF = 0.001,
  KEL = 0.0015,
  KPT = 0,
  KTP = 0,
  KDEG = 0.0089,
  R0 = 12.36,
  VC = 0.04,
  KON = 0.01 
))    

sample <- 24 * c(1, 2, 4, 10, 14, 21, 28, 35, 42, 49, 56, 63, 77)
doseinfo1 <- data.frame(ID = 1:8, amt = rep(10, 4), cmt = rep(2, 4), evid = rep(1, 4), tinf = rep(0, 4), time = rep(0, 4))
moddata <- mod2 %>%
  idata_set(data.frame(ID = 1:8, ADA = c(rep(1, 4), rep(0, 4))))%>%
  data_set(doseinfo1) %>%
  mrgsim(end = 2000) %>%
  as.data.frame() 

savedata2 <- moddata
finaldata <- moddata
head(moddata)
moddata %>%
  filter(CP > 1E-2, time %in% sample) %>% 
  # filter(!(ID %in% 1:4 & time > 1000)) %>%
  mutate(ADA = ifelse(ID %in% c(1, 2, 3, 4), "ADA (+)", "ADA (-)")) %>%
  ggplot(aes(x = time, y = CP, col = as.factor(ID))) +
  geom_point(alpha = 0.5) +
  geom_line() + 
  scale_y_continuous(trans = "log10") +
  facet_grid(cols = vars(ADA))+
  theme_bw() +
  labs(
    x = "Time (hr)",
    y = "Concentration",
    col = "ID"
  ) +
  scale_color_npg()
?facet_grid
ggsave('media-07/ADA.png', width = 6, height = 4, unit = "in")

write.csv(finaldata, "simulation/adadata.csv", row.names= F)
moddata <- read_csv('simulation/adadata.csv')


#### MM model VS TMDD model ####
mod <- mread_cache("simulation/TMDD.cpp")
mod_mm <- mread_cache("simulation/MMmodel.cpp")
mod <- param(mod, list(
  KINT = 0.003,
  KOFF = 0.001,
  KEL = 0.0015,
  KPT = 0,
  KTP = 0,
  KDEG = 0.0089,
  R0 = 12.36,
  VC = 0.04,
  KON = 0.01
))

amt_vec <- c(1, 5, 10, 15, 20)

dose_change_plot_log <- mod %>%
  data_set(as_data_set(data.frame(amt = amt_vec, ID = 1:5, cmt = 2) %>% mutate(dose = amt))) %>%
  carry_out(dose) %>%
  zero_re() %>%
  mrgsim(end = 1800) %>%
  as.data.frame() %>%
  mutate(dose = as.factor(dose)) %>%
  filter(CP > 1E-2) %>%
  ggplot(aes(x = time, y = CP, col = dose)) +
  geom_line() +
  # scale_y_continuous(trans = 'log10') +
  theme_bw() +
  labs(
    x = "Time (hr)",
    y = "Concentration (ng/mL)",
    col = "Dose (mg)"
  ) +
  scale_y_continuous(trans = "log10", limits = c(0.01, 500))


tmdd.plot <- dose_change_plot_log + scale_color_npg()
tmdd.plot
dose_change_plot_log_mm <-mod_mm %>%
   data_set(as_data_set(data.frame(amt = 300*amt_vec, ID = 1:5, cmt = 1) %>% mutate(dose = amt))) %>%
   carry_out(dose) %>%
   zero_re() %>%
   mrgsim(end = 1000) %>%
   as.data.frame() %>%
   mutate(dose = as.factor(dose/100/3)) %>%
   filter(CP > 1E-2) %>%
   ggplot(aes(x = time, y = CP, col = dose)) +
   geom_line() +
   # scale_y_continuous(trans = 'log10') +
   theme_bw() +
   labs(
     x = "Time (hr)",
     y = "Concentration (ng/mL)",
     col = "Dose (mg)"
   ) +
   scale_y_continuous(trans = 'log10', limits = c(0.01, 500)) 
scale_y_continuous
mm.plot <- dose_change_plot_log_mm + scale_color_npg()
mm.plot
library(cowplot)
plot_grid(tmdd.plot, mm.plot, ncol = 2, labels = c("A", "B"), label_size = 18)
grid.arrange(tmdd.plot, mm.plot, ncol = 2)
library(gridExtra)
