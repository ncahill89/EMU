library(fpemlocal)
library(tidyverse)
library(ggpubr)
library(ggrepel)
library(devtools)

load_all()

## get mcpr estimates from saved output
model_mcpr_survey <- get_model_mcpr(data_type = "survey")
model_mcpr_ss <- get_model_mcpr(data_type = "ss")

## meta data with country codes and ss types
meta_data <- readRDS("data/meta_data.rds")


# Figure 2 ----------------------------------------------------------------
mcpr_fpem_2018 <- readRDS("data/mcpr_fpem_2018.rds")
service_stats_2018 <- readRDS("data/service_stats_2018.rds")

mcpr_FPEM_toplot <- mcpr_fpem_2018  %>%
  group_by(division_numeric_code) %>%
  filter(year > unique(start_year)) %>%
  filter(year < unique(ss_recent_year)) %>%
  mutate(line_colour = ifelse(year < (ss_first_year-1),"FPEMcountry","overlap period")) %>%
  ungroup() %>%
  filter(division_numeric_code %in% (service_stats_2018$division_numeric_code %>% unique()))

new_labels <- c("454" = "Malawi", "508" = "Mozambique", "524" = "Nepal","646" = "Rwanda","716" = "Zimbabwe")

p2 <- ggplot() +
  geom_point(data = service_stats_2018,aes(x = year, y = emu, shape = type_full)) +
  geom_line(data = mcpr_FPEM_toplot, aes(x = year, y = value), colour = "#F8766D") +
  geom_line(data = mcpr_FPEM_toplot, aes(x = year, y = value, colour = line_colour)) +
  facet_wrap(~division_numeric_code,labeller = labeller(division_numeric_code = new_labels)) +
  scale_y_continuous(limits = c(0,0.8)) +
  labs(colour = "", shape = "") +
  xlab("Year")+
  ylab("Modern contraceptive use") +
  theme_bw()
p2


# Figure 3 ----------------------------------------------------------------

ss_exploratory_data <- readRDS("data/ss_exploratory_data.rds")

p3a <- ggplot(ss_exploratory_data, aes(x = mcpr_diff, y = emu_diff)) +
  geom_point(aes(colour = type)) +
  geom_line(aes(x = mcpr_diff, y = mcpr_diff),colour = "black") +
  ylab(expression(paste(Delta, "EMU ")))+
  theme_bw() +
  theme(axis.text.x=element_text(angle=50, vjust=0.5))+
  xlab(expression(paste(Delta, "P "))) +
  labs(colour = " ")

p3a

p3b <- ggplot(ss_exploratory_data, aes(x = mcpr_diff, y = emu_diff - mcpr_diff)) +
  geom_point(aes(colour = type)) +
  ylab(expression(paste(Delta, "EMU - ", Delta, "P")))+
  theme_bw() +
  theme(axis.text.x=element_text(angle=50, vjust=0.5))+
  xlab(expression(paste(Delta, "P")))+
  geom_hline(aes(yintercept = 0), colour = "black", size = 0.2) +
  labs(colour = "")

p3b

ggpubr::ggarrange(p3a,p3b,common.legend = TRUE, ncol = 1)


# Figure 4 ----------------------------------------------------------------

codes <- meta_data %>%
          filter(share_permission == "yes") %>%
          pull(division_numeric_code)

p4 <- ggplot(subset(model_mcpr_survey, division_numeric_code %in% codes)  , aes(x = year, y = `50%`))+
  geom_line()+
  geom_ribbon(aes(ymin = `5%`, ymax = `95%`, fill = "survey-only"), alpha = 0.5) +
  geom_line(data = subset(model_mcpr_ss, division_numeric_code %in% codes), aes(x = year, y = `50%`))+
  geom_ribbon(data = subset(model_mcpr_ss, division_numeric_code %in% codes), aes(ymin = `5%`, ymax = `95%`,fill = "survey + EMU"), alpha = 0.5) +
  guides(color = FALSE) +
  theme_bw() +
  labs(fill = "") +
  ylab("mCPR") +
  xlab("Year") +
  labs(shape = "Data type", fill = "Model") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~country)
p4

# Figure 5 ----------------------------------------------------------------

label_one <- rep(NA, codes %>% length)
label_one[match(c("Cameroon","Malawi","Mozambique","Nepal","Rwanda","Zimbabwe"),meta_data$name)] <- c("Cameroon","Malawi","Mozambique","Nepal","Rwanda","Zimbabwe")
label <- rep(label_one, each = 2)

## create a dataset with the differences between mCPR estimates for survey model and emu+survey model
level_diff <- model_mcpr_ss %>%
  filter(year == 2020) %>%
  mutate(ss_type = factor(meta_data$ss_type, levels= c("clients","facilities","users","visits"),labels= c("comm.clients","comm.facilities","FP users","service visits"))) %>%
  mutate(ss_med = `50%`*100) %>%
  mutate(survey_med = model_mcpr_survey %>% filter(year == 2020) %>% pull(`50%`)*100) %>%
  mutate(diff = ss_med - survey_med,
         abs_diff = abs(ss_med - survey_med)) %>%
  select(country, division_numeric_code,year, ss_med, survey_med, diff, abs_diff, ss_type) %>%
  pivot_longer(cols = c(diff, abs_diff),
               values_to = "value",
               names_to = "difference")

## plot the differences
p5 <- ggplot(level_diff, aes(reorder(ss_type,-(value),FUN = median), y = (value),label = label)) +
  geom_boxplot(aes(colour = ss_type)) +
  geom_point(colour = "black", alpha = 0.7) +
  facet_wrap(~difference,labeller = labeller(difference = c("diff" = "Difference", "abs_diff" = "Absolute Difference")), scales = "free_y")+
  geom_text_repel(segment.size  = 0.5,
                  segment.color = "grey50",
                  direction     = "x",
                  size = 3,
                  point.padding = 0.8,
                  force = 20,
                  min.segment.length = 0.1) +
  geom_hline(yintercept = 0, colour = "black", size = 0.2) +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("Difference in mCPR") +
  xlab("EMU data type") +
  scale_y_continuous(breaks = c(-3,-2,-1,0,1,2,3,4,5,6))
p5

# Figure 6 ----------------------------------------------------------------

## create a dataset with the differences between mCPR CIs for survey model and emu+survey model

ci_diff <- model_mcpr_ss %>%
  filter(year == 2020) %>%
  mutate(ss_type = factor(meta_data$ss_type, levels= c("clients","facilities","users","visits"),labels= c("comm.clients","comm.facilities","FP users","service visits"))) %>%
  mutate(ss_ci_width = (`97.5%` - `2.5%`)*100) %>%
  mutate(survey_ci_width = model_mcpr_survey %>% filter(year == 2020) %>% mutate(ci_width = (`97.5%` - `2.5%`)*100) %>%  pull(ci_width)) %>%
  mutate(diff = survey_ci_width - ss_ci_width) %>%
  select(country, division_numeric_code,year, ss_ci_width, survey_ci_width, diff, ss_type)

## plot CI differences
p6 <- ggplot(ci_diff, aes(x = reorder(ss_type,-diff,FUN = median), y = diff, label = label_one)) +
  stat_boxplot(aes(colour = ss_type)) +
  geom_point(size = 2) +
  geom_text_repel(segment.size  = 0.5,
                  segment.color = "grey50",
                  direction     = "both",
                  point.padding = 0.1,
                  size = 3,
                  force = 20,
                  min.segment.length = 0.1) +
  geom_hline(yintercept = 0, colour = "black", size = 0.2) +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("Difference in CI width (excl. EMU -  incl. EMU)") +
  xlab("EMU data type") +
  scale_y_continuous(breaks = c(-1,0,1,2,3,4,5,6))
p6



