#### model and graph replication code for "Welfare chauvinism among co-ethnics: Evidence from a conjoint experiment in South Korea"
# by Peter Ward and Steven Denney, published in International Migration, https://doi.org/10.1111/imig.12937


## load packages
library(tidyverse)
library(cregg)

## load data (.rda)

#load() # housedf.rda

#### Baseline models ####
####-----------------####

## baseline conjoints analysis, amce and marginal means

amce_baseline <- cj(housedf, y ~ Origin + Age + Family + Gender + Occupation + Income + Health + Record, 
                    id = ~respid, estimate = "amce")

mm_baseline <- cj(housedf, y ~ Origin + Age + Family + Gender + Occupation + Income + Health + Record, 
                       id = ~respid, estimate = "mm")

## figure 2
baseline_m <- ggplot(amce_baseline, aes(fct_rev(level), estimate)) +
  theme_light() +
  geom_point(aes(color=feature), size=1) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, color=feature), width=0) +
  #scale_color_manual(values = c("#CCA20C","#F86624","#662E9B","#E23482","#43BCCD","#82C458",
  #"#79133E", "#142d4c")) +
  coord_flip() +
  labs(title="Figure 2",
       subtitle="Effects of Applicant Attributes on Probability of Being Selected for Public Housing",
       caption="Estimates represent the effects of the randomly assigned applicant attribute values on the probability of being preferred for public housing.\nEstimates are based on the benchmark OLS model. Error bars represent 95% confidence intervals.\nThe point estimates without error bars are the reference categories.",
       y="Effect on Pr(Applicant Selected)",
       x="") +
  facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
  theme(legend.position = "none") +
  scale_color_grey()

#ggsave("Baseline_m.pdf", width = 9, height = 9)

## figure si.1
baseline_m_mm <- ggplot(amce_baseline_mm, aes(fct_rev(level), estimate)) +
  theme_light() +
  geom_point(aes(color=feature), size=1) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, color=feature), width=0) +
  #scale_color_manual(values = c("#CCA20C","#F86624","#662E9B","#E23482","#43BCCD","#82C458",
  #"#79133E", "#142d4c")) +
  coord_flip() +
  labs(title="Figure SI.1",
       subtitle="Marginal Means of Applicants' Attributes on Preference for Public Housing",
       caption="Estimates represent the mean outcome for each attribute level, averaged across all other levels.\nEstimates are based on the benchmark OLS model with clustered standard errors. Error bars represent 95% confidence intervals.",
       y="Effect on Pr(Applicant Selected)",
       x="") +
  facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
  theme(legend.position = "none") +
  scale_color_grey()

#ggsave("Figure_SI.1-Baseline_m_mm.pdf", width = 9, height = 9)


#### CATE based on public cost conditions ####
####--------------------------------------####

amce_pubcost <- cj(housedf, y ~ Origin + Age + Family + Gender + Occupation + Income + Health + Record, 
                   id = ~respid,
                   by=~pubcost)

amce_diffs <- amce_diffs(housedf, y ~ Origin + Age + Family + Gender + Occupation + Income + Health + Record, 
                         ~ pubcost, id = ~ respid)

write.csv(amce_diffs, "amce_diffs.csv")

## spacing for graphs
pdamce <- position_dodge(0.4) # move them .05 to the left and right

## subset for applicants' origin
amce_pubcost <- subset(amce_pubcost, feature == "Origin")

## figure 3
baseline_pubcost_m <- ggplot(amce_pubcost, aes(x=level, y=estimate, color=pubcost, linetype=pubcost)) +
  theme_light() +
  geom_point(aes(shape=pubcost),position=pdamce) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, position=pdamce) +
  coord_flip() +
  labs(title="Figure 3",
       subtitle="Effects of Applicants' Origins on Probability of Being Selected for Public Housing:\nConditional on Pubic Cost Intervention",
       caption="Estimates represent the effects of the randomly assigned origin on the probability of being preferred for public housing (averaged across the other attributes).\nEstimates are based on the benchmark OLS model. Error bars represent 95% confidence intervals.\nThe point estimates without error bars are the reference categories.",
       y="Effect on Pr(Applicant Selected)",
       x="") +
  facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
  scale_color_grey() + theme(legend.title = element_blank(),
                             legend.position = "top")

#ggsave("baseline_pubcost_m.pdf", width = 9, height = 9)


#### charge analysis ####
####-----------------####

## subset for first vingnette + choice; remove outliers and log amount
housedf_2 <- housedf %>% group_by(respid) %>% slice((1:2)) %>%
  subset(., y == 1) %>%
  mutate(charge2 = ifelse(charge == 0, 1, charge)) %>%
  mutate(charge2 = ifelse(charge > 2000000, NA, charge2)) %>%
  mutate(charge_log = log10(charge2))

## regs / tables 2 & 3
chargelm1 <- lm(charge_log ~ Origin + Age + Family + Gender + Occupation + Income + Health + Record + pubcost,
                data=housedf_2)
chargelm1_pubcostcontrol <- subset(housedf_2, pubcost == "Control") %>%
  lm(charge_log ~ Origin + Age + Family + Gender + Occupation + Income + Health + Record, data=.)
chargelm1_pubcostlow <- subset(housedf_2, pubcost == "Low Cost") %>%
  lm(charge_log ~ Origin + Age + Family + Gender + Occupation + Income + Health + Record, data=.)
chargelm1_pubcosthigh <- subset(housedf_2, pubcost == "High Cost") %>%
  lm(charge_log ~ Origin + Age + Family + Gender + Occupation + Income + Health + Record, data=.)


#### CATE models for appendix; si2-5 ####
####---------------------------------####

amces_gender <- cj(housedf, y ~ Origin + Age + Family + Gender + Occupation + Income + Health + Record, 
                   id = ~respid, estimate = "amce", 
                   by = ~Resp_Gender)

amces_age<- cj(housedf, y ~ Origin + Age + Family + Gender + Occupation + Income + Health + Record, 
               id = ~respid, estimate = "amce", 
               by = ~age_median)

amces_college <- cj(housedf, y ~ Origin + Age + Family + Gender + Occupation + Income + Health + Record, 
                    id = ~respid, estimate = "amce", 
                    by = ~College)

amces_polid <- cj(housedf, y ~ Origin + Age + Family + Gender + Occupation + Income + Health + Record, 
                  id = ~respid, estimate = "amce", 
                  by = ~PolID_2)


### CATE graphs for appendix; si2-5 ####
amces_gender_p <- ggplot(amces_gender, aes(level, estimate, colour=Resp_Gender)) + theme_light() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pdamce) +
  coord_flip() +
  geom_point(position=pdamce) +
  facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
  guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE),
         guide_legend("")) +
  theme(legend.title = element_blank()) + scale_color_grey(start=.6,end=.2) +
  labs(title="Figure SI.2",
       subtitle="Effects of Applicant Attributes on Probability of Being Selected for Public Housing:\nBy Respondent's Gender",
       caption="Estimates are based on the benchmark OLS model. Error bars represent 95% confidence intervals.\nThe point estimates without error bars are the reference categories.",
       y="Effect on Pr(Applicant Selected)",
       x="")

#ggsave("Figure_SI2-amces_gender_p.pdf", amces_gender_p, width = 9, height = 9)


amces_age_p <- ggplot(amces_age, aes(level, estimate, colour=age_median)) + theme_light() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pdamce) +
  coord_flip() +
  geom_point(position=pdamce) +
  facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
  guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE),
         guide_legend("")) +
  theme(legend.title = element_blank()) + scale_color_grey(start=.6,end=.2) +
  labs(title="Figure SI.3",
       subtitle="Effects of Applicant Attributes on Probability of Being Selected for Public Housing:\nBy Respondent's Age",
       caption="Estimates are based on the benchmark OLS model. Error bars represent 95% confidence intervals.\nThe point estimates without error bars are the reference categories.",
       y="Effect on Pr(Applicant Selected)",
       x="")

#ggsave("Figure_SI3-amces_age_p.pdf", amces_age_p, width = 9, height = 9)


amces_college_p <- ggplot(amces_college, aes(level, estimate, colour=College)) + theme_light() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pdamce) +
  coord_flip() +
  geom_point(position=pdamce) +
  facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
  guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE),
         guide_legend("")) +
  theme(legend.title = element_blank()) + scale_color_grey(start=.6,end=.2) +
  labs(title="Figure SI.4",
       subtitle="Effects of Applicant Attributes on Probability of Being Selected for Public Housing:\nBy Respondent's Education",
       caption="Estimates are based on the benchmark OLS model. Error bars represent 95% confidence intervals.\nThe point estimates without error bars are the reference categories.",
       y="Effect on Pr(Applicant Selected)",
       x="")

#ggsave("Figure_SI4-amces_college.pdf", amces_college_p, width = 9, height = 9)

amces_polid_p <- ggplot(amces_polid, aes(level, estimate, colour=Poliden)) + theme_light() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pdamce) +
  coord_flip() +
  geom_point(position=pdamce) +
  facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
  guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE),
         guide_legend("")) +
  theme(legend.title = element_blank()) + scale_color_grey(start=.6,end=.2) +
  labs(title="Figure SI.5",
       subtitle="Effects of Applicants' Origins on Probability of Being Selected for Public Housing:\nBy Respondent's Political Identification",
       caption="Estimates are based on the benchmark OLS model. Error bars represent 95% confidence intervals.\nThe point estimates without error bars are the reference categories.",
       y="Effect on Pr(Applicant Selected)",
       x="")

#ggsave("Figure_SI5-amces_polid_p.pdf", amces_polid_p, width = 9, height = 9)

