library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(ggpubr)


#Convert id and time into factor variables
Test <- Test %>%
  gather(key = "time", value = "score", X76, X86, X96) %>%
  convert_as_factor(ID, time)

# Inspect some random rows of the data by groups
set.seed(123)
Test %>% sample_n_by(Gen, GC, time, size = 1)

#Summary statistics
Test%>%
  group_by(Gen, GC, time)%>%
  get_summary_stats(score, type = "mean_sd")
### Visualization ###
#2. Bar graph
bar <- ggbarplot(
  Test, x = "time", y = "score",
  color = "black", fill = "GC", add = c("mean_sd"), position = position_dodge(0.8),
  facet.by = "Gen", short.panel.labs = FALSE, error.plot = "errorbar"
) + labs(x = "Days after sowing", y = "Moisture %") +
  scale_fill_manual(values = c("#1E90FF", "#FF7F50"))
bar

##Analysis
res.aov <- anova_test(
  data = Test, dv = score, wid = ID,
  within = time, between = c(Gen, GC)
)
get_anova_table(res.aov)

##Pairwise comparison
pwc <- Test %>%
  group_by(Gen, time) %>%
  pairwise_t_test(score ~ GC, paired = TRUE, p.adjust.method = "bonferroni") %>%
  select(-df, -statistic)

### Final visualization ###
pwc <- pwc %>% add_xy_position(x = "time")

#2. Bar graph
bar + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

