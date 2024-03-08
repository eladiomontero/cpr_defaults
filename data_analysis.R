library(lmerTest)
library(ggplot2)
library(mgcv)
library(itsadug)
library(ggplotify)
library(reshape2)

df_long = read.csv("~/Documents/GitHub/cpr_analysis/data_long_format.csv")
all_part = read.csv("~/Documents/GitHub/cpr_analysis/all_participants.csv")
complete <- read.csv("~/Documents/GitHub/cpr_analysis/data_wide_format.csv")

df_long$Treatment = factor(df_long$Treatment)
df_long$player_id = factor(df_long$player_id)

#Main figure with the treatments

ggplot(df_long, aes(round, extraction, color=Treatment, shape = Treatment)) + 
  stat_summary(fun.data=mean_cl_normal, geom="pointrange", size = 3, linewidth = 2, fun.args = (mult = 1)) + 
  labs(y="Mean extraction", x="Round") + 
  theme_bw() + scale_color_manual(values=c("#648FFF", "#FFB000", "#DC267F")) + 
  scale_shape_manual(values = c(16, 17, 18)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "#000001"),
        text = element_text(size = 30)) + 
  geom_hline(yintercept=11, color = "#FFB000", linetype = "dotted", linewidth = 2, alpha = 0.7) + 
  geom_hline(yintercept=23, color = "#DC267F", linetype = "dotted", linewidth = 2, alpha = 0.7) +
  geom_hline(yintercept=18, color = "#1C265F", linetype = "dotted", linewidth = 2, alpha = 0.7) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10))
ggsave("~/Documents/GitHub/cpr_analysis/images/source/treatment_extraction.svg", width=15, height=10, dpi=300)

#Mixed model
m1 <- gam(extraction ~ Treatment + s(round, by=Treatment) + s(svo_score, by=Treatment) +
            extraction_last_round + others_extraction_last_round + svo_score  + gamble_choice
          + s(player_id, bs="re"), data=df_long)

m2 <- gam(extraction ~ Treatment + s(round, by=Treatment) + s(svo_score, by=Treatment) 
          + s(player_id, bs="re"), data=df_long)
summary(m1)

#Model fit with the treatment data
last_plot() + stat_summary(aes(y=fitted(m1)), fun=mean, geom="line", linewidth = 2, alpha = 0.7)
ggsave("~/Documents/GitHub/cpr_analysis/images/source/treatment_extraction_fit.svg", width=15, height=10, dpi=300)

#Plot the differences between treatments

control_self = plot_diff(m1, view="round", comp=list(Treatment=c("Control", "Self-serving")), rm.ranef=TRUE, ylim=c(-5, 5))
control_pro = plot_diff(m1, view="round", comp=list(Treatment=c("Control", "Pro-social")), rm.ranef=TRUE, ylim=c(-5, 5), add = TRUE)
self_pro = plot_diff(m1, view="round", comp=list(Treatment=c("Self-serving", "Pro-social")), rm.ranef=TRUE, ylim=c(-5, 5))

control_self$low_band = control_self$est - control_self$CI
control_self$high_band = control_self$est + control_self$CI
control_pro$low_band = control_pro$est - control_pro$CI
control_pro$high_band = control_pro$est + control_pro$CI
self_pro$low_band = self_pro$est - self_pro$CI
self_pro$high_band = self_pro$est + self_pro$CI

ggplot(control_self, aes(round, est)) +   
  geom_line(color = "#000001", linewidth = 0.8) +
  geom_ribbon(aes(xmax = 1, ymin = low_band, ymax = high_band, fill = round <= 5), alpha = 0.3, show.legend = FALSE) +
  geom_hline(yintercept=0, color = "#000001", linetype = "solid", linewidth = 1) + 
  labs(y="Est. difference in extraction", x="Round") + 
  geom_vline(xintercept=1, color = "red", linetype = "dotted", linewidth = 1, alpha = 0.7) +
  geom_vline(xintercept=5, color = "red", linetype = "dotted", linewidth = 1, alpha = 0.7) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "#000001"), 
                     text = element_text(size = 20)) +
  xlim(1, 10) + 
  ylim(-6, 8) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  scale_fill_manual(values=c("gray", "red"), name="fill") + 
  ggtitle("Control - Self-serving")
ggsave("~/Documents/GitHub/cpr_analysis/images/source/diff_control_self.svg", width=8, height=5, dpi=300)

ggplot(control_pro, aes(round, est)) +   
  geom_line(color = "#000001", linewidth = 0.8) +
  geom_ribbon(aes(xmax = 1, ymin = low_band, ymax = high_band, fill = round <= 2), alpha = 0.3, show.legend = FALSE) +
  geom_hline(yintercept=0, color = "#000001", linetype = "solid", linewidth = 1, alpha = 0.7) + 
  labs(y="Est. difference in extraction", x="Round") + 
  geom_vline(xintercept=1, color = "red", linetype = "dotted", linewidth = 1, alpha = 0.7) +
  geom_vline(xintercept=2, color = "red", linetype = "dotted", linewidth = 1, alpha = 0.7) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "#000001"), 
                     text = element_text(size = 20)) +
  xlim(1, 10) + 
  ylim(-6, 8) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  scale_fill_manual(values=c("gray", "red"), name="fill") + 
  ggtitle("Control - Pro-social")
ggsave("~/Documents/GitHub/cpr_analysis/images/source/diff_control_pro.svg", width=8, height=5, dpi=300)


ggplot(self_pro, aes(round, est)) +   
  geom_line(color = "#000001", linewidth = 0.8) +
  geom_ribbon(aes(ymin = low_band, ymax = high_band, fill = round <= 5), alpha = 0.3, show.legend = FALSE) +
  geom_hline(yintercept=0, color = "#000001", linetype = "solid", linewidth = 1, alpha = 0.7) + 
  labs(y="Est. difference in extraction", x="Round") + 
  geom_vline(xintercept=1, color = "red", linetype = "dotted", linewidth = 1, alpha = 0.7) +
  geom_vline(xintercept=5, color = "red", linetype = "dotted", linewidth = 1, alpha = 0.7) +
  geom_vline(xintercept=10, color = "red", linetype = "dotted", linewidth = 1, alpha = 0.7) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "#000001"), 
                     text = element_text(size = 20)) +
  xlim(1, 10) + 
  ylim(-6, 8) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  scale_fill_manual(values=c("gray", "red"), name="fill") + 
  ggtitle("Self-serving - Pro-social")
ggsave("~/Documents/GitHub/cpr_analysis/images/source/diff_self_pro.svg", width=8, height=5, dpi=300)

#plot the differences between treatments and SVO

par(mfrow = c(1,1), mar = c(4,6, 3, 6))
cont_ps_svo = plot_diff2(m2, view=c("round", "svo_score"), 
                         comp=list(Treatment=c("Control","Pro-social")), 
                         rm.ranef=TRUE, add.color.legend = FALSE, 
                         show.diff = TRUE, alpha.diff = 1, 
                         ylim = c(-25, 90), n.grid = 20, nlevels = 0)
abline(h=c(-12.4,22.45,57.15), col=c("blue"), lty=c(1), lwd=c(1))
gradientLegend(valRange = c(max(cont_ps_svo$z), min(cont_ps_svo$z)), dec = 0, pos=0.5, side=4)
dev.off()

ss_ps_svo = plot_diff2(m2, view=c("round", "svo_score"), 
                       comp=list(Treatment=c("Self-serving", "Pro-social")), 
                       rm.ranef=TRUE, add.color.legend = FALSE, show.diff = TRUE, 
                       alpha.diff = 1, ylim = c(-25, 90), n.grid = 20, nlevels = 0)
abline(h=c(-12.4,22.45,57.15), col=c("blue"), lty=c(1), lwd=c(1))
gradientLegend(valRange = c(max(ss_ps_svo$z), min(ss_ps_svo$z)), dec = 0, pos=.5, side=4)

cont_ss_svo = plot_diff2(m2, view=c("round", "svo_score"), 
                         comp=list(Treatment=c("Control", "Self-serving")), 
                         rm.ranef=TRUE, add.color.legend = FALSE, show.diff = FALSE, 
                         alpha.diff = 1, ylim = c(-25, 90), n.grid = 20, nlevels = 0)
abline(h=c(-12.4,22.45,57.15), col=c("blue"), lty=c(1), lwd=c(1))
gradientLegend(valRange = c(max(cont_ss_svo$z), min(cont_ss_svo$z)), dec = 0, pos=.5, side=4)


melt_list = function(values){
  melted = as.data.frame(values$z)
  values$y = values$y
  colnames(melted) = values$y
  melted$x = values$x
  melted = melt(melted, id = "x")
  colnames(melted) = c("round", "svo_score", "value")
  melted$ci = melt(values$ci.l)$value
  melted$diff = abs(melted$value) - melted$ci
  melted$svo_score = as.numeric(as.character(melted$svo_score))
  return (melted)
}

#Function to plot an SVG heatmap
plot_heat = function(values, title = ""){
  melted = melt_list(values)
  alpha = ifelse(melted$diff >= 0, 1, 0)
  ggplot(melted, aes(x = round, y = svo_score, fill = value, alpha = as.factor(alpha))) +
    geom_tile() + scale_fill_gradientn(colours = terrain.colors(7)) +
    theme_minimal() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.background = element_rect(fill = "#FFFFFF",colour = "#FFFFFF"),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "#000001"), 
                            text = element_text(size = 20)) +
    labs(title = title,
         x = "Round",
         y = "SVO Score") +
    geom_hline(yintercept=-12, color = "red", linetype = "dotted", linewidth = 1) + 
    geom_hline(yintercept=22.45, color = "red", linetype = "dotted", linewidth = 1) + 
    geom_hline(yintercept=57.15, color = "red", linetype = "dotted", linewidth = 1) +
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) + 
    scale_y_continuous(breaks = c(seq(-45, 90, 20))) +
    scale_alpha_manual(values = c(0,1), guide = "none")+
    ylim(-45,95)
  ggsave(sprintf("~/Documents/GitHub/cpr_analysis/images/source/%s.svg", gsub(" ", "", title)), width=8, height=5, dpi=300)
}

plot_heat(cont_ss_svo, "Difference Control - Self-serving")
plot_heat(cont_ps_svo, "Difference Control - Pro-social")
plot_heat(ss_ps_svo, "Difference Self-serving - Pro-social")

#Plotting the differences in the experimental data

ggplot(subset(df_long, svo_category %in% c("Cooperative", "Altruistic")), 
       aes(round, extraction, color=Treatment, shape = Treatment, 
           alpha = as.factor(ifelse(Treatment %in% c("Control", "Self-serving") & round < 6, 1, 0.2)))) + 
  stat_summary(fun.data=mean_cl_normal, geom="pointrange", size = 2, linewidth = 2,
               fun.args = (mult = 1), show.legend = FALSE) + 
  labs(y="Mean extraction", x="Round", title = "Extraction Cooperative + Altruistic") + 
  theme_bw() + scale_color_manual(values=c("#648FFF", "#FFB000", "#DC267F")) + 
  scale_shape_manual(values = c(16, 17, 18)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "#000001"),
        text = element_text(size = 30)) + 
  geom_hline(yintercept=11, color = "#FFB000", linetype = "dotted", linewidth = 2, alpha = 0.7) + 
  geom_hline(yintercept=23, color = "#DC267F", linetype = "dotted", linewidth = 2, alpha = 0.7) +
  geom_hline(yintercept=18, color = "#1C265F", linetype = "dotted", linewidth = 2, alpha = 0.7) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  scale_alpha_manual(values = c(0.2,1), guide = "none")
ggsave("~/Documents/GitHub/cpr_analysis/images/source/coop_altr_extraction.svg", width=8, height=5, dpi=300)

ggplot(subset(df_long, svo_category %in% c("Individualistic", "Competitive")), 
       aes(round, extraction, color=Treatment, shape = Treatment, 
           alpha = as.factor(ifelse(Treatment %in% c("Control", "Pro-social") & round <= 3, 1, 0.7)))) + 
  stat_summary(fun.data=mean_cl_normal, geom="pointrange", size = 2, linewidth = 2,
               fun.args = (mult = 1), show.legend = FALSE) + 
  labs(y="Mean extraction", x="Round", title = "Extraction Individualistic + Competitive") + 
  theme_bw() + scale_color_manual(values=c("#648FFF", "#FFB000", "#DC267F")) + 
  scale_shape_manual(values = c(16, 17, 18)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "#000001"),
        text = element_text(size = 30)) + 
  geom_hline(yintercept=11, color = "#FFB000", linetype = "dotted", linewidth = 2, alpha = 0.7) + 
  geom_hline(yintercept=23, color = "#DC267F", linetype = "dotted", linewidth = 2, alpha = 0.7) +
  geom_hline(yintercept=18, color = "#1C265F", linetype = "dotted", linewidth = 2, alpha = 0.7) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  scale_alpha_manual(values = c(0.3,1), guide = "none")
ggsave("~/Documents/GitHub/cpr_analysis/images/source/indiv_comp_extraction.svg", width=8, height=5, dpi=300)

#Statistical tests

confidence_interval = function(data){
  m = mean(data)
  n = length(data)
  std = sd(data)
  margin = qt(0.975, df = n-1) * std / sqrt(n)
  return(sprintf("$\\overline{x} = %.04f$, 95\\%% CI = $[%.04f, %.04f]$", m, m - margin, m + margin))
}

confidence_interval(subset(df_long, Treatment == "Self-serving" & round <= 5)$extraction)
confidence_interval(subset(df_long, Treatment == "Control" & round <= 5)$extraction)
confidence_interval(subset(df_long, Treatment == "Pro-social" & round <= 5)$extraction)

confidence_interval(subset(df_long, Treatment == "Self-serving" & round > 5)$extraction)
confidence_interval(subset(df_long, Treatment == "Control" & round > 5)$extraction)
confidence_interval(subset(df_long, Treatment == "Pro-social" & round > 5)$extraction)


confidence_interval(subset(df_long, risk_category == "Risk-seeking" & round <= 5 & Treatment == "Self-serving")$extraction)
confidence_interval(subset(df_long, risk_category == "Risk-averse" & round <= 5 & Treatment == "Self-serving")$extraction)

confidence_interval(subset(df_long, risk_category == "Risk-seeking" & round <= 5 & Treatment == "Pro-social")$extraction)
confidence_interval(subset(df_long, risk_category == "Risk-averse" & round <= 5 & Treatment == "Pro-social")$extraction)

aov = anova(lm(extraction ~ Treatment * round, data = subset(df_long, round <= 5)))
aov


aov = anova(lm(extraction ~ Treatment, data = subset(df_long, Treatment %in% c("Self-serving", "Control") & round <= 5)))
aov

aov = anova(lm(extraction ~ Treatment, data = subset(df_long, Treatment %in% c("Pro-social", "Control") & round <= 5)))
aov


aov = anova(lm(extraction ~ Treatment, data = subset(df_long, Treatment %in% c("Self-serving", "Control") & round <= 5 & svo_category %in% c("Altruistic", "Cooperative"))))
aov

aov = anova(lm(extraction ~ Treatment, data = subset(df_long, Treatment %in% c("Pro-social", "Control") & round <= 3 & svo_category %in% c("Individualistic", "Competitive"))))
aov

#Linear regression of the droputs

all_part$dropout = all_part$participant.completion_status %in% c("lost_focus", "timeout")


all_part$Age_x[grepl('\\D', all_part$Age_x)] <- NA
all_part$Age_x <- as.numeric(all_part$Age_x)

table(all_part$dropout)

drop_lm = glm(dropout ~ svo_score + gamble_choice + Nationality_x + Age_x + Sex_x + Ethnicity_simplified_x + Total_approvals_x, data = all_part, family = "binomial")
summary(drop_lm)

not_affected = subset(default_opinions, affected == "0", select = "player_id")
not_af_df = subset(df_long, player_id %in% c(not_affected$player_id))
af_df = subset(df_long, !(player_id %in% c(not_affected$player_id)) & Treatment %in% c("Self-serving", "Pro-social"))

mean(subset(not_af_df, (Treatment == "Self-serving") & (round <= 5))$extraction)
mean(subset(af_df, (Treatment == "Self-serving") & (round <= 5))$extraction)
mean(subset(df_long, (Treatment == "Self-serving") & (round <= 5))$extraction)

mean(subset(not_af_df, (Treatment == "Pro-social") & (round <= 5))$extraction)
mean(subset(af_df, (Treatment == "Pro-social") & (round <= 5))$extraction)
mean(subset(df_long, (Treatment == "Pro-social") & (round <= 5))$extraction)

ks.test(subset(not_af_df, (Treatment == "Pro-social") & (round <= 5))$extraction, subset(af_df, (Treatment == "Pro-social") & (round <= 5))$extraction, alternative = "two.sided")

ks.test(subset(not_af_df, (Treatment == "Self-serving") & (round <= 5))$extraction, subset(af_df, (Treatment == "Self-serving") & (round <= 5))$extraction, alternative = "greater")

ks.test(subset(not_af_df, (Treatment == "Pro-social") & (round <= 5))$extraction, subset(df_long, (Treatment == "Control") & (round <= 5))$extraction, alternative = "two.sided")
ks.test(subset(not_af_df, (Treatment == "Self-serving") & (round <= 5))$extraction, subset(df_long, (Treatment == "Control") & (round <= 5))$extraction, alternative = "two.sided")



