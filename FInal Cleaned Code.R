# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(survival)
library(survminer)
library(ggpubr)
library(gridExtra)

# Exploratory Data Analysis (EDA)  (1 is male)


#1)

#Correlation between HDL and LDL cholesterol
mi.data$sex <- as.factor(mi.data$sex)
mi.data$smoking <- as.factor(mi.data$smoking)

ggplot(mi.data, aes(x=hdl, y=ldl)) + 
  geom_point() + geom_smooth(method=lm)

cor.test(mi.data$hdl, mi.data$ldl)

#Based on the results of the correlation test:
#Pearson's correlation coefficient (r) between ldl and hdl is approximately -0.864.
#The p-value associated with the correlation test is very small (< 2.2e-16), indicating strong evidence against the null hypothesis that the true correlation between ldl and hdl is zero.
#The 95% confidence interval for the correlation coefficient ranges from approximately -0.869 to -0.859.
#In summary:

#There is a strong negative correlation between ldl and hdl.
#This negative correlation suggests that as levels of ldl increase, levels of hdl tend to decrease, and vice versa.


#2)
#Are any of the covariates significantly associated with HDL or LDL? Do they have an impact on them
# Perform linear regression for each covariate separately

# Fit linear regression model for smoking and HDL
model_smoking_hdl <- lm(hdl ~ smoking, data = mi.data)
# Print summary of the linear regression model
summary(model_smoking_hdl)

model_sex_hdl <- lm(hdl ~ sex, data = mi.data)
summary(model_sex_hdl)

model_age_hdl <- lm(hdl ~ age, data = mi.data)
summary(model_age_hdl)

model_mi_hdl <- lm(hdl ~ mi, data = mi.data)
summary(model_mi_hdl)

#Same for LDL
model_smoking_ldl <- lm(ldl ~ smoking, data = mi.data)
summary(model_smoking_ldl)

model_sex_ldl <- lm(ldl ~ sex, data = mi.data)
summary(model_sex_ldl)

model_age_ldl <- lm(ldl ~ age, data = mi.data)
summary(model_age_ldl)

model_mi_ldl <- lm(ldl ~ mi, data = mi.data)
summary(model_mi_ldl)



#Some visualization plots

#Plots showing observational relationship between HDL and outcome as well as covariates

library(ggplot2)
library(ggpubr)

# Boxplot for HDL levels correlation to event (MI)
p1 <- ggplot(mi.data, aes(x = mi , y = hdl, fill = as.factor(mi))) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("dodgerblue","red"), name = "MI") +
  stat_compare_means(label.y = c(0.5, 1.5), method = "t.test", label = "p.format", vjust = -1) +  # Add p-value
  theme_bw() +
  labs(title = "Boxplot of HDL levels in MI", x = "Status of MI", y = "HDL Cholesterol level") +
  theme(plot.title = element_text(hjust = 0.5))

# Smoking
p2 <- ggplot(mi.data, aes(x = smoking , y = hdl, fill = as.factor(smoking))) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("dodgerblue","red"), name = "Smoker") +
  stat_compare_means(label.y = c(0.5, 1.5), method = "t.test", label = "p.format", vjust = -1) +  # Add p-value
  theme_bw() +
  labs(title = "Boxplot of HDL levels by Smoking Status", x = "Smoking Status", y = "HDL Cholesterol level") +
  theme(plot.title = element_text(hjust = 0.5))

# Sex
p3 <- ggplot(mi.data, aes(x = sex , y = hdl, fill = as.factor(sex))) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("dodgerblue","red"), name = "Sex") +
  stat_compare_means(label.y = c(0.5, 1.5), method = "t.test", label = "p.format", vjust = -1) +  # Add p-value
  theme_bw() +
  labs(title = "Boxplot of HDL levels by Sex", x = "Sex", y = "HDL Cholesterol level") +
  theme(plot.title = element_text(hjust = 0.5))

# Age
p4 <- ggplot(mi.data, aes(x = age , y = hdl)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(title = "Relationship between HDL and Age", x = "Age", y = "HDL Cholesterol level") +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange the plots in a grid
grid.arrange(p1, p2, p3, p4, ncol = 2)




#LDL
# Boxplot for LDL levels correlation to event (MI)
p1_ldl <- ggplot(mi.data, aes(x = mi , y = ldl, fill = as.factor(mi))) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("dodgerblue","red"), name = "MI") +
  stat_compare_means(label.y = c(0.5, 4.5), method = "t.test", label = "p.format", vjust = -1) +  # Add p-value
  theme_bw() +
  labs(title = "Boxplot of LDL levels in MI", x = "Status of MI", y = "LDL Cholesterol level") +
  theme(plot.title = element_text(hjust = 0.5))

# Smoking
p2_ldl <- ggplot(mi.data, aes(x = smoking , y = ldl, fill = as.factor(smoking))) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("dodgerblue","red"), name = "Smoker") +
  stat_compare_means(label.y = c(0.5, 4.5), method = "t.test", label = "p.format", vjust = -1) +  # Add p-value
  theme_bw() +
  labs(title = "Boxplot of LDL levels by Smoking Status", x = "Smoking Status", y = "LDL Cholesterol level") +
  theme(plot.title = element_text(hjust = 0.5))

# Sex
p3_ldl <- ggplot(mi.data, aes(x = sex , y = ldl, fill = as.factor(sex))) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("dodgerblue","red"), name = "Sex") +
  stat_compare_means(label.y = c(0.5, 4.5), method = "t.test", label = "p.format", vjust = -1) +  # Add p-value
  theme_bw() +
  labs(title = "Boxplot of LDL levels by Sex", x = "Sex", y = "LDL Cholesterol level") +
  theme(plot.title = element_text(hjust = 0.5))

# Age
p4_ldl <- ggplot(mi.data, aes(x = age , y = ldl)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(title = "Relationship between LDL and Age", x = "Age", y = "LDL Cholesterol level") +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange the plots in a grid
grid.arrange(p1_ldl, p2_ldl, p3_ldl, p4_ldl, ncol = 2)


#plots to illustrate there is no significant association between HDl, LDL or any covariates and MI
library(ggplot2)


# Create modified data for HDL
mi.data_hdl <- mi.data %>%
  mutate(hdl_bin = cut(hdl, breaks = seq(min(hdl), max(hdl), hdl_binwidth))) %>%
  group_by(hdl_bin, mi) %>%
  summarise(count = n()) %>%
  mutate(count = ifelse(count <= 5, 0, count))

# Create modified data for LDL
mi.data_ldl <- mi.data %>%
  mutate(ldl_bin = cut(ldl, breaks = seq(min(ldl), max(ldl), ldl_binwidth))) %>%
  group_by(ldl_bin, mi) %>%
  summarise(count = n()) %>%
  mutate(count = ifelse(count <= 5, 0, count))

# Create modified data for Age
mi.data_age <- mi.data %>%
  mutate(age_bin = cut(age, breaks = seq(min(age), max(age), 5))) %>%
  group_by(age_bin, mi) %>%
  summarise(count = n()) %>%
  mutate(count = ifelse(count <= 5, 0, count))

# Create modified plots
# Histogram for HDL with adjusted x-axis labels
plot_hdl <- ggplot(mi.data, aes(x = hdl, fill = mi)) +
  geom_histogram(binwidth = hdl_binwidth, position = "identity", alpha = 0.7) +
  labs(title = "Distribution of HDL Cholesterol by MI Status",
       x = "HDL Cholesterol",
       y = "Frequency",
       fill = "MI") +
  xlim(hdl_range) +  # Set x-axis limits
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5)) +  # Center title horizontally
  facet_wrap(~ mi, scales = "free_y") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))  # Adjust x-axis labels

# Histogram for LDL with adjusted x-axis labels
plot_ldl <- ggplot(mi.data, aes(x = ldl, fill = mi)) +
  geom_histogram(binwidth = ldl_binwidth, position = "identity", alpha = 0.7) +
  labs(title = "Distribution of LDL Cholesterol by MI Status",
       x = "LDL Cholesterol",
       y = "Frequency",
       fill = "MI") +
  xlim(ldl_range) +  # Set x-axis limits
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5)) +  # Center title horizontally
  facet_wrap(~ mi, scales = "free_y") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))  # Adjust x-axis labels

# Histogram for Age with adjusted x-axis labels
plot_age <- ggplot(mi.data, aes(x = age, fill = mi)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
  labs(title = "Distribution of Age by MI Status",
       x = "Age",
       y = "Frequency",
       fill = "MI") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5)) +  # Center title horizontally
  facet_wrap(~ mi, scales = "free_y") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))  # Adjust x-axis labels
# Remaining plots remain unchanged

# Bar plot for Smoking
plot_smoking <- ggplot(mi.data, aes(x = factor(smoking), fill = mi)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(title = "Distribution of Smoking Status by MI Status",
       x = "Smoking",
       y = "Frequency",
       fill = "MI") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))# Center title horizontally

# Bar plot for Sex
plot_sex <- ggplot(mi.data, aes(x = factor(sex), fill = mi)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(title = "Distribution of Sex by MI Status",
       x = "Sex",
       y = "Frequency",
       fill = "MI") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))  # Center title horizontally



# Arrange modified plots
grid.arrange(plot_hdl, plot_ldl, plot_age, plot_smoking, plot_sex, ncol = 2, top = NULL)


#Build a multivariate logistic regression model

#First do the univariate analysis ~ association between MI and variables?

# Univariate analysis for HDL and MI
model_hdl_mi <- glm(mi ~ hdl, family = binomial, data = mi.data)
summary(model_hdl_mi)

# Univariate analysis for LDL and MI
model_ldl_mi <- glm(mi ~ ldl, family = binomial, data = mi.data)
summary(model_ldl_mi)

# Univariate analysis for age and MI
model_age_mi <- glm(mi ~ age, family = binomial, data = mi.data)
summary(model_age_mi)

# Univariate analysis for sex and MI
model_sex_mi <- glm(mi ~ sex, family = binomial, data = mi.data)
summary(model_sex_mi)

# Univariate analysis for smoking and MI
model_smoking_mi <- glm(mi ~ smoking, family = binomial, data = mi.data)
summary(model_smoking_mi)




#Mendelian Randomization

#Quickly show the SNPs are correlated with increased/decreased cholesterol levels

model_hdl <- lm(snp.hdl ~ hdl, data = mi.data)
summary(model_hdl)

model_ldl <- lm(snp.ldl ~ ldl, data = mi.data)
summary(model_ldl)
# Extract p-values from the models
p_value_hdl <- summary(model_hdl)$coefficients[2, 4]
p_value_ldl <- summary(model_ldl)$coefficients[2, 4]

# Plot for HDL levels by SNP status
ggplot(mi.data, aes(x = as.factor(snp.hdl), y = hdl, fill = as.factor(snp.hdl))) + 
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Boxplot of HDL Levels by SNP Status", x = "HDL SNP Status", y = "HDL Cholesterol") +
  scale_fill_manual(values = c("dodgerblue","Red","palegreen"), name = "HDL SNP") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_compare_means(label = "p.format", method = "t.test", comparisons = list(c("0", "1"), c("0", "2"), c("1", "2")), 
                     aes(label = paste("p =", formatC(..p.format.., format = "e", digits = 1))))

# Plot for LDL levels by SNP status
ggplot(mi.data, aes(x = as.factor(snp.ldl), y = ldl, fill = as.factor(snp.ldl))) + 
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Boxplot of LDL Levels by SNP Status", x = "LDL SNP Status", y = "LDL Cholesterol") +
  scale_fill_manual(values = c("dodgerblue","Red","palegreen"), name = "LDL SNP") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_compare_means(label = "p.format", method = "t.test", comparisons = list(c("0", "1"), c("0", "2"), c("1", "2")), 
                     aes(label = paste("p =", formatC(..p.format.., format = "e", digits = 1))))
#Check is the SNPs status is associated with any of the other covariates, need to show it is only associated with the exposure
# Linear regression model for SNP status (HDL) and confounders
model_hdl_cov <- lm(snp.hdl ~ age + smoking + sex, data = mi.data)
summary(model_hdl_cov)

# Linear regression model for confounders and SNP (LDL) status
model_confounders_snp <- lm(snp.ldl ~ smoking + age + sex, data = mi.data)
summary(model_confounders_snp)

#No association between increased copies of the HDL,LDL SNP allele and any of the covariates


summary(lm(mi ~ snp.hdl + sex + age + smoking, data=mi.data))


# Perform Mendelian Randomization for hdl using logistic regression but then more effecitvely with COX PH
mi.data$unconfounded.hdl <- predict(lm(hdl ~ snp.hdl, data=mi.data))

mendelian_model <- (glm(mi ~ unconfounded.hdl + sex + age + smoking, family = binomial, data = mi.data))
summary(mendelian_model)

# Fit Cox proportional hazards regression model
cox_model_hdl <- coxph(Surv(follow.up.days, mi) ~ unconfounded.hdl + age + smoking + sex, data = mi.data)
# Summarize the model
summary(cox_model_hdl)  #This model uses the time and reduces the likelihood of false negatives since we model over time and keep time into account.


#MR for LDL
mi.data$unconfounded.ldl <- predict(lm(ldl ~ snp.ldl, data=mi.data))

mendelian_model_ldl <- (glm(mi ~ unconfounded.ldl + sex + age + smoking, family = binomial, data = mi.data))
summary(mendelian_model_ldl)  #This is not the most appropriate thing to do, still fine but explain why this gives a different mark compared to other models.

odds_ratio <- exp(-0.057704)
lower_bound <- exp(-0.057704 - 1.96 * 0.060377)
upper_bound <- exp(-0.057704 + 1.96 * 0.060377)

# Print the confidence interval
cat("95% Confidence Interval for Odds Ratio:", lower_bound, "-", upper_bound)  #Crosses 1 therefore not statistically significant

# Fit Cox proportional hazards regression model
cox_model <- coxph(Surv(follow.up.days, mi) ~ sex + age + smoking + unconfounded.ldl, data = mi.data)

# Summarize the model
summary(cox_model)  #This model uses the time and reduces the likelihood of false negatives since we model over time and keep time into account.




#Survival analysis using Kaplan Meier

#HDL
# Create a survival object
surv_object_hdl <- with(mi.data, Surv(follow.up.days, mi))

# Fit the Kaplan-Meier survival curves based on the genotype for the LDL SNP
surv_fit_hdl <- survfit(surv_object ~ snp.hdl, data = mi.data)

# Plot the survival curves
ggsurvplot(surv_fit_hdl, 
           data = mi.data, 
           risk.table = TRUE,  # Show the risk table
           pval = TRUE,        # Show p-value of log-rank test
           conf.int = TRUE,    # Show confidence intervals
           xlab = "Time (days)", 
           ylab = "Survival probability",
           palette = c("#1f77b4", "#ff7f0e", "#2ca02c"),  # Color palette for the lines
           legend.labs = c("Genotype 0", "Genotype 1", "Genotype 2"),  # Legend labels
           legend.title = "HDL SNP Genotype", 
           ggtheme = theme_minimal()
)


#LDL
# Create a survival object
surv_object <- with(mi.data, Surv(follow.up.days, mi))

# Fit the Kaplan-Meier survival curves based on the genotype for the LDL SNP
surv_fit <- survfit(surv_object ~ snp.ldl, data = mi.data)

# Plot the survival curves
ggsurvplot(surv_fit, 
           data = mi.data, 
           risk.table = TRUE,  # Show the risk table
           pval = TRUE,        # Show p-value of log-rank test
           conf.int = TRUE,    # Show confidence intervals
           xlab = "Time (days)", 
           ylab = "Survival probability",
           palette = c("#1f77b4", "#ff7f0e", "#2ca02c"),  # Color palette for the lines
           legend.labs = c("Genotype 0", "Genotype 1", "Genotype 2"),  # Legend labels
           legend.title = "LDL SNP Genotype", 
           ggtheme = theme_minimal()
)


