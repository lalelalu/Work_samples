library("tidyverse")
library("readxl")
library(sjPlot)
library(sjmisc)
library(broom)
library(ggpubr)
library(cowplot)
library(patchwork)
library(sjlabelled)
library(sjmisc)
library(rlang)
library(insight)
library(gt)
library('webshot2')
library("car")
library(data.table)
library("corrr")
rm(list = ls()) #Arbeitsspeicher leeren
R.Version()

##########################
##Prepare dataset######
##########################

###read data###
data_full <- read_excel("Daten/data.xlsx")
nrow(data_full)
###extract and rename relevant columns###
data <- subset(data_full, select = c(1, 14:16, 17:29, 31:54, 55, 56, 57))
colnames(data) <- c('id', 'age', 'gender', 'books', 'selfesteem_gen_1', 'selfesteem_gen_2R', 'selfesteem_gen_3R', 'selfesteem_gen_4', 'selfesteem_cont_1R', 'selfesteem_crit_2R', 'selfesteem_sa_2R', 'selfesteem_crit_1R', 'selfesteem_cont_4R', 'selfesteem_sa_4','selfesteem_cont_5R','selfesteem_crit_4R','selfesteem_sa_5R','bessi_deci_1','bessi_deci_2','bessi_deci_3','bessi_deci_4','bessi_deci_5','bessi_deci_6','bessi_expr_1','bessi_expr_2','bessi_expr_3','bessi_expr_4','bessi_expr_5','bessi_expr_6','bessi_team_1','bessi_team_2','bessi_team_3','bessi_team_4','bessi_team_5','bessi_team_6','bessi_crea_1','bessi_crea_2','bessi_crea_3','bessi_crea_4','bessi_crea_5','bessi_crea_6','feedback','understandability','mail') # nolint
colnames(data)
###Get all mail adresses###
mails <- filter(data, !is.na(mail))
mails <- mails$mail
set.seed(23)
#Select winners
wins <- sample(mails, 6)
win30 <- wins[1]
win20a <- wins[2]
win20b <- wins[3]
win10a <- wins[4]
win10b <- wins[5]
win10c <- wins[6]
write(wins, "mails.txt")

###make selfesteem answers numeric###
selfesteem_1 <- c("selfesteem_gen_1", "selfesteem_gen_2R", "selfesteem_gen_3R", "selfesteem_gen_4")
for (column in selfesteem_1){
  data[[column]][data[[column]] == '7 (trifft voll und ganz zu)'] <- 7
  data[[column]][data[[column]] == '1 (trifft überhaupt nicht zu)'] <- 1
  data[[column]] <- as.numeric(data[[column]])
}
selfesteem_2 <- c('selfesteem_cont_1R','selfesteem_crit_2R','selfesteem_sa_2R','selfesteem_crit_1R')
for (column in selfesteem_2){
  data[[column]][data[[column]]=='7 (sehr)'] <- 7
  data[[column]][data[[column]]=='1 (gar nicht)'] <- 1
  data[[column]] <- as.numeric(data[[column]])
}
selfesteem_3 <- c('selfesteem_cont_4R','selfesteem_sa_4','selfesteem_cont_5R','selfesteem_crit_4R','selfesteem_sa_5R')
for (column in selfesteem_3){
  data[[column]][data[[column]]=='7 (immer)'] <- 7
  data[[column]][data[[column]]=='1 (nie)'] <- 1
  data[[column]] <- as.numeric(data[[column]])
}
###make skills answers numeric###
bessicolumns <- select(data, starts_with('bessi'))
colnames(bessicolumns)
for (column in colnames(bessicolumns)){
  data[[column]][data[[column]] == '7 (extrem gut)'] <- 7
  data[[column]][data[[column]] == '1 (überhaupt nicht gut)'] <- 1
  data[[column]] <- as.numeric(data[[column]])
}
###Make understandability answers numeric###
data$understandability[data$understandability == "10 (sehr)"] <- 10
data$understandability[data$understandability == "1 (überhaupt nicht)"] <- 1
data$understandability <- as.numeric(data$understandability)

###Save original values###
data$agetrue <- data$age
data$gendertrue <- data$gender
data$ses <- data$books
data$gendertrue[data$gendertrue=='Männlich'] <- 'Male'
data$gendertrue[data$gendertrue=='Weiblich'] <- 'Female'

###dummy-code gender###
data$gender[data$gender == "Weiblich"] <- 0
data$gender[data$gender == "Männlich"] <- 1
data$gender_numeric <- data$gender
data$gender <- as.numeric(data$gender)

#####################################################
###########PARTICIPANTS##############################
#####################################################

###Extract uncomplete data entries###
datalarge <- data #save full dataset
nrow(data) #total number of participants
empty <- filter(data, is.na(age))
nrow(empty) #number of participants without information
partly <- filter(data, !is.na(age))
data_some <- partly
nrow(data_some) #number of participants with at least some information
partly <- filter(partly, is.na(understandability))
nrow(partly) #number of uncomplete but begun entries

###Exclude participants from analysis set###
data <- filter(data, !is.na(understandability)) #Exclude uncomplete data entries
data <- filter(data, data$id != 74) #Exclude participant with no real answers
#Exclude participants with more than two SDs away from mean SES for robustness analysis
plot(data$books)
data_robust <- data #Save data_robust for selectivity analysis
data$id[data$books < mean(data$books) - 2 * sd(data$books) | data$books > mean(data$books) + 2 * sd(data$books)] #Get participants' IDs
data <- filter(data, (data$books >= mean(data$books) - 2 * sd(data$books) & data$books <= mean(data$books) + 2 * sd(data$books)))
nrow(data) #number of participants in analysis sample

###Selectivity analysis: Uncomplete vs. complete entries###
#prepare dataset without empty entries but with complete entries
data_some <- filter(data_some, data_some$id != 74)
data_some$complete <- !is.na(data_some$understandability)
#Distributions differing in age or gender or SES?
table(data_some$age, data_some$complete)
chisq.test(data_some$age, data_some$complete)$expected #assumption violated: expected values < 5
chisq.test(data_some$gender, data_some$complete)$expected #testing assumption: expected values > 5
chisq.test(data_some$gender, data_some$complete)
sqrt(0.11625/nrow(data_some)) #Cramer's V
t.test(partly$age, data$age) #not significant
(mean(partly$age) - mean(data$age)) / sd(c(partly$age, data$age)) #Cohen's d
t.test(data$books, partly$books) #not significant
(mean(partly$books) - mean(data$books)) / sd(c(partly$books, data$books)) #Cohen's d
#Descriptive statistics for partly completed participants
range(partly$age)
mean(partly$age)
sd(partly$age)
table(partly$gender)
table(8 / 18, 10 / 18)
mean(partly$gender)
sd(partly$gender)
mean(partly$books)
sd(partly$books)
range(partly$books)

###Selectivity analysis: Uncomplete vs. complete entries###
#Distributions differing in age or gender or SES?
data_robust$outlier <- (data_robust$books < mean(data_robust$books) - 2 * sd(data_robust$books) | data_robust$books > mean(data_robust$books) + 2 * sd(data_robust$books))
chisq.test(data_robust$age, data_robust$outlier)$expected #assumption violated: expected values < 5
chisq.test(data_robust$gender, data_robust$outlier)$expected #assumption violated: expected values < 5
t.test(data_robust$age, data$age) #not significant
(mean(data_robust$age) - mean(data$age)) / sd(c(data_robust$age, data$age)) #Cohen's d
t.test(data$gender, data_robust$gender)
(mean(data_robust$gender) - mean(data$gender)) / sd(c(data_robust$gender, data$gender)) #Cohen's d
t.test(data$books, data_robust$books)
(mean(data_robust$books) - mean(data$books)) / sd(c(data_robust$books, data$books)) #Cohen's d
#Descriptive statistics for completed participants with outliers in SES included
mean(data_robust$age)
sd(data_robust$age)
table(data_robust$gender)
table(83 / 222, 139 / 222)
mean(data_robust$gender)
sd(data_robust$gender)
mean(data_robust$books)
sd(data_robust$books)
range(data_robust$books)

#Only for analysis of results with outliers included:
#data <- data_robust
#Only for robustness analysis of results without understandability <= 4:
#data <- filter(data, understandability > 4)
#For selectivity analysis of self-esteem: data = partly!
#data <- partly

###Descriptive statistics for the analysis sample###
mean(data$age)
median(data$age)
sd(data$age)
table(data$age)
table(data$gender)
table(80 / 217, 137 / 217)
mean(data$gender)
sd(data$gender)
mean(data$books)
sd(data$books)
range(data$books)

###check for Understandability###
mean(data$understandability)
table(data$understandability)
49+52+69
170/217 #proportion of participants with understandability >= 8
data$id[data$understandability <= 4]

#####################################################
###########Prepare variables for analysis############
#####################################################

###Mean centering age, gender, SES###
data$age <- scale(as.numeric(data$age), scale = FALSE)[, 1]
data$books <- scale(as.numeric(data$books), scale = FALSE)[, 1]
data$gender <- scale(as.numeric(data$gender), scale = FALSE)[, 1]

data$books <- data$books / 100 #scale SES
####plit SES into categories###
quantile(data$ses, probs = seq(0, 1, by = 0.25)) #quartiles in real values of SES before data transformation
data$ses2 <- cut(data$ses,
                        breaks = c(quantile(data$ses, probs = seq(0, 1, by = 0.25))),
                        labels = c('Very Low [0-40]', "Low (40-109]", 'High (109-250]', 'Very High (250-1400]'), include.lowest=TRUE)
217/4 #approximate number of participants in each category
table(data$ses2)
#Regard allocation to categories
sort(data$ses[data$ses2 == "Very Low [0-40]"])
sort(data$ses[data$ses2 == "Low (40-109]"])
sort(data$ses[data$ses2 == "High (109-250]"])
sort(data$ses[data$ses2 == "Very High (250-1400]"])

###Reverse reverse-coded questions of self-esteem###
data$selfesteem_gen_2R <- 8 - data$selfesteem_gen_2R
data$selfesteem_gen_3R <- 8 - data$selfesteem_gen_3R
data$selfesteem_cont_1R <- 8 - data$selfesteem_cont_1R
data$selfesteem_crit_2R <- 8 - data$selfesteem_crit_2R
data$selfesteem_sa_2R <- 8 - data$selfesteem_sa_2R
data$selfesteem_crit_1R <- 8 - data$selfesteem_crit_1R
data$selfesteem_cont_4R <- 8 - data$selfesteem_cont_4R
data$selfesteem_cont_5R <- 8 - data$selfesteem_cont_5R
data$selfesteem_crit_4R <- 8 - data$selfesteem_crit_4R
data$selfesteem_sa_5R <- 8 - data$selfesteem_sa_5R

###Calculate means and mean center mean columns for self-esteem###
generalcolumns <- select(data, starts_with("selfesteem_gen"))
data$selfesteem_general_true <- rowMeans(data[, colnames(generalcolumns)])
data$selfesteem_general <- scale(as.numeric(data$selfesteem_general_true), scale = FALSE)[, 1]
socialcolumns <- select(data, starts_with("selfesteem_cont") | starts_with("selfesteem_crit"))
data$selfesteem_social_true <- rowMeans(data[, colnames(socialcolumns)])
data$selfesteem_social <- scale(as.numeric(data$selfesteem_social_true), scale = FALSE)[, 1]
academiccolumns <- select(data, starts_with("selfesteem_sa"))
data$selfesteem_academic_true <- rowMeans(data[, colnames(academiccolumns)])
data$selfesteem_academic <- scale(as.numeric(data$selfesteem_academic_true), scale = FALSE)[, 1]

###Calculate means and mean center mean columns for the 4C skills###
creativitycolumns <- select(data, starts_with("bessi_crea"))
data$skill_creativity_true <- rowMeans(data[, colnames(creativitycolumns)])
data$skill_creativity <- scale(as.numeric(data$skill_creativity_true), scale = FALSE)[, 1]
decisioncolumns <- select(data, starts_with("bessi_deci"))
data$skill_decision_true <- rowMeans(data[, colnames(decisioncolumns)])
data$skill_decision <- scale(as.numeric(data$skill_decision_true), scale = FALSE)[, 1]
expressioncolumns <- select(data, starts_with("bessi_expr"))
data$skill_expression_true <- rowMeans(data[, colnames(expressioncolumns)])
data$skill_expression <- scale(as.numeric(data$skill_expression_true), scale = FALSE)[, 1]
teamcolumns <- select(data, starts_with("bessi_team"))
data$skill_team_true <- rowMeans(data[, colnames(teamcolumns)])
data$skill_team <- scale(as.numeric(data$skill_team_true), scale = FALSE)[, 1]

#####################################################
################ANALYSE##############################
#####################################################

### 4C skills - descriptive statistics###
#average inter-item correlation = average_r, (good if between 0.15 and 0.5)
#cronbach's alpha (internal consistency) = raw_alpha, (>9=great, >8=good, >7=acceptable)
skill_means <- c(round(mean(data$skill_creativity_true), 2), round(mean(data$skill_decision_true), 2), round(mean(data$skill_expression_true), 2), round(mean(data$skill_team_true), 2)) # nolint: line_length_linter.
skill_sd <- c(round(sd(data$skill_creativity_true), 2), round(sd(data$skill_decision_true), 2), round(sd(data$skill_expression_true), 2), round(sd(data$skill_team_true), 2)) # nolint: line_length_linter.
skill_min <- c(round(min(data$skill_creativity_true), 2), round(min(data$skill_decision_true), 2), round(min(data$skill_expression_true), 2), round(min(data$skill_team_true), 2)) # nolint: line_length_linter.
skill_max <- c(round(max(data$skill_creativity_true), 2), round(max(data$skill_decision_true), 2), round(max(data$skill_expression_true), 2), round(max(data$skill_team_true), 2)) # nolint: line_length_linter.

skill_cronbach <- round(c(psych::alpha(creativitycolumns)$total$raw_alpha, psych::alpha(decisioncolumns)$total$raw_alpha, psych::alpha(expressioncolumns)$total$raw_alpha, psych::alpha(teamcolumns)$total$raw_alpha), 2)
skill_average_r <- round(c(psych::alpha(creativitycolumns)$total$average_r, psych::alpha(decisioncolumns)$total$average_r, psych::alpha(expressioncolumns)$total$average_r, psych::alpha(teamcolumns)$total$average_r), 2)
gt(data.table(Skill = c("Creativity ", "Critical thinking ", "Communication ", "Collaboration "),  # nolint
                " " = c("  ", "  ", "  ", "  "), M = skill_means, SD = skill_sd, Min = skill_min, Max = skill_max,
                "  " = c("  ", "  ", "  ", "  "), "Average r" = skill_average_r, "Cronbach's alpha" = skill_cronbach))

### self-esteem - descriptive statistics###
se_means <- c(round(mean(data$selfesteem_general_true), 2), round(mean(data$selfesteem_academic_true), 2), round(mean(data$selfesteem_social_true), 2)) # nolint: line_length_linter.
se_sd <- c(round(sd(data$selfesteem_general_true), 2), round(sd(data$selfesteem_academic_true), 2), round(sd(data$selfesteem_social_true), 2)) # nolint: line_length_linter.
se_min <- c(round(min(data$selfesteem_general_true), 2), round(min(data$selfesteem_academic_true), 2), round(min(data$selfesteem_social_true), 2)) # nolint: line_length_linter.
se_max <- c(round(max(data$selfesteem_general_true), 2), round(max(data$selfesteem_academic_true), 2), round(max(data$selfesteem_social_true), 2)) # nolint: line_length_linter.

se_cronbach <- round(c(psych::alpha(generalcolumns)$total$raw_alpha, psych::alpha(academiccolumns)$total$raw_alpha, psych::alpha(socialcolumns)$total$raw_alpha), 2)
se_average_r <- round(c(psych::alpha(generalcolumns)$total$average_r, psych::alpha(academiccolumns)$total$average_r, psych::alpha(socialcolumns)$total$average_r), 2)
gt(t <- data.table("Self-esteem" = c("General self-esteem", "Academic self-esteem", "Social self-esteem"),  # nolint
                " " = c("  ", "  ", "  "), M = se_means, SD = se_sd, Min = se_min, Max = se_max,
                "   " = c(" ", "  ", "  "), "Average r" = se_average_r, "Cronbach's alpha" = se_cronbach))

###For Selectivity analysis ONLY: data = partly completed data! ###
range(data$selfesteem_general_true[!is.na(data$selfesteem_general_true)])
range(data$selfesteem_academic_true[!is.na(data$selfesteem_academic_true)])
range(data$selfesteem_social_true[!is.na(data$selfesteem_social_true)])
mean(data$selfesteem_general_true[!is.na(data$selfesteem_general_true)])
mean(data$selfesteem_academic_true[!is.na(data$selfesteem_academic_true)])
mean(data$selfesteem_social_true[!is.na(data$selfesteem_social_true)])
sd(data$selfesteem_general_true[!is.na(data$selfesteem_general_true)])
sd(data$selfesteem_academic_true[!is.na(data$selfesteem_academic_true)])
sd(data$selfesteem_social_true[!is.na(data$selfesteem_social_true)])

################################################################
### Regression models ###
#########################################################
#dependent variables:
#     skill_decision, skill_expression, skill_team, skill_creativity
#independent variables:
# 1. All three: age, gender, SES
# 2. Either general, academic, or social selfesteem
# 3. Either general, academic, or social selfesteem
#     plus all three sociodemographic variables
#     plus all three possible interactions -> Then only significant interactions
# Skill = β0 + β1(self-esteem) + β2(age) + β3(gender) + β4(SES) + β5(self-esteem × age) + β6(self-esteem × gender) + β7(self-esteem × SES) + r

###Test 4 Assumptions for regression models:###
#Linearity: linear relationship between independent variable and dependent variable
#Independence: observations of multiple predictors are independent
#Normality: dependent variables normally distributed
#Homoscedasticity: same variance in dependent variable across predictor variable

###Linearity: residuals vs. fitted on a straight line at zero
#DONE: Linearity: some not completely linear, but good enough
plot(lm(skill_creativity ~ age, data = data))
plot(lm(skill_creativity ~ gender, data = data))
plot(lm((skill_creativity) ~ (books), data = data)) #linearity not perfect: further graphical analysis:
plot(data$books, data$skill_creativity)
abline(lm(data$books ~ data$skill_creativity), col = "red") #Good
plot(lm(skill_creativity ~ (selfesteem_general), data = data)) #linearity not perfect: further graphical analysis:
plot(data$skill_creativity, data$selfesteem_general)
abline(lm(data$selfesteem_general ~ data$skill_creativity), col = "red") #Good
plot(lm((skill_creativity) ~ selfesteem_academic, data = data))
plot(lm((skill_creativity) ~ selfesteem_social, data = data))

plot(lm(skill_decision ~ age, data = data))
plot(lm(skill_decision ~ gender, data = data))
plot(lm((skill_decision) ~ (books), data = data)) #linearity not perfect: further graphical analysis:
plot(data$books, data$skill_decision)
abline(lm(data$books ~ data$skill_decision), col = "red") #Good
plot(lm((skill_decision) ~ selfesteem_general, data = data)) #linearity not perfect: further graphical analysis:
plot(data$selfesteem_general, data$skill_decision)
abline(lm(data$selfesteem_general ~ data$skill_decision), col = "red") #Good
plot(lm((skill_decision) ~ selfesteem_academic, data = data)) #linearity not perfect: further graphical analysis:
plot(data$selfesteem_academic, data$skill_decision)
abline(lm(data$selfesteem_academic ~ data$skill_decision), col = "red") #Good
plot(lm((skill_decision) ~ selfesteem_social, data = data))

plot(lm(skill_expression ~ age, data = data))
plot(lm(skill_expression ~ gender, data = data))
plot(lm((skill_expression) ~ (books), data = data))
plot(lm(skill_expression ~ (selfesteem_general), data = data))
plot(lm((skill_expression) ~ selfesteem_academic, data = data)) #linearity not perfect: further graphical analysis:
plot(data$selfesteem_academic, data$skill_expression)
abline(lm(data$selfesteem_academic ~ data$skill_expression), col = "red") #Good
plot(lm((skill_expression) ~ selfesteem_social, data = data)) #linearity not perfect: further graphical analysis:
plot(data$selfesteem_social, data$skill_expression)
abline(lm(data$selfesteem_social ~ data$skill_expression), col = "red") #Good

plot(lm(skill_team ~ age, data = data))
plot(lm(skill_team ~ gender, data = data))
plot(lm((skill_team) ~ (books), data = data)) #linearity not perfect: further graphical analysis:
plot(data$books, data$skill_team)
abline(lm(data$books ~ data$skill_team), col = "red") #Good
plot(lm(skill_team ~ (selfesteem_general), data = data))
plot(lm((skill_team) ~ selfesteem_academic, data = data))#Linearity. not very good!
plot(data$skill_team, data$selfesteem_academic)
abline(lm(data$selfesteem_academic ~ data$skill_team), col = "red")
plot(lm((skill_team) ~ selfesteem_social, data = data))

###Independence of observations (correlation between independent variables)###
#DONE: no problematic correlations between a predictor and other independent variables in the models!! (1 = superp, 1-5=okay)
vif(lm(skill_creativity ~ age + gender + books, data = data), type = "terms")
vif(lm(skill_creativity ~ selfesteem_general + age + gender + books +
selfesteem_general * age +
selfesteem_general * gender +
selfesteem_general * books, data = data), type = "terms")
vif(lm(skill_creativity ~ selfesteem_academic + age + gender + books +
selfesteem_academic * age +
selfesteem_academic * gender +
selfesteem_academic * books, data = data), type = "terms")
vif(lm(skill_creativity ~ selfesteem_social + age + gender + books +
selfesteem_social * age +
selfesteem_social * gender +
selfesteem_social * books, data = data), type = "terms")

vif(lm(skill_decision ~ age + gender + books, data = data), type = "terms")
vif(lm(skill_decision ~ selfesteem_general + age + gender + books +
selfesteem_general * age +
selfesteem_general * gender +
selfesteem_general * books, data = data), type = "terms")
vif(lm(skill_decision ~ selfesteem_academic + age + gender + books +
selfesteem_academic * age +
selfesteem_academic * gender +
selfesteem_academic * books, data = data), type = "terms")
vif(lm(skill_decision ~ selfesteem_social + age + gender + books +
selfesteem_social * age +
selfesteem_social * gender +
selfesteem_social * books, data = data), type = "terms")

vif(lm(skill_expression ~ age + gender + books, data = data), type = "terms")
vif(lm(skill_expression ~ selfesteem_general + age + gender + books +
selfesteem_general * age +
selfesteem_general * gender +
selfesteem_general * books, data = data), type = "terms")
vif(lm(skill_expression ~ selfesteem_academic + age + gender + books +
selfesteem_academic * age +
selfesteem_academic * gender +
selfesteem_academic * books, data = data), type = "terms")
vif(lm(skill_expression ~ selfesteem_social + age + gender + books +
selfesteem_social * age +
selfesteem_social * gender +
selfesteem_social * books, data = data), type = "terms")

vif(lm(skill_team ~ age + gender + books, data = data), type = "terms")
vif(lm(skill_team ~ selfesteem_general + age + gender + books +
selfesteem_general * age +
selfesteem_general * gender +
selfesteem_general * books, data = data), type = "terms")
vif(lm(skill_team ~ selfesteem_academic + age + gender + books +
selfesteem_academic * age +
selfesteem_academic * gender +
selfesteem_academic * books, data = data), type = "terms")
vif(lm(skill_team ~ selfesteem_social + age + gender + books +
selfesteem_social * age +
selfesteem_social * gender +
selfesteem_social * books, data = data), type = "terms")

###Normality (normal distribution of dependent variables)###
#DONE: Some tests = significant, but graphical analysis shows that roughly normally distributed...
shapiro.test(data$skill_creativity) #normally distributed (p > .05)
hist(data$skill_creativity)

shapiro.test(data$skill_decision) #p-value less than .05: NOT normally distributed!
hist(data$skill_decision)
qqnorm(data$skill_decision)
qqline(data$skill_decision) #Points along a straight line

shapiro.test(data$skill_expression) #normally distributed (p > .05)
hist(data$skill_expression)

shapiro.test(data$skill_team) #p-value less than .05: NOT normally distributed!
hist(data$skill_team)
qqnorm(data$skill_decision)
qqline(data$skill_decision) #Points along a straight line

###Homoscedasticity (non-constant variance?)###
# p < .05: Heteroscedasticity!
#DONE: only problematic with Model 3.1 for decision/critical thinking and with Model 2.2 for critical thinking
#SOLUTION: Use weighted regression: imposing less weight on part of data with high variance
ncvTest(lm(skill_creativity ~ age + gender + books, data = data), type = "terms")
ncvTest(lm(skill_creativity ~ selfesteem_general, data = data), type = "terms")
ncvTest(lm(skill_creativity ~ selfesteem_academic, data = data), type = "terms")
ncvTest(lm(skill_creativity ~ selfesteem_social, data = data), type = "terms")
ncvTest(lm(skill_creativity ~ selfesteem_general + age + gender + books +
selfesteem_general * age +
selfesteem_general * gender +
selfesteem_general * books, data = data), type = "terms")
ncvTest(lm(skill_creativity ~ selfesteem_academic + age + gender + books +
selfesteem_academic * age +
selfesteem_academic * gender +
selfesteem_academic * books, data = data), type = "terms")
ncvTest(lm(skill_creativity ~ selfesteem_social + age + gender + books +
selfesteem_social * age +
selfesteem_social * gender +
selfesteem_social * books, data = data), type = "terms")

ncvTest(lm(skill_decision ~ age + gender + books, data = data), type = "terms")
ncvTest(lm(skill_decision ~ selfesteem_general, data = data), type = "terms")
ncvTest(lm(skill_decision ~ selfesteem_academic, data = data), type = "terms") #p = .03!!!
ncvTest(lm(skill_decision ~ selfesteem_social, data = data), type = "terms")
ncvTest(lm(skill_decision ~ selfesteem_general + age + gender + books +
selfesteem_general * age +
selfesteem_general * gender +
selfesteem_general * books, data = data), type = "terms") #!!! p = .006
ncvTest(lm(skill_decision ~ selfesteem_academic + age + gender + books +
selfesteem_academic * age +
selfesteem_academic * gender +
selfesteem_academic * books, data = data), type = "terms")
ncvTest(lm(skill_decision ~ selfesteem_social + age + gender + books +
selfesteem_social * age +
selfesteem_social * gender +
selfesteem_social * books, data = data), type = "terms")

ncvTest(lm(skill_expression ~ age + gender + books, data = data), type = "terms") #!!! p < .02
ncvTest(lm(skill_expression ~ selfesteem_general, data = data), type = "terms")
ncvTest(lm(skill_expression ~ selfesteem_academic, data = data), type = "terms")
ncvTest(lm(skill_expression ~ selfesteem_social, data = data), type = "terms")
ncvTest(lm(skill_expression ~ selfesteem_general + age + gender + books +
selfesteem_general * age +
selfesteem_general * gender +
selfesteem_general * books, data = data), type = "terms")
ncvTest(lm(skill_expression ~ selfesteem_academic + age + gender + books +
selfesteem_academic * age +
selfesteem_academic * gender +
selfesteem_academic * books, data = data), type = "terms")
ncvTest(lm(skill_expression ~ selfesteem_social + age + gender + books +
selfesteem_social * age +
selfesteem_social * gender +
selfesteem_social * books, data = data), type = "terms")

ncvTest(lm(skill_team ~ age + gender + books, data = data), type = "terms")
ncvTest(lm(skill_team ~ selfesteem_general, data = data), type = "terms")
ncvTest(lm(skill_team ~ selfesteem_academic, data = data), type = "terms")
ncvTest(lm(skill_team ~ selfesteem_social, data = data), type = "terms")
ncvTest(lm(skill_team ~ selfesteem_general + age + gender + books +
selfesteem_general * age +
selfesteem_general * gender +
selfesteem_general * books, data = data), type = "terms")
ncvTest(lm(skill_team ~ selfesteem_academic + age + gender + books +
selfesteem_academic * age +
selfesteem_academic * gender +
selfesteem_academic * books, data = data), type = "terms")
ncvTest(lm(skill_team ~ selfesteem_social + age + gender + books +
selfesteem_social * age +
selfesteem_social * gender +
selfesteem_social * books, data = data), type = "terms") #!!! p < .008

###Problematic cases homoscedasticity: Use weighted Regression###
model <- lm(skill_decision ~ selfesteem_general + age + gender + books +
            selfesteem_general * age +
            selfesteem_general * gender +
            selfesteem_general * books, data = data)
variance <- lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2 #estimating the variance of y for different values of x
weights <- 1 / variance
weighted_model1 <- lm(skill_decision ~ selfesteem_general + age + gender + books +
                      selfesteem_general * age +
                      selfesteem_general * gender +
                      selfesteem_general * books, data = data, weights = weights)
ncvTest(weighted_model1, type = "terms")

model <- lm(skill_decision ~ selfesteem_academic, data = data)
variance <- lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2 #estimating the variance of y for different values of x
weights <- 1 / variance
weighted_model2 <- lm(skill_decision ~ selfesteem_academic, data = data, weights = weights)
ncvTest(weighted_model2, type = "terms")

###Only for analysis with SES-outliers included: Further weighted regression models!###
model <- lm(skill_expression ~ age + gender + books, data = data)
variance <- lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2 #estimating the variance of y for different values of x
weights <- 1 / variance
weighted_model3 <- lm(skill_expression ~ age + gender + books, data = data, weights = weights)
ncvTest(weighted_model3, type = "terms")

model <- lm(skill_team ~ age + gender + books + selfesteem_social +
            selfesteem_social * age +
            selfesteem_social * gender +
            selfesteem_social * books, data = data)
variance <- lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2 #estimating the variance of y for different values of x
weights <- 1 / variance
weighted_model4 <- lm(skill_team ~ age + gender + books + selfesteem_social +
                      selfesteem_social * age +
                      selfesteem_social * gender +
                      selfesteem_social * books, data = data, weights = weights)
ncvTest(weighted_model4, type = "terms")

model <- lm(skill_decision ~ selfesteem_general, data = data)
variance <- lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2 #estimating the variance of y for different values of x
weights <- 1 / variance
weighted_model5 <- lm(skill_decision ~ selfesteem_general, data = data, weights = weights)
ncvTest(weighted_model5, type = "terms")

####################################################################
######################
####### Models #######
######################
####################################################################

### 1. Skill = β0 + β2(age) + β3(gender) + β4(SES) + r ###
creativity_socios <- lm(skill_creativity ~ age + gender + books, data = data)
summary(creativity_socios)
tab_model(creativity_socios)

decision_socios <- lm(skill_decision ~ age + gender + books, data = data)
summary(decision_socios)
tab_model(decision_socios)

#Only with outliers included: Weighted regression because of heteroscedasticity!
#expression_socios <- weighted_model3
expression_socios <- lm(skill_expression ~ age + gender + books, data = data)
summary(expression_socios)
tab_model(expression_socios)

team_socios <- lm(skill_team ~ age + gender + books, data = data)
summary(team_socios)
tab_model(team_socios)

############################################################
#### 2. Skill = β0 + β1(self-esteem) + r ###
creativity_general <- lm(skill_creativity ~ selfesteem_general, data = data)
summary(creativity_general)
creativity_academic <- lm(skill_creativity ~ selfesteem_academic, data = data)
summary(creativity_academic)
creativity_social <- lm(skill_creativity ~ selfesteem_social, data = data)
summary(creativity_social)
tab_model(creativity_general)
tab_model(creativity_academic)
tab_model(creativity_social)

decision_general <- lm(skill_decision ~ selfesteem_general, data = data)
summary(decision_general)
decision_academic <- weighted_model2 #Weighted regression because of heteroscedasticity!
summary(decision_academic)
decision_social <- lm(skill_decision ~ selfesteem_social, data = data)
summary(decision_social)
tab_model(decision_general)
tab_model(decision_academic)
tab_model(decision_social)

expression_general <- lm(skill_expression ~ selfesteem_general, data = data)
summary(expression_general)
expression_academic <- lm(skill_expression ~ selfesteem_academic, data = data)
summary(expression_academic)
expression_social <- lm(skill_expression ~ selfesteem_social, data = data)
summary(expression_social)
tab_model(expression_general)
tab_model(expression_academic)
tab_model(expression_social)

team_general <- lm(skill_team ~ selfesteem_general, data = data)
summary(team_general)
team_academic <- lm(skill_team ~ selfesteem_academic, data = data)
summary(team_academic)
team_social <- lm(skill_team ~ selfesteem_social, data = data)
summary(team_social)
tab_model(team_general)
tab_model(team_academic)
tab_model(team_social)

########################################################################
### 3. Skill = β0 + β1(self-esteem) + β2(age) + β3(gender) + β4(SES) + β5(self-esteem × age) + β6(self-esteem × gender) + β7(self-esteem × SES) + r ###
creativity_gs <- lm(skill_creativity ~ age + gender + books + selfesteem_general +
                    selfesteem_general * age +
                    selfesteem_general * gender +
                    selfesteem_general * books, data = data)
(summary(creativity_gs))
#Adjust model: exclude non-significant interaction effects
creativity_gs <- lm(skill_creativity ~ age + gender + books + selfesteem_general, data = data)
(summary(creativity_gs))
creativity_as <- lm(skill_creativity ~ age + gender + books + selfesteem_academic +
                    selfesteem_academic * age +
                    selfesteem_academic * gender +
                    selfesteem_academic * books, data = data)
summary(creativity_as)
#Adjust model: exclude non-significant interaction effects
creativity_as <- lm(skill_creativity ~ age + gender + books + selfesteem_academic, data = data)
(summary(creativity_as))
creativity_ss <- lm(skill_creativity ~ age + gender + books + selfesteem_social +
                    selfesteem_social * age +
                    selfesteem_social * gender +
                    selfesteem_social * books, data = data)
summary(creativity_ss)
#Adjust model: exclude non-significant interaction effects
creativity_ss <- lm(skill_creativity ~ age + gender + books + selfesteem_social, data = data)
(summary(creativity_ss))

tab_model(creativity_gs)
tab_model(creativity_as)
tab_model(creativity_ss)

#######################################################################
decision_gs <- weighted_model1 #Weighted regression because of heteroscedasticity!
summary(decision_gs)
#Adjust model: exclude non-significant interaction effects - !with weighted regression!
model <- lm(skill_decision ~ selfesteem_general + age + gender + books + books * selfesteem_general, data = data)
variance <- lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2
weights <- 1 / variance
weighted_model1 <- lm(skill_decision ~ selfesteem_general + age + gender + books + books * selfesteem_general, data = data, weights = weights)
decision_gs <- weighted_model1
(summary(decision_gs))
decision_as <- lm(skill_decision ~
                      selfesteem_academic + age + gender + books +
                        age * selfesteem_academic +
                        gender * selfesteem_academic +
                        books * selfesteem_academic, data = data)
summary(decision_as)
#Adjust model: exclude non-significant interaction effects
decision_as <- lm(skill_decision ~ age + gender + books + selfesteem_academic +
                  selfesteem_academic * gender, data = data)
(summary(decision_as))
decision_ss <- lm(skill_decision ~
                      selfesteem_social + age + gender + books +
                        selfesteem_social * age +
                        selfesteem_social * gender +
                        selfesteem_social * books, data = data)
summary(decision_ss)
#Adjust model: exclude non-significant interaction effects
decision_ss <- lm(skill_decision ~ age + gender + books + selfesteem_social +
                  books * selfesteem_social, data = data)
(summary(decision_ss))

tab_model(decision_gs)
tab_model(decision_as)
tab_model(decision_ss)

###Figure 1###
#SIGNIFICANT Interaction for significant main effects of sociodemographics! General * ses
model3.1 <- plot_model(lm(skill_decision_true ~ agetrue + as.numeric(gender_numeric) + ses2 + selfesteem_general_true +
                        selfesteem_general_true * ses2, data = data),
                       type = "pred",
                       terms = c("selfesteem_general_true", "ses2"),
                       axis.title = c("General Self-Esteem", "Critical Thinking Skills"),
                       legend.title = "SES",
                       colors = c("#398186", "#588fce", "#482d6d", "red"),
                       title = "SES x General Self-Esteem", # nolint
                       jitter = TRUE,
                       line.size = 1.5,
                       axis.lim = c(2, 7))
#SIGNIFICANT Interaction for significant main effects of sociodemographics! Academic * gender
model3.2 <- plot_model(lm(skill_decision_true ~ agetrue + gendertrue + ses + selfesteem_academic_true +
                        selfesteem_academic_true * gendertrue, data = data),
                       type = "pred",
                       terms = c("selfesteem_academic_true", "gendertrue"),
                       axis.title = c("Academic Self-Esteem", "Critical Thinking Skills"),
                       legend.title = "Gender",
                       title = "Gender x Academic Self-Esteem",
                       jitter = TRUE,
                       line.size = 1.5,
                       axis.lim = c(2, 7))
#SIGNIFICANT Interaction for significant main effects of sociodemographics! Social * ses
model3.3 <- plot_model(lm(skill_decision_true ~ agetrue + as.numeric(gender_numeric) + ses2 + selfesteem_social_true +
                          selfesteem_social_true * ses2, data = data),
                       type = "pred",
                       terms = c("selfesteem_social_true", "ses2"),
                       axis.title = c("Social Self-Esteem", "Critical Thinking Skills"),
                       legend.title = "SES",
                       colors = c("#398186", "#588fce", "#482d6d", "red"),
                       title = "SES x Social Self-Esteem",
                       jitter = TRUE,
                       line.size = 1.5,
                       axis.lim = c(2, 7))
#Save Figure
png(filename = "simple_graphic.png", res = 300, width = 3000, height = 2100)
model3.1 + theme(legend.position = c(0.56, 0.88), #change legend key size
        legend.title = element_text(size = 12), #change legend title font size
        legend.text = element_text(size = 12)) +
        theme(text = element_text(size = 12)) +
model3.2 + theme(legend.position = c(0.6, 0.88), #change legend key size
        legend.title = element_text(size = 12), #change legend title font size
        legend.text = element_text(size = 12)) +
        theme(text = element_text(size = 12)) +
model3.3 + theme(legend.position = c(0.56, 0.88), #change legend key size
        legend.title = element_text(size = 12), #change legend title font size
        legend.text = element_text(size = 12)) +
        theme(text = element_text(size = 12))
dev.off()

###################################################################
expression_gs <- lm(skill_expression ~ age + gender + books + selfesteem_general +
                    age * selfesteem_general +
                    gender * selfesteem_general +
                    books * selfesteem_general, data = data)
summary(expression_gs)
#Adjust model: exclude non-significant interaction effects
expression_gs <- lm(skill_expression ~ age + gender + books + selfesteem_general, data = data)
summary(expression_gs)
expression_as <- lm(skill_expression ~ age + gender + books + selfesteem_academic +
                    age * selfesteem_academic +
                    gender * selfesteem_academic +
                    books * selfesteem_academic, data = data)
summary(expression_as)
#Adjust model: exclude non-significant interaction effects
expression_as <- lm(skill_expression ~ age + gender + books + selfesteem_academic, data = data)
summary(expression_as)
expression_ss <- lm(skill_expression ~ age + gender + books + selfesteem_social +
                    age * selfesteem_social +
                    gender * selfesteem_social +
                    books * selfesteem_social, data = data)
summary(expression_ss)
#Adjust model: exclude non-significant interaction effects
expression_ss <- lm(skill_expression ~ age + gender + books + selfesteem_social, data = data)
summary(expression_ss)

tab_model(expression_gs)
tab_model(expression_as)
tab_model(expression_ss)

####################################################################
team_gs <- lm(skill_team ~ age + gender + books + selfesteem_general +
                    age * selfesteem_general +
                    gender * selfesteem_general +
                    books * selfesteem_general, data = data)
summary(team_gs)
#Adjust model: exclude non-significant interaction effects
team_gs <- lm(skill_team ~ age + gender + books + selfesteem_general, data = data)
summary(team_gs)
team_as <- lm(skill_team ~ age + gender + books + selfesteem_academic +
                    age * selfesteem_academic +
                    gender * selfesteem_academic +
                    books * selfesteem_academic, data = data)
summary(team_as)
#Adjust model: exclude non-significant interaction effects
team_as <- lm(skill_team ~ age + gender + books + selfesteem_academic, data = data)
summary(team_as)
team_ss <- lm(skill_team ~ age + gender + books + selfesteem_social +
                    age * selfesteem_social +
                    gender * selfesteem_social +
                    books * selfesteem_social, data = data)
summary(team_ss)
#Adjust model: exclude non-significant interaction effects
team_ss <- lm(skill_team ~ age + gender + books + selfesteem_social, data = data)
summary(team_ss)

tab_model(team_gs)
tab_model(team_as)
tab_model(team_ss)