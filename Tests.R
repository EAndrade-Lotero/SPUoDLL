# library(lme4)
library(psych)
library(dplyr)
library(nlme)
library(tidyverse) # data wrangling and visualization
library(sjPlot)    # to visualizing mixed-effects models
library(effects)   # to visualizing mixed-effects models
library(lmerTest)  # p-values for MEMs based on the Satterthwaite approximation
library(report)    # mainly for an "report" function
library(emmeans)   # post-hoc analysis
library(knitr)     # beautifying tables
library(sjstats)   # ICC - intraclass-correlation coefficient
library(caret)     # ML, model comparison & utility functions
library(ggplot2)
library(lawstat)
library(comprehenr)

###########################################
###########################################
# LOAD DATA FROM PERFORMANCES 
###########################################
###########################################
data = read.csv('performances.csv')
data$treatment = as.factor(data$treatment)
data$queried = as.integer(data$queried)
fun <- function(x) {
  if (is.na(x)) {return(0)}
  else if (x==0) {return(0)}
  else {return(1)}
}
data$queried = unlist(lapply(data$queried, FUN = fun))

############################################################
# TESTS MAIN TEXT 4.1 AND SUPPLEMENTARY MATERIAL SECTION 2.1
############################################################

############################
# Last three Training rounds
############################

data_training = data[data['stage']=='Training rounds',]
data_training = data_training[data_training['round']>22,]
head(data_training)
describeBy(data_training$accuracy, data_training$treatment)
model = glmer(
  'accuracy ~ treatment + round + (round|player) + (1|player) + (1|object)', 
  data = data_training, 
  family = binomial
)
summary(model)

##################
# Training rounds
##################

data_training = data[data['stage']=='Training rounds',]
head(data_training)
describeBy(data_training$accuracy, data_training$treatment)
model = glmer(
  'accuracy ~ treatment + round + (round|player) + (1|player) + (1|object)', 
  data = data_training, 
  family = binomial
  )
summary(model)

#########################
# Game rounds expert dogs
#########################

data_game_experts = data %>% 
  filter(stage == 'Game rounds') %>%
  filter(expert_dog == 'True')
head(data_game_experts)
describeBy(data_game_experts$accuracy, data_game_experts$treatment)
model = glmer(
  'accuracy ~ treatment + round + (round|player) + (1|player) + (1|object)', 
  data = data_game_experts, 
  family = binomial
)
summary(model)

#########################
# Game rounds novice dogs
#########################

data_game_novices = data %>% 
  filter(stage == 'Game rounds') %>%
  filter(expert_dog == 'False')
head(data_game_novices)
describeBy(data_game_novices$accuracy, data_game_novices$treatment)
model = glmer(
  'accuracy ~ treatment + round + (round|player) + (1|player) + (1|object)', 
  data = data_game_novices, 
  family = binomial
)
summary(model)

######################
# TESTS MAIN TEXT 4.2
######################

#######################################
# Most active vs. Least active players
#######################################

df <- data %>% 
  filter(treatment == 'paired') %>%
  filter(stage == 'Game rounds') %>%
  select(dyad, player, queried) %>%
  group_by(dyad, player) %>%
  summarize(queried = sum(queried)) %>%
  ungroup() %>%
  group_by(dyad) %>%
  mutate(queriedMost = max(queried),
         queriedLeast = min(queried)) %>%
  ungroup()
df <- slice(df, seq(1, dim(df)[1], 2))
head(df)
dim(df)
plot(df$queriedMost, df$queriedLeast)
x = unlist(df$queriedMost)
y = unlist(df$queriedLeast)
cor.test(x, y, method=c("pearson", "kendall", "spearman"))


############################################################
# TESTS MAIN TEXT 4.3 AND SUPPLEMENTARY MATERIAL SECTION 2.2
############################################################

################################################
# Game rounds paired novices query vs no query
################################################

data_game_novices = data %>% 
  filter(stage == 'Game rounds') %>%
  filter(treatment=='paired') %>%
  filter(expert_dog == 'False')
head(data_game_novices)
describeBy(data_game_novices$accuracy, data_game_novices$queried)
model = glmer(
  'accuracy ~ queried + round + (round|player) + (1|player) + (1|object)', 
  data = data_game_novices, 
  family = binomial
)
summary(model)

###################################
# Linear mixed-effects regressions
# Game rounds paired novices 
# INTERNAL strategy
###################################
df <- data %>% 
  filter(stage == 'Game rounds') %>%
  filter(treatment == 'paired') %>%
  filter(expert_dog == 'False') %>%
  select(queried,player,round,accuracy) %>%
  group_by(player,round) %>%
  mutate(
    av_queried = mean(queried)
  ) %>%
  ungroup %>%
  arrange(player,round,queried)
head(df,10)
df <- df %>% 
  group_by(player,round,queried) %>%
  summarize(
    av_accuracy = mean(accuracy),
    av_queried = mean(av_queried)
  ) %>%
  select(player,round,queried,av_queried,av_accuracy)
head(df,10)
df1 <- df %>%
  spread(queried,av_accuracy) %>%
  group_by(player) %>%
  mutate(
    new_use = lead(av_queried),
    av_accuracy_no = `0`,
    av_accuracy_yes = `1`,
  ) %>%
  ungroup %>%
  select(player,round,av_accuracy_no,av_accuracy_yes,new_use)
head(df1,10)
df_no <- df1 %>%
  select(player,round,av_accuracy_no,new_use)
df_no <- df_no[complete.cases(df_no),]
head(df_no)
plot(df_no$av_accuracy_no, df_no$new_use)
model = lmer(new_use ~ av_accuracy_no + round + (round|player), data = df_no)
summary(model)

###################################
# Linear mixed-effects regressions
# Game rounds paired novices 
# EXTENDED strategy
###################################
df <- data %>% 
  filter(stage == 'Game rounds') %>%
  filter(treatment == 'paired') %>%
  filter(expert_dog == 'False') %>%
  select(queried,player,round,answered,accuracy) %>%
  group_by(player,round) %>%
  mutate(
    av_queried = mean(queried)
  ) %>%
  ungroup %>%
  arrange(player,round,queried)
head(df,10)
df <- df %>% 
  group_by(player,round,queried) %>%
  summarize(
    av_accuracy = mean(accuracy),
    av_queried = mean(av_queried),
    av_answered = mean(answered, na.rm=TRUE)
  ) %>%
  select(player,round,queried,av_queried,av_answered,av_accuracy)
head(df,10)
df1 <- df %>%
  select(player,round,queried,av_accuracy,av_queried) %>%
  spread(queried,av_accuracy) %>%
  group_by(player) %>%
  mutate(
    new_use = lead(av_queried),
    av_accuracy_yes = cummean(`1`),
  ) %>%
  ungroup %>%
  select(player,round,av_accuracy_yes,new_use)
head(df1,10)
df2 <- df %>%
  select(player,round,queried,av_answered) %>%
  spread(queried,av_answered) %>%
  mutate(
    av_answered = `1`
  ) %>% 
  select(player,round,av_answered) %>%
  group_by(player) %>%
  mutate(
    av_answered = cummean(av_answered)
  )
head(df2,10)
df_yes <- merge(df1,df2,by=c("player","round"))
df_yes <- df_yes %>% arrange(player,round)
df_yes <- df_yes[complete.cases(df_yes),]
head(df_yes,10)
model = lmer(new_use ~ av_accuracy_yes + round + (round|player), data = df_yes)
summary(model)


###########################################
###########################################
# SELF-REPORT OF UNDERSTANDING 
###########################################
###########################################
data = read.csv('rep-understanding.csv')
data$report = as.integer(data$report)
head(data)

#############################
# Paired vs. solo EXPERTS
#############################

# Difference of report means
df = data %>% 
  filter(expertise == 'experts') %>%
  select(treatment,player,report,accuracy_tr,accuracy_gr)
# head(df)
describeBy(df$report, df$treatment)
x = unlist(df %>% filter(treatment == 'paired') %>% select(report))
y = unlist(df %>% filter(treatment == 'solo') %>% select(report))
t.test(x, y, alternative = "two.sided", var.equal = FALSE)
wilcox.test(x, y, alternative = "two.sided")
model = lm(report ~ treatment + accuracy_tr + accuracy_gr, data = df)
summary(model)

# Correlations
df_experts = data %>% 
  filter(expertise == 'experts') %>%
  filter(treatment == 'paired') %>%
  filter(accuracy > .2) %>% # leave out outlier
  select(treatment,player,report,accuracy,player_responded)
head(df_experts)
x = unlist(df_experts['report'])
y = unlist(df_experts['accuracy'])
cor.test(x, y, method=c("pearson", "kendall", "spearman"))

# Correlations
df_experts = data %>% 
  filter(expertise == 'experts') %>%
  filter(treatment == 'solo') %>%
  select(treatment,player,report,accuracy,player_responded)
head(df_experts)
x = unlist(df_experts['report'])
y = unlist(df_experts['accuracy'])
cor.test(x, y, method=c("pearson", "kendall", "spearman"))

#############################
# Paired vs. solo NOVICES
#############################
df <- data %>% 
  filter(expertise == 'novices') %>%
  select(treatment,player,report,accuracy)
# head(df)
describeBy(df$report, df$treatment)
x = unlist(df %>% filter(treatment == 'paired') %>% select(report))
y = unlist(df %>% filter(treatment == 'solo') %>% select(report))
t.test(x, y, alternative = "two.sided", var.equal = FALSE)
wilcox.test(x, y, alternative = "two.sided")

# Correlations SOLO
df_novices = data %>% 
  filter(expertise == 'novices') %>%
  filter(treatment == 'solo') %>%
  select(treatment,player,report,accuracy)
head(df_novices)
x = unlist(df_novices['report'])
y = unlist(df_novices['accuracy'])
cor.test(x, y, method=c("pearson", "kendall", "spearman"))

# Correlations PAIRED
df_novices = data %>% 
  filter(expertise == 'novices') %>%
  filter(treatment == 'paired') %>%
  select(treatment,player,report,accuracy,answered)
head(df_novices)
x = unlist(df_novices['report'])
y = unlist(df_novices['accuracy'])
cor.test(x, y, method=c("pearson", "kendall", "spearman"))
y = unlist(df_novices['answered'])
cor.test(x, y, method=c("pearson", "kendall", "spearman"))

########################################
# Paired novices difference in variances
########################################
df_novices = data %>% 
  filter(expertise == 'novices') %>%
  filter(treatment == 'paired') %>%
  select(player,report,queried)
q <- list(.25,1)
quarts <- to_vec(for(i in q) quantile(df_novices$queried, i)[[1]])
quarts <- c(0,quarts)
quarts
df_novices = df_novices %>% 
  mutate(Group = cut(
    queried, 
    breaks = quarts, 
    labels = c('very low', 'normal'),
    include.lowest = TRUE
  )) 
describeBy(df_novices$report, df_novices$Group)
p <- ggplot(df_novices, aes(x=Group, y=report)) + 
  geom_violin()
p
var.test(report ~ Group, df_novices, 
         alternative = "less")
