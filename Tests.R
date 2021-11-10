library(lme4)
library(psych)
library(dplyr)

###########################################
###########################################
# PERFORMANCES 
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

#######################
# Last two training rounds
#######################

data_training = data[data['stage']=='Training rounds',]
data_training = data_training[data_training['round']>23,]
# head(data_training)
describeBy(data_training$accuracy, data_training$treatment)
model = glmer(
  'accuracy ~ treatment + (1|round)', 
  data = data_training, 
  family = binomial
)
summary(model)

#######################
# All training rounds
#######################

data_training = data[data['stage']=='Training rounds',]
# head(data_training)
describeBy(data_training$accuracy, data_training$treatment)
model = glmer(
  'accuracy ~ treatment + (1|round)', 
  data = data_training, 
  family = binomial
  )
summary(model)

#######################
# Game rounds experts
#######################

data_game_experts = data[data['stage']=='Game rounds',]
data_game_experts = data_game_experts[data_game_experts['expert_dog']=='True',]
# head(data_game_experts)
describeBy(data_game_experts$accuracy, data_game_experts$treatment)
model = glmer(
  'accuracy ~ treatment + (1|round)', 
  data = data_game_experts, 
  family = binomial
)
summary(model)

#######################
# Game rounds novices
#######################

data_game_novices = data[data['stage']=='Game rounds',]
data_game_novices = data_game_novices[data_game_novices['expert_dog']=='False',]
# head(data_game_novices)
describeBy(data_game_novices$accuracy, data_game_novices$treatment)
model = glmer(
  'accuracy ~ treatment + (1|round)', 
  data = data_game_novices, 
  family = binomial
)
summary(model)

################################################
# Game rounds paired novices query vs no query
################################################

data_game_novices = data[data['stage']=='Game rounds',]
data_game_novices = data_game_novices[data_game_novices['treatment']=='paired',]
data_game_novices = data_game_novices[data_game_novices['expert_dog']=='False',]
# head(data_game_novices)
describeBy(data_game_novices$accuracy, data_game_novices$queried)
model = glmer(
  'accuracy ~ queried + (1|round)', 
  data = data_game_novices, 
  family = binomial
)
summary(model)

################################################
# Game rounds paired novices linear regression
################################################

fun_internal <- function(queried,accuracy) {
  if (queried == 1 & accuracy == 1) {return(1)}
  else {return(0)}
} 
df <- data %>% 
  filter(stage == 'Game rounds') %>%
  filter(treatment == 'paired') %>%
  filter(expert_dog == 'False') %>%
  select(player,round,queried,accuracy) %>%
  mutate(
    internal_accu = (1-queried)*accuracy,
    external_accu = queried*accuracy
  )
head(df)
df1 <- df %>% 
  group_by(player,round) %>%
  summarize(
    queried = mean(queried),
    acc_internal_ = mean(internal_accu),
    acc_external_ = mean(external_accu)
  )
df1$aux <- 1
df1$new_use <- lead(df1$queried, 1)
head(df1)
df2 <- df1 %>% 
  mutate(
    acc_internal = cumsum(acc_internal_)/cumsum(aux),
    acc_external = cumsum(acc_external_)/cumsum(aux)
  ) %>%
  select(player,round,new_use,queried,acc_internal,acc_external)
head(df2)
model = lm(new_use ~ queried + acc_internal + acc_external, data = df2)
summary(model)
par(mfrow = c(2, 2))
plot(model)

######### CHECK THE PROBLEMS WITH THE ASSUMPTIONS

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
  select(treatment,player,report,accuracy)
# head(df)
describeBy(df$report, df$treatment)
x = unlist(df %>% filter(treatment == 'paired') %>% select(report))
y = unlist(df %>% filter(treatment == 'solo') %>% select(report))
t.test(x, y, alternative = "two.sided", var.equal = FALSE)
wilcox.test(x, y, alternative = "two.sided")

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
x = unlist(df_experts['report'])
y = unlist(df_experts['player_responded'])
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
