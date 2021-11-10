require(lme4)

data = read.csv('performances.csv')
data$treatment = as.factor(data$treatment)


#######################
# Last two training rounds
#######################

data_training = data[data['stage']=='Training rounds',]
data_training = data_training[data_training['round']>23,]
head(data_training)

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
head(data_training)

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
head(data_game_experts)

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
head(data_game_novices)

model = glmer(
  'accuracy ~ treatment + (1|round)', 
  data = data_game_novices, 
  family = binomial
)
summary(model)
