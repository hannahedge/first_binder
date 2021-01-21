library(readr)
library(ggplot)
library(dplyr)
library(afex)
library(emmeans)

# READING IN OUR DATA

my_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/cond.csv")
head(my_data)
# We see that our experimental variable is not coded as a factor so we need to
# change that.

my_data_tidied <- my_data %>%
  mutate(Condition = factor(Condition))
# This changes condition from a chr to a fct.

head(my_data_tidied)
# Here we can see that Condition has been changed to a factor.

#---------------------------------------

# SUMMARISING OUR DATA

my_data_tidied %>%
  group_by(Condition) %>%
  summarise(mean = mean(Ability), sd = sd(Ability))
# This generates mean and sd motor ability scores for each of the 3 conditions.

#---------------------------------------

# VISUALISING OUR DATA

set.seed(1234)
my_data_tidied %>% 
  ggplot(aes(x = Condition, y = Ability, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13))
# This is a visualisation of the raw data points using the geom_jitter function.
# We can also see the shape of the distribution for each condition using the
# geom_violin function.
# We have also added some summary data in the form of mean and confidence intervals.

#---------------------------------------

# BUILDING OUR ANOVA MODEL

model <- aov_4(Ability ~ Condition + (1 | Participant), data = my_data_tidied)
# The ~ symbol means predicted by, the (1|Participant) term corresponds to our
# random effect, reflecting that we have taken a random sample from the world pop.
# Data = my_data_tidied specifies what data set we are using. 
# I have mapped the output of the aov function onto a variable I'm calling model.

#---------------------------------------

# INTERPRETING THE MODEL OUTPUT

summary(model)
# The effect size (ges) is generalised eta squared and for designs with more
# than one factor it can be a useful indicator of how much variance in the
# dependent variable can be explained by each factor (plus any interactions between factors).
# We can see that there is an effect in our model. The F value is big and the p
# value is small. 
# But, we are not sure what is driving the difference yet.
# To do this, we must run some pairwise comparisons using the emmeans function.

emmeans(model, pairwise ~ Condition)
# This tells us what mean(s) differ(s) from what other mean(s).
# By default, this uses Tukey's comparison adjustment.

emmeans(model, pairwise ~ Condition, adjust = "bonferroni")
# If we want to use bonferonni, we just need to code for it as done above.
# In this case, it doesn't make any differences to our comparisons. 
# There is a significant effect of Beverage Type.
# Water group performed significantly worse than Single Expresso.
# Single Espresso performed significantly worse than Double Expresso.
# From this, it can be said that drinking some coffee improves motor performance
# relative to drinking no coffee.
