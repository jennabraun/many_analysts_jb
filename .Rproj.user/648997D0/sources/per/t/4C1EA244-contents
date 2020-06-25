#this script cleans and reshapes data for analysis
library(tidyverse)
library(performance)
library(MASS)
library(sjPlot)

#"How does grass cover influence Eucalyptus spp. seedling recruitment?" 

#there are 3 time periods - winter, spring 2006. autumn 2007.
#there are 3 euc seedling variables
#also canopy cover and euc canopy cover - not rooted within q
#annual & perennial exotic grass
#native perennial grass
#native graminioids
#precipitation may be important because resources can mediate the outcome of plant-plant interactions

data <- read.csv("Euc_data.csv")
sub <- select(data, 1:5, 10, 13, 16, 18, 24:29)
euc <- gather(sub, euc, value, 12:14)

#viz site level differences in euc seedlings
ggplot(euc, aes(value, colour = euc)) + geom_boxplot() + facet_grid(~Property)


#I considered using euc cover but there can be foliage with zero seedlings, must be trees from outside of quadrats
#not going to use

#so many zeroes

ggplot(euc, aes(ExoticAnnualGrass_cover, value, colour = euc)) + geom_smooth(method = "lm") 
ggplot(euc, aes(ExoticPerennialGrass_cover, value, colour = euc)) + geom_smooth(method = "lm")
ggplot(euc, aes(NativePerennialGrass_cover, value, colour = euc)) + geom_smooth(method = "lm")


#let's check for correlations between the three euc cover response variables 
c <- select(data, 10:29)
M <- cor(c, use = "complete.obs")
corrplot(M, method = "number")

#no correlation between big and small or medium, but yes between small and medium, positive
ggplot(data, aes(euc_sdlgs.2m)) + geom_density()
ggplot(data, aes(euc_sdlgs0_50cm)) + geom_density()

cor.test(data$euc_sdlgs0_50cm, data$euc_sdlgs50cm.2m)
cor.test(data$euc_sdlgs.2m, data$euc_sdlgs0_50cm)
cor.test(data$euc_sdlgs.2m, data$euc_sdlgs50cm.2m)


#what are the differences between properties
shapiro.test(data$annual_precipitation)
kruskal.test(data$Property, data$annual_precipitation)
kruskal.test(data$Season, data$euc_sdlgs0_50cm)
kruskal.test(data$Season, data$euc_sdlgs50cm.2m)
kruskal.test(data$Season, data$euc_sdlgs.2m)

kruskal.test(data$Property, data$euc_sdlgs0_50cm)

#are properties remeasured or independent between seasons?
prop <- data %>% select(Property, Season) %>% group_by(Property, Season) %>% distinct()

#there's a lot of zeroes, how many?
sum(data$euc_sdlgs0_50cm == 0)
sum(data$euc_sdlgs.2m != 0)

#that's a lot of zeros - let's do a logistic regression instead 

data <- mutate(data, small_bin = ifelse(euc_sdlgs0_50cm == 0,0, 1))
data <- mutate(data, med_bin = ifelse(euc_sdlgs50cm.2m == 0,0, 1))
data <- mutate(data, large_bin = ifelse(euc_sdlgs.2m == 0,0, 1))
data <- mutate(data, all = euc_sdlgs0_50cm + euc_sdlgs50cm.2m + euc_sdlgs.2m)

sum(data$all == 0)

m1.all <- glm(small_bin ~ ExoticPerennialGrass_cover*annual_precipitation +ExoticAnnualGrass_cover*annual_precipitation + NativePerennialGrass_cover*annual_precipitation +NativePerennialGraminoid_cover*annual_precipitation, family = binomial(), data = data)
summary(m1.all)
m1 <- glm(small_bin ~ ExoticPerennialGrass_cover+annual_precipitation +ExoticAnnualGrass_cover + NativePerennialGrass_cover +NativePerennialGraminoid_cover + Property, family = binomial(), data = data)
summary(m1)
car::Anova(m1, type = 2, test = "LR")
AIC(m1.all, m1)

m2.all <- glm(large_bin ~ ExoticPerennialGrass_cover*annual_precipitation +ExoticAnnualGrass_cover*annual_precipitation + NativePerennialGrass_cover*annual_precipitation +NativePerennialGraminoid_cover*annual_precipitation + Property, family = binomial(), data = data)
summary(m2.all)

library(glmmTMB)
m2 <-  glm(large_bin ~ ExoticPerennialGrass_cover +ExoticAnnualGrass_cover + NativePerennialGrass_cover*annual_precipitation +NativePerennialGraminoid_cover, family = binomial(), data = data)
summary(m2)

m3 <- glmmTMB(large_bin ~ ExoticPerennialGrass_cover +ExoticAnnualGrass_cover + NativePerennialGrass_cover +NativePerennialGraminoid_cover + (1|Property), family = binomial(), data = data)

summary(m3)
AIC(m2.all, m2)


plot_model(m2, "int")






#small size models
m1 <- glmmTMB(euc_sdlgs0_50cm ~ ExoticPerennialGrass_cover + ExoticAnnualGrass_cover + (1|Property), family= "poisson", data)
summary(m1)
check_overdispersion(m1)
#overdispersed
#models with both property and precipitation break

m1 <- glm(euc_sdlgs0_50cm ~ ExoticPerennialGrass_cover*annual_precipitation*ExoticAnnualGrass_cover, family = "poisson", data = data)
check_overdispersion(m1)

m2 <- glm.nb(euc_sdlgs0_50cm ~ ExoticPerennialGrass_cover*annual_precipitation +ExoticAnnualGrass_cover*annual_precipitation + NativePerennialGrass_cover*annual_precipitation +NativePerennialGraminoid_cover*annual_precipitation, data = data)
summary(m2)

#try a 3-way interaction of significant terms
m3 <- glm.nb(euc_sdlgs0_50cm ~ ExoticPerennialGrass_cover*annual_precipitation*ExoticAnnualGrass_cover+ NativePerennialGrass_cover +NativePerennialGraminoid_cover, data = data)
summary(m3)

m4 <- glm.nb(euc_sdlgs0_50cm ~ ExoticPerennialGrass_cover*annual_precipitation + ExoticAnnualGrass_cover*annual_precipitation + NativePerennialGrass_cover +NativePerennialGraminoid_cover, data = data)
summary(m4)

AIC(m1, m2, m3, m4)

plot_model(m4, "pred", terms =  c("ExoticPerennialGrass_cover", "annual_precipitation"))

#reproducability
version
