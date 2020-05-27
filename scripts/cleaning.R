#this script cleans and reshapes data for analysis
library(tidyverse)
library(corrplot)

#"How does grass cover influence Eucalyptus spp. seedling recruitment?" 

#there are 3 time periods - winter, spring 2006. autumn 2007.
#there are 3 euc seedling variables
#also canopy cover and euc canopy cover
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

ggplot(euc, aes(ExoticAnnualGrass_cover, value, colour = euc)) + geom_smooth(method = "lm") + facet_grid(~Season)
ggplot(euc, aes(ExoticPerennialGrass_cover, value, colour = euc)) + geom_smooth(method = "lm") + facet_grid(~Season)
ggplot(euc, aes(NativePerennialGrass_cover, value, colour = euc)) + geom_smooth(method = "lm") + facet_grid(~Season)


#let's check for correlations between the three euc cover response variables 
c <- select(data, 10:29)
M <- cor(c, use = "complete.obs")
corrplot(M, method = "number")

#no correlation between big and small or medium, but yes between small and medium, positive
shapiro.test(data$euc_sdlgs0_50cm)
shapiro.test(data$euc_sdlgs.2m)
ggplot(data, aes(euc_sdlgs.2m)) + geom_density()
ggplot(data, aes(euc_sdlgs0_50cm)) + geom_density()


library(glmmTMB)

m1 <- glmmTMB(euc_sdlgs0_50cm ~ ExoticPerennialGrass_cover + ExoticAnnualGrass_cover + (1|Property) + (1|Season), family= "poisson", data)
summary(m1)
library(performance)
check_overdispersion(m1)

m1.nb <- glmmTMB(euc_sdlgs0_50cm ~ ExoticPerennialGrass_cover + ExoticAnnualGrass_cover + NativePerennialGrass_cover + NativePerennialGraminoid_cover + (1|Property), family= "nbinom1", data)
check_overdispersion(m1.nb)
check_zeroinflation(m1)
AIC(m1, m1.nb)


summary(m1.nb)

m2 <- glmmTMB(euc_sdlgs.2m ~ ExoticPerennialGrass_cover + ExoticAnnualGrass_cover + (1|Property), family= "poisson", data)
m2.nb <- glmmTMB(euc_sdlgs.2m ~ ExoticPerennialGrass_cover + ExoticAnnualGrass_cover + NativePerennialGrass_cover + NativePerennialGraminoid_cover + (1|Property), family= "nbinom2", data)
AIC(m2, m2.nb)
summary(m2.nb)


m3 <- glmmTMB(euc_sdlgs.2m ~ NativePerennialGrass_cover + Euc_canopy_cover+  (1|Property), family= "nbinom2", data)
summary(m3)


m4 <- glmmTMB(euc_sdlgs50cm.2m ~ ExoticPerennialGrass_cover*annual_precipitation  +(1|Property), family= "nbinom2", data)
summary(m4)


m.q <- glm(euc_sdlgs50cm.2m ~ ExoticPerennialGrass_cover*annual_precipitation, family = quasipoisson, data = data) 

summary(m.q)
shapiro.test(data$Euc_canopy_cover)


#reproducability
version
