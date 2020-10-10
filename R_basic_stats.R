# Week 3 Professional Skills
# R refresher
# R basic stats

# 1. Basic ANOVA ####

# create data frames
A <- c(115,120,135,155,160,170,175,200,205,220)
B <- c(115,145,75,60,95,95,170,105,130,120)
C <- c(155,75,110,145,85,105,140,75,140,110)

height <- c(A,B,C)
fertiliser <- c(rep("A",length(A)),rep("B",length(B)),rep("C",length(C)))
alldata <- data.frame(fertiliser,height)

# Build model
(height_lm <- lm(height~fertiliser,data=alldata))

# execute ANOVA
anova(height_lm)

# Summary function gives you all the results of ANOVA and more.
summary(height_lm)


# Does data satisfy the assumptions needed to conduct ANOVA? ####

# get residuals
height_resids <- resid(height_lm)
# put residuals into shapiro wilkson normality test
# if p-value is greater than 0.05, assume distribution of resids is normal
shapiro.test(height_resids)
# check equality of variances
bartlett.test(height~fertiliser,data=alldata)

plot(height_lm)

# the data satifies the assumptions for ANOVA.

# Fertiliser significantly affects seedling height. Technically, we  
# cannot conclude that fertiliser A leads to significantly taller seedlings than 
# fertilisers B and C.

# Which fertilisers are driving the significant ANOVA result?
# Can use Tukey's test
# However, need object of class "aov" for this.
# aov() basically conducts ANOVA in one step rather than 2.

# make aov object
(height_aov <- aov(height~fertiliser,data=alldata))

# conduct Tukey test
TukeyHSD(height_aov)
# A and B have significantly different heights
# A and C have significantly different heights
# B and C do not have significantly different heights
# THEREFORE:  A differs significantly from B and C

