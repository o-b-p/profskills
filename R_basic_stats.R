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


