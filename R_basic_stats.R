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

# 2. More on analyses with single categorical explanatory variable
#    and continuous response

# import data
soils <- read.csv("C:/Users/erika/Documents/profskills/Peru_Soil_Data.csv")
summary(soils)

# check distribution of data variables
hist(soils$Soil_pH) # left-skewed towards ph 4.0
hist(soils$Soil_pH,breaks=10) # see in more detail

# add line
abline(v=median(soils$Soil_pH)) #median
abline(v=mean(soils$Soil_pH))

# how does continuous data vary accross discrete categorigal variables?
# make box plot
soils$Habitat <- as.factor(soils$Habitat)
plot(Soil_pH ~ Habitat, data=soils)
plot(Potassium~Habitat,data=soils)

# use ANOVAs to determine if ph and p vary significantly with habitat types
lm_pH <- lm(Soil_pH~Habitat,data=soils)
anova(lm_pH)

lm_K <- lm(Potassium~Habitat,data=soils)
anova(lm_K)

# check assuptions
lm_pH_resids <- resid(lm_pH)
shapiro.test(lm_pH_resids) # distribution of resids >0.05, therefore normal
bartlett.test(Soil_pH ~Habitat,data=soils)

lm_K_resids <- resid(lm_K)
shapiro.test(lm_K_resids)
bartlett.test(Potassium ~Habitat,data=soils)

plot(lm_pH)
plot(lm_K)
# Potassium clearly does not have homogeneous variances across floodplain and terra firme

hist(soils$Soil_pH,breaks=10)
hist(soils$Potassium,breaks=10)
# potassium is right-skewed

plot(Soil_pH~Habitat,data=soils)
plot(Potassium~Habitat,data=soils)

# log transform potassium data, as assumptions for anova were violated
soils$log_K <- log(soils$Potassium)

# re-do analysis with log transformed k data
lm_log_K <- lm(log_K ~Habitat,data=soils)
anova(lm_log_K)

# check residuals again to see if they conform to anova assumptions
lm_log_K_resids <- resid(lm_K)
shapiro.test(lm_log_K_resids)
bartlett.test(log_K ~Habitat,data=soils)
plot(lm_log_K)



# 3. Relationships between continous variables ####

