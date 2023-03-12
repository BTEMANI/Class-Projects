# Factor analysis set up 

library("corpcor")
library("GPArotation")
library("psych")
library("IDPmisc")
library('readr')
studentSurvey<-read_csv('/Users/korigray/Desktop/Data-Science-Program/DS103-Metrics and Processing/L5/studentSurvey/studentSurvey.csv')
StudentsurveyCode<-read_csv('/Users/korigray/Desktop/Data-Science-Program/DS103-Metrics and Processing/L5/studentSurvey/studentSurveyCodebook.csv')

# Data Wrangle
SS1 <- studentSurvey[,31:42]
SS2 <- NaRV.omit(SS1) 
# Take away n/a's with line 13 code

# Absence of Multicollinearity
StudentSurveymatrix <- cor(SS2)
View(round(StudentSurveymatrix, 2))

# Bartlett's Test - to double check findings
cortest.bartlett(SS2)
# Ignore the red warning after running


det(StudentSurveymatrix)
# Greater than .00001 - It is sufficient for factor analysis


# Factor Analysis 

# Initial Pass to Determine Approximate Number of Factors
SSModel1 <- principal(SS2, nfactors = 12, rotate = "none")
SSModel1

# SS loading value is the eigenvalues- the higher the value it is more important
# There is only 1 factor that can be examined

# Examine the Scree Plot
plot(SSModel1$values, type="b")

# Second Pass to Test the Suspected Number of Factors
SSModel2 <- principal(SS2, nfactors = 1, rotate = "none")

# Examining Residuals to Determine Model Fit
# Looking for very little difference between the correlation matrix and the loadings generated through your model
# Rule of thumb-have good model fit if the percentage of large residuals (over .05) is less than 50%


residuals <- factor.residuals(StudentSurveymatrix, SSModel2$loadings) 
residuals <- as.matrix(residuals[upper.tri(residuals)])
largeResid <- abs(residuals) > .05
sum(largeResid)
sum(largeResid/nrow(residuals))

# Residuals are 45%. It is less than 50%. 
# Only having 1 factor is a good model fit

SSModel3 <- principal(SS2, nfactors = 1, rotate = "varimax")
print.psych(SSModel3, cut=.3, sort=TRUE)
# There is only 1 factor indicated 
# Items are not related to each other


# Calculating Reliability
# Is my survey reliable? Does it measure the same thing every time?


# Data Wrangling
# subset has 1 factor
goodSS <- SS2[, c(1,2,3,4,6,7,8,9,10,11,12)]

# Reverse recode
SS2$Area1r <- NA
SS2$Area1r[SS2$Area1 == 1] <- 5
SS2$Area1r[SS2$Area1 == 2] <- 4
SS2$Area1r[SS2$Area1 == 3] <- 3
SS2$Area1r[SS2$Area1 == 4] <- 2
SS2$Area1r[SS2$Area1 == 5] <- 1

SS2$Area2r <- NA
SS2$Area2r [SS2$Area2 == 1] <- 5
SS2$Area2r [SS2$Area2 == 2] <- 4
SS2$Area2r [SS2$Area2 == 3] <- 3
SS2$Area2r [SS2$Area2 == 4] <- 2
SS2$Area2r [SS2$Area2 == 5] <- 1

SS2$Area3r <- NA
SS2$Area3r[SS2$Area3 == 1] <- 5
SS2$Area3r[SS2$Area3 == 2] <- 4
SS2$Area3r[SS2$Area3 == 3] <- 3
SS2$Area3r[SS2$Area3 == 4] <- 2
SS2$Area3r[SS2$Area3 == 5] <- 1

SS2$Area4r <- NA
SS2$Area4r[SS2$Area4 == 1] <- 5
SS2$Area4r[SS2$Area4 == 2] <- 4
SS2$Area4r[SS2$Area4 == 3] <- 3
SS2$Area4r[SS2$Area4 == 4] <- 2
SS2$Area4r[SS2$Area4 == 5] <- 1

SS2$Area5r <- NA
SS2$Area5r[SS2$Area5 == 1] <- 5
SS2$Area5r[SS2$Area5 == 2] <- 4
SS2$Area5r[SS2$Area5 == 3] <- 3
SS2$Area5r[SS2$Area5 == 4] <- 2
SS2$Area5r[SS2$Area5 == 5] <- 1

SS2$Area6r <- NA
SS2$Area6r[SS2$Area6 == 1] <- 5
SS2$Area6r[SS2$Area6 == 2] <- 4
SS2$Area6r[SS2$Area6 == 3] <- 3
SS2$Area6r[SS2$Area6== 4] <- 2
SS2$Area6r[SS2$Area6 == 5] <- 1

SS2$Area7r <- NA
SS2$Area7r[SS2$Area7 == 1] <- 5
SS2$Area7r[SS2$Area7 == 2] <- 4
SS2$Area7r[SS2$Area7 == 3] <- 3
SS2$Area7r[SS2$Area7== 4] <- 2
SS2$Area7r[SS2$Area7 == 5] <- 1

SS2$Area8r <- NA
SS2$Area8r[SS2$Area8 == 1] <- 5
SS2$Area8r[SS2$Area8 == 2] <- 4
SS2$Area8r[SS2$Area8 == 3] <- 3
SS2$Area8r[SS2$Area8== 4] <- 2
SS2$Area8r[SS2$Area8 == 5] <- 1

SS2$Area9r <- NA
SS2$Area9r[SS2$Area9 == 1] <- 5
SS2$Area9r[SS2$Area9 == 2] <- 4
SS2$Area9r[SS2$Area9 == 3] <- 3
SS2$Area9r[SS2$Area9== 4] <- 2
SS2$Area9r[SS2$Area9 == 5] <- 1

SS2$Area10r <- NA
SS2$Area10r[SS2$Area10 == 1] <- 5
SS2$Area10r[SS2$Area10 == 2] <- 4
SS2$Area10r[SS2$Area10 == 3] <- 3
SS2$Area10r[SS2$Area10 == 4] <- 2
SS2$Area10r[SS2$Area10 == 5] <- 1

SS2$Area11r <- NA
SS2$Area11r[SS2$Area11 == 1] <- 5
SS2$Area11r[SS2$Area11 == 2] <- 4
SS2$Area11r[SS2$Area11 == 3] <- 3
SS2$Area11r[SS2$Area11 == 4] <- 2
SS2$Area11r[SS2$Area11 == 5] <- 1


SS2$Area12r <- NA
SS2$Area12r[SS2$Area12 == 1] <- 5
SS2$Area12r[SS2$Area12 == 2] <- 4
SS2$Area12r[SS2$Area12 == 3] <- 3
SS2$Area12r[SS2$Area12 == 4] <- 2
SS2$Area12r[SS2$Area12 == 5] <- 1



alpha(goodSS)

# Interpret Output for Inter-Rater Reliability
# Has good reliability raw_alpha is.92

# Interpret Output for Inter-Item Reliability
# All items in r.drop are above .3
 


# The surevey is considered  reliable through Inter-rater reliability


