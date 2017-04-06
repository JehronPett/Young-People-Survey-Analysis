# Gender Analysis ---------------------------------------------------------
library(ggplot2)
library(corrplot)

male <- responses[which(responses$Gender == "male"),]
female <- responses[which(responses$Gender == "female"),]

#Gender v Romantic
hist(female$Romantic, breaks = 5, main = "female x Romantic")
hist(male$Romantic, breaks = 5, main = "male x Romantic")

#Gender v Shopping
hist(female$Shopping, breaks = 5, main = "female x Shopping")
hist(male$Shopping, breaks = 5, main = "male x Shopping")

#Gender v Musicals
hist(female$Musical, breaks = 5, main = "female x Musical")
hist(male$Musical, breaks = 5, main = "male x Musical")

#Gender v War
hist(female$War, breaks = 5, main = "female x War")
hist(male$War, breaks = 5, main = "male x War")

#Gender v PC
hist(female$PC, breaks = 5, main = "female x PC")
hist(male$PC, breaks = 5, main = "male x PC")

#Gender v Height v Weight
ggplot(data= responses, aes(x = Height, y = Weight, color = Gender)) + geom_point()


# Linear Regression Analysis ------------------------------------------------------------

corrplot(cor(na.omit(responses[,134:144])), method="circle", tl.cex = 0.7)

plot(responses$Height, responses$Weight, main = "Height~Weight", xlab = "Height", ylab = "Weight")


# More Analysis -----------------------------------------------------------


onlyChild <- responses[which(responses$Only.child == 'yes'),]
oneOfMany <- responses[which(responses$Only.child == 'no'),]
