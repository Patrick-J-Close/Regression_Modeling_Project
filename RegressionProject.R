#R Libraries and datasets
library(ggplot2)
library(GGally)  #Not needed for submission document
data(mtcars)

#Data transformations
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am, labels = c("Auto", "Man"))
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
str(mtcars)

#Exploratory Data Analysis
#Boxplot of mpg by am
meds <- aggregate(mpg ~ am, data = mtcars, median)
g <- ggplot(aes(x=am, y=mpg), data = mtcars) + geom_boxplot(aes(fill=am))
g <- g + geom_text(data=meds, aes(label = mpg, y =  mpg + 0.5))
print(g)

#Scatterplot matrix
pairs(mtcars[,1:8], panel = panel.smooth)

#Use ggpairs to do summary plots (DO NOT use in submission, not covered in class)
ggpairs(mtcars, colours = 'am', alpha = 0.4)

#Statistical Inference
#t-test on mpg by am
t.test(mpg ~ am, data = mtcars)

#Regression Analysis
#simple linear model of mpg by am
lmodel <- lm(mpg ~ am, data = mtcars)
summary(lmodel)
sum_lmodel <- summary(lmodel)
sum_lmodel$r.squared
sum_lmodel$adj.r.squared

#multiple regression model
lmodels_all <- lm(mpg ~., data = mtcars)
bestmodel <- step(lmodels_all, direction = "both")
summary(bestmodel)
sum_best <- summary(bestmodel)
sum_best$r.squared
sum_best$adj.r.squared

Model_compare <- data.frame(sum_lmodel$r.squared, sum_lmodel$adj.r.squared)
colnames(Model_compare) <- c("r.squared", "adj.r.squared")
Model_compare[2,] <- c(sum_best$r.squared, sum_best$adj.r.squared)
rownames(Model_compare) <- c("single factor model", "multi-factor model")


par(mfrow = c(2,2))
plot(bestmodel)

#compare simple linear model with multiple regression
model_anova <- anova(lmodel, lmodels_all)
print(model_anova$F)