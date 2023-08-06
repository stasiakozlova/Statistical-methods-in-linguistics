library(vcd)

head(HairEyeColor)
x <- margin.table(HairEyeColor, c(1, 2))
head(x)
barplot(x, beside = TRUE)
chisq.test(x)
assocstats(x)
assocplot(x)

head(Titanic)
ftable(Titanic, row.vars = c(1:3))
titanic <- apply(Titanic, c(1,4), sum)
titanicR
chisq.test(titanic)
assocstats(titanic)
mosaicplot(titanic, col = c("red", "green"))

library(Rling)
data(NSL)
str(NSL)
boxplot(NSL$MannerPath ~ NSL$Cohort, xlab = "Cohort", ylab = "Proportion of separate expressions", main = "Path and motion in NSL")
tapply(NSL$MannerPath, NSL$Cohort, mean)
aggregate(MannerPath ~ Cohort, data = NSL, function(x) shapiro.test(x)$p.value)
library(car)
leveneTest(MannerPath ~ Cohort, data = NSL)
NSL.aov <- aov(MannerPath ~ Cohort, data = NSL)
summary(NSL.aov)
TukeyHSD(NSL.aov)
kruskal.test(MannerPath ~ Cohort, data = NSL)
library(nparcomp)
npar <- nparcomp(MannerPath ~ Cohort, data = NSL, type = "Tukey")
npar$Analysis

#ЗАДАНИЕ №1
data("captions")
head(captions)
boxplot(captions$test ~ captions$cap_group)
tapply(captions$test, captions$cap_group, mean)
#Можно высчитать mean - среднее или median - медиану
aggregate(test ~ cap_group, data = captions, function(x) shapiro.test(x)$p.value)
#Тест Шапиро показывает, что данные распределены нормально
leveneTest(test ~ cap_group, data = captions)
#В тесте Левена проверяем равенство дисперсий (Pr(>F)) - если меньше 0.05, то отвергаем нулевую гипотезу о равенстве дисперсий
#Нужно использовать oneway.test, потому что дисперсии не равны
oneway.test(test ~ cap_group, data = captions)
npar1 <- nparcomp(test ~ cap_group, data = captions, type = "Tukey")
npar1$Analysis
#По столбцу p-value смотрим, есть ли отличия между группами (если больше 0.05, то различия есть)

data("sharedref")
head(sharedref)
ref <- aggregate(mod ~ age + cohort, data = sharedref, FUN = mean)
ref                 
interaction.plot(ref$age, ref$cohort, ref$mod)
aggregate(mod ~ age + cohort, data = sharedref, function(x) shapiro.test(x)$p.value)
#Если все позиции распределены нормально, кроме одной, то этой одной можно пренебречь и сказать, что в целом данные распределены нормально
fligner.test(mod ~ interaction(age, cohort), data = sharedref)
sharedref.aov <- aov(mod ~ age*cohort, data = sharedref)
summary(sharedref.aov)
TukeyHSD(sharedref.aov)

data("ToothGrowth")
head(ToothGrowth)
table(ToothGrowth$supp, ToothGrowth$dose)
aggregate(ToothGrowth$len, by = list(ToothGrowth$supp, ToothGrowth$dose), FUN = mean)
fit <- aov(len ~ supp*dose, data = ToothGrowth)
summary(fit)
interaction.plot(ToothGrowth$dose, ToothGrowth$supp, ToothGrowth$len)
TukeyHSD(fit)
ToothGrowth$dose <- as.factor()