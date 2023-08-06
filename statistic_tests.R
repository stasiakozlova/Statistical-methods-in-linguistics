#Создаём датасет
word <- c("say", "go", "be like", "think", "zero", "be (just)", "misc")
british <- c(209, 120, 120, 123, 66, 11, 16)
canadian <- c(219, 135, 79, 27, 123, 5, 24)
dataset <- data.frame(word = word, British_English = british, Canadian_English = canadian)
head(dataset)

#Мы видим, что данные в двух датасетах явно различаются, хоть слова рассматриваются и одинаковые. Значит, нужно построить qq plot, чтобы убедиться в нормальности распределения данных.
summary(dataset$British_English)
summary(dataset$Canadian_English)
qqnorm(dataset$British_English)
qqline(dataset$British_English)
#qqplot показывает, что для британского английского данные распределены нормально.
qqnorm(dataset$Canadian_English)
qqline(dataset$Canadian_English)
#qqplot показывает, что и для канадского английского данные тоже распределены нормально.

#Проверим с помощью теста Шапиро-Уилка, как распределены данные.
shapiro.test(dataset$British_English)
#p-value больше, чем 0.05, значит для британского английского данные распределены нормально.
shapiro.test(dataset$Canadian_English)
#p-value больше, чем 0.05, значит для канадского английского данные распределены нормально.

#Раз нам нужно показать силу связи между двумя наборами данных, нам необходим t-test для обоих наборов. Укажем paired = TRUE, так как слова и для британского, и для канадского английского в выборках одинаковы.
t.test(dataset$British_English, dataset$Canadian_English, paired = TRUE)
#Укажем альтернативную гипотезу "greater", потому что нам нужно увидеть, насколько различаются данные.
t.test(dataset$British_English, dataset$Canadian_English, paired = TRUE, alternative = "greater")
#p-value получился больше 0.05; значит, принимаем изначальную гипотезу о том, что различия между наборами данных не значимы.

#Построим график, показывающий, что оба набора данных распредедены нормально.
boxplot(dataset$British_English, dataset$Canadian_English)
#Boxplot показывает нам, что для британского английского медиана совпадает с верхним квартилем; для канадского английского медиана находится посередине между верхним и нижнем квартилем. Выбросов нет. Распределение визуально похоже.