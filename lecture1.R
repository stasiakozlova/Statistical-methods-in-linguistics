library(Rling)
data(pym_high)
data(pym_low)

head(pym_high)
head(pym_low)

str(pym_high)
str(pym_low)

summary(pym_high$assoc)
summary(pym_low$assoc)
boxplot(pym_high$assoc, pym_low$assoc, names = c("high", "low"))

boxplot.stats(pym_low$assoc)$out
pym_low[pym_low$assoc == 3, ]

shapiro.test(pym_high$assoc)
shapiro.test(pym_low$assoc)

t.test(pym_high$assoc, pym_low$assoc)
t.test(pym_high$assoc, pym_low$assoc, alternative = "greater")
t.test(pym_low$assoc, pym_high$assoc, alternative = "less")

wilcox.test(pym_high$imag, pym_low$imag, alternative = "greater")

pym_low[order(pym_low$imag),]
pym_low[order(-pym_low$imag),]
pym_low[order(pym_low$syl, pym_low$let),]