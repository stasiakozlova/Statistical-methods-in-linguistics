library(Rling)
data(ldt)
head(ldt)
summary(ldt$Length)
summary(ldt$Mean_RT)
plot(ldt$Length, ldt$Mean_RT, main = "Scatter plot of word length and mean reaction times")
m <- lm(ldt$Mean_RT ~ ldt$Length)
abline(m)
cor(ldt$Length, ldt$Mean_RT)
head(fitted(m)) #��������, ����������� ��� ����� ���������

attach(ldt)
Mean_RT_1 <- Mean_RT[Mean_RT <1200]
length(Mean_RT_1)
Length_1 <- Length[Mean_RT <1200]
length(Length_1)
m1 <- lm(Mean_RT_1 ~ Length_1)
abline(m1, lty=2)
cor(Mean_RT_1, Length_1)
shapiro.test(ldt$Mean_RT)
shapiro.test(ldt$Length)
cor(ldt$Mean_RT, ldt$Length, method = "spearman")
cor(ldt$Mean_RT, ldt$Length, method = "kendall")
cor.test(ldt$Mean_RT, ldt$Length, method = "spearman")
cor.test(ldt$Mean_RT, ldt$Length, method = "kendall")
detach(ldt)

#������� 1
plot(log1p(ldt$Freq), ldt$Mean_RT)
m2 <- lm(ldt$Mean_RT ~ log1p(ldt$Freq))
abline(m2)
shapiro.test(ldt$Mean_RT)
shapiro.test(log1p(ldt$Freq))
cor(ldt$Mean_RT, log1p(ldt$Freq), method = "spearman")
attach(ldt)
Freq1 <- Freq[Mean_RT <1200]
plot(log1p(Freq1), Mean_RT_1)
m3 <- lm(Mean_RT_1 ~ log1p(Freq1))
abline(m3)
shapiro.test(Mean_RT_1)
shapiro.test(log1p(Freq1))
cor(Mean_RT_1, log1p(Freq1))
detach(ldt)

lex <- c(47, 89, 131, 186, 245, 284, 362, 444, 553, 627)
gram <- c(0, 2, 1, 3, 5, 9, 7, 16, 25, 34)
plot(gram ~ lex, xlab = "vocabulary size", ylab = "complexity score")
lines(lowess(gram ~ lex))
shapiro.test(gram)
shapiro.test(lex)
cor.test(gram, lex, method = "spearman", alternative = "greater")

install.packages("corrgram")
library(corrgram)
cor(ldt, method = "spearman")
corrgram(ldt, lower.panel = panel.shade, upper.panel = panel.pie, cor.method = "spearman")

#������� 2
data("pym_high")
data("pym_low")
head(pym_high)
head(pym_low)
plot(pym_high$imag ~ pym_high$conc)
m4 <- lm(pym_high$imag ~ pym_high$conc)
abline(m4)
shapiro.test(pym_high$imag)
shapiro.test(pym_high$conc)
cor.test(pym_high$imag, pym_high$conc, method = "kendall")
corrgram(pym_high, lower.panel = panel.shade, upper.panel = panel.pie, cor.method = "spearman")
plot(pym_low$imag ~ pym_low$conc)
m5 <- lm(pym_low$imag ~ pym_low$conc)
abline(m5)
shapiro.test(pym_low$imag)
shapiro.test(pym_low$conc)
cor.test(pym_low$imag, pym_low$conc, method = "kendall")
corrgram(pym_low, lower.panel = panel.shade, upper.panel = panel.pie, cor.method = "spearman")