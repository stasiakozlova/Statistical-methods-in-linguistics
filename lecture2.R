library(Rling)
data(pym_high)
data(pym_low)

summary(pym_high$conc)
summary(pym_low$conc)

qqnorm(pym_high$conc)
qqline(pym_high$conc)
plot(density(pym_high$conc))

stripchart(list(pym_high$conc, pym_high$conc), method = "jitter")

pym_high[pym_high$conc > 6, 1]
pym_high[pym_high$conc > 6, 1, drop = FALSE]
pym_high[pym_high$conc > 6, c(3, 5), drop = FALSE]
pym_high[pym_high$conc > 2 & pym_high$conc < 4, c(3, 5), drop = FALSE]

shapiro.test(pym_low$conc)
wilcox.test(pym_low$conc)

word <- c("lorry", "windscreen", "petrol", "dustbin", "advert", "anticlockwise", "crisps", "naff", "whilst", "snog")
british <- c(427, 594, 490, 507, 508, 648, 451, 315, 563, 355)
american <- c(652, 650, 572, 591, 475, 1256, 485, 621, 904, 481)
exercise <- data.frame(Word = word, British_speakers =  british, American_speakers = american)
head(exercise)
str(exercise)
boxplot(exercise$British_speakers, exercise$American_speakers)
shapiro.test(exercise$British_speakers)
shapiro.test(exercise$American_speakers)
boxplot.stats(exercise$American_speakers)$out
wilcox.test(exercise$British_speakers, exercise$American_speakers, paired = TRUE, alternative = "less")

head(pym_high)
str(pym_high)
summary(pym_high$assoc)
diff <- rnorm(50, -1.35, 1.27)
new <- pym_high$assoc + diff
new <- round(new, 2)
t.test(pym_high$assoc, new, alternative = "greater", paired = TRUE)

head(sleep)
plot(extra ~ group, data = sleep)
with (sleep, t.test(extra[group == 1], extra[group == 2]))