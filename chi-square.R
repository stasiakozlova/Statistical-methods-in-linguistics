library(Rling)
library(vcd)
over <- cbind(c(22,4), c(5,12))
rownames(over) <- c("met", "nonmet")
colnames(over) <- c("acad", "conv")
barplot(over, col = c("grey20", "grey80"), main = "Bar plot of (non-)metaphoric uses of over", xlab = "register", ylab = "frequency", beside = TRUE)
legend("topright", fill = c("grey20", "grey80"), c("met", "non-met"))
chisq.test(over)$expected

chisq.test(over)
test <- cbind(c(12,1), c(4,6))
chisq.test(test)$expected
fisher.test(test)

assocstats(over)

#ÇÀÄÀÍÈÅ ¹1
preposition <- cbind(c(735, 331), c(4, 3))
rownames(preposition) <- c("met", "nonmet")
colnames(preposition) <- c("on", "onto")
preposition
barplot(preposition)
chisq.test(preposition)$expected
fisher.test(preposition)
assocstats(preposition)

see.m <- c(44, 48, 27, 17)
see.nm <- c(26, 135, 98, 19)
see.reg <- rbind(see.m, see.nm)
colnames(see.reg) <- c("aca", "conv", "fic", "news")
see.reg
barplot(see.reg, beside = TRUE, col=c("grey20", "grey80"), main = "Bar plot of (non-)metaphoric uses of see")
legend("topright", fill=c("grey20", "grey80"), c("met", "non-met"))
chisq.test(see.reg)$expected
assocstats(see.reg)
mosaic(see.reg, shade = TRUE, varnames = FALSE)
assoc(see.reg, shade = TRUE, varnames = FALSE)

#ÇÀÄÀÍÈÅ ¹2
on.m <- c(255, 163, 112, 205)
on.nm <- c(37, 96, 121, 77)
on.reg <- rbind(on.m, on.nm)
colnames(on.reg) <- c("academic", "conversations", "fiction", "news")
on.reg
barplot(on.reg, beside = TRUE, col=c("grey20", "grey80"), main = "Bar plot of (non-)metaphoric uses of on")
legend("topright", fill=c("grey20", "grey80"), c("met", "non-met"))
chisq.test(on.reg)$expected
assocstats(on.reg)
mosaic(on.reg, shade = TRUE, varnames = FALSE)
assoc(on.reg, shade = TRUE, varnames = FALSE)
