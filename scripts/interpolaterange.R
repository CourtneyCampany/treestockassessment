standard <- read.csv("reports/container_assessment.csv")


#min coefs
with(standard, plot(log10(min_size_index)~log10(container_volume)))
abline(minmod <- lm(log10(standard$min_size_index) ~ log10(standard$container_volume)))
minpred <- coef(minmod)

#max coefs
with(standard, plot(log10(max_size_index)~log10(container_volume)))
abline(maxmod <- lm(log10(standard$max_size_index) ~ log10(standard$container_volume)))
maxpred <- coef(maxmod)


##determine missing SI ranges for field measured containers
## 5, 14, 15, 18, 800

##min values
l5min <- minpred[2]*log10(5) + minpred[1]
l5max <- maxpred[2]*log10(5) + maxpred[1]
l5min2 <- 10^l5min
l5max2 <- 10^l5max

l14min <- minpred[2]*log10(14) + minpred[1]
l14max <- maxpred[2]*log10(14) + maxpred[1]
l14min2 <- 10^l14min
l14max2 <- 10^l14max

l15min <- minpred[2]*log10(15) + minpred[1]
l15max <- maxpred[2]*log10(15) + maxpred[1]
l15min2 <- 10^l15min
l15max2 <- 10^l15max

l18min <- minpred[2]*log10(18) + minpred[1]
l18max <- maxpred[2]*log10(18) + maxpred[1]
l18min2 <- 10^l18min
l18max2 <- 10^l18max

l800min <- minpred[2]*log10(800) + minpred[1]
l800max <- maxpred[2]*log10(800) + maxpred[1]
l800min2 <- 10^l800min
l800max2 <- 10^l800max

library(magicaxis)

plot(log10(min_size_index[1:36])~log10(container_volume[1:36]), data=standard,  xlim=c(0.5,3.8),ylim=c(0.3,3.5),
     xlab="Container volume (L)", ylab="sizeindex",axes=FALSE, type='l', lwd=2)
lines(log10(max_size_index[1:36])~log10(container_volume[1:36]), data=standard,lwd=2)
magicaxis::magaxis(side=c(1,2), unlog=c(1,2), frame.plot=FALSE)
box()

points(log10(5), log10(l5min2), pch=15, col="red")
points(log10(5), log10(l5max2), pch=17, col="red")

points(log10(14), log10(l14min2), pch=15, col="blue")
points(log10(14), log10(l14max2), pch=17, col="blue")

points(log10(15), log10(l15min2), pch=15, col="gold")
points(log10(15), log10(l15max2), pch=17, col="gold")

points(log10(18), log10(l18min2), pch=15, col="green")
points(log10(18), log10(l18max2), pch=17, col="green")

points(log10(800), log10(l800min2), pch=15, col="purple")
points(log10(800), log10(l800max2), pch=17, col="purple")

