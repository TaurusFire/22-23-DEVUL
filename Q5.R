plot(comp.plastic$PWaste, comp.plastic$GDP, pch=16, col = alpha('black', 0.5))
fit1 = ksmooth(comp.plastic$PWaste, comp.plastic$GDP, bandwidth = 50)
lines(x=fit1$x, y=fit1$y, col = 'red', lwd =4)

plot(comp.plastic$CoastalPopPC, comp.plastic$PWaste, pch=16, col = alpha('black', 0.5))
fit2 = ksmooth(comp.plastic$CoastalPopPC, comp.plastic$PWaste, bandwidth = 10)
lines(x=fit2$x, y=fit2$y, col = 'red', lwd =4)


#w/o outlier
plot(comp.plastic$PWaste, comp.plastic$GDP, pch=16, col = alpha('black', 0.5),
     xlim =c(0,250), 
     xlab = "Plastic waste per capita in kg/day", 
     ylab = "GDP per capita of country")
fit1 = ksmooth(comp.plastic$PWaste, comp.plastic$GDP, bandwidth = 40)
lines(x=fit1$x, y=fit1$y, col = 'red', lwd =4)

plot(comp.plastic$CoastalPopPC, comp.plastic$PWaste, pch=16, col = alpha('black', 0.5), ylim =c(0,250),
     ylab = "Plastic waste per capita in kg/day", 
     xlab = "Coastal population of country")
fit2 = ksmooth(comp.plastic$CoastalPopPC, comp.plastic$PWaste, bandwidth = 10)
lines(x=fit2$x, y=fit2$y, col = 'red', lwd =4)


