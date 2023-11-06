#association between plastic waste and the other variables. 

#GDP, Population, Coastal Pop, Urban Pop 

#PCP
?parallelplot
parallelplot(plastic[,c(4,6:9)], horizontal = FALSE, groups = plastic$IncomeStatus, 
             auto.key = TRUE)

library(corrplot)
#correlation plot between variables
comp.plastic = na.omit(plastic[,c(4,6:9)])
cor(comp.plastic)
corrplot(cor(comp.plastic))
#seems to be a decent correlation between pwaste and gdp and pwaste and urban 
#pop (and to a lesser extend coastal pop)

#gdp and waste: smoothing maybe?
plot(comp.plastic$GDP, comp.plastic$PWaste, pch=16, col = alpha('black', 0.5))


plot(comp.plastic$UrbanPopPC, comp.plastic$PWaste, pch=16, col = alpha('black', 0.5))
fit1 = ksmooth(comp.plastic$UrbanPopPC, comp.plastic$PWaste, bandwidth = 15)
lines(x=fit1$x, y=fit1$y, col = 'red', lwd =4)


#w/o outlier
plot(comp.plastic$GDP, comp.plastic$PWaste, pch=16, col = alpha('black', 0.5), ylim =c(0,250))

plot(comp.plastic$UrbanPopPC, comp.plastic$PWaste, pch=16, col = alpha('black', 0.5), ylim =c(0,250),
    ylab = "Plastic waste per capita in kg/day", 
    xlab = "Urban population of country")
fit1 = ksmooth(comp.plastic$UrbanPopPC, comp.plastic$PWaste, bandwidth = 15)
lines(x=fit1$x, y=fit1$y, col = 'red', lwd =4)

cor(comp.plastic$UrbanPopPC, comp.plastic$PWaste)


#Is there any evidence of strong associations that may be helpful for modelling?
#correlations, use values? not a ton of strong evidence