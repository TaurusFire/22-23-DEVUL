library(lattice)
library(ggplot2)
#NEED histograms for each region and income status.
#region
par(mfrow = c(1,1))
par(mar=c(18,5,1,1))

region_names <- c(
  'East Asia & Pacific'="EA & P",
  'Europe & Central Asia'="E & CA",
  'Latin America & Caribbean'="LA & C",
  'Middle East & North Africa'="ME & N Afr",
  'North America'="NA",
  "South Asia" = "S Asia",
  "Sub-Saharan Africa" = "S-S. A"
)

nomodplastic = plastic[-which(plastic$Code == "MDA"),]
ggplot(nomodplastic, aes(x = PWaste)) + geom_histogram() + facet_grid(Region ~ ., labeller = as_labeller(region_names)) +xlim(c(0,210))+xlab("Plastic waste per capita in kg/day")
ggplot(nomodplastic, aes(x = MPWaste)) + geom_histogram() + facet_grid(Region ~ ., labeller = as_labeller(region_names))+xlab("Mismanaged plastic waste per capita in kg/day")


#now for income status
ggplot(nomodplastic, aes(x = PWaste)) + geom_histogram() + facet_grid(IncomeStatus ~ .) + xlim(c(0,210)) +xlab("Plastic waste per capita in kg/day")
ggplot(nomodplastic, aes(x = MPWaste)) + geom_histogram() + facet_grid(IncomeStatus ~ .) +xlab("Mismanaged plastic waste per capita in kg/day")



#2 boxplots on same scale for each income status, and then for each region?
par(mfrow=c(1,1))
par(mar=c(5,5,1,1))
boxplot(plastic$PWaste~plastic$IncomeStatus, col=2:5)
#lmc extra outlier
boxplot(plastic$PWaste~plastic$IncomeStatus, col=2:5, ylim = c(0,220))

#region
par(mar=c(13,5,1,1))
boxplot(plastic$PWaste~plastic$Region, col=2:8, las =2, xlab="", 
        ylab ="Plastic waste per capita in kg/day")
mtext("Region", side=1, line=11)

#w/o extreme
par(mar=c(13,5,1,1))
boxplot(plastic$PWaste~plastic$Region, col=2:8, las =2, xlab="",
        ylim = c(0, 220),
        ylab ="Plastic waste per capita in kg/day")
mtext("Region", side=1, line=11)

#africa
afrplastic = plastic[which(plastic$Region == "Sub-Saharan Africa"),]
summary(afrplastic$PWaste)

#eca
ecaplastic = plastic[which(plastic$Region == "Europe & Central Asia"),]
summary(ecaplastic$PWaste)

t.test(afrplastic$PWaste, ecaplastic$PWaste, paired = FALSE)
