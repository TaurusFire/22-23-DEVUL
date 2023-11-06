# nature, direction, and strength of any discovered relationship
# Conditional relationships - sometimes the relationship can change fundamentally given another variable
#the more vertical the line the 'better' - smaller proportion of mismanaged plastic waste
#better for results to be more left aligned
par(mar=c(15,15,15,11))
xyplot(MPWaste~PWaste|Region, data = nomodplastic, pch = 16, col = alpha('black', 0.5), 
       xlab = "Plastic waste per capita in kg/day", ylab = "Mismanaged plastic waste per capita in kg/day")
#and for income status
xyplot(MPWaste~PWaste|IncomeStatus, data = nomodplastic, pch = 16, col = alpha('black', 0.5), 
       xlab = "Plastic waste per capita in kg/day",
       ylab = "Mismanaged plastic waste per capita in kg/day")

#do we have values where the mpwaste is greater than the pwaste for an observation? OUTLIER values
par(mfrow=c(1,1))
par(mar=c(5,5,1,1))
plot(x= nomodplastic$PWaste, y=nomodplastic$MPWaste, col = nomodplastic$IncomeStatus,
     xlab = "Plastic waste per capita in kg/day", 
     ylab = 'Mismanaged plastic waste per capita in kg/day',
     main = "Plastic waste vs mismanaged plastic waste",
     xlim = c(0,210))
abline(a=0, b=1)
unicols = c(unique(nomodplastic$IncomeStatus)[1], unique(nomodplastic$IncomeStatus)[4],
            unique(nomodplastic$IncomeStatus)[2], unique(nomodplastic$IncomeStatus)[3])
legend("topleft", legend = c("LIC", "LMC", "UMC", "HIC"), 
       col = unicols, lty =1)

#notice how HICs and UMCs are closer to the x-axis, and then spread changes as you rise
#as you get closer to straight line, all groups are present

#consider correlations for MPWaste and Pwaste for each region/income status?

lic.inds = which(plastic$IncomeStatus == "LIC")
lic.plast = plastic[lic.inds,]
cor(x=lic.plast$PWaste, y=lic.plast$MPWaste, use = 'complete.obs')

lmc.inds = which(plastic$IncomeStatus == "LMC")
lmc.plast = plastic[lmc.inds,]
cor(x=lmc.plast$PWaste, y=lmc.plast$MPWaste, use = 'complete.obs')

umc.inds = which(plastic$IncomeStatus == "UMC")
umc.plast = plastic[umc.inds,]
cor(x=umc.plast$PWaste, y=umc.plast$MPWaste, use = 'complete.obs')

hic.inds = which(plastic$IncomeStatus == "HIC")
hic.plast = plastic[hic.inds,]
cor(x=hic.plast$PWaste, y=hic.plast$MPWaste, use = 'complete.obs')

#what about region?

cor(plastic[which(plastic$Region == "East Asia & Pacific"),]$PWaste,
    plastic[which(plastic$Region == "East Asia & Pacific"),]$MPWaste,
    use = 'complete.obs')

cor(plastic[which(plastic$Region == "Europe & Central Asia"),]$PWaste,
    plastic[which(plastic$Region == "Europe & Central Asia"),]$MPWaste,
    use = 'complete.obs')

cor(plastic[which(plastic$Region == "Latin America & Caribbean"),]$PWaste,
    plastic[which(plastic$Region == "Latin America & Caribbean"),]$MPWaste,
    use = 'complete.obs')

cor(plastic[which(plastic$Region == "Middle East & North Africa"),]$PWaste,
    plastic[which(plastic$Region == "Middle East & North Africa"),]$MPWaste,
    use = 'complete.obs')

cor(plastic[which(plastic$Region == "North America"),]$PWaste,
    plastic[which(plastic$Region == "North America"),]$MPWaste,
    use = 'complete.obs')

cor(plastic[which(plastic$Region == "South Asia"),]$PWaste,
    plastic[which(plastic$Region == "South Asia"),]$MPWaste,
    use = 'complete.obs')

cor(plastic[which(plastic$Region == "Sub-Saharan Africa"),]$PWaste,
    plastic[which(plastic$Region == "Sub-Saharan Africa"),]$MPWaste,
    use = 'complete.obs')

#chi-square test?