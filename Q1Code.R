?hist
summary(plastic$PWaste)
summary(plastic$MPWaste)

#histograms
par(mfrow = c(2,1))
hist(plastic$PWaste, breaks = seq(0,350,20), col = '#48a307', xlab = 'Plastic waste per capita in kg/day', main = '')
rug(plastic$PWaste)
hist(plastic$MPWaste, breaks = seq(0,350,20), col = '#9e2828', xlab = 'Mismanaged plastic waste per capita in kg/day', main = '')
rug(plastic$MPWaste)

par(mfrow = c(1,2))
hist(plastic$PWaste, breaks = seq(0,350,20), col = '#48a307', xlab = 'Plastic waste per capita \n in kg/day', main = '',
     ylim = c(0,140))
rug(plastic$PWaste)
hist(plastic$MPWaste, breaks = seq(0,350,20), col = '#9e2828', xlab = 'Mismanaged plastic waste per capita \n in kg/day', main = '',
     ylim=c(0,140))
rug(plastic$MPWaste)
#more squished distribution: kurtosis/ leptokurtic for mpwaste 
#mismanaged plastic waste is a subset - always has to be less


#boxplots
par(mfrow=c(1,1))
boxplot(plastic$PWaste, plastic$MPWaste, col = c('#48a307','#9e2828'),
        ylab = "Amount of waste per capita in kg/day",
        names = c("Plastic waste","Mismanaged plastic waste") )

#boxplots w/o extreme
par(mfrow=c(1,1))
boxplot(plastic$PWaste, plastic$MPWaste, col = c('#48a307','#9e2828'),
        ylab = "Amount of waste per capita in kg/day",
        names = c("Plastic waste","Mismanaged plastic waste"),
        ylim = c(0,250))

#do we have values where the mpwaste is greater than the pwaste for an observation? OUTLIER values
par(mfrow=c(1,1))
plot(x= plastic$PWaste, y=plastic$MPWaste, col = plastic$IncomeStatus, 
     xlab = "Plastic waste per capita in kg/day", 
     ylab = 'Mismanaged plastic waste per capita in kg/day',
     main = "Plastic waste vs mismanaged plastic waste",
     xlim = c(0,210))
abline(a=0, b=1)
unicols = c(unique(plastic$IncomeStatus)[1], unique(plastic$IncomeStatus)[4],
            unique(plastic$IncomeStatus)[2], unique(plastic$IncomeStatus)[3])
legend("topleft", legend = c("LIC", "LMC", "UMC", "HIC"), 
       col = unicols, lty =1)
#looking at multiple vars, outlier! moldova, LMC
#can't have mismanaged plastic waste greater than plastic waste
#notice how HICs are closer to the x-axis, and then spread changes as you rise

#consider diff smoothings for diff groups 

library(naniar)
#missingness
gg_miss_case(plastic, order_cases = TRUE)

library(visdat)

vis_dat(plastic)
md.pattern(plastic)

library(mice)
?mice
MPWna_indices = which(is.na(plastic$MPWaste))
PWna_indices = which(is.na(plastic$PWaste))

missingplastics = plastic[MPWna_indices,]
incomecounts = table(missingplastics$IncomeStatus)
barplot(incomecounts, xlab = "Income Status", ylab = "Number of missing values",
        main = "Number of Missing Values by Income Status of Countries",
        ylim = c(0,16))
