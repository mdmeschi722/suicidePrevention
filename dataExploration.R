# add required packages
# advancing master branch
# bug fix test - more git playing
# data exploration branch
# new feature stuff
# this will advance the new branch
# oh boy - 
#
# testing merge conflict in git
# adding a comment to check into github
# adding another comment



require(ggplot2)
require(tidyr)
require(dplyr)
require(xts)
require(lubridate)
require(rpart)
require(ISLR)
require(randomForest)
require(rpart)
require(caret)
require(e1071)
require(tree)
require(caret)
library(rpart.plot)



# read in the Kaggle file 
file <- "C:/Users/Mark/SuicidePrevention/master.csv"
dfMasterRecords <- read.table(file, sep = ",", header = TRUE, stringsAsFactors = FALSE)


# Add quantile feature to categorize the suicides per 100k metric 
quantileForSuicideRate <- quantile(dfMasterRecords$suicidesPer100k, c(.1, .2, .3, .4, .5, .6, .7, .8, .9))
quantileForSuicideRate

# Category for four Quantiles of suicide rate
funcCreateCategoricalFeature <- function(suicidePerOneHunK) {
    if (suicidePerOneHunK <= .92)
        retCat <- "A"
    
    if (suicidePerOneHunK > .92 & suicidePerOneHunK <= 5.99)
        retCat <- "B" 
    
    if (suicidePerOneHunK > 5.99 & suicidePerOneHunK <= 16.62)
        retCat <- "C"
    
    if (suicidePerOneHunK > 16.62 & suicidePerOneHunK <= 224.97)
        retCat <- "D"
    
    return(retCat)
}
# Category for 10 quantiles of suicide rate
funcCreateCategoricalFeatureQuantilesCat <- function(rate) {
    if (rate <= .41)
        retCat <- "A"
    
    if (rate > .41 & rate <= 1.6)
        retCat <- "B" 
    
    if (rate > 1.6 & rate <= 3.54)
        retCat <- "C"
    
    if (rate > 3.54 & rate <= 5.99)
        retCat <- "D"
    
    if (rate > 5.99 & rate <= 9.09)
        retCat <- "E"
    
    if (rate > 9.09 & rate <= 13.563)
        retCat <- "F"
    
    if (rate > 13.563 & rate <= 20.53)
        retCat <- "G"
    
    if (rate > 20.53 & rate <= 33.291)
        retCat <- "H"
    
    if (rate > 33.291)
        retCat <- "I"
    
    
    return(retCat)
}
# Binary variable for suicide rate protection 
funcCreateCategoricalFeatureQuantilesCat2 <- function(rate) {

    
    if (rate <= 33.291)
        retCat <- "B"
    
    if (rate > 33.291)
        retCat <- "A"
    
    
    return(retCat)
}
# add gdp per capita feature to help with decision tree 
funcCreateCategoricalFeatureGdpPerCapita <- function(gdpPerCapita) {
    if (gdpPerCapita < 3447)
        retCat <- "A"
    
    if (gdpPerCapita >= 3447 & gdpPerCapita <= 9372)
        retCat <- "B" 
    
    if (gdpPerCapita > 9372 & gdpPerCapita <= 24874)
        retCat <- "C"
    
    if (gdpPerCapita > 24874 & gdpPerCapita <= 126352)
        retCat <- "D"
    
    return(retCat)
}
# add
funcCreateCategoricalFeature <- function(suicidePerOneHunK) {
    if (suicidePerOneHunK <= .92)
        retCat <- "A"
    
    if (suicidePerOneHunK > .92 & suicidePerOneHunK <= 5.99)
        retCat <- "B" 
    
    if (suicidePerOneHunK > 5.99 & suicidePerOneHunK <= 16.62)
        retCat <- "C"
    
    if (suicidePerOneHunK > 16.62 & suicidePerOneHunK <= 224.97)
        retCat <- "D"
    
    return(retCat)
}
#
funcCreateCategoricalFeatureGeo <- function(Country) {

    
    if (Country %in% c("Cabo Verde", "Mauritius", "Seychelles", "South Africa"))
        retCat <- "Africa"
    
    if (Country %in% c("Australia", "Fiji", "Japan", "Kiribati", "Macau", "Maldives", "Mongolia",
                       "New Zealand", "Philippines", "Republic of Korea", "Singapore", "Sri Lanka", "Thailand"))
        retCat <- "Asia"
    
    if (Country %in% c("Aruba", "Bahamas", "Barbados", "Cuba", "Dominica", "Grenada", "Jamaica",
                       "Puerto Rico", "Saint Kitts and Nevis", "Sain Lucia", "Saint Vincent and Grenadines", 
                       "Trinidad and Tobago", "Antigua and Barbuda", "Saint Lucia"))
        retCat <- "Carribean"
    
    if (Country %in% c("Belarus", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Czech Republic", 
                       "Estonia","Lithuania", "Montenegro", "Poland", "Romania", "Russian Federation", 
                       "Serbia", "Slovakia", "Slovenia", "Ukraine", "Albania", "Hungary", "Latvia"))
        retCat <- "Eastern Europe"
    
    if (Country %in% c("Armenia", "Azerbaijan", "Cyprus", "Georgia", "Israel", "Kazakhstan", 
                       "Turkey", "Turkmenistan", "Kyrgyzstan"))
        retCat <- "Eurasia"
    
    if (Country %in% c("Bahrain", "Kuwait", "Oman", "Qatar", "United Arab Emirates"))
        retCat <- "Middle East"
    
    if (Country %in% c("Canada", "Mexico", "Panama", "United States"))
        retCat <- "North America"
    
    if (Country %in% c("Argentina", "Belize", "Brazil", "Chile", "Colombia", "Costa Rica", "Ecuador", 
                       "El Salvador", "Guatemala", "Guyana", "Nicaragua", "Paraguay", "Suriname", 
                       "Uruguay", "Uzbekistan"))
        retCat <- "South America"
    
    if (Country %in% c("Austria", "Belgium", "Denmark", "Finland", "France", "Germany", "Greece", 
                       "Iceland", "Ireland", "Italy", "Luxembourg", "Malta", "Netherlands", "Norway", 
                       "Portugal", "San Marino", "Spain", "Sweden", "Switzerland", "United Kingdom"))
        retCat <- "Western Europe"
     
    
    return(retCat)
}

dfMasterRecords <- dfMasterRecords %>% mutate(geographicRegion = 
                                                  sapply(dfMasterRecords$?..country, funcCreateCategoricalFeatureGeo))



dfMasterRecords <- dfMasterRecords %>% mutate(Suicide.100k.cat = 
                                                  sapply(dfMasterRecords$suicidesPer100k, funcCreateCategoricalFeature))

dfMasterRecords <- dfMasterRecords %>% mutate(suicideBy100KCat2 = 
                                                  sapply(dfMasterRecords$suicidesPer100k, funcCreateCategoricalFeatureQuantilesCat2))

dfMasterRecords <- dfMasterRecords %>% mutate(gdpPerCaptiaCat = 
                                                  sapply(dfMasterRecords$gdp_per_capita, funcCreateCategoricalFeatureGdpPerCapita))


summary(dfMasterRecords$suicidesPer100k)
# The median amount of suicides per 100K population is mucher lower
# than the mean 
#  
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    0.92    5.99   12.82   16.62  224.97 

# do a density plot of our label 

plotDensity <- ggplot(dfMasterRecords, aes(x=dfMasterRecords$suicidesPer100k)) +
    geom_density(color="darkblue", fill="lightblue") + theme_bw() + labs(x = "Suicides Per 100k Population", y = "Desnity",
                                       title = "Density Plot of Suicides per 100K population By Country")
plotDensity

# plot by time 
dfSuicidesByYear <- dfMasterRecords %>% group_by(year) %>% summarise(suidicdesPer100k = sum(suicidesPer100k))

linePlotSuicidesByYear <- ggplot(dfSuicidesByYear, aes(x=year, y=dfSuicidesByYear$suidicdesPer100k)) + 
    geom_line() + theme_bw() + labs(x = "Year", y = "Suicides Per 100k Population",
                                    title = "Line Chart of Suicides Per 100K by Year")

linePlotSuicidesByYear

# massive drop off in 2016 - perhaps fewer countries contributed data that year 
dfSuicidesByYearByCountry <- dfMasterRecords %>% group_by(year,?..country,gdpPerCaptiaCat) %>% summarise(suidciesPer100K = sum(suicidesPer100k))

# do a count of reporting countries by year 
dfCountOfReportingCountriesByYear <- dfSuicidesByYearByCountry %>% group_by(year) %>% summarise(countries=n())
linePlotReportingCountries <- ggplot(dfCountOfReportingCountriesByYear, aes(x=year, y=countries)) + geom_line() +
    theme_bw() + labs(x = "Year", y = "Reporting Countries",
                      title = "Reporting Countries Per Year")
                                
linePlotReportingCountries


# assess impact of the year on the suicide by 100k label, we have to take the mean of the suicides per 100k by year and plot that 
dfMeanSuicidesPer100kByYear <- dfMasterRecords %>% group_by(year) %>% summarise(meanSuicidesPer100k = mean(suicidesPer100k))

#plot mean suicides per 100 k by year to assess the impact of the year feature 
linePlotMeanSuicidesPer100kByYear <- ggplot(dfMeanSuicidesPer100kByYear, aes(x=year, y=meanSuicidesPer100k)) + geom_line() +
    theme_bw() + labs(x = "Year", y = "Mean suicides Per 100k",
                      title = "Mean Suicides Per 100K Per Year Between 1985 and 2016")


linePlotMeanSuicidesPer100kByYear

## Looks like year IS a significant factor


# suicides by country 
dfMeanSuicideRateByCountry <- dfMasterRecords %>% group_by(dfMasterRecords$?..country) %>% summarise(meanSuicides = mean(suicidesPer100k))
dfMeanSuicideRateByCountry

dfMeanSuicideRateByCountry$`dfMasterRecords$?..country`

dfHighMeanSuicideByCountry <- dfMeanSuicideRateByCountry %>% filter(meanSuicides > 25.6 )
dfLowMeanSuicideByCountry <- dfMeanSuicideRateByCountry %>% filter(meanSuicides < 1.5)



names(dfHighMeanSuicideByCountry) <- c("country", "mean")
names(dfLowMeanSuicideByCountry) <- c("country", "mean")


# bar chart of 10 countries with the highest mean suicide rate and 10 countries with the lowest mean suicide rate 
barHighPlotCountries <- ggplot(dfHighMeanSuicideByCountry, aes(x=country, y = mean )) + 
    geom_bar(aes(fill = country), stat = "identity") + theme_bw() + labs(title = "Ten Countries with the Highest Suicide Rates")
barHighPlotCountries

barLowPlotCountries <- ggplot(dfLowMeanSuicideByCountry, aes(x=country, y = mean)) + 
    geom_bar(aes(fill = country), stat = "identity") + theme_bw() + labs(title = "Ten Countries with the lowest Suicide Rates")

barLowPlotCountries

#suidcide rate by geographic regio 
dfMeanSuicideRateByGeographicRegion <- dfMasterRecords %>% group_by(dfMasterRecords$geographicRegion) %>%
                                                                        summarise(meanSuicides = mean(suicidesPer100k))
names(dfMeanSuicideRateByGeographicRegion) <- c("GeographicRegion", "Mean")

barGeo <- ggplot(dfMeanSuicideRateByGeographicRegion, aes(x=GeographicRegion, y = Mean )) + 
    geom_bar(aes(fill = GeographicRegion), stat = "identity") + theme_bw() + labs(title = "Suicide Rate By Region")
barGeo




# do facet wrap line scatterplot by country of suicide rate by year 
plotSuicidesByYearByCountry <- ggplot(dfSuicidesByYearByCountry, aes(x = year, y = suidciesPer100K)) + geom_point() +
                                        stat_smooth(method = "lm", se = FALSE) + facet_wrap(~dfSuicidesByYearByCountry$?..country) + theme_bw()
plotSuicidesByYearByCountry

# scatter plot suicide rate by year for countries in the 1st quartile of gdp by year
# GDP of 3,447 or less per person per year 
dfGdpA <- dfSuicidesByYearByCountry %>% filter(gdpPerCaptiaCat == 'A')


plotSuicidesByYearByCountry1Quartile <- ggplot(dfGdpA,  aes(x = year, y = suidciesPer100K)) + geom_point() +
    stat_smooth(method = "lm", se = FALSE) + facet_wrap(~dfGdpA$?..country) + theme_bw() + labs(title = "Suicide Rate by country and year with GDP Per Capita < 3,447")

plotSuicidesByYearByCountry1Quartile

# scatter plot suicide rate by year for countries in the 2nd quartile of gdp by year
# GDP between 3,447 and 9,372 per person per year
dfGdpB <- dfSuicidesByYearByCountry %>% filter(gdpPerCaptiaCat == 'B')

plotSuicidesByYearByCountry2Quartile <- ggplot(dfGdpB,  aes(x = year, y = suidciesPer100K)) + geom_point() +
    stat_smooth(method = "lm", se = FALSE) + facet_wrap(~dfGdpB$?..country) + theme_bw() + labs(title = "Suicide Rate by country and year with GDP Per Capita >3,447 and <= 9,372")

plotSuicidesByYearByCountry2Quartile


# scatter plot suicide rate by year for countries in the 3rd quartile of gdp by year
# GDP between 9,372 and 24,874 per person per year
dfGdpC <- dfSuicidesByYearByCountry %>% filter(gdpPerCaptiaCat == 'C')

plotSuicidesByYearByCountry3Quartile <- ggplot(dfGdpC,  aes(x = year, y = suidciesPer100K)) + geom_point() +
    stat_smooth(method = "lm", se = FALSE) + facet_wrap(~dfGdpC$?..country) + theme_bw() + labs(title = "Suicide Rate by country and year with GDP Per Capita > 9,372 and <= 24,874")

plotSuicidesByYearByCountry3Quartile

# scatter plot suicide rate by year for countries in the 3rd quartile of gdp by year
# GDP between 9,372 and 24,874 per person per year
dfGdpD <- dfSuicidesByYearByCountry %>% filter(gdpPerCaptiaCat == 'D')

plotSuicidesByYearByCountry4Quartile <- ggplot(dfGdpD,  aes(x = year, y = suidciesPer100K)) + geom_point() +
    stat_smooth(method = "lm", se = FALSE) + facet_wrap(~dfGdpD$?..country) + theme_bw() + labs(title = "Suicide rate by country and year with GDP Per Capita > 126,352")

plotSuicidesByYearByCountry4Quartile



# scatterplot to determine impact of continuous variables 
# View how time and gdp per capita have impact on suicide rat
plotScatterHDI <- ggplot(dfMasterRecords, aes(dfMasterRecords$HDI.for.year, dfMasterRecords$suicidesPer100k)) +
                             geom_point(aes(color=age, shape=sex)) + theme_bw() + 
                             labs(x = "HDI", y = "Suicides per 100k", title = "Impact of Human Development Index")

plotScatterHDI

# scatterplot by Gdp per capita 

plotScatterGdpPerCapita <- ggplot(dfMasterRecords, aes(dfMasterRecords$gdp_per_capita, dfMasterRecords$suicidesPer100k)) +
                            geom_point(aes(color=age, shape=sex)) + theme_bw() + 
                            labs(x = "GDP Per Capita", y = "Suicides per 100k", title = "Impact of GDP per capita") 
plotScatterGdpPerCapita

# Impact of population on suicide rate by doing a scatterplot 
plotScatterPopulation <- ggplot(dfMasterRecords, aes(dfMasterRecords$population, y = dfMasterRecords$suicidesPer100k)) + 
    geom_point(aes(color = age, shape = sex)) + theme_bw() + labs(x = "Population", y = "Suicides Per 100k", title = "Impact of Populaton")

plotScatterPopulation

# Impact on rate by total gdp 
plotScatterTotalGdp <- ggplot(dfMasterRecords, aes(gdp_for_year, suicidesPer100k)) + geom_point(aes(color = age, shape = sex)) +
    theme_bw() + labs(x = "GDP per Year", y = "Suicides Per 100k", title = "Impact of Total GDP on suicide Rate") 
plotScatterTotalGdp


# 101 different countries 
vectUniqueCountries <- unique(dfMasterRecords$?..country)

# throw out the outliers (>50 per 100k rate)
dfOutlierRemoved <- dfMasterRecords %>% filter(suicidesPer100k <=51 )
dfOutlierRemoved

# set up factors to evaluate 
dfMasterRecords$sex <- as.factor(dfMasterRecords$sex)
dfMasterRecords$age <- as.factor(dfMasterRecords$age)
dfMasterRecords$generation <- as.factor((dfMasterRecords$generation))
dfMasterRecords$suicideBy100KCat2 <- as.factor((dfMasterRecords$suicideBy100KCat2))
dfMasterRecords$?..country <- as.factor((dfMasterRecords$?..country))
dfMasterRecords$geographicRegion <- as.factor(dfMasterRecords$geographicRegion)
dfMasterRecords$Suicide.100k.cat <- as.factor(dfMasterRecords$Suicide.100k.cat)
dfMasterRecords$year <- as.factor(dfMasterRecords$year)


#box plot suicides per 100K by Sex 
boxplot(dfMasterRecords$suicidesPer100k ~ dfMasterRecords$sex, 
        main = "Suicides Per 100k by Sex")

boxplot(dfMasterRecords$suicidesPer100k ~ dfMasterRecords$age, 
        main = "Suicides per 100K by Age Range")

boxplot(dfMasterRecords$suicidesPer100k ~ dfMasterRecords$generation, 
        main = "Suicides Per 100k by Generation")

boxplot(dfMasterRecords$suicidesPer100k ~ dfMasterRecords$geographicRegion, 
        main = "Suicides Per 100k by Geographic Region")


######################################
######################################
# Decision Tree (classification)

# choose features for classification in decision tree
# Features include the following: 
# year, sex, age, population, gdp per capita, generation, geographicRegion, suicideBY100KCat2

dfDt1 <- dfMasterRecords %>% select(-HDI.for.year, -country.year, -suicides_no, -gdp_for_year, -Suicide.100k.cat, 
                                          -suicidesPer100k, -gdpPerCaptiaCat)


# set up for repeated cross validation
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats =3)
set.seed(1122016)

# divide data into test and 
intrain <- createDataPartition(y = dfDt1$suicideBy100KCat2, p= 0.7, list = FALSE)
training <- dfDt1[intrain,]
testing <- dfDt1[-intrain,]

# use information gain for splits for training model 
dtree_fit <- train(suicideBy100KCat2 ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)

dtree_fit
# print out the decisioin tree
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

# test prediction with trained model 
test_pred <- predict(dtree_fit, newdata = testing)
cmLocal <- confusionMatrix(test_pred, testing$suicideBy100KCat2)

cmLocal$table
cmLocal$overall


# try with gini instead of information gain for dtree
dtree_fit_gini <- train(suicideBy100KCat2 ~., data = training, method = "rpart",
                        parms = list(split = "gini"),
                        trControl=trctrl,
                        tuneLength = 10)

dtree_fit_gini

pGini <- prp(dtree_fit_gini$finalModel, box.palette = "Blues", tweak = 1.2)

pGini

test_pred_gini <- predict(dtree_fit_gini, newdata = testing)

cmLocal2 <- confusionMatrix(test_pred_gini, testing$suicideBy100KCat2)
cmLocal2$table
cmLocal$overall

######################################
######################################
######################################
# Random Forest 
# try a classification random Forest 
set.seed(99)
# set training set of 22,976 ~ 80% of observations being evaluated 
train = sample(1:nrow(dfMasterRecords), 22976)


# throw out the HDI index feature and other features
dfRandomFor <- dfMasterRecords %>% select(-HDI.for.year, -country.year, -year, -suicides_no, -gdp_for_year, -Suicide.100k.cat, 
                                          -suicidesPer100k, -gdpPerCaptiaCat, -?..country)

dfRandomFor2 <- dfMasterRecords %>% select(-HDI.for.year, -country.year, -year, -suicides_no, -gdp_for_year, -suicideBy100KCat2, 
                                           -suicidesPer100k, -gdpPerCaptiaCat, -?..country)

str(dfRandomFor)

rfSuicideRateCat <- randomForest(suicideBy100KCat2 ~ ., data = dfRandomFor, subset = train)

rfSuicideRateCat

# error rate at 49.27% - perhaps too many categories for predicting.  Also, need to get country in there somehow, perhaps grouping 
# into geographic region. 

rfSuicideRateCat2 <- randomForest(suicideBy100KCat2 ~ ., data = dfRandomFor, mtry = 4, subset = train, importance = TRUE)
rfSuicideRateCat2

# mtry = 4 reduced OOB estimate pretty good 3.56 percent 
rfSuicideRateCat3 <- randomForest(suicideBy100KCat2 ~ ., data = dfRandomFor, mtry = 5, subset = train, importance = TRUE)
rfSuicideRateCat3

oob.err = double(13)
test.err = double(13)

for(mtry in 1:13){
    fit = randomForest(suicideBy100KCat2~., data = dfRandomFor, subset=train, mtry=mtry, ntree = 350)
    oob.err[mtry] = fit$mse[350]
    pred = predict(fit, dfRamdomFor[-train,])
    test.err[mtry] = with(dfRandomFor[-train,], mean( (suicideBy100KCat2-pred)^2 ))
}

fit$err.rate

?randomForest

# try a regular decision tree alone - hopefully it will be able to take factors of > 53 
dfDecTree <- dfMasterRecords %>% select( -HDI.for.year, -country.year, -suicides_no, -gdp_for_year, -Suicide.100k.cat, -gdpPerCaptiaCat,
                                         -suicidesPer100k, -gdpPerCaptiaCat)

dtSuicideRateCat2 <- train(suicideBy100KCat2 ~ ., data = dfDecTree, subset = train, method = "rpart" )
dtSuicideRateCat2

dtSuicideRateCat2_1 = predict(dtSuicideRateCat2, data = dfDecTree)
table(dtSuicideRateCat2_1, dfDecTree$suicideBy100KCat2)


## regular tree 
fit <- rpart(suicideBy100KCat2 ~ ., method = "class", data=dfDecTree)

plotcp(fit)
printcp(fit)
summary(fit)

plot(fit, uniform = TRUE, main = "Calssificaiton Tree for Suicide Prevention")
text(fit, use.n=TRUE, all=TRUE, cex=.8 )

dfRandomFor <- dfMasterRecords %>% select(-HDI.for.year, -country.year, -suicides_no, -Suicide.100k.cat, 
                                          -suicidesPer100k, -?..country)

anotherTree <- tree(suicideBy100KCat2 ~., data = dfRandomFor)
summary(anotherTree)
plot(anotherTree)
text(anotherTree, pretty = 0)
