### MATH 23C FINAL PROJECT: AN INVESTIGATION INTO BOSTON HOUSING AND WEATHER
### By Rudra Barua, Hope Ha, and Matti Tan

### THE DATASETS
boston <- read.csv("boston.csv"); head(boston)
# This is the Ames housing dataset, compiled by Dean De Cock, as a modernized
# and expanded version of the Boston Housing Dataset. 
# It can be found at this link: https://www.kaggle.com/c/house-prices-advanced-regression-techniques/overview

boston2 <- read.csv("boston2.csv"); head(boston2)
# This is the Ames housing dataset, compiled by Dean De Cock, as a modernized
# and expanded version of the Boston Housing Dataset. This has the added column
# of the sale prices of the houses, in comparison to boston.csv. 
# It can be found at this link: https://www.kaggle.com/c/house-prices-advanced-regression-techniques/overview

weather <- read.csv("Bostonweather.csv"); head(weather) 
# This is a Boston weather dataset, compiled by Jennifer Peng, that contains 
# Boston weather data from January 2013 to April 2018. 
# It can be found at this link: https://www.kaggle.com/jqpeng/boston-weather-data-jan-2013-apr-2018
# The data itself was collected from here: https://www.wunderground.com/history/airport/KBOS/2018/1/1/DailyHistory.html?&reqdb.zip=&reqdb.magic=&reqdb.wmo=

### DATASET STANDARDS
# 1. A dataframe. 
is.data.frame(boston); is.data.frame(boston2); is.data.frame(weather)
# all return TRUE, all are dataframes.

# 2. At least two categorical or logical columns.
head(boston$CentralAir); head(boston$PavedDrive)
# The columns CentralAir and PavedDrive are clearly logical columns, with Y/N 
# options, for whether the house has central air systems and a paved driveway
# respectively. 
head(boston$GarageType); head(boston$RoofStyle)
# The columns GarageType and RoofStyle are clearly categorical columns. The former
# has attached, detached, and builtin as different types of garages; similarly,
# the latter has Gable, Hip, Flat, and Gambrel as different types of roofs. 

# 3. At least two numeric columns.
head(boston$FullBath); head(boston$GarageCars); head(boston$OverallQual)
# Some examples of numerical columns: FullBath gives how many full bathrooms
# each house hase, GarageCars gives the size of the garage in car capacity,
# and OverallQual gives the rating of overall material and finish quality. 

# 4. At least 20 rows, preferably more, but real-world data may be limited.
nrow(boston); nrow(boston2); nrow(weather)
# The Boston Housing dataset has 1459 rows, the Boston Housing dataset with
# sale prices has 1460 rows, and the Boston Weather dataset has 3749 rows. 

### GENERAL ADDITIONAL POINTS:
# 1. A data set with lots of columns, allowing comparison of many different variables. ️
ncol(boston); ncol(boston2); ncol(weather)
# The Boston Housing dataset has 80 columns, the Bosting Housing dataset with 
# sale prices has 81 columns (as expected), and the Boston Weather dataset has 
# 25 columns, which allows for the comparison of many different variables. 

# 2. A data set that is so large that it can be used as a population from which
# samples are taken.
nrow(boston)
# Again, as the Boston Housing dataset has 1459 rows of available data, we 
# can successfully use it as a population from which to take samples (as done 
# later in the script in lines 527-550)

# 11. Nicely labeled graphics using ggplot, with good use of color, line styles,
# etc., that tell a convincing story.
# We use ggplot throughout the project. 

###############################################################################
### MODELLING THE DATA: Histograms and Barplots 

### REQUIRED GRAPHICAL DISPLAYS
# 2. A Histogram
#GGPlot histogram of Lot Area
library(ggplot2)
LotAreaPlot <- ggplot(boston, aes(x=LotArea)) + 
  geom_histogram(bins = 30,color="darkblue", fill="lightblue") 

#Final Plot of Lot Area in Square Feet
print(LotAreaPlot + labs(title = "Boston Housing Lot Area"
                         ,y= "Count", x = "Lot Area (Square Feet)"))

#A histogram of the different lot areas displays a leftward skew. It appears to
#be that very few houses in Boston have lot areas greater than 20,000 square feet.

### REQUIRED GRAPHICAL DISPLAYS
# 1. A Barplot
#GGPlot Barplot of Overall Quality
#Organising our data into counts of discrete values (score from 1-10)
#Overall Quality measures the material and finish quality right after construction
df <- as.data.frame(table(boston$OverallQual)); df

#This barplot has the exact count of each score on top of their respective bar
OverallQualityPlot <- ggplot(df, aes(x=Var1, y = Freq)) + 
  geom_bar(stat = "identity", color = "darkblue", fill = "lightblue") + 
  geom_text(aes(label = Freq), vjust=-0.3, size=3.5)+
  theme_minimal()

print(OverallQualityPlot + labs(title = "Boston Housing Overall Quality Rating",
                                y = "Count", x = "Rating (Score Out of 10)"))

#The barplot displays how the most common rating of overall quality is 5 with
#428 respondents giving this score. And, the least common raating is 1 with 
#only 2 respondents giving this score.

###############################################################################
### MODELLING THE DATA: Pie Charts

### ADDITIONAL POINTS
# 5. A graphical display that is different from those in the textbook or in the class scripts.
# AND
# 19. A graphical display that is different from those in the class scripts.

#Different Graphical Representation 
#Lot Configuration Type Counts
Inside <- length(which(boston$LotConfig == "Inside")) 
Corner <- length(which(boston$LotConfig == "Corner"))
FR2 <- length(which(boston$LotConfig == "FR2"))
CulDeSac <- length(which(boston$LotConfig == "CulDSac"))
FR3 <- length(which(boston$LotConfig == "FR3"))

#Types of locations and shapes of the lot such as whether or not it is an
#inside lot (Inside), corner lot (Corner), cul-de-sac (CulDSac), a lot with 
#frontage on 2  sides of the property (FR2), or a lot with frontage on 
#3 sides of the property (FR3)

#Creating a pie-chart
df <- data.frame(
  Type = c("Inside", "Corner", "Cul-De-Sac", "FR2", "FR3"),
  Count = c(Inside, Corner, CulDeSac, FR2, FR3)
)

#First constructing it as a barplot 
bp<- ggplot(df, aes(x="", y=Count, fill=Type))+
  geom_bar(width = 1, stat = "identity")

#Then re-configuring it to a pie-chart
pie <- bp + coord_polar("y", start=0) 
plot(pie)

#The pie-chart above illustrates how a majority of Boston houses have "inside"
#lot configurations while house lots that have frontage in 3 sides of the 
#property are the least common.

###############################################################################
### INVESTIGATION: How can we use novel statistics to examine the data for 
# the surface area of open porches in Boston houses? 

### ADDITIONAL POINTS:
# 13. Appropriate use of novel statistics (e.g. trimmed mean, maximum or minimum,
# skewness, ratios).

#Novel Statistics
#Let us take a more robust look at the data for the surface area of open
#porches in Boston houses. Since not all houses have open porches, we get
#a very wide spread of values that may lead us astray when we look at 
#central tendency values
Porch <- boston$OpenPorchSF; Porch #extracting the data

#Maximum and minimum
max(Porch)
min(Porch)
#As seen, we have a massive difference between the maximum and minimum 
#values of porch surface area sizes. 
length(which(Porch == 0)) 
#and we also have 642 houses that do not have open porches
642/1459 #this is about 44% of our data
mean(Porch) 
#Hence, this mean might not really capture the true average of Boston houses
#with porches because of the many outliers in this data.

# Another novel statistic we can use to gain information on the dataset is 
# skewness. 
# install.packages("e1071")
library(e1071)
skewness(Porch)

# as this returns 2.682255, it indicates that the surface area of open porches
# in Boston houses is skewed right, which means that most data falls to the 
# right side of the graph. 

#Another novel statistic that might give us more information is the median
median(Porch)#this is the middle value in the data

#But, probably one of the best measures of central tendency for this data
#is the trimmed mean. By taking a trimmed mean, we remove a predetermined 
#amount of the data (in this case, the lowest and highest values, outliers),
#and find an average for the remaining data observations 
#R has a built-in function for this:
mean(Porch, trim = 0.44) #trimming 44% from each side removes extreme values
#This looks like a more reasonable mean surface area for the average 
#Boston house porch

###############################################################################
### INVESTIGATION: What are the highest correlated variables? How might we model
# this?

### ADDITIONAL POINTS:
# 16. Appropriate use of covariance or correlation.

# We would like to make a heatmap of the correlations of the numeric variables. 
# Install corrplot
library(corrplot)
numeric <- which(sapply(boston2, is.numeric)) # Index the numeric columns
length(numeric) # 38 numeric columns
numcols <- boston2[,numeric] # Lets grab all the numeric columns
# We want only the correlations for the numeric columns
numcorrs <- cor(numcols, use="pairwise.complete.obs") 
# We want to sort the correlations by highest to lowest
corrs <- as.matrix(sort(numcorrs[,'SalePrice'], decreasing = TRUE)) 
# We can just ignore the correlations that are less than 0.05, otherwise our
# heatmap will get messy
newcorrs <- names(which(apply(corrs, 1, function(x) abs(x)>0.05)))
# Convert it to a matrix
numcorrs <- numcorrs[newcorrs, newcorrs]
corrplot(numcorrs, method="color",tl.cex = 0.7,number.cex=.7) # Pretty cool right!
# Now lets make a correlation matrix for just the relationships that have a correlation
# above 0.5
newcorrs <- names(which(apply(corrs, 1, function(x) abs(x)>0.5)))
# Convert it to a matrix
numcorrs <- numcorrs[newcorrs, newcorrs]
corrplot(numcorrs, method="number",tl.cex = 0.7,number.cex=.7) # Pretty cool right!

# A different style graph
corrplot.mixed(numcorrs, lower = "square", upper = "number", tl.cex = 0.7,cl.cex = .7, number.cex=.7,tl.pos = "lt")

###############################################################################
### INVESTIGATION: Is there a relationship between whether a house has
# central air conditioning and its overall condition? 

# We can investigate this using a permutation test. 

### REQUIRED GRAPHICAL DISPLAYS
# 1. A Barplot
#GGPlot Barplot of Overall Condition
#Organising our data into counts of discrete values (score from 1-10)
#Overall Condition measures the current state of the house in terms of 
#wear and tear, physical comfort and present structural stability. (i think lol)
df <- as.data.frame(table(boston$OverallCond)); df

#This barplot has the exact count of each score on top of their respective bar
OverallConditionPlot <- ggplot(df, aes(x=Var1, y = Freq)) + 
  geom_bar(stat = "identity", color = "darkblue", fill = "lightblue") + 
  geom_text(aes(label = Freq), vjust=-0.3, size=3.5)+
  theme_minimal()

print(OverallConditionPlot + labs(title = "Boston Housing Overall Condition Rating",
                                  y = "Count", x = "Rating (Score Out of 10)"))

### REQUIRED ANALYSIS
# 1. A permutation test.

#Permutation Test
sum(boston$CentralAir == "Y") #number of houses with central air conditioning
sum(boston$CentralAir == "N") #number of houses without central air conditioning

#Calculating the observed average of overall housing condition scores of 
#houses with and without central air conditioning 
#Houses With Central Air Conditioning Condition Average 
CA.Yes.Avg <- sum(boston$OverallCond*(boston$CentralAir == "Y"))/sum(boston$CentralAir == "Y"); CA.Yes.Avg 

#Houses Without Central Airconditioning Condition Average 
CA.No.Avg <- sum(boston$OverallCond*(boston$CentralAir == "N"))/sum(boston$CentralAir == "N"); CA.No.Avg

#Observed difference
observed <- CA.Yes.Avg - CA.No.Avg; observed 
#It appears that on average, houses with central air conditioning 
#have slightly higher scores

#Now replace Male with a random sample
CentralAir <- sample(boston$CentralAir); CentralAir #permuted Central Air Y/N column
sum(CentralAir == "Y")
#still have 1358 houses with central air conditioning
CA.Yes.Avg <- sum(boston$OverallCond*(CentralAir == "Y"))/sum(CentralAir == "Y"); CA.Yes.Avg
CA.No.Avg <- sum(boston$OverallCond*(CentralAir == "N"))/sum(CentralAir == "N"); CA.No.Avg
CA.Yes.Avg - CA.No.Avg #as likely to be negative or positive

#Repeat 10000 times
N <- 10000
Mean.Differences <- numeric(N)
for(i in 1:N){
  CentralAir <- sample(boston$CentralAir); CentralAir #permuted Central Air Y/N column
  CA.Yes.Avg <- sum(boston$OverallCond*(CentralAir == "Y"))/sum(CentralAir == "Y")
  CA.No.Avg <- sum(boston$OverallCond*(CentralAir == "N"))/sum(CentralAir == "N")
  Mean.Differences[i] <- CA.Yes.Avg - CA.No.Avg #as likely to be negative or positive
}

mean(Mean.Differences)#very close to 0!
hist(Mean.Differences, breaks = "FD")
#Now display the observed difference on the histogram (for instructive purposes)
#We won't use ggplot here for simplicity
abline(v = observed, col = "red") #This would ordinarily display a red vertical 
# line where our observed mean difference is -- however, it is so far at the ends
# of the graph, it is not visible. This is indeed confirmed by the extremely small
# pvalue. 
#What is the probability (the P value) that a difference this large
#could have arisen with a random subset?
pvalue <- (sum(Mean.Differences >= observed)+1)/(N+1); pvalue
#Since we have an incredibly small p-value that is certainly < 0.05, 
#we then have a minute chance that we'd observe such a discrepancy in our data.
#Therefore, there is sufficient evidence to reject the null hypothesis that 
#there is no significant difference in the average overall condition score 
#of Boston houses between houses that have and that do not have central
#air conditioning. Therefore, there is a relationship between whether a house 
# has central air conditioning and its overall condition. 

###############################################################################
### INVESTIGATION: Is there a relationship between whether a house has
# central air conditioning and whether a house has a paved driveway? 

# We can conduct this investigation by using a contingency table, followed by 
# a chi-square test. 

# We'll start with the contingency table. 

### REQUIRED GRAPHICAL DISPLAY
# 4. A Contingency Table. 
boston <- read.csv("boston.csv"); head(boston)
# identifying non-empty entries for PavedDrive and CentralAir
index<-which(!is.na(boston$PavedDrive) & !is.na(boston$CentralAir)&
               ((boston$PavedDrive == "Y")|(boston$PavedDrive == "N")) & 
               ((boston$CentralAir == "Y")|(boston$CentralAir == "N")))

length(index) # we selected 1427 rows
bostonCleaned <- boston[index,] # new, clean dataset
CentralAir2 <- boston$CentralAir[index] # new CentralAir
PavedDrive2 <- boston$PavedDrive[index] # new PavedDrive

# we can then turn these into logical columns, so that we can construct 
# our contingency table. 
CenAir <- CentralAir2 == "Y"; head(CentralAir2); head(CenAir) # this checks out
PD <- PavedDrive2 == "Y"; head(PavedDrive2); head(PD) # this also checks out
BostonLogical <- data.frame(CenAir, PD); head(BostonLogical) # new dataframe
# that has our logical columns. 

# we can then manually calculate the entries in our contingency table
CentralPave <- which(BostonLogical$CenAir&BostonLogical$PD);head(CentralPave)
# the above line of code filters for which houses have Central Åir and 
# Paved Driveways. 
length(CentralPave) # there are 1258 counts. 

CentralNotPave <- which(BostonLogical$CenAir& !BostonLogical$PD)
# the above line of code filters for which houses have Central Åir and 
# do NOT have Paved Driveways. 
length(CentralNotPave) # there are 74 counts.

NotCentralPave <- which(!BostonLogical$CenAir& BostonLogical$PD)
# the above line of code filters for which houses do NOT have Central Åir but
# do have Paved Driveways. 
length(NotCentralPave) # there are 43 counts. 

NotCentralNotPave <- which(!BostonLogical$CenAir& !BostonLogical$PD)
# the above line of code filters for which houses do NOT have Central Åir and
# do NOT have Paved Driveways. 
length(NotCentralNotPave) # there are 52 counts. 

# Given these counts, our contingency table should look like:
#                             CENTRAL AIR 
#                          FALSE      TRUE
#
#                 FALSE     52         43
# PAVED DRIVEWAY 
#                  TRUE     74       1258

# We can check this with the table function, and indeed:
table(BostonLogical$CenAir, BostonLogical$PD); table(bostonCleaned$CentralAir, bostonCleaned$PavedDrive)
# Using our logical columns and our cleaned dataset returns the same contingency
# table which matches with what we expected:
#       FALSE TRUE
# FALSE    52   43
# TRUE     74 1258

### REQUIRED ANALYSIS
# 3. Analysis of a contingency table. 

# Our null hypothesis is that whether a house in Boston has Central Air is 
# independent of whether the house has a Paved Driveway.

# Our contingency table serves as our observed counts:
Observed <- table(BostonLogical$CenAir, BostonLogical$PD); Observed
#        FALSE TRUE
# FALSE    52   43
# TRUE     74  1258

# We can analyze this contingency table by running a chi-squared test in order
# to understand whether a house in Boston has central air conditioning is related 
# to whether the house has a paved driveway. 

# Our expected counts are: 
Expected <- outer(rowSums(Observed), colSums(Observed))/sum(Observed); Expected
#          FALSE       TRUE
# FALSE   8.388227   86.61177
# TRUE  117.611773 1214.38823

# We can then run the chi-squared test on our data, either manually or through
# the built-in function. 

# Manually: 
Chi2 <-sum((Observed-Expected)^2/Expected); Chi2 # to calculate the chi-squared value
Pvalue<- pchisq(Chi2,1,lower.tail = FALSE); Pvalue # to find the p-value

chisq.test(BostonLogical$CenAir, BostonLogical$PD) # the built-in chi-square
# test confirms our result! 

# This returns that our p-value is 6.764544e-60, which means that the probability
# of this result occurring by chance is extremely low. As 6.764544e-60 is less than
# 0.01, we conclude that the result is significant, and as such, we can reject
# the null hypothesis that whether a house has central air conditioning is independent
# of whether a house has a paved driveway.  We therefore conclude that whether 
# a house has central air conditioning and whether a house has a paved driveway 
# are dependent in this dataset (they share some sort of relationship).

###############################################################################
### INVESTIGATION: Does Lot Area predict Garage Area? 

### ADDITIONAL POINTS:
# 14. Use of Linear regression. 

#Linear Regression
#We shall investigate whether or not Lot Area predicts Garage Area 
#This will inform us about how homeowners allocate the amount of land 
#they have for housing. Hence, we are using Lot Area (in square feet) as the
#predictor for Garage Area (in square feet). 

#Making a scatter plot of the data 
LotvGaragePlot <- ggplot(boston, aes(x=LotArea, y=GarageArea)) + 
  geom_point()

print(LotvGaragePlot + labs(title = "Housing Garage Area Against Lot Area"
                            , x = "Lot Area (Square Feet)", 
                            y = "Garage Area (Square Feet)"))

#Using the built-in R function 
Boston.lm <- lm(boston$GarageArea ~ boston$LotArea, data = boston); Boston.lm

#Plotting the regression line 
coeff=coefficients(Boston.lm)
#Regression Line Equation
eq = paste0("y = ", round(coeff[2],3), "*x + ", round(coeff[1],3))

#Adding to plot
print(LotvGaragePlot + 
        labs(title = "Housing Garage Area Against Lot Area"
             , x = "Lot Area (Square Feet)", y = "Garage Area (Square Feet)") 
      + geom_abline(intercept = 336.97230, slope = 0.01383, color="blue", 
                    linetype="dashed")+ ggtitle(eq)) 

#Hence, the line of best fit that we find through linear regression depicts
#how there might be a positive relationship/correlation between lot area 
#and garage area. An increase in lot area might predict an increase in the
#garage area of the house. Intuitively, this is sensible since a larger lot area
#will allow for more land to be allocated for garages. Intriguingly, this might
#be indicative of how people choose to use the lot area they have.

###########################################################################
### INVESTIGATION: Is the lot area of a house connected to the type of sale 
# condition of Boston Houses? 

### ADDITIONAL POINTS: 
# 15. Calculation and display of a logistic regression curve. 

#Logistic Regression
#We will use lot area to determine the type of sale condition of Boston Houses.
#There are generally 2 types of sale condition: Normal (typical single buyer 
#and seller) and Non-normal (Sold to a family, possible loan etc.)
#It will be interesting to investigate whether the lot area of a house is 
#connected to the type of sale which could be an indicator of the type of 
#buyers for Boston houses
library(stats4) #will need this library (install if necessary)

#In this revised data set, the sale type entries were replaced with binary
#values 1 and 0 from "Normal" and the other non-normal entries, respectively. 

#Now we can extract the  as a Bernoulli random variable:
Y <- boston$SaleCondition; Y

#To perform logistic regression, we will assume that 
#p = exp(alpha x+beta)/(1 + exp(alpha x+beta))
#where 0 =< p =< 1

#First, we shall model the probability of a sale condition
# as a function of lot area
#Here, the predictor is the lot area column 
LA <- boston$LotArea; LA

#Creating the log-likelohood function for lot frontage as the predictor
MLL<- function(alpha, beta) {
  -sum( log( exp(alpha+beta*LA)/(1+exp(alpha+beta*LA)) )*Y
        + log(1/(1+exp(alpha+beta*LA)))*(1-Y) )
}

results<-mle(MLL, start = list(alpha = 0, beta = 0))#initial guess
results@coef#from this we know that...
#alpha = 1.554842e-08
#beta = 1.451108e-04

plot(LA,Y)
curve( exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)

#The curve found through logistic regression appears to be a reasonable
#approximation of how lot area and sale type are related due to the points in
#the upper portion of the graph being crossed by the curve. Hence, there appears 
#to be a positive relationship between normal type sales and lot area.  
#However, without an appropriate statistical test and as the lower points are 
#not captured by the curve, it is not possible to draw a definite conclusion.

###############################################################################

### INVESTIGATION: Is there a statistically significant difference between the 
# average sale price in a house in a cul-de-sac versus a house on a corner? 

### REQUIRED ANALYSIS:
# 4. Comparison of analysis by classical methods (chi-square, CLT) and simulation methods.

### ADDITIONAL POINTS: 
# 12. An example where permutation tests or other computational techniques clearly work better
# than classical methods.

# To answer this, we can use both simulation and classical means. We begin by 
# using simulation with a permutation test. 

# We only want data for	corner lots and	cul-de-sac lots
lotconfig <- boston2[boston2$LotConfig == "CulDSac" | boston2$LotConfig == "Corner",]
# We want only want the Sale Prices
config <- boston2[c("LotConfig", "SalePrice")]
config <- na.omit(config)
# Choose out the CulDSac/Corner data indexes
idxCD <- which(config$LotConfig == "CulDSac")
idxCO <- which(config$LotConfig == "Corner")
CDprice <- config$SalePrice[idxCD]
# Get the average sale price of CulDSac houses
CDavg <- mean(CDprice); CDavg
COprice <- config$SalePrice[idxCO]
# Get the average sale price of Corner houses
COavg <- mean(COprice); COavg
# Take the difference of the two to see which has the higher number average sale price
obsdiff <- CDavg - COavg; obsdiff 
# This implies that on average, houses on cul-de-sacs sell at a higher sale price
# than houses on corners

type <- sample(config$LotConfig);
CDavg <- sum(config$SalePrice*(type=="CulDSac"))/sum(type=="CulDSac"); CDavg
COavg <- sum(config$SalePrice*(type=="Corner"))/sum(type=="Corner"); COavg
CDavg - COavg   # As likely to be negative or positive
# Repeat 10000 times for our permutation test
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  type <- sample(config$LotConfig) # Permuted lot config column
  CDavg <- sum(config$SalePrice*(type=="CulDSac"))/sum(type=="CulDSac")
  COavg <- sum(config$SalePrice*(type=="Corner"))/sum(type=="Corner")
  diffs[i] <- CDavg - COavg   # As likely to be negative or positive
}
mean(diffs) # Should be close to zero
hist(diffs, breaks = "FD")
# Now display the observed difference on the histogram
abline(v=obsdiff)
pvalue <- (sum(diffs >= obsdiff)+1)/(N+1); pvalue
# The probability (the P value, 9.999e-05) that a difference this large
# could have arisen with a random subset is 0.019998%. 
# Our data provides sufficient evidence against the null hypothesis of there being
# no difference in the average sale price of a house located in a cul-de-sac over
# a house located on a corner.
# Instead it provides sufficient evidence for the hypothesis that there is 
# a difference in the average sale price of a house located in a cul-de-sac over
# a house located on a corner.
# We can asses that the average sale price of a house on a cul-de-sac in Boston
# is greater than the average sale price of a house on a corner in Boston.

# We can now use classical means and compare, so let us look at a two-sample t-test
t.test(config$SalePrice[idxCD], config$SalePrice[idxCO], var.equal = TRUE)
# This also gives us a p-value of 6.344e-05, which is very comparable to our 
# earlier p-value, and confirms our earlier results. 

# although they are comparable, our permutation test results are more reliable
# because our two-sample t-test particularly relies on the assumption that 
# our data follows a normal distribution, while the permutation test has no 
# such assumptions or requirements. 

###############################################################################

### INVESTIGATION: GrLivArea is the above grade (ground) living area square feet,
# and if random variable Y is the above ground living area square feet of one 
# of the houses in Boston, it might not be unreasonable to assume that the 
# random variable X = (Y − mu)/sigma has a standard normal distribution, especially
# given the large number of data points we have. According to the Central
# Limit Theorem, if we take our population with mean μ and standard deviation sigma
# and we take sufficiently large random samples from the population, then the
# distribution of the sample means will be approximately normally distributed.
# Thus, we would theoretically expect our random variable X = (Y − mu)/sigma to have 
# a standard normal distribution. 

# We want to perform a simulation by taking samples of six houses at a time, 
# to see whether this sampling distribution behaves as if drawn from a population
# with a normal distribution.

### ADDITIONAL POINTS
# 18. Use of theoretical knowledge of sampling distributions.

### ADDITIONAL POINTS:
# 2. A data set that is so large that it can be used as a population from which
# samples are taken.

hist(boston$GrLivArea, prob = TRUE) # looks like it could be normal
GrLivArea <- boston$GrLivArea
mu <- mean(GrLivArea)
sigma <- sd(GrLivArea)
N <- 10000
n <- 6
get.sample <- function() sample(GrLivArea, n) # defining our sampling function

# To standardize 10000 samples: 
standardizedsample <- numeric(N)
for(i in 1:N) {
  x <- get.sample()
  standardizedsample[i] <- (mean(x) - mu)/(sigma / sqrt(n))
}

# We can now make a histogram of our standardized samples: 

### REQUIRED GRAPHICAL DISPLAYS
# 3. Probability Density Graph
hist(standardizedsample, prob = TRUE) # histogram
curve(dnorm(x), add = TRUE) # normal distribution curve

# Indeed, our standard normal curve fits our histogram fairly well,
# and so our variable X does seem to be distributed standard normal. 

###############################################################################

### INVESTIGATION: What range of house sale prices captures the mean 95% of the
# time? 

# We can use a student-t confidence interval to answer this question. 

### ADDITIONAL POINTS:
# 20. Calculation of a confidence interval.
boston2 <- read.csv("boston2.csv")
head(boston2)

# Let us make a histogram of the means for 10000 samples
N <- 10^4 # Trials
n <- 10 # Sample size
mu <- mean(boston2$SalePrice); mu # Mean: 180921.2
sigma <- sd(boston2$SalePrice); sigma # Standard dev: 79442.5
xbar <- numeric(N) # To store our results

# Let us generate our samples
for (i in 1:N) {
  samp <- sample(boston2$SalePrice, n) # Take size n sample
  xbar[i] <- mean(samp) # Store the sample mean
}
# Now lets compare the samples with the corresponding normal distribution.
hist(xbar, probability = TRUE, main = "Histogram of Sample Sales Price Means", breaks = "FD")

# Overlay normal density curve
curve(dnorm(x, mu, sigma/sqrt(n)), add = TRUE) # It fits pretty well

# Now let us construct the student t 95% confidence interval
# This is a modification of Paul's 8D Script
# Initialize the counts for missing the lower/upper points of confidence interval 
missedL <- 0; missedU <- 0
# Create a good sized plot
plot(x = c(mu - 5000, mu + 5000), y = c(1,100), type = "n", xlab = "", ylab = "")
# Lets take 1000 samples
N <- 1000
counter <- 0 # Set counter to 0
# Run the loop
for (i in 1:N) {
  x <- sample(boston2$SalePrice, n) # Take samples size of 10
  L <- mean(x) + qt(0.025, n - 1) * sd(x)/sqrt(n) # Lower endpoint
  U <- mean(x) + qt(0.975, n - 1) * sd(x)/sqrt(n) # Upper endpoint
  # Increment the value is missing the upper or lower endpoint
  if (mu < L) missedL <- missedL + 1
  if (mu > U) missedU <- missedU + 1
  if (L < mu && U > mu) counter <- counter + 1
  if (i <= 100) segments(L, i, U, i) # Plot the results 
}

# Plot a vertical line for the true mean
abline(v = mu, col = "blue")
# What fraction of the time did the interval include the true mean?
(N - missedL - missedU)/N
# What fraction of the time did it miss to the left?
missedL/N
# What fraction of the time did it miss to the right?
missedU/N
# This confidence interval is not to be trusted because the errors are not symmetrical.

# We can use the built-in t-test function to confirm:
t.test(boston2$SalePrice, conf.level=0.95)$conf.int

###############################################################################

### INVESTIGATION: Does the average wind in Boston from 2013-2018 follow a 
# Gamma Distribution? 

### ADDITIONAL POINTS 
# 17. Use of theoretical knowledge of chi-square, gamma, or beta distributions.

# As we saw in Paul's seminar topic -- his gamma fitter app -- the average 
# wind speed at the Carleton Turbine can be fitted using a gamma distribution
# quite well. 

# As we are also exploring Boston Housing, we'd also like to explore Boston 
# temperature and see whether the average wind speed in Boston also can be 
# well-approximated using a gamma distribution. 
library(stats4)  
weather <- read.csv("Bostonweather.csv"); head(weather)
AverageWind <- weather$Avg.Wind..mph. # our average wind values from the dataset
# we set all initial values to 1. 
shape <- 1
rate <- 1
xshape <- 1
xrate <- 1

hist(AverageWind, prob = TRUE) # this looks promising for a gamma distribution! 

# using Paul's function to calculate the negative log-likelihood to find our
# paramters based on the data: 
MLL<- function(shape, rate) {
  -sum(log(dgamma(AverageWind,shape,rate)))
}
results<-mle(MLL, start = list(shape = xshape, rate  = xrate)) #an initial guess is required
shape <- results@coef[1]; rate <- results@coef[2]
shape; rate
# Our shape parameter is therefore 8.44354, and our rate parameter is 0.7792171. 

### REQUIRED GRAPHICAL DISPLAYS
# 3. Probability Density Graph

# We can now overlay a gamma curve with these parameters over our histogram 
# to visually understand whether this is a good fit. 
hist(AverageWind, prob=TRUE)
curve(dgamma(x, shape, rate=rate), add = TRUE)
# Indeed, it looks like this curve is a good fit for our data. 

# We can also check the mean and variance of the average wind speed, and see
# if it matches with the calculated mean and variance based on our gamma 
# distribution: 
mean(AverageWind); var(AverageWind) # the true mean and variance
round(shape/rate,4); round(shape/rate^2,6) # mean and variance based on gamma distribution
# indeed, they are very close! 

# To see if a statistical test matches our visual understanding, we can 
# conduct a chi-square test against the null hypothesis that our data
# does follow a gamma distribution. Given our earlier display, we would expect
# this relationship to NOT be statistically significant -- and that is, we 
# expect to not be able to reject the null hypothesis that our data does 
# follow a gamma distribution.

### ADDITIONAL POINTS
# 6. Appropriate use of R functions for a probability distribution other than binomial, normal, or chi-square

# to conduct our chi-square test to see whether this is the actually the case
# we start by cutting our observed values into 10 bins: 
bins <- qgamma(0.1 * (0:10), shape = 8.44354, rate = 0.7792171); bins
observed <- as.data.frame(table(cut(AverageWind, bins, labels = FALSE)))$Freq
observed

# to calculate our expected values: 
expected <- sum(observed)/10; expected

# calculating the chi-square vaulue: 
Chi2 <-sum((observed-expected)^2/expected); Chi2

### REQUIRED ANALYSIS
# 2. A p-value or other statistic based on a distribution function.

# To find the p-value based on the distribution of the chi-squared function:
Pvalue<- pchisq(Chi2,8,lower.tail = FALSE); Pvalue
# Our p-value is 2.880328e-22. 

# ADDITIONAL POINTS:
# 8. A convincing demonstration of a relationship that might not have been 
# statistically significant but that turns out to be so.

# Interpreting our p-value: 
# As our p-value is 1.162261e-21, which is less than 0.05, we have found that 
# our result is statistically significant.  As such, we reject the null hypothesis
# that our data follows a gamma distribution, despite our earlier analysis
# that suggested the average windspeed DID follow a gamma distribution. 

#################################################################################

