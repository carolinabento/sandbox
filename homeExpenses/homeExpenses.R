# This is the R code that I used to create the Home Expenses Analysis (http://carolbento.com/homeExpenses.html)
# The data for the analysis of the 1st semester of 2013 were
#	- march.csv
#	- april.csv
#	- may.csv
#	- june.csv
#	- july.csv
#	- august.csv


#####
#
#	Auxiliary Functions
#
#####


# args 	: data - data frame with monthly data
#		: categoriesVector - unique category vector for the given monthly data
#
#
# Given a data frame and a vector with unique categories, returns another data fram with sum (amount spent) by category

createSumDataFrame <- function(data,categoriesVector){
sumDf <- data.frame(Category= character(0),Total=numeric(0))
	for(cat in categoriesVector){
	row <- data.frame(Category = cat, Total = sum(subset(data, Category == cat)$Price))
	sumDf <- rbind(sumDf,row)
	}
sumDf;
}



# args 	: categoriesVector - unique category vector for the given monthly data
#
# Given a vector of unique categories, returns another data frame with the sum (amount spent) by the common categories for all months
createCommonCategorySumDf <- function(categoriesVector){
commonSum <-data.frame(Month= character(0),Category= character(0),Total=numeric(0))
	for(commonCat in categoriesVector){
			sumMarch <- sum(subset(marchSum,Category==commonCat)$Total)
			sumApril <- sum(subset(aprilSum,Category==commonCat)$Total)
			sumMay <- sum(subset(maySum,Category==commonCat)$Total)

			marchRow <-  data.frame(Month="March",Category=commonCat,Total=sum(subset(marchSum,Category==commonCat)$Total))

			aprilRow <-  data.frame(Month="April",Category=commonCat,Total=sum(subset(aprilSum,Category==commonCat)$Total))

			mayRow <-  data.frame(Month="May",Category=commonCat,Total=sum(subset(maySum,Category==commonCat)$Total))
			
			commonSum<- rbind(commonSum,marchRow)
			commonSum<- rbind(commonSum,aprilRow)
			commonSum<- rbind(commonSum,mayRow)
	}
	commonSum
}


# args 	: data - data frame
#		: cat - category
#
# (Alias function to obtain the Total column in the data frame, specifically dealing with non existing categories)
# Given a data frame and a catgeory, returns the sum (amount spent) in that category
getTotal <- function(data,cat){

	res <- (subset(data, Category == cat)$Total)[1]
	out <- 0
	if(! is.na(res)){
		out <-res
	}

	out
}


# args 	: df1 and df2 - data frames
#
#Given two data frames, returns the aggregate total of the products in those data frames
aggregateTotal <- function(df1, df2){

	resDf <- data.frame(Category=character(0), Total=numeric(0))

	df1Categories <- levels(df1$Category)
	df2Categories <- levels(df2$Category)

	for(cat in df1Categories){
		if(cat %in% df2Categories){
			row <- data.frame(Category= cat, Total= (subset(df1, Category==cat)$Total + subset(df2, Category==cat)$Total))
			resDf <- rbind(resDf,row)
		}else{
			row <- data.frame(Category= cat, Total= (subset(df1, Category==cat)$Total))
			resDf <- rbind(resDf,row)
		}
	}

	for(cat in df2Categories){
		if(!(cat %in% df1Categories)){
			row <- data.frame(Category= cat, Total= subset(df2, Category==cat)$Total)
			resDf <- rbind(resDf,row)
		}
	}
	resDf
}





#########################
#	Trimester Analysis	#
#########################



#Loading the data and selecting the categories for each month
marchExpenses <- read.csv("./data/march.csv",quote="",header=TRUE)
marchCategories <- as.vector(unique(marchExpenses$Category))

aprilExpenses <- read.csv("./data/april.csv",quote="",header=TRUE)
aprilCategories <- as.vector(unique(aprilExpenses$Category))

mayExpenses <- read.csv("./data/may.csv",quote="",header=TRUE)
mayCategories <- as.vector(unique(mayExpenses$Category))


juneExpenses <- read.csv("./data/june.csv",quote="",header=TRUE)
juneCategories <- as.vector(unique(juneExpenses$Category))

julyExpenses <- read.csv("./data/july.csv",quote="",header=TRUE)
julyCategories <- as.vector(unique(julyExpenses$Category))

augustExpenses <- read.csv("./data/august.csv",quote="",header=TRUE)
augustCategories <- as.vector(unique(augustExpenses$Category))

# Load the utiliy functions
source("../utils.R")


# General statistical of the data

#March
marchSum <- createSumDataFrame(marchExpenses,marchCategories);
marchTotal <- sum(na.omit(marchSum$Total));
marchMean <- mean(na.omit(marchSum$Total));
marchMedian <-  median(na.omit(marchSum$Total));
marchMode <- mode(na.omit(marchSum$Total);

#April
aprilSum <- createSumDataFrame(aprilExpenses,aprilCategories);
aprilTotal <- sum(na.omit(aprilSum$Total));
aprilMean <- mean(na.omit(aprilSum$Total));
aprilMedian <-  median(na.omit(aprilSum$Total));
aprilMode <- mode(na.omit(aprilSum$Total));

#May
maySum <- createSumDataFrame(mayExpenses,mayCategories);
mayTotal <- sum(na.omit(maySum$Total));
mayMean <- mean(na.omit(maySum$Total));
mayMedian <-  median(na.omit(maySum$Total));
mayMode <- mode(na.omit(maySum$Total));


#June
juneSum <- createSumDataFrame(juneExpenses,juneCategories);
juneTotal <- sum(na.omit(juneSum$Total));
juneMean <- mean(na.omit(juneSum$Total));
juneMedian <-  median(na.omit(juneSum$Total));
juneMode <- mode(na.omit(juneSum$Total));

#July
julySum <- createSumDataFrame(julyExpenses,julyCategories);
julyTotal <- sum(na.omit(julySum$Total));
julyMean <- mean(na.omit(julySum$Total));
julyMedian <-  median(na.omit(julySum$Total));
julyMode <- mode(na.omit(julySum$Total));

#August
augustSum <- createSumDataFrame(augustExpenses,augustCategories);
augustTotal <- sum(na.omit(augustSum$Total));
augustMean <- mean(na.omit(augustSum$Total));
augustMedian <-  median(na.omit(augustSum$Total));
augustMode <- mode(na.omit(augustSum$Total));


# Total spent in the trimester
# Values used to create the first chart
marchTotal <- sum(marchSum$Total)
aprilTotal <- sum(aprilSum$Total)
mayTotal <- sum(maySum$Total)
juneTotal <- sum(juneSum$Total)
julyTotal <- sum(julySum$Total)
augustTotal <- sum(augustSum$Total)

total <- c(marchTotal,aprilTotal,mayTotal,juneTotal,julyTotal,augustTotal);

# Common product categories for the trimester

commonCategories <- intersect(intersect(intersect(intersect(intersect(marchCategories,aprilCategories),
								mayCategories),juneCategories),julyCategories),augustCategories)



#Monthly expenses by category
commonSumDf <- createCommonCategorySumDf(commonCategories)


marchCommon <- subset(commonSumDf,commonSumDf$Month=="March")$Total
aprilCommon <- subset(commonSumDf,commonSumDf$Month=="April")$Total
mayCommon <- subset(commonSumDf,commonSumDf$Month=="May")$Total
juneCommon <- subset(commonSumDf,commonSumDf$Month=="June")$Total
julyCommon <- subset(commonSumDf,commonSumDf$Month=="July")$Total
augustCommon <- subset(commonSumDf,commonSumDf$Month=="August")$Total



# Values used to create the third chart, i.e., the stacked bar chart
# In this case, the 'Eat Out' and 'Take Away' categories were merged into the 'Eat Out' category, 
#and the 'Meat' and 'Fish' categories were merged, for better reading/analysis of the chart
totalAggregate <- aggregateTotal(aggregateTotal(aggregateTotal(aggregateTotal(marchSum,aprilSum), maySum),juneSum),augustSum)

marchSumTotalByCat <- c(getTotal(marchSum,'Groceries'),
	getTotal(marchSum,'Dairies'),
	getTotal(marchSum,'Meat'),
	getTotal(marchSum,'Fish'),
	getTotal(marchSum,'Fruits and Vegetables'),
	getTotal(marchSum,'Hygiene'),
	getTotal(marchSum,'Take Away'),
	getTotal(marchSum,'Home Supplies'),
	getTotal(marchSum,'Gas'),
	getTotal(marchSum,'Eat Out'),
	getTotal(marchSum,'Beverages'),
	getTotal(marchSum,'Books'),
	getTotal(marchSum,'Clothing and Accessories'))

aprilSumTotalByCat <- c(getTotal(aprilSum,'Groceries'),
	getTotal(aprilSum,'Dairies'),
	getTotal(aprilSum,'Meat'),
	getTotal(aprilSum,'Fish'),
	getTotal(aprilSum,'Fruits and Vegetables'),
	getTotal(aprilSum,'Hygiene'),
	getTotal(aprilSum,'Take Away'),
	getTotal(aprilSum,'Home Supplies'),
	getTotal(aprilSum,'Gas'),
	getTotal(aprilSum,'Eat Out'),
	getTotal(aprilSum,'Beverages'),
	getTotal(aprilSum,'Books'),
	getTotal(aprilSum,'Clothing and Accessories'))

maySumTotalByCat <- c(getTotal(maySum,'Groceries'),
	getTotal(maySum,'Dairies'),
	getTotal(maySum,'Meat'),
	getTotal(maySum,'Fish'),
	getTotal(maySum,'Fruits and Vegetables'),
	getTotal(maySum,'Hygiene'),
	getTotal(maySum,'Take Away'),
	getTotal(maySum,'Home Supplies'),
	getTotal(maySum,'Gas'),
	getTotal(maySum,'Eat Out'),
	getTotal(maySum,'Beverages'),
	getTotal(maySum,'Books'),
	getTotal(maySum,'Clothing and Accessories'))

juneSumTotalByCat <- c(getTotal(juneSum,'Groceries'),
	getTotal(juneSum,'Dairies'),
	getTotal(juneSum,'Meat'),
	getTotal(juneSum,'Fish'),
	getTotal(juneSum,'Fruits and Vegetables'),
	getTotal(juneSum,'Hygiene'),
	getTotal(juneSum,'Take Away'),
	getTotal(juneSum,'Home Supplies'),
	getTotal(juneSum,'Gas'),
	getTotal(juneSum,'Eat Out'),
	getTotal(juneSum,'Beverages'),
	getTotal(juneSum,'Books'),
	getTotal(juneSum,'Clothing and Accessories'))

julySumTotalByCat <- c(getTotal(julySum,'Groceries'),
	getTotal(julySum,'Dairies'),
	getTotal(julySum,'Meat'),
	getTotal(julySum,'Fish'),
	getTotal(julySum,'Fruits and Vegetables'),
	getTotal(julySum,'Hygiene'),
	getTotal(julySum,'Take Away'),
	getTotal(julySum,'Home Supplies'),
	getTotal(julySum,'Gas'),
	getTotal(julySum,'Eat Out'),
	getTotal(julySum,'Beverages'),
	getTotal(julySum,'Books'),
	getTotal(julySum,'Clothing and Accessories'))


augustSumTotalByCat <- c(getTotal(augustSum,'Groceries'),
	getTotal(augustSum,'Dairies'),
	getTotal(augustSum,'Meat'),
	getTotal(augustSum,'Fish'),
	getTotal(augustSum,'Fruits and Vegetables'),
	getTotal(augustSum,'Hygiene'),
	getTotal(augustSum,'Take Away'),
	getTotal(augustSum,'Home Supplies'),
	getTotal(augustSum,'Gas'),
	getTotal(augustSum,'Eat Out'),
	getTotal(augustSum,'Beverages'),
	getTotal(augustSum,'Books'),
	getTotal(augustSum,'Clothing and Accessories'))

