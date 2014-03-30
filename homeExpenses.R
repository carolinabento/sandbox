# This is the R code that I used to create the Home Expenses Analysis (http://carolbento.com/homeExpenses.html)
# The data for the analysis of the 1st trimester of 2013 were
#	- march.csv
#	- april.csv
#	- may.csv


#########################
#	Auxiliary Fucntions	#
#########################


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




# args 	: df1 adnd df2 - data frame corresponding to the expenses in 2 distinct months
#
# Given the 2 data frames, returns an aggregate data frame consisting in the sum (amount spent) by category in each month.
# Categories that are present in only one of the data frames, are also included
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




# General statistical of the data

#March
marchSum <- createSumDataFrame(marchExpenses,marchCategories);
marchTotal <- sum(marchSum$Total);
marchMean <- mean(marchSum$Total);
marchMedian <-  median(marchSum$Total);
marchMode <- mode(marchSum$Total);

#April
aprilSum <- createSumDataFrame(aprilExpenses,aprilCategories);
aprilTotal <- sum(aprilSum$Total);
aprilMean <- mean(aprilSum$Total);
aprilMedian <-  median(aprilSum$Total);
aprilMode <- mode(aprilSum$Total);

#May
maySum <- createSumDataFrame(mayExpenses,mayCategories);
mayTotal <- sum(maySum$Total);
mayMean <- mean(maySum$Total);
mayMedian <-  median(maySum$Total);
mayMode <- mode(maySum$Total);


# This was used to create the first chart
# Total spent in the trimester
total <- c(marchTotal,aprilTotal,mayTotal);

# Common product categories for the trimester
commonCategories <- intersect(intersect(marchCategories,aprilCategories), mayCategories)

# This was used to create the second chart, i.e., the pie chart
# This chart only shows the total amount spent for the common categories
total3M <- aggregateTotal(aggregateTotal(marchSum,aprilSum), maySum)



# This was used to create the third chart, i.e., the stacked bar chart
# In this case, the 'Eat Out' and 'Take Away' categories were merged into the 'Eat Out' category, 
#and the 'Meat' and 'Fish' categories were merged, for better reading/analysis of the chart

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
