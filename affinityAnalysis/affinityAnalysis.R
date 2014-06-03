# This is the R code that I used to create the Affinity Analysis (http://carolbento.com/homeExpenses.html)
# The data for the analysis of the 1st semester of 2013 were
#	- march.csv
#	- april.csv
#	- may.csv
#	- june.csv
#	- july.csv
#	- august.csv

library("arules");
library("arulesViz");

#####
#
#	Auxiliary Functions
#
#####

# Aggregate transactions by date
# args 	: data - data frame
#
# Performs a inner join of the data frame with the transactions and a data frame with unique ids
# for those transactions
getTransactions <- function(data){
	transDates <- as.vector(unique(data$Date))
	transIds <- seq(1:length(transDates))

	transDf <- data.frame(TransId=transIds,Date=transDates)
	
	merge(data,transDf, by="Date")
}

# Create a csv file with <transactionItems>
# args 	: data - data frame with the transactions
#		: filename - name of the output file
#
# Given a data frame with transactions, they are output to the file with each line containing
# the set of items in the transaction
createTransactionFiles <- function(data, filename){
	if(file.exists(filename)){
		file.remove(filename)
	}

	transIds <- (unique(data$TransId))

	for(id in transIds){
		transString <- paste(as.vector(unique(subset(data, TransId == id))$Name),collapse=",")
		write.table(transString,filename, quote=FALSE,append=TRUE, col.names=FALSE,row.names=FALSE)
	}
}


#########################
#	Affinity Analysis	#
#########################


#load monthly food and expenses recods
march <- read.csv("data/march.csv", header=TRUE)
april <- read.csv("data/april.csv", header=TRUE)
may <- read.csv("data/may.csv", header=TRUE)
june <- read.csv("data/june.csv", header=TRUE)
july <- read.csv("data/july.csv", header=TRUE)
august <- read.csv("data/august.csv", header=TRUE)


#keep only the information that is needed: ie, Date and Name columns
march <- subset(march, Category == "Groceries" | Category == "Fruits and Vegetables" | Category == "Meat" | Category == "Fish" | Category == "Hygiene" | Category == "Dairies",select = -c(2,3,5,6,7))
april <- subset(april, Category == "Groceries" | Category == "Fruits and Vegetables" | Category == "Meat" | Category == "Fish" | Category == "Hygiene" | Category == "Dairies",select = -c(2,3,5,6,7))
may <- subset(may, Category == "Groceries" | Category == "Fruits and Vegetables" | Category == "Meat" | Category == "Fish" | Category == "Hygiene" | Category == "Dairies",select = -c(2,3,5,6,7))
june <- subset(june, Category == "Groceries" | Category == "Fruits and Vegetables" | Category == "Meat" | Category == "Fish" | Category == "Hygiene" | Category == "Dairies",select = -c(2,3,5,6,7))
july <- subset(july, Category == "Groceries" | Category == "Fruits and Vegetables" | Category == "Meat" | Category == "Fish" | Category == "Hygiene" | Category == "Dairies",select = -c(2,3,5,6,7))
august <- subset(august,Category == "Groceries" | Category == "Fruits and Vegetables" | Category == "Meat" | Category == "Fish" | Category == "Hygiene" | Category == "Dairies",select = -c(2,3,5,6,7))



#associate the purchases with transaction ids
monthData <- subset(rbind(march,april,may,june,july,august), Name != "")
transactions <- getTransactions(monthData)


#create transactions file
createTransactionFiles(transactions, "../market-basket/basket.csv")

#load the transaction file
transactions <- read.transactions("../market-basket/basket.csv", format = "basket", sep=",");

#support => a frequent item set appears at least in 10% of the transactions
#confidence => for 50% of the transactions containing the given itemset the rule is correct.
rules <- apriori(transactions, parameter=list(support=0.1, confidence=0.5));

#plot relative item frequency for all the items in the transactions
# that appear in, at least, 10% of the transactions
png("./itemFrequency.png", width=1200, height=800)
itemFrequencyPlot(transactions,support = 0.1, cex.names=1.5, cex.axis=1.5, cex.lab=1.1, col="#2f7ed8", ylab="Item Frequency (Relative)")
dev.off()

#top-10
png("./itemFrequencyTop10.png", width=1200, height=800)
itemFrequencyPlot(transactions,support = 0.1, cex.names=1.5, cex.axis=1.5, cex.lab=1.1, col="#7658BE", ylab="Item Frequency (Relative)", topN=10)
dev.off()

#select the top-30 rules according to importance (lift)
top30 <- head(sort(rules, by="lift"), 30);

#plot the associations between rules as a graph
png("./rulesGraph.png", width = 1500, height=1000)
plot(top30, method="graph",control=list(cex=1.75, main=""));
dev.off()