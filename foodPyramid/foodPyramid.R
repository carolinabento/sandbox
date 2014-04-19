# This is the R code that I used to create the Home Expenses Analysis (http://carolbento.com/foodPyramid.html)
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
#		: dictionary - dictionary with entries in the format <product,foodGroup>
#
#
# Given a data set and a dictionary, assigns a food pyramid category to each product and discards the non-food products

matchFoodGroup <- function(data, dictionary){

	resultDf <- data.frame(Product = character(0), Group = character(0), Quantity = numeric(0), Price = numeric(0))
	products <- as.vector(data$Name)
	
	index <- 1
	while(index <= length(products)){
		#print(paste("Product",  products[index],sep=" "))
		product <- products[index]
		match <- as.vector(subset(dictionary,Product == product)$Group[1])
		
		
		if(! is.na(match)){
			#print(match)
			price <- sum(as.numeric(as.vector(subset(data, Name == product)$Price)))
			quantity <- sum(as.numeric(as.vector(subset(data, Name == product)$Quantity)))
			
			if(is.na(quantity)){
				quantity <- 0
			}

			row <- as.vector(c(product,match,quantity,as.numeric(price)))
			#print(row)
			if(nrow(subset(resultDf, Product == product)) == 0){
				if(nrow(resultDf) > 0){
					#adding new factors to the data.frame
					levels(resultDf[,1]) <- append(levels(resultDf[,1]),product)
					levels(resultDf[,2]) <- append(levels(resultDf[,2]),match)
					levels(resultDf[,3]) <- append(levels(resultDf[,3]),quantity)
					levels(resultDf[,4]) <- append(levels(resultDf[,4]),price)
				}

				resultDf <- rbind(resultDf, row)
				names(resultDf) <- c("Product","Group","Price")

			}
		}
		
		index <- index + 1
	}

	names(resultDf) <- c("Product","Group","Quantity","Price")
	resultDf
}



# args 	: data - data frame with monthly data
#
#
# Given a data set returns the number of items per food group
nItemsDf <- function(data){
	groups <- as.vector(unique(data$Group))
	size <- length(groups)
	resultDf <- data.frame(Group=rep(NA, size), NItems = rep(0, size), Price=rep(0, size))

	index <- 1
	while(index <= size){
		groupSelect <- subset(data,Group == groups[index])
		price <- sum(as.numeric(as.vector(groupSelect$Price[1])))
		resultDf[index,] <- c(groups[index], nrow(groupSelect),price)

		index <- index + 1
	}
	resultDf
}

#####


# Load the product dictionary
dictionary <- read.csv("data/productDictionary.csv", header=TRUE)

# Load the utiliy functions
source("../utils.R")


# Load monthly food and expenses recods, keeping only the information that is needed: ie, Name, Quantity and Price columns
march <- subset(read.csv("data/march.csv", header=TRUE), select = -c(1,2,3))
april <- subset(read.csv("data/april.csv", header=TRUE), select = -c(1,2,3))
may <- subset(read.csv("data/may.csv", header=TRUE), select = -c(1,2,3))
june <- subset(read.csv("data/june.csv", header=TRUE), select = -c(1,2,3))
july <- subset(read.csv("data/july.csv", header=TRUE), select = -c(1,2,3))
august <- subset(read.csv("data/august.csv", header=TRUE), select = -c(1,2,3))

# Get the total number of items per food group
nItemsMarch <- nItemsDf(matchFoodGroup(march,dictionary))
nItemsApril <- nItemsDf(matchFoodGroup(april,dictionary))
nItemsMay <- nItemsDf(matchFoodGroup(may,dictionary))
nItemsJune <- nItemsDf(matchFoodGroup(june,dictionary))
nItemsJuly <- nItemsDf(matchFoodGroup(july,dictionary))
nItemsAugust <- nItemsDf(matchFoodGroup(august,dictionary))

# Aggregate the total number of items per food group
# Values used to generate the pie chart
totalNItems <- aggregateTotal(aggregateTotal(aggregateTotal(aggregateTotal(aggregateTotal(nItemsMarch,nItemsApril), nItemsMay),nItemsJune),nItemsJuly),nItemsAugust)


# Comparison among quantity (kilos) and total amount of money spent on meat & fish vs fruits, vegetables and cereals
# Values used to generate the line plot (using the total quantity in kilos) and the area plot (using the total price)

#Meat and Fish
marchMF <- subset(marchFood, Group == "meat and fish",select = -c(1,2))
marchMeatFish <- c(sum(as.numeric(as.vector(marchMF$Quantity))),sum(as.numeric(as.vector(marchMF$Price))))

aprilMF <- subset(aprilFood, Group == "meat and fish",select = -c(1,2))
aprilMeatFish <- c(sum(as.numeric(as.vector(aprilMF$Quantity))),sum(as.numeric(as.vector(aprilMF$Price))))

mayMF <- subset(mayFood, Group == "meat and fish",select = -c(1,2))
mayMeatFish <- c(sum(as.numeric(as.vector(mayMF$Quantity))),sum(as.numeric(as.vector(mayMF$Price))))

juneMF <- subset(juneFood, Group == "meat and fish",select = -c(1,2))
juneMeatFish <- c(sum(as.numeric(as.vector(juneMF$Quantity))),sum(as.numeric(as.vector(juneMF$Price))))

julyMF <- subset(julyFood, Group == "meat and fish",select = -c(1,2))
julyMeatFish <- c(sum(as.numeric(as.vector(julyMF$Quantity))),sum(as.numeric(as.vector(julyMF$Price))))

augustMF <- subset(augustFood, Group == "meat and fish",select = -c(1,2))
augustMeatFish <- c(sum(as.numeric(as.vector(augustMF$Quantity))),sum(as.numeric(as.vector(augustMF$Price))))

#fruits
marchF <- subset(marchFood, Group == "fruits",select = -c(1,2))
marchFruits<- c(sum(as.numeric(as.vector(marchF$Quantity))),sum(as.numeric(as.vector(marchF$Price))))

aprilF <- subset(aprilFood, Group == "fruits",select = -c(1,2))
aprilFruits<- c(sum(as.numeric(as.vector(aprilF$Quantity))),sum(as.numeric(as.vector(aprilF$Price))))

mayF <- subset(mayFood, Group == "fruits",select = -c(1,2))
mayFruits<- c(sum(as.numeric(as.vector(mayF$Quantity))),sum(as.numeric(as.vector(mayF$Price))))

juneF <- subset(juneFood, Group == "fruits",select = -c(1,2))
juneFruits<- c(sum(as.numeric(as.vector(juneF$Quantity))),sum(as.numeric(as.vector(juneF$Price))))

julyF <- subset(julyFood, Group == "fruits",select = -c(1,2))
julyFruits<- c(sum(as.numeric(as.vector(julyF$Quantity))),sum(as.numeric(as.vector(julyF$Price))))

augustF <- subset(augustFood, Group == "fruits",select = -c(1,2))
augustFruits<- c(sum(as.numeric(as.vector(augustF$Quantity))),sum(as.numeric(as.vector(augustF$Price))))


#vegetables
marchV <- subset(marchFood, Group == "vegetables",select = -c(1,2))
marchVegetables<- c(sum(as.numeric(as.vector(marchV$Quantity))),sum(as.numeric(as.vector(marchV$Price))))

aprilV <- subset(aprilFood, Group == "vegetables",select = -c(1,2))
aprilVegetables<- c(sum(as.numeric(as.vector(aprilV$Quantity))),sum(as.numeric(as.vector(aprilV$Price))))

mayV <- subset(mayFood, Group == "vegetables",select = -c(1,2))
mayVegetables<- c(sum(as.numeric(as.vector(mayV$Quantity))),sum(as.numeric(as.vector(mayV$Price))))

juneV <- subset(juneFood, Group == "vegetables",select = -c(1,2))
juneVegetables <- c(sum(as.numeric(as.vector(juneV$Quantity))),sum(as.numeric(as.vector(juneV$Price))))

julyV <- subset(julyFood, Group == "vegetables",select = -c(1,2))
julyVegetables <- c(sum(as.numeric(as.vector(julyV$Quantity))),sum(as.numeric(as.vector(julyV$Price))))

augustV <- subset(augustFood, Group == "vegetables",select = -c(1,2))
augustVegetables <- c(sum(as.numeric(as.vector(augustV$Quantity))),sum(as.numeric(as.vector(augustV$Price))))


#cereals
marchC <- subset(marchFood, Group == "cereals",select = -c(1,2))
marchCereals <- c(sum(as.numeric(as.vector(marchC$Quantity))),sum(as.numeric(as.vector(marchC$Price))))

aprilC <- subset(aprilFood, Group == "cereals",select = -c(1,2))
aprilCereals<- c(sum(as.numeric(as.vector(aprilC$Quantity))),sum(as.numeric(as.vector(aprilC$Price))))

mayC <- subset(mayFood, Group == "cereals",select = -c(1,2))
mayCereals<- c(sum(as.numeric(as.vector(mayC$Quantity))),sum(as.numeric(as.vector(mayC$Price))))

juneC <- subset(juneFood, Group == "cereals",select = -c(1,2))
juneCereals <- c(sum(as.numeric(as.vector(juneC$Quantity))),sum(as.numeric(as.vector(juneC$Price))))

julyC <- subset(julyFood, Group == "cereals",select = -c(1,2))
julyCereals <- c(sum(as.numeric(as.vector(julyC$Quantity))),sum(as.numeric(as.vector(julyC$Price))))

augustC <- subset(augustFood, Group == "cereals",select = -c(1,2))
augustCereals <- c(sum(as.numeric(as.vector(augustC$Quantity))),sum(as.numeric(as.vector(augustC$Price))))
