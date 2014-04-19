# Utility Functions


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