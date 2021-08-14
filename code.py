# Air-France_Paid-Ads-Analysis
Using R for data visualization, EDA, ROA analysis

install.packages("readxl")
library(readxl)
install.packages("compare")
install.packages("stringr")
install.packages("xlsx")
air_france_dataset <- read_excel("Air France Case Spreadsheet Supplement.xls", sheet =2)
print(air_france_dataset)

##############copy the orginal data and drop NA values for future visualization analysis
for_plot <-data.frame(air_france_dataset)a
for_plot_dropped <- na.omit(for_plot)
sum(is.na(for_plot_dropped))


#############summary for the origianl dataset
summary(air_france_dataset)


#############change categorical values into numeric numbers
#match type
for (i in 1:nrow(air_france_dataset)){
  if (air_france_dataset$`Match Type`[i] == "Advanced") {air_france_dataset$`Match Type`[i] <- 1}
  else if (air_france_dataset$`Match Type`[i] == "Broad") {air_france_dataset$`Match Type`[i] <- 2}
  else if (air_france_dataset$`Match Type`[i] == "Standard") {air_france_dataset$`Match Type`[i] <- 3}
  else if (air_france_dataset$`Match Type`[i] == "Exact") {air_france_dataset$`Match Type`[i] <- 4}
  else {air_france_dataset$`Match Type`[i] <- 5}
}
air_france_dataset$`Match Type` <- as.numeric(air_france_dataset$`Match Type`)
print(air_france_dataset)
#publish name
for (i in 1:nrow(air_france_dataset)){
  if (air_france_dataset$`Publisher Name`[i] == "Google - Global") {air_france_dataset$`Publisher Name`[i] <- 1}
  else if (air_france_dataset$`Publisher Name`[i] == "Google - US") {air_france_dataset$`Publisher Name`[i] <- 1}
  else if (air_france_dataset$`Publisher Name`[i] == "Yahoo - US") {air_france_dataset$`Publisher Name`[i] <- 2}
  else if (air_france_dataset$`Publisher Name`[i] == "Overture - Global") {air_france_dataset$`Publisher Name`[i] <- 3}
  else if (air_france_dataset$`Publisher Name`[i] == "Overture - US") {air_france_dataset$`Publisher Name`[i] <- 3}
  else if (air_france_dataset$`Publisher Name`[i] == "MSN - Global") {air_france_dataset$`Publisher Name`[i] <- 4}
  else if (air_france_dataset$`Publisher Name`[i] == "MSN - US") {air_france_dataset$`Publisher Name`[i] <- 4}
  else {air_france_dataset$`Publisher Name`[i] <- 5}
}
air_france_dataset$`Publisher Name` <- as.numeric(air_france_dataset$`Publisher Name`)

print(air_france_dataset$`Publisher Name`)
print(air_france_dataset)

#############Drop Categorical Columns besides Publisher & Match Type
dropped_air_france <- air_france_dataset[ -c(1, 3:4, 6:11) ]

#############add columns of ROA
dropped_air_france$ROA <- ((dropped_air_france$Amount)-(dropped_air_france$`Total Cost`))/dropped_air_france$`Total Cost` *100

print(dropped_air_france)

#drop the row with infiniti value of ROA
dropped_air_france <- dropped_air_france[-which(dropped_air_france$ROA == "Inf"),]

#check the structure of dataset
str(dropped_air_france)

#no missing value
sum(is.na(dropped_air_france))

#############subsets for different match type
advanced <- dropped_air_france[which(dropped_air_france$`Match Type` == 1),]
broad <- dropped_air_france[which(dropped_air_france$`Match Type` == 2),]
standard <- dropped_air_france[which(dropped_air_france$`Match Type`== 3),]
exact <- dropped_air_france[which(dropped_air_france$`Match Type` == 4),]
summary(advanced)
summary(broad)
summary(exact)
summary(standard)

#mean, median for different match type

advanced_3 <- advanced[ c('Clicks', 'Total Cost/ Trans.', 'Amount', 'Total Cost', 'Total Volume of Bookings') ]
summary(advanced_3)

broad_3 <- broad[ c('Clicks', 'Total Cost/ Trans.', 'Amount', 'Total Cost', 'Total Volume of Bookings') ]
summary(broad_3)

standard_3 <- standard[ c('Clicks', 'Total Cost/ Trans.', 'Amount', 'Total Cost', 'Total Volume of Bookings') ]
summary(standard_3)

exact_3 <- exact[ c('Clicks', 'Total Cost/ Trans.', 'Amount', 'Total Cost', 'Total Volume of Bookings') ]
summary(exact_3)


#############correlations

str(dropped_air_france)

#two variables correlation
cor(dropped_air_france$`Publisher Name`, dropped_air_france$`Search Engine Bid`) 
cor(dropped_air_france$`Trans. Conv. %`, dropped_air_france$ROA) 

#correlation among different variables
pearson_cor <- cor(x= dropped_air_france,use="pairwise.complete.obs", method = "pearson")
cor <- cor(x= dropped_air_france,use="pairwise.complete.obs")
library(compare)
compare(cor,pearson_cor)
cor <- round(cor,2)
write.csv(cor, file = "air_france_cor_dropped.csv")

#heatmap
library(reshape2)
melted_cor <- melt(cor)
head(melted_cor,5)

library(ggplot2)

heatmap <- ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + scale_fill_gradient(low = "white",high = "steelblue") + theme(axis.text.x = element_text(angle=60, hjust=1))
print(heatmap)

#############plots with 2 variables having significant correlation 
attach(dropped_air_france)
plot(dropped_air_france$Clicks, dropped_air_france$Amount, main="Clicks & Amount", 
     xlab="clicks ", ylab="amount ", pch=19)

plot(dropped_air_france$Clicks, dropped_air_france$`Total Volume of Bookings`, main="Clicks & Volume", 
     xlab="clicks ", ylab="volume ", pch=19)

plot(dropped_air_france$`Trans. Conv. %`, dropped_air_france$ROA, main="Trans.Conv & ROA", 
     xlab="Tran.Conv", ylab= "ROA ", pch=19)

plot(dropped_air_france$`Avg. Cost per Click`, dropped_air_france$ROA, main="Avg_CPC & ROA", 
     xlab="avg_cpc ", ylab= "ROA", pch=19)

for_matchtype <- dropped_air_france[-which(dropped_air_france$`Match Type` == 5),]
boxplot(`Avg. Cost per Click`~`Match Type`, data=for_matchtype, notch=FALSE, col=(c("gold","orange")),
        main="Match Type & Avg_CPC", xlab="Match Type", ylab="Avg_CPC")

boxplot(`Avg. Cost per Click`~`Publisher Name`, data=dropped_air_france, notch=FALSE, col=(c("light blue","#009999")),
        main="Channel & Avg_CPC", xlab="publisher name", ylab="Avg_CPC")

plot(dropped_air_france$`Match Type`, dropped_air_france$ROA, main="Match Type & ROA", 
     xlab="match type ", ylab= "ROA", pch=19)

boxplot(ROA~`Publisher Name`, data=dropped_air_france, notch=FALSE, col=(c("light blue","#009999")),
        main="Channel & ROA", xlab="publisher name", ylab="ROA")

#############plots with 3 variables having significant correlation 
library(scatterplot3d) 
attach(dropped_air_france) 
scatterplot3d(Clicks,Amount,`Total Volume of Bookings`, pch=20, highlight.3d=FALSE,
              type="h", main="Clicks & Volumn & Amount")



#############ggplot with categorical variables
for_plot$ROA <- ((for_plot$Amount)-(for_plot$Total.Cost))/for_plot$Total.Cost *100

print(for_plot)
#search bid in different channel and match type

library(ggplot2)
ggplot(for_plot, aes(x=Publisher.Name, y=Search.Engine.Bid, color=Match.Type, shape=Match.Type)) +
  geom_point(size=6, alpha=0.6) + theme(axis.text.x = element_text(angle=60, hjust=1))

#amount in different channel and match type

library(ggplot2)
ggplot(for_plot, aes(x=Publisher.Name, y=Amount , color=Match.Type, shape=Match.Type)) +
  geom_point(size=6, alpha=0.6) + theme(axis.text.x = element_text(angle=60, hjust=1))

#ROA in different channel and match type

library(ggplot2)
ggplot(for_plot, aes(x=Publisher.Name, y=ROA , color=Match.Type, shape=Match.Type)) +
  geom_point(size=6, alpha=0.6) + theme(axis.text.x = element_text(angle=60, hjust=1))

#avg_cpc in different channel and match type
library(ggplot2)
ggplot(for_plot, aes(x=Publisher.Name, y=Avg..Cost.per.Click , color=Match.Type, shape=Match.Type)) +
  geom_point(size=6, alpha=0.6) + theme(axis.text.x = element_text(angle=60, hjust=1))

################branded or unbranded keywords
library(stringr)
#identify those keywords with "air france" inside as branded keywords
branded_keywords <- str_subset(for_plot$Keyword, "air france")
branded_keywords_2 <- str_subset(for_plot$Keyword,"airfrance")

#subset data to using branded or unbranded keywords
for_branded <- for_plot[which(for_plot$Keyword %in% branded_keywords),]
for_branded_1 <- for_plot[which(for_plot$Keyword %in% branded_keywords_2),]
for_branded <- rbind(for_branded, for_branded_1)
for_branded_numeric <- data.frame(for_branded)
for_unbranded <- for_plot[!row.names(for_plot) %in% row.names(for_branded),]
for_unbranded_numeric <- data.frame(for_unbranded)


#relable values in keyword column to either "branded keyword" or "unbranded keyword"
for_branded$Keyword <-  "branded_keyword"
for_unbranded$Keyword <- "unbranded_keyword"
for_plot_keyword <- rbind(for_branded, for_unbranded)
for_plot_keyword <- for_plot_keyword[-which(for_plot_keyword$ROA == "Inf"),]
str(for_plot_keyword)

for_branded_numeric$Keyword <-  0
for_unbranded_numeric$Keyword <- 1
for_plot_keyword_numeric <- rbind(for_branded_numeric, for_unbranded_numeric)
for_plot_keyword_numeric <- for_plot_keyword_numeric[-which(for_plot_keyword_numeric$ROA == "Inf"),]
str(for_plot_keyword_numeric)

print(for_plot_keyword$Keyword)
write.csv(for_plot_keyword, file = "for_plot_keyword_1.csv")

# ROA of branded or unbranded keywords in different channel 
library(ggplot2)
ggplot(for_plot_keyword, aes(x=Publisher.Name, y=ROA , color=Keyword, shape=Keyword)) +
  geom_point(size=6, alpha=0.6) + theme(axis.text.x = element_text(angle=60, hjust=1))

# Avg.CPC of branded or unbranded keywords in different channel 
library(ggplot2)
for_plot_keyword$Avg..Cost.per.Click <- round(for_plot_keyword$Avg..Cost.per.Click, 4)
print(head(for_plot_keyword))

ggplot(for_plot_keyword, aes(x=Publisher.Name, y=Avg..Cost.per.Click , color=Keyword, shape=Keyword)) +
  geom_point(size=6, alpha=0.6) + theme(axis.text.x = element_text(angle=60, hjust=1))


sum(is.na(for_plot_keyword$Avg..Cost.per.Click))


# Avg.CPC of branded or unbranded keywords in different channel 
library(ggplot2)


ggplot(for_plot_keyword, aes(x=Publisher.Name, y=`Trans. Conv. %` , color=Keyword, shape=Keyword)) +
  geom_point(size=6, alpha=0.6) + theme(axis.text.x = element_text(angle=60, hjust=1))

#Optimize the ROA

############predict the binary variable
#check the distribution of ROA besides -100 (mean compaign generating no transactions)
positive_ROA <- dropped_air_france[-which(dropped_air_france$`Total Volume of Bookings` == 0),]
round(positive_ROA$ROA, 2)
summary(positive_ROA$ROA)

#add keyword columns with branded =1 & unbranded =2 to dropped_air_france
keyword_column_numeric <- for_plot_keyword_numeric[c("Keyword")]
numeric_df_numeric_keyword <- cbind(dropped_air_france,keyword_column_numeric)
print(tail(numeric_df_numeric_keyword))

#categorize the ROA into either less or greater than 0
better_ROA <- subset(for_plot_keyword_numeric, ROA >= 0)
bad_ROA <- for_plot_keyword_numeric[!row.names(for_plot_keyword_numeric) %in% row.names(better_ROA),]

#relable better_ROA as 1 and bad_ROA as 0
better_ROA$ROA <- 1
bad_ROA$ROA <- 0
predict_ROA <- rbind(better_ROA, bad_ROA)
print(tail(predict_ROA))

#change match type into numeric in predict_ROA
for (i in 1:nrow(predict_ROA)){
  if (predict_ROA$Match.Type[i] == "Advanced") {predict_ROA$Match.Type[i] <- 1}
  else if (predict_ROA$Match.Type[i] == "Broad") {predict_ROA$Match.Type[i] <- 2}
  else if (predict_ROA$Match.Type[i] == "Standard") {predict_ROA$Match.Type <- 3}
  else if (predict_ROA$Match.Type[i] == "Exact") {predict_ROA$Match.Type[i] <- 4}
  else {predict_ROA$Match.Type[i] <- 5}
}
predict_ROA$Match.Type <- as.numeric(predict_ROA$Match.Type)
print(head(predict_ROA))


#model for predicting ROA performance
ROA_model <- glm(ROA~Keyword+predict_ROA$Publisher.Name+predict_ROA$Match.Type+predict_ROA$Avg..Cost.per.Click+predict_ROA$Clicks+predict_ROA$Total.Cost, data=predict_ROA, family="binomial")
summary(ROA_model)

ll.null <- ROA_model$null.deviance/-2
ll.proposed <- ROA_model$deviance/-2

(ll.null - ll.proposed)/ll.null

1-pchisq(2*(ll.proposed-ll.null),df=(length(ROA_model$coefficients)))

#OPTIMIZED MODEL

ROA_model_opt <- glm(ROA~Keyword+predict_ROA$Publisher.Name+predict_ROA$Avg..Cost.per.Click+predict_ROA$Trans..Conv...+predict_ROA$Total.Volume.of.Bookings+predict_ROA$Total.Cost, data=predict_ROA, family="binomial")
summary(ROA_model_opt)

ll.null_opt <- ROA_model_opt$null.deviance/-2

ll.proposed_opt <- ROA_model_opt$deviance/-2

(ll.null_opt - ll.proposed_opt)/ll.null_opt

1-pchisq(2*(ll.proposed_opt-ll.null_opt),df=(length(ROA_model_opt$coefficients)))

#Classification Tree
library(rpart)
library(rpart.plot)

ROA_tree <- rpart(Keyword~Keyword+`Avg..Cost.per.Click`+`Trans..Conv...`+`Total.Volume.of.Bookings`+`Total.Cost`, data=predict_ROA, method = "class", cp = 0.015)
rpart.plot(ROA_tree, type = 1, extra = 1, box.palette = "auto")
plotcp(ROA_tree)


ROA_tree <- rpart(`Match Type`~., data=for_plot_keyword_numeric, method = "class")
rpart.plot(ROA_tree, type = 1, extra = 1, box.palette = "auto")
plotcp(ROA_tree) 

#use Kayak or not
google <- subset(dropped_air_france, `Publisher Name` == 1)
yahoo <- subset(dropped_air_france, `Publisher Name` == 2)
MSN <- subset(dropped_air_france, `Publisher Name` == 3)
Overture <- subset(dropped_air_france, `Publisher Name` == 4)

avg_ROA_google <- mean(google$ROA)
avg_ROA_yahoo <- mean(yahoo$ROA)
avg_ROA_MSN <- mean(MSN$ROA)
avg_ROA_overture <- mean(Overture$ROA)
avg_ROA_Kayak <- 31.02
avg_ROA <- c(avg_ROA_google, avg_ROA_yahoo, avg_ROA_MSN, avg_ROA_overture, avg_ROA_Kayak)

barplot(avg_ROA, main="Avg_ROA in different channels", names.arg = c("Google", "Yahoo", "MSN", "Overture", "Kayak"), col=c("light blue","#009999"))


