##################################################
# Created on 12/17/2020
# Air France Internet Marketing Strategy Analysis
##################################################


###########################################################
#######          Data Importing and Liabrary        #######  
###########################################################

## Reading the dataset
library(readxl)
df_air <- read_excel("R/Group 7/Air France Case Spreadsheet Supplement.xls")
View(df_air)

## library
library(readxl)
library(dplyr)
library(plotly)
library(ggplot2)
library(stats)
library(class)
library(writexl)
library(ROCR)
library(rpart)
library(rpart.plot)
library(class)
library(caret)

###########################################################
#######               Data Massaging                #######  
###########################################################

#### lowercase the columns name
colnames(df_air) <- tolower(colnames(df_air))

#### Checking missing value and data structure
colSums(is.na(df_air))

#### Cleaning the data
# replacing missing value with "NA"
df_air$`bid strategy`[which(is.na(df_air$`bid strategy`))]<- "NA"

# eliminating the row which total cost == 0 
df_air <- df_air[-which(df_air$`total cost` == 0),] 

# eliminating useless columns
df_air$`publisher id`<- NULL
df_air$`keyword id`<-NULL
df_air$`keyword type`<-NULL

# Changing variables tpyes

#df_air$`publisher name` <- as.numeric(as.factor(df_air$`publisher name`))
#df_air$`match type` <- as.numeric(as.factor(df_air$`match type`))
#df_air$status <- as.numeric(as.factor(df_air$status))



###########################################################
#######        Publisher Strategy Analysis          #######  
###########################################################

#### creating KPIs of ROA, Prob of booking, cost per booking

df_air$roa <- df_air$amount / df_air$`total cost` - 1
df_air$`booking rate` <- df_air$`engine click thru %` * df_air$`trans. conv. %` / 10000
df_air$`cost per booking` <- df_air$`total cost`/df_air$`total volume of bookings`

df_air$`cost per booking`[which(df_air$`cost per booking` == "Inf")]<- 0

#### calculating average ROA by publishers

publisher <- unique(df_air$`publisher name`)

roa <- c()

# for loop to calculate the mean
for (i in 1:length(publisher)){
  
  if (i > 8){break}
  
  roa <- c(roa, mean(df_air$roa[which(df_air[,1] == publisher[i])]))
  
  i <- i + 1

}#closing the ROA for loop

print(roa)

## creating a bar chart to compare the channels
roa_per_channel <- cbind.data.frame(publisher,roa)

plot_ly(roa_per_channel, x=publisher, y=roa, type = "bar",
        color = "blue" , alpha = 0.5) %>%
  layout(title = "Return on Advertising",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

#### calculating average Booking Rate by publishers

book_rate <- c()

# for loop to calculate the mean
for (i in 1:length(publisher)){
  
  if (i > 8){break}
  
  book_rate <- c(book_rate, mean(df_air$`booking rate`[which(df_air[,1] == publisher[i])]))
  
  i <- i + 1
  
}#closing the book rate for loop

print(book_rate)

## creating a bar chart to compare the channels
book_rate_channel <- cbind.data.frame(publisher,book_rate)

plot_ly(book_rate_channel, x=publisher, y=book_rate, type = "bar",
        color = "blue" , alpha = 0.5) %>%
  layout(title = "Booking Rate",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

#### calculate average cost per booking by publishers

cost_per_booking <- c()

# for loop to calculate the mean
for (i in 1:length(publisher)){
  
  if (i > 8){break}
  
  cost_per_booking <- c(cost_per_booking, mean(df_air$`cost per booking`[which(df_air[,1] == publisher[i])]))
  
  i <- i + 1
  
}#closing the book rate for loop

print(cost_per_booking)

## creating a bar chart to compare the channels
cost_per_booking_channel <- cbind.data.frame(publisher,cost_per_booking)

plot_ly(cost_per_booking_channel, x=publisher, y=cost_per_booking, type = "bar",
        color = "blue" , alpha = 0.5) %>%
  layout(title = "Cost Per Booking",
         xaxis = list(title = ""),
         yaxis = list(title = ""))


#### Rescaling the dataset by normalization

## normalization UDF
df_normalize <- function(x){
  
  my_min <- min(x, na.rm = TRUE)
  my_max <- max(x, na.rm = TRUE)
  min_max <- (x-my_min)/(my_max-my_min)
  
  return(min_max)
}#closing my_normalize

roa_norm <- df_normalize(roa)
book_rate_norm <- df_normalize(book_rate)
cost_per_booking_norm <- df_normalize(cost_per_booking)

#### weighting KPIs to scoring the channels
publisher_score <- 0.4 * roa_norm + 0.4 * book_rate_norm - 0.2 * cost_per_booking_norm


## creating a bar chart to compare the channels
publisher_score_channel <- cbind.data.frame(publisher,publisher_score)

plot_ly(publisher_score_channel, x=publisher, y=publisher_score, type = "bar",
        color = "blue" , alpha = 0.5) %>%
  layout(title = "Publisher Score",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

## creating a bubble chart to summarize 

df_air_summary <- df_air %>% group_by(`publisher name`) %>%
  summarise(
    publisher_counts = n(),
    total_revenue = sum(`total cost`),
    avg_cpc = mean(`avg. cost per click`),
    avg_prob = mean(`booking rate`),
    avg_ROA = mean(`roa`)
  )

summary(df_air_summary)

bubble <- plot_ly(df_air_summary, x = ~ avg_prob, y = ~ avg_ROA,
                  textposition = "auto",
                  type = 'scatter', 
                  mode = 'markers', 
                  size = ~avg_cpc, 
                  color = ~`publisher name`, 
                  colors = 'Paired',
                  marker = list(opacity = 0.8, sizemode = 'diameter')) %>%
  layout(title = 'Publisher Strategy',
         xaxis = list(title = "Booking Rate", showgrid = TRUE),
         yaxis = list(title = "ROA", showgrid = TRUE),
         showlegend = TRUE)

bubble

#### Conclusion - the most efficient channels (highest score) 
#### is Yahoo - US, and followed by MSN - Global


###########################################################
#######           Bid Strategy Analysis             #######  
###########################################################

df_bid <- df_air

bid <- unique(df_bid$`bid strategy`)

publisher

roa_bid <- c()

yahoo_us_bid <- c()
msn_glb_bid <- c()
google_glb_bid <- c()
overture_glb_bid <- c()
google_us_bid <- c()
overture_us_bid <- c()
msn_us_bid <- c()


# for loop to calculate the bid mean
for (i in 1:length(bid)){
  
  if (i > 10){break}
  
  yahoo_us_bid <- c(yahoo_us_bid, mean(df_bid$roa[which(df_bid$`bid strategy` == bid[i] &
                               df_bid$`publisher name` == publisher[1])]))
  msn_glb_bid <- c(msn_glb_bid, mean(df_bid$roa[which(df_bid$`bid strategy` == bid[i] &
                                                          df_bid$`publisher name` == publisher[2])]))
  google_glb_bid <- c(google_glb_bid, mean(df_bid$roa[which(df_bid$`bid strategy` == bid[i] &
                                                        df_bid$`publisher name` == publisher[3])]))
  overture_glb_bid <- c(overture_glb_bid, mean(df_bid$roa[which(df_bid$`bid strategy` == bid[i] &
                                                              df_bid$`publisher name` == publisher[4])]))
  google_us_bid <- c(google_us_bid, mean(df_bid$roa[which(df_bid$`bid strategy` == bid[i] &
                                                                  df_bid$`publisher name` == publisher[5])]))
  overture_us_bid <- c(overture_us_bid, mean(df_bid$roa[which(df_bid$`bid strategy` == bid[i] &
                                                            df_bid$`publisher name` == publisher[6])]))
  msn_us_bid <- c(msn_us_bid, mean(df_bid$roa[which(df_bid$`bid strategy` == bid[i] &
                                                            df_bid$`publisher name` == publisher[7])]))
  
  i <- i + 1
  
}#closing the bid for loop

roa_bid <- cbind.data.frame(bid, yahoo_us_bid, msn_glb_bid, google_glb_bid, overture_glb_bid,
                            google_us_bid, overture_us_bid, msn_us_bid)

View(roa_bid)

roa_bid[roa_bid == "NaN"] <- 0

roa_bid_sum <- c()

for (i in 1:length(bid)){
  
  if (i > 9){break}
  
  roa_bid_sum <- c(roa_bid_sum, sum(roa_bid[i,2:8]))
  
  i<- i + 1
  
}#closing the campaign sum for loop

roa_bid_sum_df <- cbind.data.frame(bid, roa_bid_sum)

roa_bid_sum_df <- roa_bid_sum_df[order(-roa_bid_sum_df$roa_bid_sum),]

roa_bid_sum_df_top4 <- roa_bid_sum_df[1:4,]

plot_ly(data = roa_bid_sum_df_top4, x=roa_bid_sum_df_top4$bid, y= roa_bid_sum_df_top4$roa_bid_sum, type = "bar",
        color = "blue" , alpha = 0.5) %>%
  layout(title = "ROA by Bid Strategy",
         xaxis = list(title = "Bid Strategy"),
         yaxis = list(title = "Sum of Avg. ROA"))


#### conclusion - The top 3 ROA Bid Strategy are Position 1-2 Target,
#### Position 2-5 Bid Strategy, and Position 1-4 Bid Strategy

###########################################################
#######              Campaign Analysis              #######  
###########################################################

df_camp <- df_air

campaign <- unique(df_camp$campaign)

roa_camp <- c()

yahoo_us_camp <- c()
msn_glb_camp <- c()
google_glb_camp <- c()
overture_glb_camp <- c()
google_us_camp <- c()
overture_us_camp <- c()
msn_us_camp <- c()

# for loop to calculate the campaign mean
for (i in 1:length(campaign)){
  
  if (i > 25){break}
  
  yahoo_us_camp <- c(yahoo_us_camp, mean(df_camp$roa[which(df_camp$campaign == campaign[i] & 
                                                             df_camp$`publisher name` == publisher[1])]))
  msn_glb_camp <- c(msn_glb_camp, mean(df_camp$roa[which(df_camp$campaign == campaign[i] &
                                                             df_camp$`publisher name` == publisher[2])]))
  google_glb_camp <- c(google_glb_camp, mean(df_camp$roa[which(df_camp$campaign == campaign[i] &
                                                             df_camp$`publisher name` == publisher[3])]))
  overture_glb_camp <- c(overture_glb_camp, mean(df_camp$roa[which(df_camp$campaign == campaign[i] &
                                                             df_camp$`publisher name` == publisher[4])]))
  google_us_camp <- c(google_us_camp, mean(df_camp$roa[which(df_camp$campaign == campaign[i] &
                                                             df_camp$`publisher name` == publisher[5])]))
  overture_us_camp <- c(overture_us_camp, mean(df_camp$roa[which(df_camp$campaign == campaign[i] &
                                                             df_camp$`publisher name` == publisher[6])]))
  msn_us_camp <- c(msn_us_camp, mean(df_camp$roa[which(df_camp$campaign == campaign[i] &
                                                             df_camp$`publisher name` == publisher[7])]))
  
  i <- i + 1
  
}#closing the campaign for loop

roa_camp <- cbind.data.frame(campaign, yahoo_us_camp, msn_glb_camp, google_glb_camp, overture_glb_camp,
                            google_us_camp, overture_us_camp, msn_us_camp)

View(roa_camp)

roa_camp[roa_camp == "NaN"] <- 0

colSums(is.na(roa_camp))

camp_sum <- c()

for (i in 1:length(campaign)){
  
  if (i > 24){break}
  
  camp_sum <- c(camp_sum, sum(roa_camp[i,2:8]))
  
  i<- i + 1
  
}#closing the campaign sum for loop

camp_sum_df <- cbind.data.frame(campaign, camp_sum)

camp_sum_df <- camp_sum_df[order(-camp_sum_df$camp_sum),]

camp_sum_df_top4 <- camp_sum_df[1:4,]

plot_ly(data = camp_sum_df_top4, x=camp_sum_df_top4$campaign, y= camp_sum_df_top4$camp_sum, type = "bar",
        color = "blue" , alpha = 0.5) %>%
  layout(title = "ROA by Campaign Strategy",
         xaxis = list(title = "Campaign Strategy"),
         yaxis = list(title = "Sum of Avg. ROA"))

#### Conclusion - the top 3 ROA campaigns are Geo Targeted DC, 
#### Western Europe Destinations, and Air France Brand & French Destinations

#### pie chart for financial performance
total_revenue <- sum(df_air$amount)
yahoo_revenue <- sum(df_air$amount[df_air$`publisher name` == "Yahoo - US"])
google_revenue <- sum(df_air$amount[df_air$`publisher name` == "Google - Global"]) + sum(df_air$amount[df_air$`publisher name` == "Google - US"])
msn_revenue <- sum(df_air$amount[df_air$`publisher name` == "MSN - Global"]) + sum(df_air$amount[df_air$`publisher name` == "MSN - US"])
overture_revenue <- sum(df_air$amount[df_air$`publisher name` == "Overture - Global"]) + sum(df_air$amount[df_air$`publisher name` == "Overture - US"])
yahoo_rev <- yahoo_revenue / total_revenue
google_rev <- google_revenue / total_revenue
msn_revenue <- msn_revenue / total_revenue
overture_rev <- overture_revenue / total_revenue

lst_rev <- c(yahoo_rev,google_rev,msn_revenue,overture_rev)
label <- c("yahoo","google","msn","overture")
pie(lst_rev,label)

###########################################################
#######              Keyword Analysis               #######  
###########################################################

df_air_summary_kw <- df_air %>% group_by(keyword) %>%
  summarise(
    publisher_counts = n(),
    total_revenue = sum(`total cost`),
    avg_cpc = mean(`avg. cost per click`),
    avg_prob = mean(`booking rate`),
    avg_ROA = mean(`roa`)
  )

summary(df_air_summary_kw)

df_air_summary_kw$score_kw <- 0.4 * df_air_summary_kw$avg_cpc + 
  0.3 * df_air_summary_kw$avg_prob + 0.2 * df_air_summary_kw$avg_ROA

df_air_summary_kw <- df_air_summary_kw[order(-df_air_summary_kw$score_kw),]

df_air_summary_kw_top5 <- df_air_summary_kw[1:5,]

plot_ly(data = df_air_summary_kw_top5 , x=df_air_summary_kw_top5$keyword, y=df_air_summary_kw_top5$score_kw, type = "bar",
        color = "blue" , alpha = 0.5) %>%
  layout(title = "Top 5 Score Keyword",
         xaxis = list(title = "Keyword"),
         yaxis = list(title = "Score"))

ggplot(data=df_air_train, aes(x=`total volume of bookings`, y=`total cost`,color=`publisher name`)) +
  geom_pie(aes(alpha=0.2),shape = 4)




###########################################################
#######           Decision Tree Analysis            #######  
###########################################################

####################################################
#### Dropping the variables which are not useful ####
df_air2 <- as.data.frame(df_air)

# grouping overture(1) VS website(google/ MSN/Yahoo)(0)
df_air2$binary_pub <- as.numeric(as.factor(df_air2$`publisher name`))
df_air2$binary_pub <- gsub(1, 0, df_air2$binary_pub)
df_air2$binary_pub <- gsub(2, 0, df_air2$binary_pub)
df_air2$binary_pub <- gsub(3, 0, df_air2$binary_pub)
df_air2$binary_pub <- gsub(4, 0, df_air2$binary_pub)
df_air2$binary_pub <- gsub(7, 0, df_air2$binary_pub)
df_air2$binary_pub <- gsub(5, 1, df_air2$binary_pub)
df_air2$binary_pub <- gsub(6, 1, df_air2$binary_pub)

# creating new variable 'binary_pub'   

df_air2$binary_pub <- as.numeric(df_air2$binary_pub)


# building training/ testing sample

set.seed(2020)

random_sample <- function(df, n){
  training_indexes <- sample(1:nrow(df), n*nrow(df))
  
  training_data <- df[training_indexes, ]
  testing_data <- df[-training_indexes, ]
  
  return(list(training_data, testing_data))
  
}#closing the random sampling UDF


#normalization UDF
my_normalize <- function(x){
  my_min <- min(x, na.rm=T)
  my_max <- max(x, na.rm = T)
  min_max <- (x-my_min)/(my_max - my_min)
  return(min_max)
}

# converting variable to normalization
df_air2$Search_Engine_Bid_norm <- my_normalize(x= df_air2$`search engine bid`)
df_air2$Clicks_norm <- my_normalize(x= df_air2$clicks)
df_air2$Click_Charges_norm <- my_normalize(x= df_air2$`click charges`)
df_air2$Avg_Cost_per_Clicknorm <- my_normalize(x= df_air2$`avg. cost per click`)
df_air2$Impressions_norm <- my_normalize(x= df_air2$impressions)
df_air2$Engine_Click_Thru_norm <- my_normalize(x= df_air2$`engine click thru %`)
df_air2$Avg_Pos_norm <- my_normalize(x= df_air2$`avg. pos.`)
df_air2$Trans_Conv_norm <- my_normalize(x= df_air2$`trans. conv. %`)
df_air2$Total_Cost_Trans_norm <- my_normalize(x= df_air2$`total cost/ trans.`)
df_air2$Amount_norm <- my_normalize(x= df_air2$amount)
df_air2$Total_Cost_norm <- my_normalize(x= df_air2$`total cost`)
df_air2$Total_Volume_of_Bookings <- my_normalize(x= df_air2$`total volume of bookings`)

#sampling

random_output <- random_sample(df = df_air2, n=0.8)
training_overture <- random_output[[1]]
testing_overture <- random_output[[2]]
summary(df_air2)

#### logistic regression

colnames(df_air2)
logi_overture_2 <- glm(binary_pub ~ `search engine bid`+
                         clicks+`click charges`+ `avg. cost per click`+
                         impressions+`engine click thru %`+amount,
                       data=training_overture, family = 'binomial')
summary(logi_overture_2)

# business insight
(exp(-1.761)-1)*100 #= -82.81271
# with every single unit increase in Avg. cost per click ,
# the odds of success will decrease by 82. 
# keeping every other variables constant

clean_nor_overture <- glm(binary_pub ~ Search_Engine_Bid_norm+Clicks_norm+
                            Click_Charges_norm+Avg_Cost_per_Clicknorm+
                            Impressions_norm+Engine_Click_Thru_norm+
                            Amount_norm,
                          data= training_overture, family = "binomial")
summary(clean_nor_overture)

# Impression has the highest positive impact on overture.
# Amount has the highest negative impact on the overture.

## confusion Matrix

my_prediction <- predict(logi_overture_2, testing_overture, type = "response")

confusionMatrix(data=as.factor(as.numeric(my_prediction>0.5)), 
                reference = as.factor(as.numeric(testing_overture$binary_pub)))


##### AUC-ROC curve

pred_overture_logit <- prediction(my_prediction, testing_overture$binary_pub)
perf_overture_logit <- performance(pred_overture_logit,"tpr","fpr")
plot(perf_overture_logit, col='blue', lyt=3, lwd=3)

#Decision Tree
overture_tree <- rpart(binary_pub ~ `search engine bid`+
                         clicks+`click charges`+ `avg. cost per click`+
                         impressions+`engine click thru %`+amount,
                       data=training_overture, method = "class", cp = 0.03)
#plotting decision tree
rpart.plot(overture_tree, type =1 , extra = 4,
           box.palette = c("pink", "green"),
           branch.lty =3, shadow.col = "gray")

