install.packages("gmodels") 
install.packages("ggplot2")
install.packages("dplyr")
library("gmodels")
library("ggplot")
library("dplyr")

yt_df <- read.csv("D:/Semester 1/DANA/Assignment3/Youtube.csv")   #reading csv file

View(yt_df)            #viewing data of the dataframe

dim(yt_df)            # rows and columns of the dataframe

str(yt_df)            # Define structure of the dataframe,number of variables and its type

colnames(yt_df)       #fetching colnames of the dataframe

names(yt_df)

#Q1 Converting Impression variable into 3 group of Category "Low,Medium,High"
# Checking Summary Statistics for Impression Variable first

list("Summary" = summary(yt_df$Impressions), 
     "Standard Deviation" = sd(yt_df$Impressions, na.rm=TRUE), 
     "Range" = range(yt_df$Impressions, na.rm=TRUE), 
     "Inter-Quartile Range(IQR)" = IQR(yt_df$Impressions, na.rm=TRUE)) 

# Checking data distribution for Impression using Histogram

hist(yt_df$Impressions, 
     main = "Histogram for Impressions", 
     xlab = "Youtube Impressions",
     freq = TRUE,
     breaks = 10,
     border = "Blue",
     col = "Turquoise",
     labels = TRUE,
     las=1   ,
     ylim = c(0,150))

# Checking data distribution using Box plot 

boxplot(yt_df$Impressions,
        las=1,
        main = "Box Plot for Impressions",
        xlab = "Number of times thumbnails for each video were shown",
        ylab = "Impressions",
        col = "Light Yellow",
        border = "red",
        horizontal = F,
        outline = T)

# Create a new variable to Transform Impressions into categorical using binning technique,dividing the values into 3 categories

yt_df$Impressions_cat <- cut(yt_df$Impressions, breaks = 3, labels = c("Low_Imp", "Medium_Imp", "High_Imp"))

# Check Frequency Distribution
table(yt_df$Impressions_cat)

barplot(table(yt_df$Impressions_cat),las=1,main = "Bar Plot for Impression Category",
        xlab = "Impression",ylab = "Frequency",ylim = c(0,250),col = "Pink",border = "Orange")

# Checking if binning is equally done
numbers_of_bins = 3
table(cut(yt_df$Impressions, breaks = unique(quantile(yt_df$Impressions,probs =seq.int(0,1, by=1/numbers_of_bins))), 
          include.lowest=TRUE))

cbind(Impression_cnt=table(yt_df$Impressions_cat),Impression_Contri=(table(yt_df$Impressions_cat)/dim(yt_df)[1])*100)

# yt_df %>% 
# ggplot(aes(Impressions_cat,table(Impressions)))+
#   geom_col() +
#   labs(title="Barplot with labels on bars")
#   geom_text(aes(label = signif(table(Impressions_cat), digits = 3)),nudge_y = 50)*/


# Q2 Repeating the above procedure for 5 other Numeric VAriables

# Checking Summary Statistics for Comment Variable 

list("Summary" = summary(yt_df$Comments.added), 
     "Standard Deviation" = sd(yt_df$Comments.added, na.rm=TRUE), 
     "Range" = range(yt_df$Comments.added, na.rm=TRUE), 
     "Inter-Quartile Range(IQR)" = IQR(yt_df$Comments.added, na.rm=TRUE)) 

# Checking data distribution for Impression using Histogram

hist(yt_df$Comments.added, 
     main = "Histogram for Comments Added", 
     xlab = "Youtube Comments",
     freq = TRUE,
     breaks = 10,
     border = "Red",
     col = "Orange",
     labels = TRUE,
     las=1   ,
     ylim = c(0,200))

# Checking data distribution using Box plot 

boxplot(yt_df$Comments.added,
        las=1,
        main = "Box Plot for Comments Added",
        xlab = "Number of Comments Added for each Video",
        ylab = "Comments Added",
        col = "Maroon",
        border = "red",
        horizontal = T,
        outline = T)

# Create a new variable to Transform Impressions into categorical using binning technique,dividing the values into 3 categories

yt_df$Comments.added.cat <- cut(yt_df$Comments.added, breaks = 3, labels = c("Low_Comm", "Medium_Comm", "High_Comm"))

# Check Frequency Distribution
table(yt_df$Comments.added.cat)

barplot(table(yt_df$Comments.added.cat),las=1,main = "Bar Plot for Comment Added Category",
        xlab = "Comment Added",ylab = "Frequency",ylim = c(0,250),col = "Pink",border = "Orange")

# Checking if binning is equally done
numbers_of_bins = 3
table(cut(yt_df$Comments.added, breaks = unique(quantile(yt_df$Comments.added,probs =seq.int(0,1, by=1/numbers_of_bins))), 
          include.lowest=TRUE))

cbind(Comment_cnt=table(yt_df$Comments.added.cat),Comment_Contri=(table(yt_df$Comments.added.cat)/dim(yt_df)[1])*100)

########################################################################################################33

# Checking Summary Statistics for Like Variable 

list("Summary" = summary(yt_df$Likes), 
     "Standard Deviation" = sd(yt_df$Likes, na.rm=TRUE), 
     "Range" = range(yt_df$Likes, na.rm=TRUE), 
     "Inter-Quartile Range(IQR)" = IQR(yt_df$Likes, na.rm=TRUE)) 

# Checking data distribution for Impression using Histogram

hist(yt_df$Likes, 
     main = "Histogram for Likes Variable", 
     xlab = "Youtube Likes",
     freq = TRUE,
     breaks = 10,
     border = "Orange",
     col = "Beige",
     labels = TRUE,
     las=1   ,
     ylim = c(0,100))

# Checking data distribution using Box plot 

boxplot(yt_df$Likes,
        las=1,
        main = "Box Plot for Likes Varibale",
        xlab = "Number of Likes for each Video",
        ylab = "Youtube Likes",
        col = "Orange",
        border = "red",
        horizontal = T,
        outline = T)

# Create a new variable to Transform Impressions into categorical using binning technique,dividing the values into 3 categories

yt_df$Likes_cat <- cut(yt_df$Likes, breaks = 3, labels = c("Low_Likes", "Medium_Likes", "High_Likes"))

# Check Frequency Distribution
table(yt_df$Likes_cat)

barplot(table(yt_df$Likes_cat),las=1,main = "Bar Plot for Likes Category",
        xlab = "Likes",ylab = "Frequency",ylim = c(0,250),col = "Pink",border = "Orange")

# Checking if binning is equally done
numbers_of_bins = 3
table(cut(yt_df$Likes, breaks = unique(quantile(yt_df$Likes,probs =seq.int(0,1, by=1/numbers_of_bins))), 
          include.lowest=TRUE))

cbind(Like_cnt=table(yt_df$Likes_cat),Like_Contri=(table(yt_df$Likes_cat)/dim(yt_df)[1])*100)



########################################################################################################33

# Checking Summary Statistics for Dislike Variable 

list("Summary" = summary(yt_df$Dislikes), 
     "Standard Deviation" = sd(yt_df$Dislikes, na.rm=TRUE), 
     "Range" = range(yt_df$Dislikes, na.rm=TRUE), 
     "Inter-Quartile Range(IQR)" = IQR(yt_df$Dislikes, na.rm=TRUE)) 

# Checking data distribution for Impression using Histogram

hist(yt_df$Dislikes, 
     main = "Histogram for Dislike Variable", 
     xlab = "Youtube Dislikes",
     freq = TRUE,
     breaks = 10,
     border = "Orange",
     col = "Dark Green",
     labels = TRUE,
     las=1   ,
     ylim = c(0,120))

# Checking data distribution using Box plot 

boxplot(yt_df$Dislikes,
        las=1,
        main = "Box Plot for Dislike Varibale",
        xlab = "Number of Dislikes for each Video",
        ylab = "Youtube Dislikes",
        col = "Green",
        border = "red",
        horizontal = T,
        outline = T)

# Create a new variable to Transform Impressions into categorical using binning technique,dividing the values into 3 categories

yt_df$Dislikes_cat <- cut(yt_df$Dislikes, breaks = 3, labels = c("Low_Disikes", "Medium_Dislikes", "High_Dislikes"))

# Check Frequency Distribution
table(yt_df$Dislikes_cat)

barplot(table(yt_df$Dislikes_cat),las=1,main = "Bar Plot for Dislikes Category",
        xlab = "Disikes",ylab = "Frequency",ylim = c(0,200),col = "Pink",border = "Orange")

# Checking if binning is equally done
numbers_of_bins = 3
table(cut(yt_df$Dislikes, breaks = unique(quantile(yt_df$Dislikes,probs =seq.int(0,1, by=1/numbers_of_bins))), 
          include.lowest=TRUE))

cbind(Dislike_cnt=table(yt_df$Dislikes_cat),Dislike_Contri=(table(yt_df$Dislikes_cat)/dim(yt_df)[1])*100)

#################################################################################################################

# Checking Summary Statistics for Views Variable 

list("Summary" = summary(yt_df$Views), 
     "Standard Deviation" = sd(yt_df$Views, na.rm=TRUE), 
     "Range" = range(yt_df$Views, na.rm=TRUE), 
     "Inter-Quartile Range(IQR)" = IQR(yt_df$Views, na.rm=TRUE)) 

# Checking data distribution for Views using Histogram

hist(yt_df$Views, 
     main = "Histogram for Views Variable", 
     xlab = "Youtube Views",
     freq = TRUE,
     breaks = 10,
     border = "Dark Blue",
     col = "Light Blue",
     labels = TRUE,
     las=1   ,
     ylim = c(0,150))

# Checking data distribution using Box plot 

boxplot(yt_df$Views,
        las=1,
        main = "Box Plot for Views Varibale",
        xlab = "Number of Views for each Video",
        ylab = "Youtube Views",
        col = "Dark Blue",
        border = "Blue",
        horizontal = T,
        outline = T)

# Create a new variable to Transform Impressions into categorical using binning technique,dividing the values into 3 categories

yt_df$Views_cat <- cut(yt_df$Views, breaks = 3, labels = c("Low_Views", "Medium_Views", "High_Views"))

# Check Frequency Distribution
table(yt_df$Views_cat)

barplot(table(yt_df$Views_cat),las=1,main = "Bar Plot for Views Category",
        xlab = "Views",ylab = "Frequency",ylim = c(0,250),col = "Pink",border = "Orange")

# Checking if binning is equally done
numbers_of_bins = 3
table(cut(yt_df$Views, breaks = unique(quantile(yt_df$Views,probs =seq.int(0,1, by=1/numbers_of_bins))), 
          include.lowest=TRUE))

cbind(View_cnt=table(yt_df$Views_cat),View_Contri=(table(yt_df$Views_cat)/dim(yt_df)[1])*100)

################################################################################################################

# Checking Summary Statistics for Subscriber Variable 

list("Summary" = summary(yt_df$Subscribers), 
     "Standard Deviation" = sd(yt_df$Subscribers, na.rm=TRUE), 
     "Range" = range(yt_df$Subscribers, na.rm=TRUE), 
     "Inter-Quartile Range(IQR)" = IQR(yt_df$Subscribers, na.rm=TRUE)) 

# Checking data distribution for Subscriber using Histogram

hist(yt_df$Subscribers, 
     main = "Histogram for Subscriber Variable", 
     xlab = "Youtube Subscriber",
     freq = TRUE,
     breaks = 10,
     border = "Orange",
     col = "Magenta",
     labels = TRUE,
     las=1   ,
     ylim = c(0,200),
     xlim = c(0,20000))

# Checking data distribution using Box plot 

boxplot(yt_df$Subscribers,
        las=1,
        main = "Box Plot for Subscriber Varibale",
        xlab = "Number of Subscriber for each Video",
        ylab = "Youtube Subscriber",
        col = "Light pink",
        border = "Magenta",
        horizontal = T,
        outline = T)

# Create a new variable to Transform Impressions into categorical using binning technique,dividing the values into 3 categories

yt_df$Subs_cat <- cut(yt_df$Subscribers, breaks = 3, labels = c("Low_Subs", "Medium_Subs", "High_Subs"))

# Check Frequency Distribution
table(yt_df$Subs_cat)

barplot(table(yt_df$Subs_cat),las=1,main = "Bar Plot for Subscriber Category",
        xlab = "Subscriber",ylab = "Frequency",ylim = c(0,200),col = "Pink",border = "Orange")

# Checking if binning is equally done
numbers_of_bins = 3
table(cut(yt_df$Subscribers, breaks = unique(quantile(yt_df$Subscribers,probs =seq.int(0,1, by=1/numbers_of_bins))), 
          include.lowest=TRUE))

cbind(subscriber_cnt=table(yt_df$Subs_cat),Subscriber_Contri=(table(yt_df$Subs_cat)/dim(yt_df)[1])*100)

#########################################################################################################

#c Relationship Between Categorical Variables

# a. Views vs Likes

joint <- CrossTable(yt_df$Likes_cat,yt_df$Views_cat)

joint$t

barplot(joint$t,col = c("light blue","blue","dark blue"),las=1,ylab = "Frequency",ylim = c(0,250),main = "Bar Plot for YouTube Views VS Likes ")
legend("topright",c("Low_Likes","Medium_Likes","High_Likes"),col= c("light blue","blue","dark blue"),pch = 15)

cor.test(yt_df$Likes,yt_df$Views)
qplot(yt_df$Likes,yt_df$Views,color=yt_df$Likes_cat,shape= yt_df$Views_cat,main = "Scatter Plot Between YouTube Views and Likes",
      xlab = "Youtube Likes",ylab = "Comments")

# Views vs Subscribers

joint <- CrossTable(yt_df$Subs_cat,yt_df$Views_cat)

joint$t

barplot(joint$t,col = rainbow(10),las=1,ylab = "Frequency",ylim = c(0,250),main = "Bar Plot for YouTube Views VS Subscribers ")
legend("topright",c("Low_Subscriber","Medium_Subscriber","High_Subscriber"),col= rainbow(10),pch = 15)

cor.test(yt_df$Subscribers,yt_df$Views)
qplot(yt_df$Subscribers,yt_df$Views,color=yt_df$Subs_cat,shape= yt_df$Views_cat,main = "Scatter Plot Between YouTube Views and Subscribers",
      xlab = "Youtube Subscribers",ylab = "Views")+scale_color_manual(values=c("red", "#E69F00", "#56B4E9"))


# Views vs Dislikes

joint <- CrossTable(yt_df$Dislikes_cat,yt_df$Views_cat)

joint$t

barplot(joint$t,col = c("pink", "#E69F00", "Brown"),las=1,ylab = "Frequency",ylim = c(0,250),main = "Bar Plot for YouTube Views VS Dislikes ")
legend("topright",c("Low_Dislike","Medium_Dislike","High_Dislike"),col=c("pink", "#E69F00", "Brown"),pch = 15)

cor.test(yt_df$Dislikes,yt_df$Views)
qplot(yt_df$Dislikes,yt_df$Views,color=yt_df$Dislikes_cat,shape= yt_df$Views_cat,main = "Scatter Plot Between YouTube Views and Dislikes",
      xlab = "Youtube Dislikes",ylab = "Views")+scale_color_manual(values=c("red", "#E69F00", "#56B4E9"))


# Likes vs Dislikes

joint <- CrossTable(yt_df$Dislikes_cat,yt_df$Likes_cat)

joint$t

barplot(joint$t,col = c("sky blue", "#E69F00", "Brown"),las=1,ylab = "Frequency",ylim = c(0,200),main = "Bar Plot for YouTube Likes VS Dislikes ")
legend("topright",c("Low_Dislike","Medium_Dislike","High_Dislike"),col=c("sky blue", "#E69F00", "Brown"),pch = 15)

cor.test(yt_df$Dislikes,yt_df$Likes)
qplot(yt_df$Dislikes,yt_df$Likes,color=yt_df$Dislikes_cat,shape= yt_df$Views_cat,main = "Scatter Plot Between YouTube Likes and Dislikes",
      xlab = "Youtube Dislikes",ylab = "Likes")+scale_color_manual(values=c("brown", "#E69F00", "#56B4E9"))

# Comments vs Dislikes

joint <- CrossTable(yt_df$Dislikes_cat,yt_df$Comments.added.cat)

joint$t

barplot(joint$t,col = c("sky blue", "#E69F00", "Brown"),las=1,ylab = "Frequency",ylim = c(0,250),main = "Bar Plot for YouTube Comments VS Dislikes ")
legend("topright",c("Low_Dislike","Medium_Dislike","High_Dislike"),col=c("sky blue", "#E69F00", "Brown"),pch = 15)

cor.test(yt_df$Dislikes,yt_df$Comments.added)
qplot(yt_df$Dislikes,yt_df$Comments.added,color=yt_df$Dislikes_cat,shape= yt_df$Comments.added.cat,main = "Scatter Plot Between YouTube Comments and Dislikes",
      xlab = "Youtube Dislikes",ylab = "Comments")+scale_color_manual(values=c("brown", "#E69F00", "#56B4E9"))

# Comparing other numerical variable

cor.test(yt_df$Shares,yt_df$Likes)
qplot(yt_df$Likes,yt_df$Shares,color=yt_df$Likes_cat,main = "Scatter Plot Between YouTube Likes and Share",
      xlab = "Youtube Likes",ylab = "Share")+scale_color_manual(values=c("Dark green", "#E69F00", "#56B4E9"))

plot(yt_df$Impressions,yt_df$Likes)

# Comparing other numerical variable

cor.test(yt_df$Comments.added,yt_df$Views)
qplot(yt_df$Comments.added,yt_df$Views,color=yt_df$Views_cat,main = "Scatter Plot Between YouTube Views and Comments",
      xlab = "Youtube Comments",ylab = "Views")+scale_color_manual(values=c("MAgenta", "brown", "#56B4E9"))

plot(yt_df$Subscribers,yt_df$Comments.added)

# f) Linear regression

lr_views <- lm(Views ~ Likes+Dislikes+Impressions+Shares, data = yt_df)

summary(lr_views)

par(mfrow =c(2,2))
plot(lr_views)
par(mfrow=c(1,1))


cor.test(yt_df$Shares,yt_df$Views)

# Writing dataframe to csv in local
write.csv(yt_df,"D:/Semester 1/DANA/Assignment3/YouTube_Clean_Data.csv", row.names = FALSE)
