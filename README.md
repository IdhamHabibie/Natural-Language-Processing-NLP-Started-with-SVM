# Extract API using Drone Emprit
Python, R, Linux, Asterisk, PBX. 

# finding the correlation between influencer and ideas
rm(list=ls())
library(tm)
library(dplyr)
library(reshape2)
library(reshape)
library(stringr)

# Set the Directory
setwd("D:/R_Application/R_Shiny_Idham/Dataset - PA2")
Dataset1 <- read.csv('bigalpha.csv')
Dataset2 <- read.csv('ekodigi.csv')
Dataset3 <- read.csv('emoney.csv')
Dataset4 <- read.csv('investasi.csv')
Dataset5 <- read.csv('jouska.csv')
Dataset6 <- read.csv('nabungsaham.csv')

# Gabungan Dataset 
dataset <- rbind(Dataset1, Dataset2, Dataset3, Dataset4, Dataset5, Dataset6)

#Memfilter data yang mengandung kata-kata financial services
subset <- dataset[grep("gopay|(G|g|9|6)(O|o|0)(|-| )(P|p)(A|a|4)(Y|y)|ovo|(O|o|0)(V|v)(O|o|0)|dana|(D|d)(A|a|4)(N|n)(4|a|A)|doku|(D|d)(o|0)(K|k|q|Q)(U|u|oe|Oe|oE|o3|03|0e)|linkaja|(L|l)(i|1|I)(N|n)(K|k)(4|a)(J|j)(4|a)|pinjaman online|(P|p)(i|1|I)(N|n)(J|j|7)(4|a|A)(M|m)(A|4|a)(N|n) (0|o)(n|N)(L|1|l)(1|i)(N|n)(3|e)|fintech|(F|f|p|)(I|i|1)(N|n)(T|t)(E|3|e)(C|c|ch|Ch|cH|K|k) |cashback|(c)(4|a)(s|5)(h|H)(| )(B|b)(4|a|A)(C|c)(K|k)|akulaku|(4|a)(q|k)(U|u|oe|03|0e|o3)(L|l)(4|a|A|)(q|k|K|Q)(u|U|oe|03|)|kredivo|(K|k)(R|r)(E|3|e)(D|d)(I|i|1)(v|f|P|p|F|V)(o|0|O)|amartha|(A|4|a)(M|m)(A|4|a)(R|r)(T|t)(H|h| |)(A|4|a)|cashless|(C|c)(A|a|4)(S|s|5)(H|h|)(| )(L|l)(E|e|3)(SS|ss|Ss|sS|S|s)|BNI cash|(B|b)(N|n)(i|1|I)(| )(C|c)(A|a|4)(S|s|5)(H|h|)|Brizzy|(B|b)(R|r)(I|i)(Z|z|zz|zZ|Zz)(I|i|y|Y)|flazz|(F|f|p)(L|l)(A|a|4)(Zz|Z|zz|z|zZ|ss|Ss|sS|S|s)|fintech ilegal|(F|f|p|)(I|i|1)(N|n)(T|t)(E|3|e)(C|c|ch|Ch|cH|CH|K|k)(| )(I|i|1)(L|l|ll|LL|Ll|lL)(E|3|e)(G|g|6|9)(A|a|4)(L|l)|e-money|(E|e|3)(|-)(M|m)(O|o|0)(N|n)(E|e|3)(Y|y)", dataset$Mentions), ]

# Extract the Influencer
# Cleansing Data @Mentions
subset$Mentions <- gsub("RT", "", subset$Mentions) #remove 'RT'
subset$Mentions <- gsub("http[^[:space:]]*", "", subset$Mentions) #remove URL
subset$Mentions <- gsub("&amp;", "", subset$Mentions) #remove html &
subset$Mentions <- iconv(subset$Mentions, "latin1", "ASCII", sub="") # Remove accent chars
subset$Mentions <- trimws(subset$Mentions, which = c("left"))
subset$Mentions <- tolower(subset$Mentions)
subset$Mentions <- gsub("go pay|(G|g|9|6)(O|o|0)(-| )(P|p)(A|a|4)(Y|y)", "gopay", subset$Mentions) #ganti go pay dan go-pay jadi gopay


# Cleaning Data @Influencer 
subset$Influencer <- gsub("RT", "", subset$Influencer) #remove 'RT'
subset$Influencer <- gsub("http[^[:space:]]*", "", subset$Influencer) #remove URL
subset$Influencer <- gsub("&amp;", "", subset$Influencer) #remove html &
subset$Influencer <- iconv(subset$Influencer, "latin1", "ASCII", sub="") # Remove accent chars
subset$Influencer <- trimws(subset$Influencer, which = c("left"))

# Memfilter sebuah kata dari beberapa influencer sebelumnya
list_influencer <- "@marshyellow_|@bumbunasgor|@harkyuto|@helpmejsy|@bichtarrest"

# Grep with the list of influencer only
total <- subset[grep(list_influencer,subset$Mentions),]
dim(total)

# Defining the List of Financial Services, List of Dates, List of Influencer 
# List of the Best Influencer (Top 5)
list_influencer_data <- c("marshyellow_","bumbunasgor","harkyuto","helpmejsy","bichtarrest")

# List of the best Financial Services (Top 7)
list_financial_services <- c("gopay","ovo","dana","fintech","cashback","cashless","emoney")

# Find the Correlation between all these things 
# Using Looping method 

# Define New Dataset 
new_dataframe <- c(influencer = character(), financial_services = character(), total = double(), stringsAsFactors=FALSE)
final_dataframe <- c(influencer = character(), financial_services = character(), total = double(), stringsAsFactors=FALSE)
k <- 1
for(i in 1:length(list_influencer_data)){
  for(j in 1:length(list_financial_services)){
    
    # Obtain all influencer data firstly
    influencer_filter <- total$Mentions[str_detect(total$Mentions,list_influencer_data[i])==TRUE]
    
    # Collect them to one single dataframe 
    influencer_financial_services <- str_detect(influencer_filter,list_financial_services[j])
    
    # Create the new dataframe
    new_dataframe <- c(influencer = list_influencer_data[i], financial_services = list_financial_services[j], total = sum(influencer_financial_services))
    final_dataframe <- rbind(final_dataframe, new_dataframe)
  }
}

# Data Cleansing
final_dataframe <- data.frame(final_dataframe)[-1,]
final_dataframe <- final_dataframe[final_dataframe$total!=0,]
final_dataframe$total <- as.numeric(as.character(final_dataframe$total)) # Change from Factor to numeric

# Data Visualization
ggplot(data = final_dataframe, aes(x = influencer, y = total, fill = financial_services)) +
  geom_col(position = position_dodge()) + labs(y = "Quantity of Tweeting", x = "Date") + ggtitle("The Correlation between Influencer and the Financial Services Type") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16), axis.title.x = element_text(color = "grey20", size = 14, angle = 0, hjust = .5, vjust = 0, face = "plain"),axis.title.y = element_text(color = "grey20", size = 14, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 90, hjust = 1, vjust = 0, face = "plain"),
        axis.text.x = element_text(color = "grey20", size = 12, angle = 65, hjust = .5, vjust = .5, face = "plain"))
