#Group Name: EXEMPLARY ANALYSTS
#Group Members:
#1.  SACHIN SHINGARI
#2. SHAILENDRA KUMAR
#3. UJWALA ISHWARAPPAGOL
#4. USHA HARIRAM

#Load the companies and rounds data into two data frames and name them 
#companies and rounds2 respectively.

companies<-read.delim("companies.txt",header = TRUE,stringsAsFactors = FALSE,sep="\t")
rounds2<-read.csv("rounds2.csv",header = TRUE,stringsAsFactors = FALSE)

#call the library dplyr
library(dplyr)

#####################################################################################################
#Checkpoint 1: Data Cleaning
#1. How many unique companies are present in rounds2?

#convert the company_permalink to uppercase letters
rounds2$company_permalink<-toupper(rounds2$company_permalink)

#unique companies from rounds2
rounds2_unique<-distinct(rounds2, company_permalink)
nrow(rounds2_unique)

#2.How many unique companies are present in companies?
#convert the column permalink to uppercase letters
companies$permalink<-toupper(companies$permalink)

#unique companies in companies data frame
companies_unique<-distinct(companies,permalink)
nrow(companies_unique)

#3. In the companies data frame, which column can be used as the unique key for each company? 
    #Write the name of the column.
#Answer:permalink

#4. Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N
#Find out if there are any companies which re in rounds2 but not in companies data frame.
companies_diff<-!(rounds2_unique$company_permalink %in% companies$permalink)
length(which(companies_diff=="TRUE"))
#Answer: All companies in rounds2 file are present in companies file

#5. Merge the two data frames so that all  variables (columns)  in the companies frame are added to the 
#rounds2 data frame. Name the merged frame master_frame. How many observations are present in master_frame ?
#Answer: rename "company_permalink" to "permalink" such that both files have a common column before merge
names(rounds2)[1]<-paste("permalink")

#merge both companies and rounds2
master_frame<-merge(rounds2,companies,by="permalink")
nrow(master_frame)
#write.csv(master_frame,file = "master_frame.csv")
#---------------------------------------------------------------------------------------------------------#
#Checkpoint 2
#We can see that the column raised_amount_usd has NA values. This has to be changed to numeric values 
#to do aggregation on the column.

#How many NA values are present in the column raised_amount_usd ?
length(which(is.na(master_frame$raised_amount_usd)))

#Replace NA values in raised_amount_usd column with 0
master_frame[which(is.na(master_frame$raised_amount_usd)),"raised_amount_usd"]<-0

#1. Average funding amount of venture type
#Answer: 
venture <- subset(master_frame, funding_round_type == "venture")
mean(venture$raised_amount_usd)
message("Average funding amount of venture type is ", mean(venture$raised_amount_usd))

#2. Average funding amount of angel type
#Answer:
angel <- subset(master_frame,funding_round_type == "angel")
mean(angel$raised_amount_usd)
message("Average funding amount of angel type is ", mean(angel$raised_amount_usd))

#3. Average funding amount of seed type
#Answer: 
seed <- subset(master_frame, funding_round_type == "seed")
mean(seed$raised_amount_usd)
message("Average funding amount of seed type is ", mean(seed$raised_amount_usd))

#4. Average funding amount of private equity type
#Answer:
private_equity <- subset(master_frame, funding_round_type == "private_equity")
mean(private_equity$raised_amount_usd)
message("Average funding amount of private equity type is ", mean(private_equity$raised_amount_usd))

#5. Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, 
#which investment type is the most suitable for them?
#Answer: Venture

#--------------------------------------------------------------------------------------------------------#
#Checkpoint 3: Country Analysis
#Spark Funds wants to see the top nine countries which have received the highest total funding 
# (across ALL sectors for the chosen investment type)

venture <- subset(master_frame, funding_round_type == "venture")


#group by country code
group_by_country<-group_by(venture,country_code)

#summarise based on the total amount of fund received and sort it.
country_sum<-summarise(group_by_country,sum(raised_amount_usd))
arrange_country_sum<-arrange(country_sum,desc(`sum(raised_amount_usd)`))
top9<-arrange_country_sum[1:9, ]
View(top9)
#write.csv(top9, file = "top9.csv")

#Answer: The top 3 countries are: USA, GBR and IND
#As per the list, CHN is in second place, but since it isn't in the English Speaking Countries, it is ruled out

#-------------------------------------------------------------------------------------------------------#

#Checkpoint 4: Sector Analysis 1
#load mapping file
mapping<-read.csv("mapping.csv",header = TRUE,stringsAsFactors = FALSE)

#Since we need to convert the data from wide to long format, call the tidyr library
library(tidyr)

#There is a blank column which has no significance in the mapping file, let us remove that column
mapping$Blanks <- NULL

#Now convert the dataframe from wide to long
map_long<-gather(mapping,main_sector,value,2:9)

#Let us consider records where value=1. Ignore the other values as they carry no information.
sector_mapping<-subset(map_long,map_long$value==1)

#Now, we will no longer require the value column, so let us remove that column as well
sector_mapping$value <- NULL

#Renaming category_list as primary_sector
sector_mapping$primary_sector<-sector_mapping$category_list
sector_mapping$category_list<-NULL

#Call the stringr library to perform string operations in the category list column
library(stringr)

#We know that the suitable investment type is "venture". For this type:
#Extract the primary sector of each category list from the category_list column
venture$primary_sector<-str_split(venture$category_list,"\\|",simplify = TRUE)[,1]

#Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors 
venture_sector<-merge(venture,sector_mapping,by="primary_sector")
#write.csv(venture_sector, file = "venture_sector.csv") 

#-------------------------------------------------------------------------------------------------------#
#Checkpoint 5: Sector Analysis 2
#Create three separate data frames D1, D2 and D3 for each of the three countries containing the observations of 
#funding type FT falling within the 5-15 million USD range. 

#Get funding types falling in between the range 5 and 15 million
funding_required<-subset(venture_sector,venture_sector$raised_amount_usd>=5000000 & venture_sector$raised_amount_usd<=15000000 )
#write.csv(funding_required, file = "funding_required.csv")

#Create three separate data frames one for each country, i.e. USA, GBR and IND
D1<-subset(funding_required,funding_required$country_code=="USA")
D2<-subset(funding_required,funding_required$country_code=="GBR")
D3<-subset(funding_required,funding_required$country_code=="IND")

#1. Total number of Investments (count)
#Total number of investments for each country c1,c2,c3
nrow(D1)
nrow(D2)
nrow(D3)

#2. Total amount of investment (USD)
#Total amount for each country 
sum(D1$raised_amount_usd)
sum(D2$raised_amount_usd)
sum(D3$raised_amount_usd)

#3,4,5. Top Sector name (no. of investment-wise) 

#For D1, Group by main_sector along with count of investments 
D1_group<-group_by(D1,main_sector)
D1_count<-summarise(D1_group,length(main_sector))
D1_sum<-summarise(D1_group,sum(raised_amount_usd))

D1_count_sum<-merge(D1_count,D1_sum,by="main_sector")
D1_arrange<-arrange(D1_count_sum,desc(`length(main_sector)`))
D1_top_sector<-D1_arrange[1,1]
D1_top_sector
D1_secondtop_sector<-D1_arrange[2,1]
D1_secondtop_sector
D1_thirdtop_sector<-D1_arrange[3,1]
D1_thirdtop_sector


#For D2, Group by main_sector along with count of investments 
D2_group<-group_by(D2,main_sector)
D2_count<-summarise(D2_group,length(main_sector))
D2_sum<-summarise(D2_group,sum(raised_amount_usd))

D2_count_sum<-merge(D2_count,D2_sum,by="main_sector")
D2_arrange<-arrange(D2_count_sum,desc(`length(main_sector)`))
D2_top_sector<-D2_arrange[1,1]
D2_top_sector
D2_secondtop_sector<-D2_arrange[2,1]
D2_secondtop_sector
D2_thirdtop_sector<-D2_arrange[3,1]
D2_thirdtop_sector



#For D3, group by main_sector along with count of investments 
D3_group<-group_by(D3,main_sector)
D3_count<-summarise(D3_group,length(main_sector))
D3_sum<-summarise(D3_group,sum(raised_amount_usd))

D3_count_sum<-merge(D3_count,D3_sum,by="main_sector")
D3_arrange<-arrange(D3_count_sum,desc(`length(main_sector)`))
D3_top_sector<-D3_arrange[1,1]
D3_top_sector
D3_secondtop_sector<-D3_arrange[2,1]
D3_secondtop_sector
D3_thirdtop_sector<-D3_arrange[3,1]
D3_thirdtop_sector



#6,7,8 Number of investments in top sector, second top sector, third top sector
View(D1_arrange) #note the length of main sector for 1, 2, 3 rows.
View(D2_arrange)
View(D3_arrange)

#9. For point 3 (top sector count-wise), which company received the highest investment?
#For country 1, USA
S1<-subset(D1,D1$main_sector==D1_top_sector)
S1_group_by<-group_by(S1,permalink)
S1_sum<-summarise(S1_group_by,sum(raised_amount_usd))
S1_arrange<-arrange(S1_sum,desc(`sum(raised_amount_usd)`))
#Get top company key
D1_S1_top_company<-toString(S1_arrange[1,1])

#Get top company name
companies[which(companies$permalink==D1_S1_top_company),"name"]


#For country 2, GBR
S2<-subset(D2,D2$main_sector==D2_top_sector)
S2_group_by<-group_by(S2,permalink)
S2_sum<-summarise(S2_group_by,sum(raised_amount_usd))
S2_arrange<-arrange(S2_sum,desc(`sum(raised_amount_usd)`))
#Get top company key
D2_S2_top_company<-toString(S2_arrange[1,1])

#Get top company name
companies[which(companies$permalink==D2_S2_top_company),"name"]

#For country 3, IND
S3<-subset(D3,D3$main_sector==D3_top_sector)
S3_group_by<-group_by(S3,permalink)
S3_sum<-summarise(S3_group_by,sum(raised_amount_usd))
S3_arrange<-arrange(S3_sum,desc(`sum(raised_amount_usd)`))
#Get top company key
D3_S3_top_company<-toString(S3_arrange[1,1])

#Get top company name
companies[which(companies$permalink==D3_S3_top_company),"name"]

#10.For point 4 (second best sector count-wise), which company received the highest investment?
#For country 1, USA
S4<-subset(D1,D1$main_sector==D1_secondtop_sector)
S4_group_by<-group_by(S4,permalink)
S4_sum<-summarise(S4_group_by,sum(raised_amount_usd))
S4_arrange<-arrange(S4_sum,desc(`sum(raised_amount_usd)`))
#Get top company key
D1_S4_top_company<-toString(S4_arrange[1,1])

#Get top company name
companies[which(companies$permalink==D1_S4_top_company),"name"]


#For country 2, GBR
S5<-subset(D2,D2$main_sector==D2_secondtop_sector)
S5_group_by<-group_by(S5,permalink)
S5_sum<-summarise(S5_group_by,sum(raised_amount_usd))
S5_arrange<-arrange(S5_sum,desc(`sum(raised_amount_usd)`))
#Get top company key
D2_S5_top_company<-toString(S5_arrange[1,1])

#Get top company name
companies[which(companies$permalink==D2_S5_top_company),"name"]


#For country 3, IND
S6<-subset(D3,D3$main_sector==D3_secondtop_sector)
S6_group_by<-group_by(S6,permalink)
S6_sum<-summarise(S6_group_by,sum(raised_amount_usd))
S6_arrange<-arrange(S6_sum,desc(`sum(raised_amount_usd)`))
#Get top company key
D3_S6_top_company<-toString(S6_arrange[1,1])

#Get top company name
companies[which(companies$permalink==D3_S6_top_company),"name"]

############################################################################################
                                #end of R coding#
