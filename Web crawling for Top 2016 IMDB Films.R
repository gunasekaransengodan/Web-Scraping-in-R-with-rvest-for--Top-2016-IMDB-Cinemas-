#install Packages
install.packages("rvest") #https://cran.r-project.org/web/packages/rvest/rvest.pdf
install.packages("ggplot2")

#Loading the rvest package
library('rvest')
library('ggplot2')

#----------------------------------------------------------------------------------------------------------------
#Specifying the url for desired website to be scrapped
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)

#----------------------------------------------------------------------------------------------------------------

#scraping the following data

#Rank , #Title, #Description , #Runtime, #Genre, #Rating, #Votes, #Gross_Earning_in_Mil, #Director
#Actor, #Metascore

#---------------------------------------------------------------------------------------------------------------

#Step 1: RANK

#use Selector Gadget : http://selectorgadget.com
#Seelct Rank filed and get(Copy) HTML CSS Selector; Make sure all are selected or necesary films rank got selected

#Using CSS selectors to scrap the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Let's have a look at the rankings
head(rank_data)
tail(rank_data)

#Data-Preprocessing: Converting rankings to numerical
rank_data<-as.numeric(rank_data)

#Let's have another look at the rankings
head(rank_data)
tail(rank_data)

#---------------------------------------------------------------------------------------------------------------

#Step 2: Title

title_data_html <- html_nodes(webpage,'.lister-item-header a')
title_data <- html_text(title_data_html)
head(title_data)
tail(title_data)

#---------------------------------------------------------------------------------------------------------------

#Step 3: Description

description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')
description_data <- html_text(description_data_html)
head(description_data)
tail(description_data)

#Data-Preprocessing: removing '\n'
description_data<-gsub("\n","",description_data)

#Let's have another look at the description data 
head(description_data)

#---------------------------------------------------------------------------------------------------------------

#Step 4: Runtime

runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')
runtime_data <- html_text(runtime_data_html)
head(runtime_data)
tail(runtime_data)

#Data-Preprocessing: removing mins and converting it to numerical
runtime_data<-gsub(" min","",runtime_data)
runtime_data<-as.numeric(runtime_data)

#Let's have another look at the runtime data
head(runtime_data)
tail(runtime_data)

#---------------------------------------------------------------------------------------------------------------

#Step 5: Gener

genre_data_html <- html_nodes(webpage,'.genre')
genre_data <- html_text(genre_data_html)
head(genre_data)
tail(genre_data)

#Data-Preprocessing: removing \n
genre_data<-gsub("\n","",genre_data)

#Data-Preprocessing: removing excess spaces
genre_data<-gsub(" ","",genre_data)

#taking only the first genre of each movie
genre_data<-gsub(",.*","",genre_data)

#Convering each genre from text to factor
genre_data<-as.factor(genre_data)

#Let's have another look at the genre data
head(genre_data)
tail(genre_data)

#---------------------------------------------------------------------------------------------------------------

#Step 6: Rating

rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')
rating_data <- html_text(rating_data_html)
head(rating_data)
tail(rating_data)

#Data-Preprocessing: converting ratings to numerical
rating_data<-as.numeric(rating_data)

#Let's have another look at the ratings data
head(rating_data)
tail(rating_data)

#---------------------------------------------------------------------------------------------------------------

#Step 7: Vote

votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')
votes_data <- html_text(votes_data_html)
head(votes_data)
tail(votes_data)

#Data-Preprocessing: removing commas
votes_data<-gsub(",","",votes_data)

#Data-Preprocessing: converting votes to numerical
votes_data<-as.numeric(votes_data)

#Let's have another look at the votes data
head(votes_data)
tail(votes_data)

#---------------------------------------------------------------------------------------------------------------

#Step 7: Director

directors_data_html <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')
directors_data <- html_text(directors_data_html)
head(directors_data)
tail(directors_data)

#Data-Preprocessing: converting directors data into factors
directors_data<-as.factor(directors_data)

#Let's have another look at the directors data
head(directors_data)
tail(directors_data)

#---------------------------------------------------------------------------------------------------------------

#Step 7: Actors

actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')
actors_data <- html_text(actors_data_html)
head(actors_data)
tail(actors_data)

#Data-Preprocessing: converting actors data into factors
actors_data<-as.factor(actors_data)

#Let's have another look at the Actors data
head(actors_data)
tail(actors_data)

#---------------------------------------------------------------------------------------------------------------

#Step 7: Metascore

metascore_data_html <- html_nodes(webpage,'.metascore')
metascore_data <- html_text(metascore_data_html)
head(metascore_data)
tail(metascore_data)

#Data-Preprocessing: removing extra space in metascore
metascore_data<-gsub(" ","",metascore_data)

#Let's have another look at the Metascore data
head(metascore_data)
tail(metascore_data)

#Lets check the length of metascore data
length(metascore_data)

for (i in c(17,36,38,43,54,80,81)) #Checked manually
{
  a<-metascore_data[1:(i-1)]
  b<-metascore_data[i:length(metascore_data)]
  metascore_data<-append(a,list("NA"))
  metascore_data<-append(metascore_data,b)
}

#Data-Preprocessing: converting metascore to numerical
metascore_data<-as.numeric(metascore_data)

#Let's have another look at length of the metascore data
length(metascore_data)

#Let's look at summary statistics
summary(metascore_data)

#---------------------------------------------------------------------------------------------------------------

#Step 7: Gross Revenue

gross_data_html <- html_nodes(webpage,'.ghost~ .text-muted+ span')
gross_data <- html_text(gross_data_html)
head(gross_data)
tail(gross_data)

#Data-Preprocessing: removing '$' and 'M' signs
gross_data<-gsub("M","",gross_data)

gross_data<-substring(gross_data,2,6)

#Let's check the length of gross data
length(gross_data)

#Filling missing entries with NA
for (i in c(11,17,28,36,38,50,54,59,78,80,81)) #Checked manually
{
  a<-gross_data[1:(i-1)]
  b<-gross_data[i:length(gross_data)]
  gross_data<-append(a,list("NA"))
  gross_data<-append(gross_data,b)
}

#Data-Preprocessing: converting gross to numerical
gross_data<-as.numeric(gross_data)

#Let's have another look at the length of gross data
length(gross_data)

summary(gross_data)

#---------------------------------------------------------------------------------------------------------------

#Step Final: Frame all in one

#Combining all the lists to form a data frame
movies_df<-data.frame(Rank = rank_data, 
                      Title = title_data,
                      Description = description_data, 
                      Runtime = runtime_data,
                      Genre = genre_data, 
                      Rating = rating_data,
                      Metascore = metascore_data, 
                      Votes = votes_data,   
                      Gross_Earning_in_Mil = gross_data,
                      Director = directors_data, 
                      Actor = actors_data)

#Structure of the data frame
str(movies_df)

###
### Some Result
###
#Analyse 100 Move Data
#which Genre had the longest runtime
qplot(data = movies_df,Runtime,fill = Genre,bins = 30)

#which genre has the highest votes
ggplot(movies_df,aes(x=Runtime,y=Rating)) + geom_point(aes(size=Votes,col=Genre))

#which genre has the highest average gross earnings 
ggplot(movies_df,aes(x=Runtime,y=Gross_Earning_in_Mil)) + geom_point(aes(size=Rating,col=Genre))







