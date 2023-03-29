# x <-139 + 121
# x
# class(x)
# x_char <- as.character(x)
# class(x_char)
# download.file("https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0101EN-Coursera/v2/dataset/movies-db.csv", 
#               destfile="movies-db.csv")

# download.file("https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0101EN-Coursera/v2/dataset/movies-db.xls", 
#               destfile="movies-db.xls")
# my_data <- read.csv("movies-db.csv")
# my_data
#install.packages("httr", repos = "https://cran.ma.imperial.ac.uk/")
# install.packages("pkgconfig", repos = "https://cran.ma.imperial.ac.uk/")
# install.packages("rvest",repos = "https://cran.ma.imperial.ac.uk/")
# install.packages("broom", type="binary",repos = "https://cran.ma.imperial.ac.uk/" )
# install.packages("data.table", repos = "https://cran.ma.imperial.ac.uk/") 
library(httr)
library(rvest)
# require("httr")
# require("rvest")
url<-'https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country'
response<-GET(url)
response
get_wiki_covid19_page <- function() {
 
  wiki_base_url <-  "https://en.wikipedia.org/w/index.php"

  wiki_params <- list(title = "Template:COVID-19_testing_by_country")
  
   wiki_response <- GET(wiki_base_url, query = wiki_params) 
 
return(wiki_response)
}
wiki_covid19_page_response <- get_wiki_covid19_page()

print(wiki_covid19_page_response)
root_node <- read_html(wiki_covid19_page_response)

table_node <- html_nodes(root_node, "table")

df <- html_table(table_node[2])

df
print(df)
data.frame(df)
data_frame<- data.frame(df)
data_frame
# class(lek)
summary(data_frame)
preprocess_covid_data_frame <- function(data_frame) {
    
    shape <- dim(data_frame)

    # Remove the World row
   data_frame<-data_frame[!(data_frame$`Country.or.region`=="World"),]
    # Remove the last row
    data_frame <- data_frame[1:172, ]
    
    # We dont need the Units and Ref columns, so can be removed
    data_frame["Ref."] <- NULL
    data_frame["Units[b]"] <- NULL
    
    # Renaming the columns
    names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
    
    # Convert column data types
    data_frame$country <- as.factor(data_frame$country)
    data_frame$date <- as.factor(data_frame$date)
    data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
    data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
    data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
    data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
    data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
    
    return(data_frame)
}
new_covid_data_frame <- preprocess_covid_data_frame(data_frame)

# head(new_covid_data_frame)
# summary(new_covid_data_frame)