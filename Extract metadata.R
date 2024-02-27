#Extract metadata based on DOIs for the North Sea Review project

# data and packages ----
library(readxl)

paper_db_data <- read_excel("Data/Metadata/paper_db_data.xlsx")
paper_db_data%>%
  group_by(Doi)%>%
  count()%>%
  filter(n > 1)
# get metadata
# test format for first 20 entries
elephant_1_20 <- rcrossref::cr_cn(paper_db_data$Doi[1:20], format = "bibentry") 

# bibentry can be converted later on to a dataframe 
# loop over all dois from the database: 
elephant <- vector(mode = "list", length = nrow(paper_db_data)) #check before that only papers with doi are in paper db!

for(i in 1:6135){
  elephant[[i]] <- rcrossref::cr_cn(paper_db_data$Doi[i], format = "bibentry")
  elephant[[i]][length(elephant[[i]])+1] <- paper_db_data$Doi[i]
  print(paste("done with:", i))
}

# first run for 1 - 686, 687 breaks function: Format 'bibentry' for '10.2903/j.efsa.2016.4501' is not supported by the DOI registration agency: 'op'.
# continue with 688:
for(i in 688:6135){
  elephant[[i]] <- rcrossref::cr_cn(paper_db_data$Doi[i], format = "bibentry")
  elephant[[i]][length(elephant[[i]])+1] <- paper_db_data$Doi[i]
  print(paste("done with:", i))
}

#3453 missing
for(i in 3454:6135){
  elephant[[i]] <- rcrossref::cr_cn(paper_db_data$Doi[i], format = "bibentry")
  elephant[[i]][length(elephant[[i]])+1] <- paper_db_data$Doi[i]
  print(paste("done with:", i))
}


# 5815 missing
for(i in 5816:6135){
  elephant[[i]] <- rcrossref::cr_cn(paper_db_data$Doi[i], format = "bibentry")
  elephant[[i]][length(elephant[[i]])+1] <- paper_db_data$Doi[i]
  print(paste("done with:", i))
}

#get dois for paper 687, 3453 and 5815
paper_db_data$Doi[687]
paper_db_data$Doi[3453]
paper_db_data$Doi[5815]

# convert into a dataframe: 
df_doi_meta <- data.table::rbindlist(elephant, fill = TRUE) # in first try: df_doi_meta has only 6111 rows --> 24 are missing?

paper_db_data%>%
  filter(!(Title...4 %in% df_doi_meta$title))

x <- paper_db_data%>%
  filter(...1 == 55)
df_doi_meta%>%
  filter(title == x$Title...4)
# dois and titles not useful for comaparison as they are in too many cases not similar, eg. cr_cn() has not found doi in some cases or
# title is written differently than in the database
# Solution add to the loop for each entry as last element the original DOI, which was used in the cr_cn() function, also good for matching with data base
df_doi_meta <- data.table::rbindlist(elephant, fill = TRUE)
# now element 224 of input is not a data.frame, data.table or list
# same for 1054, 1565, 1567, 1568, 1570, 1572, 2623, 3698, 3731, 4590, 4591, 4592
# those elements will need manual look up
elephant[[224]] <- list(elephant[[224]])
elephant[[1054]] <- list(elephant[[1054]])
elephant[[1565]] <- list(elephant[[1565]])
elephant[[1567]] <- list(elephant[[1567]])
elephant[[1568]] <- list(elephant[[1568]])
elephant[[1570]] <- list(elephant[[1570]])
elephant[[1572]] <- list(elephant[[1572]])
elephant[[2623]] <- list(elephant[[2623]])
elephant[[3698]] <- list(elephant[[3698]])
elephant[[3731]] <- list(elephant[[3731]])
elephant[[4590]] <- list(elephant[[4590]])
elephant[[4591]] <- list(elephant[[4591]])
elephant[[4592]] <- list(elephant[[4592]])
elephant[[4594]] <- list(elephant[[4594]])
elephant[[4596]] <- list(elephant[[4596]])
elephant[[4598]] <- list(elephant[[4598]])
elephant[[4604]] <- list(elephant[[4604]])
elephant[[4802]] <- list(elephant[[4802]])
elephant[[5316]] <- list(elephant[[5316]])
elephant[[5583]] <- list(elephant[[5583]])
elephant[[5884]] <- list(elephant[[5884]])

# 21 elements had to be converted to a list + 3 elements that broke the loop = 24, add those three DOIs now to df_doi_meta
# export and do manual check up for the weird 24 papers in excel, then import again:
write_excel_csv(df_doi_meta, file = "extracted_doi.csv")
# titles are list elements
df_doi_meta$titles <- unlist(df_doi_meta$title) # does not work
for(i in 1:nrow(df_doi_meta)){
  if(is.null(df_doi_meta$title[[i]]) == TRUE){
    df_doi_meta$title[[i]] <- "look up"
  }
}

df_doi_meta$title <- unlist(df_doi_meta$title)
glimpse(df_doi_meta)

write_excel_csv(df_doi_meta, file = "extracted_doi.csv")

# extract publication year and title for scopus paper ----
library(readr)
scopus_data <- read_delim("Data/Metadata/scopus_data.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

scopus_meta <- data.frame("link" = rep(NA, nrow(scopus_data)),
                          "title" = rep(NA, nrow(scopus_data)),
                          "pub_year" = rep(NA, nrow(scopus_data)))

for(k in 1:nrow(scopus_data)){
  li <- readLines(scopus_data$Link[k])
  scopus_meta$link[k] <- scopus_data$Link[k]
  for(i in 1:length(li)){
    if(sum(grep('sc-page-title', li[i])) == 1){ # could add other options here - "doi" and "DOI" appear the most obvious to me
      
      subs <- strsplit(li[i], '\"') # this could be critical - the different websites may vary here
      
      for(j in 1:length(subs[[1]])){
        if(startsWith(subs[[1]][j], 'Document details - ') == T){
          if(substr(subs[[1]][j], nchar(subs[[1]][j]), nchar(subs[[1]][j])) == " "){
            scopus_meta$title[k] <- paste0(substr(subs[[1]][j], 20, 999), substr(subs[[1]][j+1], 1, 999))
          } else {
            scopus_meta$title[k] <- (substr(subs[[1]][j], 20, 999))
          }
        }
      }
      
      break
    }
  }
  for(l in 1:length(li)){
    if(sum(grep('/section><input ', li[l])) == 1){ # could add other options here - "doi" and "DOI" appear the most obvious to me
      
      subs <- strsplit(li[l], '\"') # this could be critical - the different websites may vary here
      #print(subs[[1]][36])
      scopus_meta$pub_year[k] <- subs[[1]][36]
      # for(j in 1:length(subs[[1]])){
      #   if(startsWith(subs[[1]][j], 'Document details - ') == T){
      #     print(substr(subs[[1]][j], 20, 999))
      #   }
      # }
      
      break
    }
  }
  
  print(paste("done with:", k))
  
  
}

# export scopus metadata
write.csv(scopus_meta, file = "Data/Metadata/new_metadata_by_scopuslink.csv")

# Merge all extracted metadata to the original database
library(tidyverse)
paper_db_data <- readxl::read_excel("Data/Metadata/paper_db_data.xlsx")
new_metadata_by_doi_cleaned <- readxl::read_excel("Data/Metadata/new_metadata_by_doi_cleaned.xlsx")
new_metadata_by_scopus_cleaned <- readxl::read_excel("Data/Metadata/new_meatadata_by_Scopus_cleaned.xlsx")
new_metadata_by_link_cleaned <- read_csv("Data/Metadata/metaData_links.csv")
glimpse(new_metadata_by_link_cleaned)

paper_db_data <- paper_db_data%>%
  rename("doi_from_database" = Doi, 
         "link" = Link)
new_metadata_by_scopus_cleaned <- new_metadata_by_scopus_cleaned%>%
  rename("link" = `link from database`)

doi_joined <- right_join(x = paper_db_data, y = new_metadata_by_doi_cleaned, by = "doi_from_database")
paper_db_data[10, ]$doi_from_database
cookie <- new_metadata_by_doi_cleaned

which(new_metadata_by_doi_cleaned$"doi_from_database" == "10.1186/s10152-019-0532-z")
new_metadata_by_doi_cleaned <- new_metadata_by_doi_cleaned[-10, ]
new_metadata_by_doi_cleaned <- new_metadata_by_doi_cleaned%>%
  select(title, doi_from_database, year)%>%
  distinct()

doi_joined <- right_join(x = paper_db_data, y = new_metadata_by_doi_cleaned, by = "doi_from_database")
paper_db_data[2481, ]$doi_from_database
cookie <- new_metadata_by_doi_cleaned%>%
  filter(doi_from_database == "10.1023/A:1020361124656")
View(cookie)
which(new_metadata_by_doi_cleaned$"doi_from_database" == "10.1023/A:1020361124656")
new_metadata_by_doi_cleaned <- new_metadata_by_doi_cleaned[-2434, ]
doi_joined <- right_join(x = paper_db_data, y = new_metadata_by_doi_cleaned, by = "doi_from_database")
paper_db_data[4093, ]$doi_from_database
cookie <- new_metadata_by_doi_cleaned%>%
  filter(doi_from_database == "10.1117/12.274755")
View(cookie)
which(new_metadata_by_doi_cleaned$"doi_from_database" == "10.1117/12.274755")
new_metadata_by_doi_cleaned <- new_metadata_by_doi_cleaned[-3841, ]
doi_joined <- right_join(x = paper_db_data, y = new_metadata_by_doi_cleaned, by = "doi_from_database")
paper_db_data[5120, ]$doi_from_database
cookie <- new_metadata_by_doi_cleaned%>%
  filter(doi_from_database == "10.1007/698-2014-330")
View(cookie)
which(new_metadata_by_doi_cleaned$"doi_from_database" == "10.1007/698-2014-330" & new_metadata_by_doi_cleaned$year == 2014)
new_metadata_by_doi_cleaned <- new_metadata_by_doi_cleaned[-4570, ]
doi_joined <- right_join(x = paper_db_data, y = new_metadata_by_doi_cleaned, by = "doi_from_database")


scopus_joined <- right_join(x = paper_db_data, y = new_metadata_by_scopus_cleaned, by = "link")
new_metadata_by_scopus_cleaned <- new_metadata_by_scopus_cleaned%>%
  select(link, title, pub_year)%>%
  distinct()
scopus_joined <- right_join(x = paper_db_data, y = new_metadata_by_scopus_cleaned, by = "link")
paper_db_data[4776, ]$link
cookie <- new_metadata_by_scopus_cleaned%>%
  filter(link == "https://www.scopus.com/inward/record.uri?eid=2-s2.0-85042568292&partnerID=40&md5=a3fcb73ce77f11df48dbe7e5a8db533f")
View(cookie)
which(new_metadata_by_scopus_cleaned$link == "https://www.scopus.com/inward/record.uri?eid=2-s2.0-85042568292&partnerID=40&md5=a3fcb73ce77f11df48dbe7e5a8db533f")
new_metadata_by_scopus_cleaned <- new_metadata_by_scopus_cleaned[-461, ]
scopus_joined <- right_join(x = paper_db_data, y = new_metadata_by_scopus_cleaned, by = "link")


new_metadata_by_link_cleaned <- new_metadata_by_link_cleaned%>%
  select(title, date, link)%>%
  distinct()

link_joined <- right_join(x = paper_db_data, y = new_metadata_by_link_cleaned, by = "link")
paper_db_data[5320, ]$link
cookie <- new_metadata_by_link_cleaned%>%
  filter(link == "http://www.vliz.be/imisdocs/publications/356007.pdf")
View(cookie)
which(new_metadata_by_link_cleaned$link =="http://www.vliz.be/imisdocs/publications/356007.pdf")

new_metadata_by_link_cleaned <- new_metadata_by_link_cleaned[-282, ]

link_joined <- right_join(x = paper_db_data, y = new_metadata_by_link_cleaned, by = "link")
paper_db_data[5339, ]$link
cookie <- new_metadata_by_link_cleaned%>%
  filter(link == "http://www.waveworkshop.org/13thWaves/Papers/Groll_wave_Banff2013.pdf")
View(cookie)
which(new_metadata_by_link_cleaned$link =="http://www.waveworkshop.org/13thWaves/Papers/Groll_wave_Banff2013.pdf")

new_metadata_by_link_cleaned <- new_metadata_by_link_cleaned[-277, ]
link_joined <- right_join(x = paper_db_data, y = new_metadata_by_link_cleaned, by = "link")

paper_db_data[5784, ]$link
cookie <- new_metadata_by_link_cleaned%>%
  filter(link == "http://aei.pitt.edu/96396/")
View(cookie)
which(new_metadata_by_link_cleaned$link =="http://aei.pitt.edu/96396/")

new_metadata_by_link_cleaned <- new_metadata_by_link_cleaned[-173, ]
link_joined <- right_join(x = paper_db_data, y = new_metadata_by_link_cleaned, by = "link")


#merge all three tables
doi_joined <- doi_joined%>%
  select(`Article ID`, `Article URL`, title, year)
scopus_joined <- scopus_joined%>%
  select(`Article ID`, `Article URL`, title, pub_year)%>%
  rename("year" = pub_year)
link_joined <- link_joined%>%
  select(`Article ID`, `Article URL`, title, date)%>%
  rename("year" = date)

new_metadata <- rbind(doi_joined, link_joined, scopus_joined)
new_metadata <- new_metadata%>%
  rename("pdf_name" = `Article ID`)


# join with sysrev data
answers <- readxl::read_excel("~/Desktop/Doktorarbeit/North_Sea_Review/Answers_P99278_1219_A5795.xlsx")
articles <- readxl::read_excel("~/Desktop/Doktorarbeit/North_Sea_Review/Articles_P99278_1219_A5795.xlsx")

articles_cleaned <- articles%>%
  mutate(title = gsub(".pdf", "", Title))%>%
  select(`Article ID`, `Article URL`, title)%>%
  rename("pdf_name" = title)
articles_cleaned%>%
  janitor::get_dupes(pdf_name)

articles_metadata <- right_join(x = new_metadata, y = articles_cleaned, by = "pdf_name")
test <- scopus_joined%>%
  janitor::get_dupes(`Article ID`)

articles_metadata <- articles_metadata%>%
  select(pdf_name, title, year, `Article ID`, `Article URL.y`)%>%
  rename("Article_URL" = `Article URL.y`)
articles_metadata%>%
  janitor::get_dupes(`Article ID`)

# join with answers
answers_metadata <- right_join(x = articles_metadata, y = answers, by = "Article ID")
answers_metadata_cleaned <- answers_metadata%>%
  select(!c(`User Notes`, Title, Journal, Authors, Status, `User Count`, Users, `Article URL`))


# test for duplicate papers, clear for NAs
answers_metadata_cleaned <- answers_metadata_cleaned%>%
  drop_na(title)



dupl <- answers_metadata_cleaned%>%
  janitor::get_dupes(title)   


write.csv(dupl, file = "Data/duplicates.csv")

# import cleaned duplicates again: 
duplicate_cleaned <- readxl::read_excel("Data/duplicate_cleaned.xlsx")
blub <- duplicate_cleaned%>%
  filter(Keep == 1)%>%
  group_by(title)%>%
  slice_sample(n = 1)
blub$Keep <- 2
duplicate_cleaned[duplicate_cleaned$`Article ID` %in% blub$`Article ID`, ]$Keep <- 2
duplicates_out <- duplicate_cleaned%>%
  filter(Keep != 2) # 0 = false papers, 1 = papers duplicated, 2 = selected duplicate

answers_for_analysis <- answers_metadata_cleaned%>%
  filter(!(`Article ID` %in% duplicates_out$`Article ID`))


dupl_in_answers <- answers_for_analysis%>%
  janitor::get_dupes(`Article ID`)

write.csv(dupl_in_answers, file = "Data/dupl_in_answers.csv")

answers_dupl_cleaned<- readxl::read_excel("Data/dupl_in_answers_cleaned.xlsx")
answers_dupl_out <- answers_dupl_cleaned%>%
  filter(Keep == 0)

rows_out <- which(answers_for_analysis$title %in% answers_dupl_out$title)
answers_for_analysis <- answers_for_analysis[-c(rows_out), ]


answers_for_analysis%>%
  janitor::get_dupes(Article_URL)
# 0 duplicates :D
summary(answers_for_analysis)
write.csv(answers_for_analysis, file = "Data/answers_for_analysis.csv")
answers_for_analysis_with_years <- read_delim("Data/answers_for_analysis_with_years.csv", 
                                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
answers_for_analysis_with_years <- answers_for_analysis_with_years%>%
  drop_na(year)

write_csv(answers_for_analysis_with_years, file = "Data/answers_meta_final.csv")

# Just for fun analysis ----
# answers_with_user <- answers_metadata%>%
#   select(!c(`User Notes`, Title, Journal, Authors, Status, `User Count`, `Article URL`))
# answers_with_user <- answers_with_user%>%
#   drop_na(title)
# dupl_selection <- duplicate_cleaned[, c(6, 29)]
# dupl_selection$`Article ID` <- as.character(dupl_selection$`Article ID`)
# answers_with_users_dupl_marked <- right_join(x = answers_with_user, y = dupl_selection, by = "Article ID")
# answers_with_user[4018, ]$`Article ID`
# cookie <- dupl_selection%>%
#   filter(`Article ID` == "14883303")
# View(cookie)
# which(dupl_selection$`Article ID` =="14883303")
# dupl_selection <- dupl_selection[-964, ]
# answers_with_users_dupl_marked <- right_join(x = answers_with_user, y = dupl_selection, by = "Article ID")
# answers_with_user[4193, ]$`Article ID`
# cookie <- dupl_selection%>%
#   filter(`Article ID` == "14884624")
# View(cookie)
# which(dupl_selection$`Article ID` =="14884624")
# dupl_selection <- dupl_selection[-219, ]
# answers_with_users_dupl_marked <- right_join(x = answers_with_user, y = dupl_selection, by = "Article ID")
# answers_with_user[4205, ]$`Article ID`
# cookie <- dupl_selection%>%
#   filter(`Article ID` == "14886278")
# View(cookie)
# which(dupl_selection$`Article ID` =="14886278")
# dupl_selection <- dupl_selection[-275, ]
# answers_with_users_dupl_marked <- right_join(x = answers_with_user, y = dupl_selection, by = "Article ID")

shark <- answers_with_users_dupl_marked%>%
  drop_na(Comment)%>%
  select(title, Users, Comment)
glimpse(shark)
View(shark)
unique(shark$Users)

