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
elephant <- vector(mode = "list", length = nrow(paper_db_data))

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


# li_jan <- readLines('https://www.nature.com/articles/s41558-019-0601-y') # insert URL here
# li <- readLines(scopus_data$Link[1])
# li <- readLines("https://www.scopus.com/inward/record.uri?eid=2-s2.0-33644617084&partnerID=40&md5=037305236324e22a9e8539a8f79069d7")
# doi <- 0
# for(i in 1:length(li)){
#   if(sum(grep('doi', li[i])) == 1 |
#      sum(grep('DOI', li[i])) == 1){ # could add other options here - "doi" and "DOI" appear the most obvious to me
#     subs <- strsplit(li[i], '\"') # this could be critical - the different websites may vary here
#     for(j in 1:length(subs[[1]])){
#       if(startsWith(subs[[1]][j], "10.") == T){
#         print(subs[[1]][j])
#         doi <- subs[[1]][j]
#       }
#     }
#     break
#   }
# }
# 
# subs
# doi
# 
# 
# li <- readLines('https://www.scopus.com/inward/record.uri?eid=2-s2.0-0032314286&partnerID=40&md5=ea57fc027bcf5d67989f81bf298ab4e9') # insert URL here
# li <- readLines(scopus_data$Link[92])
# li <- readLines(scopus_data$Link[1])
# for(i in 1:length(li)){
#   if(sum(grep('sc-page-title', li[i])) == 1){ # could add other options here - "doi" and "DOI" appear the most obvious to me
#     
#     subs <- strsplit(li[i], '\"') # this could be critical - the different websites may vary here
#     
#     for(j in 1:length(subs[[1]])){
#       if(startsWith(subs[[1]][j], 'Document details - ') == T){
#         if(substr(subs[[1]][j], nchar(subs[[1]][j]), nchar(subs[[1]][j])) == " "){
#           print(paste0(substr(subs[[1]][j], 20, 999), substr(subs[[1]][j+1], 1, 999)))
#         } else {
#           print(substr(subs[[1]][j], 20, 999))
#         }
#       }
#     }
#     
#     break
#   }
# }
# 
# for(i in 1:length(li)){
#   if(sum(grep('/section><input ', li[i])) == 1){ # could add other options here - "doi" and "DOI" appear the most obvious to me
#     
#     subs <- strsplit(li[i], '\"') # this could be critical - the different websites may vary here
#     print(subs[[1]][36])
#     # for(j in 1:length(subs[[1]])){
#     #   if(startsWith(subs[[1]][j], 'Document details - ') == T){
#     #     print(substr(subs[[1]][j], 20, 999))
#     #   }
#     # }
#     
#     break
#   }
# }


#####
# year <- 0
# for(i in 1:length(li)){
#   if(sum(grep('year=', li[i])) == 1){ # could add other options here - "doi" and "DOI" appear the most obvious to me
#     
#     subs <- strsplit(li[i], ';') # this could be critical - the different websites may vary here
#     
#     for(j in 1:length(subs[[1]])){
#       if(startsWith(subs[[1]][j], 'year=') == T){
#         print(substr(subs[[1]][j], 6, 9))
#         #year <- substr(subs[[1]][j], 6, 9)
#       }
#     }
#     
#     break
#   }
# }



# Testing how to get metadata information ----
# compare if sysrev title and metadata title match or not

str_detect(string = paper_db$Title...11, pattern = paper_db$Title...4)

all(paper_db$Title...11 == paper_db$Title...4)
identical(paper_db$Title...4,paper_db$Title...11)

cookie <- ifelse(paper_db$Title...4 == paper_db$Title...11,1,0)

library(readxl)
writ

install.packages("rcrossref")
library(rcrossref)

rcrossref::cr_citation(doi = "10.1177/0309133319879324")
rcrossref::cr_cn(doi = "10.1177/0309133319879324", format = "text")
rcrossref::cr_cn(url = "https://www.scopus.com/inward/record.uri?eid=2-s2.0-85074715219&doi=10.1016%2fj.ecss.2019.106440&partnerID=40&md5=c24c2af05df2b06f16687751c1c576da")


cookie <- rcrossref::cr_cn(doi = paper_db$Doi[1:4], format = "bibentry")

df <- data.table::rbindlist(cookie, fill = TRUE)
write.csv(df, file = "test.csv")



write.csv(cookie, file = "test.csv")
rcrossref::cr_cn(doi = "10.1016%2fj.scitotenv.2019.133715", format = "text")

elephant <- rcrossref::cr_cn(dois = paper_db$Doi)



rcrossref::cr_cn(doi = "10.1177/0309133319879324", format = "text", raw = TRUE)
typeof(cookie)

install.packages("bibtex")
library(bibtex)

write.csv(cookie, file = "test.csv")

