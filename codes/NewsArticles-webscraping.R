rm(list=ls())
library("rvest")
library(tm)

text.clean = function(x)                    # text data
{ 
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
# x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}


links <- read.csv("F:/BIG DATA/ISB/Assignments/Term 1/Practicum-2/links2.csv")

links_week <- subset(links, links$ID == 'theweek')
links_economictimes <- subset(links, links$ID == 'economictimes')
links_indianexpress <- subset(links, links$ID == 'indianexpress')
links_digit <- subset(links, links$ID == 'digit')
links_hindu <- subset(links, links$ID == 'hindu')
links_economicindiatimes <- subset(links, links$ID == 'economicindiatimes')
links_businessstandard <- subset(links, links$ID == 'businessstandard')
links_businessstandard2 <- subset(links, links$ID == 'businessstandard2')

corpus <- NULL

scrape_website <- function(urls, htmltag){
  
  weblinks <- urls
  text.all  <-  NULL
  
  for (url in weblinks) {
    
    url  <-  url
    page  <-  read_html(url)
    
    text  <-  html_text(html_nodes(page, htmltag))
    text.all <- c(text.all,text)
  }
  
  return(text.all)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

week_urls <- links_week$link
week_urls <- trim(week_urls)
week_text <- scrape_website(week_urls, '.article')
corpus <- c(corpus,week_text)

economictimes_urls <- links_economictimes$link
economictimes_urls <- trim(economictimes_urls)
economictimes_text <- scrape_website(economictimes_urls, '.Normal')
corpus <- c(corpus,economictimes_text)

indianexpress_urls <- links_indianexpress$link
indianexpress_urls <- trim(indianexpress_urls)
indianexpress_text <- scrape_website(indianexpress_urls, 'p')
corpus <- c(corpus,indianexpress_text)

digit_urls <- links_digit$link
digit_urls <- trim(digit_urls)
digit_text <- scrape_website(digit_urls, 'p')
corpus <- c(corpus,digit_text)

hindu_urls <- links_hindu$link
hindu_urls <- trim(hindu_urls)
hindu_text <- scrape_website(hindu_urls, '.sports-article p')
corpus <- c(corpus,hindu_text)

economicindiatimes_urls <- links_economicindiatimes$link
economicindiatimes_urls <- trim(economicindiatimes_urls)
economicindiatimes_text <- scrape_website(economicindiatimes_urls, '.Normal')
corpus <- c(corpus,economicindiatimes_text)

businessstandard_urls <- links_businessstandard$link
businessstandard_urls <- trim(businessstandard_urls)
businessstandard_text <- scrape_website(businessstandard_urls, '.p-content')
corpus <- c(corpus,businessstandard_text)

businessstandard2_urls <- links_businessstandard2$link
businessstandard2_urls <- trim(businessstandard2_urls)
businessstandard2_text <- scrape_website(businessstandard2_urls, '.bB1 div')
corpus <- c(corpus,businessstandard2_text)

corpus = gsub("\n",' ',corpus)
corpus <- corpus[!grepl(" © The Hindu ",corpus)]
corpus <- corpus[!grepl("Printable version",corpus)]
corpus <- corpus[!grepl("jQuery",corpus)]
corpus <- corpus[!grepl("page_article_detailsonam3232",corpus)]
corpus <- corpus[!grepl("googletag",corpus)]
corpus <- corpus[!grepl("gads",corpus)]
corpus <- corpus[!grepl("var",corpus)]
corpus <- corpus[!grepl("googletagservices",corpus)]
corpus <- corpus[!grepl("document.write",corpus)]
corpus <- corpus[sapply(corpus, nchar) > 40]
corpus  = text.clean(corpus)  

data1 <- data.frame(corpus)
data2 <- read.csv("F:/BIG DATA/ISB/Assignments/Term 1/Practicum-2/newsarticlescorpusmiscellaneous.csv")
data <- rbind(data1,data2)

write.csv(data,'F:/BIG DATA/ISB/Assignments/Term 1/Practicum-2/newsarticlescorpus.csv', row.names = F)


