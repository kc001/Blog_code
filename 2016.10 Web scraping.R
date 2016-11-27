#####################################
# 1. Obtain index of search results #
#####################################

# Use the Application Programming Inferface (API) for indeed.com
# For "publisher=" enter your unique publisher ID number in the query below
library(RCurl)
search.1 <- getURL("http://api.indeed.com/ads/apisearch?publisher=########&format=xml&q=data+scientist&l=&sort=relevance&radius=25&st=%20employer&jt=fulltime&start=0&limit=25&fromage=31&filter=1&latlong=0&co=us&chnl=&userip=1.2.3.4&useragent=Mozilla/%2F4.0%28Firefox%29&v=2",
                   .opts=curlOptions(followlocation = TRUE))

search.2 <- getURL("http://api.indeed.com/ads/apisearch?publisher=########&format=xml&q=data+scientist&l=&sort=relevance&radius=25&st=%20employer&jt=fulltime&start=26&limit=25&fromage=31&filter=1&latlong=0&co=us&chnl=&userip=1.2.3.4&useragent=Mozilla/%2F4.0%28Firefox%29&v=2",
                   .opts=curlOptions(followlocation = TRUE))

search.3 <- getURL("http://api.indeed.com/ads/apisearch?publisher=########&format=xml&q=data+scientist&l=&sort=relevance&radius=25&st=%20employer&jt=fulltime&start=51&limit=25&fromage=31&filter=1&latlong=0&co=us&chnl=&userip=1.2.3.4&useragent=Mozilla/%2F4.0%28Firefox%29&v=2",
                   .opts=curlOptions(followlocation = TRUE))

search.4 <- getURL("http://api.indeed.com/ads/apisearch?publisher=########&format=xml&q=data+scientist&l=&sort=relevance&radius=25&st=%20employer&jt=fulltime&start=76&limit=25&fromage=31&filter=1&latlong=0&co=us&chnl=&userip=1.2.3.4&useragent=Mozilla/%2F4.0%28Firefox%29&v=2",
                   .opts=curlOptions(followlocation = TRUE))

library(XML)
xml.1 <- xmlParse(search.1)
xml.2 <- xmlParse(search.2)
xml.3 <- xmlParse(search.3)
xml.4 <- xmlParse(search.4)
print(xml.4)


################################################################
# Obtain url for each discrete job posting from search results #
################################################################

# use xpathApply function to extract each url
# To return a vector instead of a list, unlist the output
urls.1 <- unlist(xpathApply(xml.1, path="//response/results/result/url", fun=xmlValue))
urls.2 <- unlist(xpathApply(xml.2, path="//response/results/result/url", fun=xmlValue))
urls.3 <- unlist(xpathApply(xml.3, path="//response/results/result/url", fun=xmlValue))
urls.4 <- unlist(xpathApply(xml.4, path="//response/results/result/url", fun=xmlValue))

urls <- c(urls.1, urls.2, urls.3, urls.4)
remove(urls.1, urls.2, urls.3, urls.4)



#################################################
# 2. Use urls to obtain actual job descriptions #
#################################################

jobs <- vector()

for (i in 1:length(urls)) { # for each job
    
    print(i)
    
    url <- urls[i]
    
    raw.html.job <- getURL(url, .opts=curlOptions(followlocation = TRUE))
    
    parsed.html.job <- htmlTreeParse(raw.html.job, useInternal=TRUE)
    
    job.desc <- unlist(xpathApply(parsed.html.job, path="//span[@id='job_summary']", fun=xmlValue))
    
    jobs[i] <- job.desc
    remove(url, raw.html.job, parsed.html.job, job.desc)
    
}

remove(i, urls)

