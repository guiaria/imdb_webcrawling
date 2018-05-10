# Web crawling 
# Collect 200 page of movie link in imdb year 2016
# Extract data into dataframe as follow
    #   • Title
    #   • Rating
    #   • Description • Country
    #   • Language
    #   • Genres
    #   • Runtime
  # Taniya Seesanong        57070503451
  # Prayuth Patumcharoenpol 57070503476
  # Passavich Limgirtnuwat  57070503477


result <- data.frame("Title"=character(), "Rating"=double(),"Description"=character(),"Country"=character(), Language=character(),Genres=character(), Runtime=character(), stringsAsFactors = FALSE)
string1 <- "http://www.imdb.com/search/title?release_date=2016&page=" 
page_num <- 1

while(page_num <= 200)
{
  url_dummy  <- paste(string1,toString(page_num),sep = "")
  print(url_dummy)
  dummy <- read_html(url_dummy)
  urllist <- dummy %>% html_nodes("h3.lister-item-header > a ") %>%  html_attr("href")
  urllist <- paste("http://www.imdb.com/",urllist,sep = "")
  urllist <- as.list(urllist)
  #url_list <- c(url_list,urllist)
  page_num <- page_num +1
  
  x <- 0
  for(x in seq(urllist)){
    y <- urllist[[x]]
    y # debug
    ready_url <- read_html(y)
    
    dummy_title <- ready_url %>% html_nodes('#title-overview-widget > div.vital > div.title_block > div > div.titleBar > div.title_wrapper > h1') %>%  html_text()
    dummy_rating <- ready_url %>% html_nodes("#title-overview-widget >div.plot_summary_wrapper > div.titleReviewBar >div:nth-child(1) > a > div") %>% html_text() %>% as.numeric()
    if(identical(dummy_rating,numeric(0))){
      dummy_rating <- NA
    }
    
    dummy_des <-  ready_url %>% html_nodes("div.plot_summary > div.summary_text") %>% html_text(trim = TRUE)
    if(identical(dummy_des,character(0))){
      dummy_des <- NA
    }
    else if(stri_detect_fixed(dummy_des,"Add a Plot")){
      dummy_des <- NA
    }
    
    dummy_country <-ready_url %>%  html_nodes("#titleDetails > div:contains('Country') > a") %>% html_text() %>% toString()
    if(identical(dummy_country,character(0))){
      dummy_country <- NA
    }
    dummy_lang <- ready_url %>% html_nodes("#titleDetails > div:contains('Language')>a ") %>% html_text() %>% toString()
    if(identical(dummy_lang,character(0))){
      dummy_lang <- NA
    }
    dummy_genre <-ready_url %>% html_nodes("#titleStoryLine > div:contains('Genres') > a") %>% html_text() %>% toString()
    if(identical(dummy_genre,character(0))){
      dummy_lang <- NA
    }
    dummy_runtime <- ready_url %>% html_nodes("#titleDetails > div:contains('Runtime')>time ") %>% html_text()
    if(identical(dummy_runtime,character(0))){
      dummy_runtime <- NA
    }
       
    result <- rbind(result,data.frame("Title"= dummy_title,"Rating"= dummy_rating,"Description" = dummy_des ,"Country" = dummy_country,"Language" = dummy_lang, "Genres" = dummy_genre, "Runtime" =dummy_runtime, stringsAsFactors = FALSE)) 
  }
   
}
write.csv(result,file = "result_test.csv")