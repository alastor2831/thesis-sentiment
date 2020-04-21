
# import packages ---------------------------------------------------------
library(tidyverse)
library(pdftools)
library(tidytext)
library(ggplot2)
library(rvest)
library(httr)
library(pipeR) #(see https://renkun-ken.github.io/pipeR-tutorial/Pipe-operator/Pipe-for-side-effect.html)
#library("svMisc") (for progress bar during reading files maybe..)


# url query vectors -------------------------------------------------------

indbio_url <- "https://odr.chalmers.se/simple-search?location=%2F&query=&rpp=50&sort_by=dc.date.issued_dt&order=desc&filter_field_1=has_content_in_original_bundle&filter_type_1=equals&filter_value_1=true&filter_field_2=title&filter_type_2=notcontains&filter_value_2=och&filter_field_3=title&filter_type_3=notcontains&filter_value_3=hos&filter_field_4=title&filter_type_4=notcontains&filter_value_4=f%C3%B6r&filter_field_5=title&filter_type_5=notcontains&filter_value_5=av&filter_field_6=subject&filter_type_6=equals&filter_value_6=Industrial+Biotechnology"
sysbio_url <- "https://odr.chalmers.se/simple-search?location=%2F&query=&rpp=50&sort_by=dc.date.issued_dt&order=desc&filter_field_1=has_content_in_original_bundle&filter_type_1=equals&filter_value_1=true&filter_field_2=title&filter_type_2=notcontains&filter_value_2=och&filter_field_3=title&filter_type_3=notcontains&filter_value_3=hos&filter_field_4=title&filter_type_4=notcontains&filter_value_4=f%C3%B6r&filter_field_5=title&filter_type_5=notcontains&filter_value_5=av&filter_field_6=subject&filter_type_6=equals&filter_value_6=Bioinformatics+and+Systems+Biology"
food_url <- "https://odr.chalmers.se/simple-search?location=%2F&query=&rpp=60&sort_by=dc.date.issued_dt&order=desc&filter_field_1=has_content_in_original_bundle&filter_type_1=equals&filter_value_1=true&filter_field_2=title&filter_type_2=notcontains&filter_value_2=och&filter_field_3=title&filter_type_3=notcontains&filter_value_3=hos&filter_field_4=title&filter_type_4=notcontains&filter_value_4=f%C3%B6r&filter_field_5=title&filter_type_5=notcontains&filter_value_5=av&filter_field_6=subject&filter_type_6=equals&filter_value_6=Food+Science"
kth_to_17_url <-   "http://kth.diva-portal.org/smash/resultList.jsf?query=&language=en&searchType=UNDERGRADUATE&noOfRows=50&sortOrder=relevance_sort_desc&sortOrder2=title_sort_asc&onlyFullText=false&sf=all&aq=%5B%5B%5D%5D&aqe=%5B%5D&aq2=%5B%5B%7B%22dateIssued%22%3A%7B%22from%22%3A%222015%22%2C%22to%22%3A%222020%22%7D%7D%2C%7B%22organisationId%22%3A%225903%22%2C%22organisationId-Xtra%22%3Atrue%7D%5D%5D&af=%5B%22hasFulltext%3Atrue%22%2C%22language%3Aeng%22%5D"
kth_from_18_url <- "http://kth.diva-portal.org/smash/resultList.jsf?query=&language=en&searchType=UNDERGRADUATE&noOfRows=50&sortOrder=relevance_sort_desc&sortOrder2=title_sort_asc&onlyFullText=false&sf=all&aq=%5B%5B%5D%5D&aqe=%5B%5D&aq2=%5B%5B%7B%22organisationId%22%3A%22879224%22%2C%22organisationId-Xtra%22%3Atrue%7D%5D%5D&af=%5B%22personOrgId%3A879224%22%2C%22hasFulltext%3Atrue%22%2C%22language%3Aeng%22%2C%22categoryId%3A11500%22%2C%22categoryId%3A11528%22%5D"
su_url <- "https://su.diva-portal.org/smash/resultList.jsf?dswid=61&af=%5B%22language%3Aeng%22%2C%22hasFulltext%3Atrue%22%2C%22thesisLevel%3AH2%22%5D&p=1&fs=true&language=en&searchType=UNDERGRADUATE&query=&aq=%5B%5B%5D%5D&aq2=%5B%5B%7B%22dateIssued%22%3A%7B%22from%22%3A%222015%22%2C%22to%22%3A%222020%22%7D%7D%2C%7B%22organisationId%22%3A%22535%22%2C%22organisationId-Xtra%22%3Atrue%7D%5D%5D&aqe=%5B%5D&noOfRows=50&sortOrder=relevance_sort_desc&sortOrder2=title_sort_asc&onlyFullText=false&sf=all"


# custom function definitions ---------------------------------------------

get_chalmersodr_thesis <- function(url){
  #stopifnot(is.character(url) == TRUE)
  url %>% 
    read_html() %>%
    {
      tibble(year = html_nodes(., xpath = '//td[@headers="t1"]') %>% html_text(),
            title = html_nodes(., xpath = '//td[@headers="t2"]') %>% html_text(),
            author = html_nodes(., xpath = '//td[@headers="t3"]') %>% html_text(),
            # xpath expression checkes for the string "handle" in all href attributes and returns them 
            id = html_nodes(., xpath = '//a[contains(@href,"handle")]/@href') %>%
                  html_text() %>% 
                  str_sub(-6),
            group = str_sub(url, -22)
            )
    } %>% 
    mutate(link = map_chr(id, function(id) {str_c("https://odr.chalmers.se/bitstream/20.500.12380/",
                                  id, "/1/", id, ".pdf", collapse = "")}
                          ),
           group = case_when(
             str_detect(group, "Food") ~ "Food",
             str_detect(group, "strial") ~ "IndBio",
             str_detect(group, "Systems") ~ "SysBio")
           )
}
get_diva_thesis <- function(url){
  stopifnot(is.character(url) == TRUE)
  url %>% 
    read_html() %>%
    {
      tibble(
        title = html_nodes(., xpath = '//a[@class="titleLink singleRow linkcolor"]') %>% html_text(),
        year = html_nodes(., xpath = '//a[@class="titleLink singleRow linkcolor"]//following-sibling::span[position() < 2]') %>% html_text(),
        author= html_nodes(., xpath = '//div[@class="ui-button ui-widget ui-state-default ui-corner-all ui-button-text-icon-left toggleOrganisation"]') %>% html_text(),
        link = html_nodes(., xpath = '//a[contains(@href,"get/diva2")]/@href') %>% html_text()
      )
    } %>% 
    mutate(link = map_chr(link, function(link) {
                                  ifelse(test = str_detect(url, "kth"),
                                         yes = str_c("http://kth.diva-portal.org/smash/", link, collapse = ""),
                                         no = str_c("http://su.diva-portal.org/smash/", link, collapse = ""))
                                  }
                  ),
           id = str_extract(link, "\\d{6,7}"),
           group = ifelse(str_detect(url, "kth"), "KTH", "SU")
    )
}
download_pdf <- function(link, id, group){
  dest_folder <- str_c(getwd(), "/", group, "/", collapse = "")
  if(!dir.exists(dest_folder))
    dir.create(dest_folder)
  
  dest_file <- str_c(dest_folder, id, ".pdf", collapse = "")
  if(!file.exists(dest_file)) {
    sleep <- sample(2:8, 1)
    print(paste("Pausing for ", sleep, "seconds"))
    Sys.sleep(sleep)
    download.file(link, destfile = dest_file)
  }
  else{
    print("File already exists. . . ")
    #Sys.sleep(2)
  }
}
get_acknowledgement <- function(id, group){
  file_path <- str_c("./", group, "/", id, ".pdf")
  if(file.exists(file_path)){
    # read the entire pdf and store it in a temp variable as a character
    # lenght(raw) will be equal to the number of pages: each page is an element
    print(str_c("Reading file ", file_path))
    temp_page <- pdftools::pdf_text(file_path)
    # check if there is an acknowledgement section
    if(sum(str_count(temp_page, "Acknowledgement")) > 0 |
       sum(str_count(temp_page, "ACKNOWLEDGEMENT")) > 0 |
       sum(str_count(temp_page, "Foreword") > 0) |
       sum(str_count(temp_page, "FOREWORD") > 0)) {
      #this is to avoid getting the ToC instead of the Ack section in case it is before the ToC
      #first get only pages with the Ack word in it, then the page with more numbers in it will be the
      #ToC so we save the index of the one with less numbers using which.min
      ack_index <- str_subset(temp_page, c("Acknowledgement|Foreword|ACKNOWLEDGEMENT|FOREWORD")) %>>%
        str_count("[[:digit:]]") %>>%
        (~print(paste0("Matches found: ", length(.)))) %>>%
        which.min()
      
      temp_page %>%
        str_subset(c("Acknowledgement|Foreword|ACKNOWLEDGEMENT|FOREWORD")) %>%
        `[`(ack_index) %>%
        strsplit("\\n") %>>%
        unlist %>>%
        #(? glue::glue('File {pdf} was read succesfully')) %>>% # %>>% glimpse(.))
        tibble(line = seq_along(.),
               id = id,
               group = group) %>>%
        {invisible(.) %>% glimpse(.)}
    } else {
      print("No Acknowledgment section")
      tibble(. = c("NO-SECTION"),
             line = 0,
             id = id,
             group = group)
    }
  }
  else{
    print(str_c("File", file_path, " not found"))
    tibble(. = c("NO-FILE"),
           line = 0,
           id = id,
           group = group)
  }
}


# 1- getting the data -----------------------------------------------------

# Let's start by gathering some (meta)data on the thesis from their online repositories. 
# Chalmers first
chalmers_thesis <- map_dfr(c(indbio_url, sysbio_url, food_url) ,
                     .f = get_chalmersodr_thesis) 

# Then KTH
kth_thesis <- map_dfr(c(kth_to_17_url, kth_from_18_url), .f = get_diva_thesis)

# And last, SU
su_thesis <- get_diva_thesis(su_url)

# Since they all have the same columns we can can stack them all toghether in one tibble
all_thesis <- bind_rows(chalmers_thesis, kth_thesis, su_thesis)

all_thesis %>% 
  count(group, sort = TRUE) %>% 
  mutate(group = reorder(group, n)) %>% 
  ggplot(aes(group, n)) +
  geom_col() +
  coord_flip()

all_thesis$link[1] %>% 
  group_by(group) %>% #filter(group == "KTH") %>% 
  count(year)

# let's download the pdfs
# To run a function on each row of a data frame we can simply use pmap which takes
# a list as input and data frames in R are actually lists. This next chunk of code
# calls download_pdf function on each row of the thesis data frame

all_thesis %>%
  #filter(group == "SysBio" | group == "IndBio" | group == "Food") %>% # edit here to download one group at the time
  filter(group == "SU") %>% 
  select(link, id, group) %>% 
  pmap(.f = download_pdf) %>% 
  invisible() # otherwise it would print an empty list for each file

# Now we can want to extract the Acknowledgement page (if present) from each pdf to run the
# sentiment analysis. With pdftools package we read the thesis into R as character vector,
# one page for element. In get_acknowledgement function the paragraph is splitted by lines and
# we return each section in its own tibble, preserving the id, linenumber and group, with
# one row per line of text

# Let's read them in several steps and then stack the tibbles together. First Chalmers

chalmers_acknowledgements <- all_thesis %>% 
  filter(group == "SysBio" | group == "IndBio" | group == "Food") %>% # edit here to read one group at the time
  select(id, group) %>% 
  pmap(.f = get_acknowledgement) %>% 
  bind_rows() %>% 
  `colnames<-`(c("text", "linenumber", "id", "group"))

# Then KTH and SU
kth_acknowledgements <- all_thesis %>% 
  filter(group == "KTH") %>% # edit here to read one group at the time %>% 
  filter(id != "1147632") %>% # this thesis is written in 2 columns layout and we ignore it
  filter(id != "1295394") %>% # this one is too big to read so we ignore it too
  select(id, group) %>% 
  pmap(.f = get_acknowledgement) %>% 
  bind_rows() %>% 
  `colnames<-`(c("text", "linenumber", "id", "group"))

su_acknowledgements <- all_thesis %>% 
  filter(group == "SU") %>% # edit here to read one group at the time %>% 
  select(id, group) %>% 
  pmap(.f = get_acknowledgement) %>% 
  bind_rows() %>% 
  `colnames<-`(c("text", "linenumber", "id", "group"))


all_acknowledgements <- bind_rows(chalmers_acknowledgements,
                                  kth_acknowledgements,
                                  su_acknowledgements) %>%
  mutate(university = ifelse(group %in% c("SysBio", "IndBio","Food"),
                                          "Chalmers", group)) 
  
# all_acknowledgements %>% 
#   #select(id, group) %>% 
#   group_by(group) %>% 
#   distinct(id) %>% 
#   count(group)

with_ack <- all_acknowledgements %>% 
  filter(text != "NO-SECTION") %>% 
  group_by(group, university) %>%
  distinct(id) %>% 
  count(group, name = "with_ack")
               
ack_proportions <- all_thesis %>%
  count(group, name = "total", sort = TRUE) %>% 
  inner_join(with_ack) %>%
  mutate(`%` = with_ack/total * 100)

ack_proportions %>%
  select(-c(`%`, university)) %>% 
  melt(id.vars = c("group")) %>% 
  ggplot(mapping = aes(x = group, y = value, fill = variable)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = c("KTH", "IndBio", "SU", "SysBio", "Food")) +
  labs(title = "Proportion of thesis with(out) acnknowledgement section",
       y = "n of thesis", x = "University/Group") +
  scale_fill_discrete(name=" ",
                      labels = c("without", "with")) +
  theme_dark()


all_acknowledgements %>% 



all_acknowledgements %>%
  unnest_tokens(word, text, token = "words") %>% 
  anti_join(stop_words)
  

kth_ack %>%
  bind_rows(indbio_ack) %>% 
  filter(text != "NO-SECTION") %>%
  group_by(id, group) %>% 
  count(id) %>% 
  nrow
  count(text, group)


indbio_tidy_ack <- indbio_ack %>% 
  filter(text == "NO-SECTION") %>%
  count(text, group)

    count(id)
  mutate(present = ifelse(text == c("NO-SECTION"), FALSE, TRUE)) %>% 
  group_by(group) %>% 
  summarize(total = sum(present))
  

# read_thesis <- function(group, id){
#   file_path <- str_c("./", group, "/", id, ".pdf")
#   print(file_path)
#   if(file.exists(file_path)){
#     raw_file <- pdftools::pdf_text(file_path)
#     print(str_c("File ", id, " was read"))
# 
#     raw_splitted <- raw_file %>% str_split("\\n")
# 
#     map_dfr(.x = 1:length(raw_file), .f = function(x) {tibble(text = raw_splitted[[x]],
#                                                 pagenumber = x,
#                                                 linenumber = 1:length(raw_splitted[[x]]))}
#     )
    


    return(tibble(
      id = id,
      group = group,
      pagenumber = 1:length(temp),
      text = temp))
  }
  else{
    print(str_c("File ", id, "not found!"))
    return(tibble("NO-FILE"))
  }    
}

thesis_text <- all_thesis_twenty %>% 
  select(group, id) %>% 
  pmap(.f = read_pdfs) %>% 
  invisible()
  
numbers <- as.character(1:10)

more_stop_words <- stop_words %>% 
  bind_rows(tibble(word = as.character(1:1000),
                   lexicon = "custom"))


tidy_thesis <- bind_rows(thesis_text) %>% 
  unnest_tokens(word, text, token = "words") %>% 
  anti_join(more_stop_words, by = "word") %>% 
  filter(word != 0 | "figure")


tidy_thesis %>% 
  group_by(group) %>% 
  count(word, sort = TRUE) %>% 
  head(15)
    
  


pdf_files_list <- list.files("./Sysbio", pattern = "[[:digit:]]{6}", full.names = TRUE, recursive = TRUE)


  
  tribble(
    ~colA, ~colB, ~colC,
    "hi", 453, "Rock",
    "my", 562, "Rock",
    "home", 675, "Indie",
    "tho", 544, "Rock",
    "asd", 092, "Punk"
  )
  
  




get_tidy_acknowledgments <- function(ack){
  
}

        #unnest_tokens(word, text) %>% 
        #anti_join(less_stop_words, by = "word")
#### files with ack section before the ToC
# Food 163059 ~~~ edited manually - switched pages
# Sysb 166094 ~~~ edited deleted ToC page


# raw[max(str_which(raw, c("Acknowledgement(?![[:punct:]])")))]




acknowledgments_sysbio <- map(.x = pdf_files_list , .f = get_acknowledgments)
 

  all_acknowledgments_sysbio <- acknowledgments_sysbio %>% 
    bind_rows() %>% 
    select(-text) 
    
  colnames(all_acknowledgments_sysbio)<- c("text", "line", "id", "group")

tidy_acknowledgements <- all_acknowledgments_sysbio %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% 
    count(word, sort = T)
    
tidy_acknowledgements %>% 
  inner_join(nrc_joy) %>% 
  count(word, sort = T)


  less_stop_words <- stop_words %>%
    filter(lexicon == "snowball") %>% 
    tail(-29)
  
  

  
get_acknowledgments(pdf_files_list[1])

tidy_thesis_indbio %>% 
  bind_rows()
  count()

rawt <- pdf_text("./IndBio/137085.pdf")


acknowledgments_sysbio %>%
  bind_rows() %>% 
  unnest_ptb(word, text)

# let's see the proportion of thesis with an acknowledgment section

thesis_stats <- tibble("total_thesis" = nrow(all_thesis_df),
                       "with_acknowledgments" = bind_rows(tidy_thesis_indbio) %>%
                         filter(is.na(id) == TRUE) %>%
                         nrow)
  
  
    count(group, word, sort = T)

if(nrow(ack) > 1)
  

tibble()

bind_rows(tidy, tidy2)

  

t1 %>% 
  count(word, sort = T) %>% 
  filter(n > 2) %>% 
  ggplot(aes(word, n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip()
  
  mutate(word = reorder(word, n)) 



pronoums <-stop_words %>% 
  filter(lexicon == "snowball") %>% 
  select(word) %>% 
  head(29) %>% 
  as.character()
  
  c("I", "mine", "me", "you", "yours", "your", "he", "his", "him", "she", "her", "hers", "we", "our", "ours", "they", "theirs", "them")  
  


raw2 %>% 
  count(word, sort = T)



  