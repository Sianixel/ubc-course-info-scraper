install.packages("tidyverse")
install.packages("rvest")

library(tidyverse)
library(rvest)
#library(RSelenium)
#library(stringr)

root_dir <- "https://courses.students.ubc.ca/"
root_url <- "https://courses.students.ubc.ca/cs/courseschedule?tname=subj-all-departments&sessyr=2020&sesscd=W&pname=subjarea"


get_root_urls <- function(root){
    course_code_urls <- read_html(root) %>%
    html_nodes("#mainTable a") %>%
    c()
    #paste(root_dir, sep = "", collapse = NULL)
    return(course_code_urls)
}

get_root_urls(root_url)