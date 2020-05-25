library(tidyverse)
library(rvest)
library(RSelenium)
library(stringr)
library(htmltab)
library(dplyr)
library(htmltab)
library(XML)


#Gathering URLs from root page
root_url <- download.file("https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-all-departments",
                          destfile = "root_page.html", quiet = TRUE)
root_add <- "https://courses.students.ubc.ca"
root_source <- read_html("root_page.html")

root_urls <- html_nodes(root_source, "#mainTable a") %>%
  html_attrs() %>%
  c()
full_course_code_urls <- paste(root_add, root_urls, sep = "", collapse = NULL)
#url_df <- data.frame(matrix(unlist(full_urls), nrow=length(full_urls), byrow=T))
full_course_code_urls

#Gathering course urls from each course code page
num_course_codes <- length(full_course_code_urls)

# Traversing courses
for (k in 1:num_course_codes){
  
  #Finding urls for each course
  current_course_code <- download.file(full_course_code_urls[k], destfile = "current_course_code.html")
  
  current_course_code_source <- read_html("current_course_code.html")
  course_urls <- html_nodes(current_course_code_source, "#mainTable a") %>%
    html_attrs() %>%
    c()
  full_course_urls <- paste(root_add, course_urls, sep = "", collapse = NULL)
  full_course_urls
  
  #Getting urls for each course section
  for (j in 1: length(full_course_urls)){
    
    current_course <- download.file(full_course_urls[j], destfile = "current_course.html")
    
    #Getting links for each course section
    section_urls <- read_html("current_course.html") %>%
      html_nodes("td > a") %>%
      html_attrs() %>%
      c()
    full_section_urls <- paste(root_add, section_urls, sep = "", collapse = NULL)
    
    #Getting name, activity, term, for each section
    course_section_table <- read_html("current_section.html") %>%
      html_nodes("table") %>%
      .[2] %>%
      html_table(header = NA, trim = TRUE, fill = FALSE)
    
    course_section_table <- course_section_table[[1]]
    
    course_name_activity_term <- select(course_section_table, Section, Activity, Term)
    #course_name_activity_term
    
    #Creating empty vector to store course instructors in order appearing on table
    course_instructors <- c()
    
    #Building vector instructors for course sections
    for(i in 1:length(full_section_urls)){
      
      current_section <- download.file(full_section_urls[i], destfile = "current_section.html")
      
      #Getting individual instructor
      course_section_instructor <- read_html("current_section.html") %>%
        html_nodes("table") %>%
        .[3] %>%
        html_table(fill=TRUE) %>%
        .[[1]] %>%
        select(X2) %>%
        unlist() %>%
        .[[1]]
      
      course_instructors <- append(course_instructors, course_section_instructor)
    }
    #Adding urls and instructor for each course section to table
    course_name_activity_term$Instructor = course_instructors
    course_name_activity_term$URL = full_section_urls
    print(course_name_activity_term)
  }
}
