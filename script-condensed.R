library(tidyverse)
library(rvest)
library(RSelenium)
library(stringr)
library(htmltab)
library(dplyr)
library(htmltab)

# CONSTANTS

root <- "https://courses.students.ubc.ca"
root_courses <- "https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-all-departments"
test_section <- "https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-course&dept=BALA&course=580A"


# FUNCTIONS

get.urls.root = function(url){
  
  #Gathering URLs from root page
  root_url <- download.file(url, destfile = "root_page.html", quiet = TRUE)
  root_source <- read_html("root_page.html")
  
  root_urls <- html_nodes(read_html("root_page.html"), "#mainTable a") %>%
    html_attrs() %>%
    c()
  
  print(root_urls)
  
  full_course_code_urls <- paste(root, root_urls, sep = "")
  
  full_course_code_urls
  
  return(full_course_code_urls)
  
}

#Get instructor for course section
get.instructor = function(section_url){
  
  
  #Getting individual instructor
  course_section_instructor <- read_html("current_section.html") %>%
    html_nodes("table") %>%
    .[3] %>%
    html_table(fill=TRUE) %>%
    .[[1]] %>%
    select(X2) %>%
    unlist() %>%
    .[[1]]
  
  return(course_instructor)
}

#Get urls for each section in each course
get.section.urls = function(course_url){
  
  current_course <- download.file(course_url, destfile = "current_course.html")
  
  #Getting links for each course section
  section_urls <- read_html("current_course.html") %>%
    html_nodes("td > a") %>%
    html_attrs() %>%
    c()
  full_section_urls <- paste(root_add, section_urls, sep = "", collapse = NULL)
  
  return(full_section_urls)
  
}

#Get information for each section in each course
get.section.info = function(course_url, full_section_urls){
  
  current_course <- download.file(course_url, destfile = "current_course.html")
  
  #Getting name, activity, term, for each section
  course_section_table <- read_html("current_section.html") %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(header = NA, trim = TRUE, fill = FALSE)
  
  course_section_table <- course_section_table[[1]]
  
  course_nat <- select(course_section_table, Section, Activity, Term)
  
  #Creating vector of instructors in order appearing on previous page
  course_instructors <- c()

  for(i in 1:length(full_section_urls)){
    
    get.instructor(full_section_urls)
    
    course_instructors <- append(course_instructors, course_section_instructor)
  }
  
  #Adding instructor for each course section to table
  course_nat$Instructor = course_instructors
  print(course_nat)
  
  return(course_nat)
}

#Get url for each course page
get.urls.course = function(course_code_url){
  
  #Finding urls for each course
  current_course_code <- download.file(course_code_url, destfile = "current_course_code.html")
  current_course_code_source <- read_html("current_course_code.html")
  
  course_urls <- html_nodes(current_course_code_source, "#mainTable a") %>%
    html_attrs() %>%
    c()
  
  full_course_urls <- paste(root_add, course_urls, sep = "", collapse = NULL)
  
  return(full_course_urls)
  
}

main <- function(root_courses){
  root_urls <- get.urls.root(root_courses)
  course_urls <- get.urls.course(root_urls)
  section_urls <- get.section.urls(course_urls)
  course_info <- get.section.info(course_urls)
}
  


