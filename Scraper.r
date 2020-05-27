#PACKAGE INSTALLATION
    #install.packages("tidyverse")
    #install.packages("rvest")

#PACKAGE LOAD
    library(tidyverse)
    library(rvest)
    library(curl)

#CONSTANTS

    root_dir <- "https://courses.students.ubc.ca"
    root_url <- "https://courses.students.ubc.ca/cs/courseschedule?tname=subj-all-departments&sessyr=2020&sesscd=W&pname=subjarea"

    sample_course_code <- c("https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-department&dept=ADHE")
    sample_course <- c("https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-course&dept=CPSC&course=110")
    sample_section <- c("https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-section&dept=BA&course=508&section=818",   
    "https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-section&dept=BA&course=509&section=BA1",   
    "https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-section&dept=BA&course=512&section=001",   
    "https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-section&dept=BA&course=512&section=002",   
    "https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-section&dept=BA&course=512&section=003",
    "https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-section&dept=BA&course=512&section=004",   
    "https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-section&dept=BA&course=512&section=005",   
    "https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-section&dept=BA&course=512&section=006",   
    "https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-section&dept=BA&course=515&section=300")

#FACULTY LABELS:
    lfs <- "Faculty of Land and Food Systems"
    arts <- "Faculty of Arts"
    edu <- "Faculty of Education"
    med <- "Faculty of Medicine"
    aps <- "Faculty of Applied Science"
    arch <- "Faculty of Arch and Landscape Arch"
    inf <- "School of Information"
    sci <- "Faculty of Science"
    audi <- "School of Audiology and Speech Scie"
    saud <- "Faculty of Comm and Bus Admin"
    fors <- "Faculty of Forestry"
    biom <- "Faculty of Biomedical Engineering"
    dent <- "Faculty of Dentistry"
    econ <- "Vancouver School of Economics"
    regi <- "Regi"
    inds <- "Faculty of Grad and Pdoc Studies"
    jrnl <- "School of Journalism, Writing, Andmedia"
    kin <- "School of Kinesiology"
    law <- "Peter A. Allard School of Law"
    van <- "Vantage College"
    mus <- "School of Music"
    nurs <- "School of Nursing"
    phar <- "Faculty of Pharmaceutical Sciences"
    plan <- "School of Comm and Reg Planning"
    trsc <- "No Faculty (Blank for Cross Edits)"
    cont <- "Centre for Continuing Education"

#METHODS

make_faculty_filter <- function(faculty_name){

    root_data <- read_html(root_url) %>%
        html_table(header = NA, trim = TRUE, fill = FALSE)
    root_data <- root_data[[1]]
    colnames(root_data)=c("course_code", "department", "faculty")
    filtered_course_codes_tbl <- filter(root_data, faculty == faculty_name) %>%
                        separate(col = course_code,
                                into = c("code", "char"),
                                sep = " ")

    filtered_course_codes <- filtered_course_codes_tbl$code

    return(filtered_course_codes)
}

get_root_urls <- function(root){
    course_code_urls <- read_html(root) %>%
    html_nodes("#mainTable a") %>%
    html_attrs() %>%
    c()
    
    #CHANGE PARAMETER HERE TO CHANGE FACULTY FILTER
    filter <- make_faculty_filter(saud)

    course_code_urls <- paste(root_dir, course_code_urls, sep = "", collapse = NULL)

    course_code_urls_df <- data.frame("full_url" = course_code_urls,
                                    "temp_url" = course_code_urls) %>%
            separate(col = temp_url,
                into = c("garbage", "course_code"),
                sep = "department&dept=") %>%
        subset(course_code %in% filter) %>%
        pull(full_url)

    #course_code_urls <- course_code_urls$full_url

    return(course_code_urls_df)
}

get_course_urls <- function(ccurl){
    
    num_course_codes <- length(ccurl)
    course_urls <- c()
    
    for(k in 1:num_course_codes){

        current_course_code <- ccurl[k]

        current_course_urls <- read_html(current_course_code) %>%
        html_nodes("#mainTable a") %>%
        html_attrs() %>%
        c()

        current_course_urls <- paste(root_dir, current_course_urls, sep = "", collapse = NULL)

        course_urls <- append(course_urls, current_course_urls)
    }
    temp = data.frame(url = course_urls) %>%
            filter(url != root_dir)
    course_urls <- temp$url

    return(course_urls)
}

get_instructor <- function(sectionlinks){

    num_sections <- length(sectionlinks)
    #course_section_instructor <- c()
    course_instructors <- c()

    for(i in 1:num_sections){

        download.file(sectionlinks[i], "current_section.html")
        current_section <- "current_section.html"

        course_section_instructor <- read_html(current_section) %>%
            html_nodes("table") %>%
            .[3] %>%
            html_text() %>%
            c()

        course_instructors <- append(course_instructors, course_section_instructor)
        #course_instructors <- course_section_instructor
    }

    return(course_instructors)
}

get_section_urls <- function(courselinks){

    num_courses <- length(courselinks)
    section_urls <- c()

    for(j in 1:num_courses){

        download.file(courselinks[j], "current_section.html")
        current_section <- "current_section.html"
        #current_section <- courselinks[j]

        current_section_urls <- read_html(current_section) %>% 
            html_nodes("td > a") %>%
            html_attr('href') %>%
            c()

        #section_urls <- current_section_urls

        current_section_urls <- paste(root_dir, current_section_urls, sep = "", collapse = NULL)
        section_urls <- append(section_urls, current_section_urls)
        
    }
    
    temp = data.frame(url = section_urls) %>%
            filter(url != root_dir)
    section_urls <- temp$url

    return(section_urls)
}

get_course_section_information <- function(courselinks){

    course_num <- length(courselinks)
    course_information <- data.frame("Section" = c(), "Activity" = c(), "Term" = c(), "Instructor" = c())

    for (j in 1:course_num){

        current_course <- courselinks[j]
        
        course_instructors <- get_section_urls(c(current_course)) %>%
            get_instructor()
         
        #Getting name, activity, term, for each section
        current_course_information <- read_html(current_course) %>%
            html_nodes("table") %>%
            .[2] %>%
            .[[1]] %>%
            html_table(header = NA, trim = TRUE, fill = FALSE) %>%
            select(Section, Activity, Term) %>%
            filter(Section != "")

        #Adding urls and instructor for each course section to table
        current_course_information$Instructor = course_instructors
        course_information <- rbind(course_information, current_course_information)
    }
    return(course_information)    
}

clean_information <- function(courseinfo){

    #Type between "" with regular expressions

    activity_filter <- "Lecture"
    term_filter <- "2" 
    instructor_filter <- "." #Any part of instructor name

    cleaned_table <- filter(courseinfo, !grepl('Instructor', Instructor)) %>%
        separate(Instructor, c("Trim", "Instructor"), sep = ": ") %>%
        separate(Section, c("Course Code", "Course Number", "Section"), sep = " ") %>%
        select(Section, Activity, Term, Instructor) %>%
        filter(Activity == activity_filter) %>% 
        filter(!grepl(term_filter, Term)) %>% 
        filter(!grepl(instructor_filter, Instructor))
    
    return(cleaned_table)
}

main <- function(root){

    course_information <- get_root_urls(root_url) %>%
        get_course_urls() %>% 
        get_course_section_information() %>%
        clean_information()

    print(course_information)

    write.csv(course_information, "UBCCompleteCourseData.csv")

    
    return(course_information)
}

#PROGRAM CALL
main(root_url)

#TESTS
    #test_root_urls <- get_root_urls(root_url)
    #test_course_urls <- get_course_urls(sample_course_code)
    #test_get_instructor <- get_instructor(sample_section)
    #test_get_course_section_information <- get_course_section_information(sample_course)
    #print(test_root_urls)
    #print(test_course_urls)
    #print(test_get_instructor)
    #print(test_get_course_section_information)

    #Save full dataframe
    #course_code_urls <- get_root_urls(root_url)
    #course_code_urls
    #course_urls <- get_course_urls(course_code_urls)
    #course_urls
    #section_urls <- get_section_urls(course_urls)
    #course_information <- get_course_section_information(course_urls)
    #cleaned_information <- clean_information(course_information)
    #Export full dataframe into csv
    #write.csv(course_information, "UBCCompleteCourseData.csv")
    #course_information
