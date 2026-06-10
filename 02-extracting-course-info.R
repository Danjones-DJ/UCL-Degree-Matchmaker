# Load Packages -----------------------------------------------------------
pacman::p_load(rvest, 
               tidyverse, 
               stringr, 
               netstat,
               lubridate
               )

# 00: Load Data and Inspect Structure -------------------------------------
courses_list = read_rds("data/UCL_Courses_DF.rds")

# What aspects are consistent across all courses?
h2_lists <- map(courses_list$degree_url[1:5], ~ read_html(.x) %>% 
                  html_elements("h2") %>% 
                  html_text(trim = TRUE))

# Reduce to only show elements every page has
common = Reduce(intersect, h2_lists)

# common # Inspect

# 01: Start scraping sections! --------------------------------------------
# testURL = courses_list$degree_url[1]
# page = read_html(testURL)
# 
# page %>% html_element("#key-information")      %>% html_text2()
# page %>% html_element("#entry-requirements")   %>% html_text2()
# page %>% html_element("#about-this-course")    %>% html_text2()
# page %>% html_element("#course-structure")     %>% html_text2()
# page %>% html_element("#modules")              %>% html_text2()
# page %>% html_element("#contact-hours")        %>% html_text2()
# page %>% html_element("#fees-funding")         %>% html_text2()
# page %>% html_element("#discover-uni")         %>% html_text2()
# page %>% html_element("#what-course-give-you") %>% html_text2()
# 
# # Messy, but I can clean post-scraping. Benefit of this is also scraping 
# # should take "less" time if I'm running less per page right now.

# 02: Test appending logic ------------------------------------------------

# myData <- data.frame(
#   Name = c("Dan"),
#   Age = c(21)
# )
# 
# myData$job <- NA
# 
# myData[myData$Name == "Dan", "job"] <- "Coaching"
# 
# myData


UCL_Courses = courses_list
UCL_Courses$key_information <- NA
UCL_Courses$entry_requirements <- NA
UCL_Courses$about <- NA
UCL_Courses$course_structure <- NA
UCL_Courses$modules <- NA    
UCL_Courses$contact_hours <- NA
UCL_Courses$discover_uni <- NA
UCL_Courses$what_gives_you <- NA

skimr::skim(UCL_Courses)


for(i in seq_len(nrow(UCL_Courses))) {
  
  url <- UCL_Courses$degree_url[i]
  page <- read_html(url)
  
  UCL_Courses$key_information[i] = page %>% html_element("#key-information") %>% html_text2()
  UCL_Courses$entry_requirements[i] = page %>% html_element("#entry-requirements")   %>% html_text2()
  UCL_Courses$about[i] = page %>% html_element("#about-this-course")    %>% html_text2()
  UCL_Courses$course_structure[i] = page %>% html_element("#course-structure")     %>% html_text2()
  UCL_Courses$modules[i] = page %>% html_element("#modules")              %>% html_text2()
  UCL_Courses$discover_uni[i] = page %>% html_element("#discover-uni")         %>% html_text2()
  UCL_Courses$what_gives_you[i] = page %>% html_element("#what-course-give-you") %>% html_text2()
  
  print(paste("Completed Scraping:", UCL_Courses$title[i]))
}


View(UCL_Courses)
skimr::skim(UCL_Courses)

# 04: Initial cleaning and NA handling ------------------------------------
# Going to drop the contact hours column
# Going to have an NA's in Module with "Not listed"

UCL_raw = UCL_Courses %>%
  select(-contact_hours) %>%
  mutate(modules = replace_na(modules, "Not listed"))

skimr::skim(UCL_raw)

# 05: Save to prep for cleaning -------------------------------------------
saveRDS(UCL_raw, "data/ucl_all_courses.rds")


