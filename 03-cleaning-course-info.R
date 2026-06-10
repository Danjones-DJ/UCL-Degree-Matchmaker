# Load Packages -----------------------------------------------------------
pacman::p_load(rvest, 
               tidyverse, 
               stringr, 
               netstat,
               lubridate
)

# 00: Load Data and Inspect Structure -------------------------------------
ucl_raw = read_rds("data/ucl_all_courses.rds")

# glimpse(ucl_raw)
# head(ucl_raw)


# 01: Grade Info ----------------------------------------------------------

ucl_raw2 = ucl_raw %>%
  mutate(
    A_Level_Grades = str_remove(str_extract(entry_requirements, "Grades\\n(?:A\\*|[A-E]){3}"), "Grades\\n"),
    A_Level_Subjects = str_extract(entry_requirements, "(?<=\\nSubjects\\n).*?(?=\\nGCSEs\\n)")
  ) %>%
  mutate(A_Level_Subjects = replace_na(A_Level_Subjects, "No specific subjects required (Look for UCL's list of Preferred Subjects)"))


# 02: Compulsory Module Info ----------------------------------------------

getCompModules = function(index) {
  text = ucl_raw2$modules[index]
  
  if(is.na(text) || str_squish(text) == "") return(NA_character_)
  
  comp <- str_locate_all(text, "Compulsory modules")[[1]]
  opt  <- str_locate_all(text, "Optional modules")[[1]]
  
  if(nrow(comp) == 0 || nrow(opt) == 0) return(NA_character_)
  
  all_compulsory <- c()
  
  for(j in seq_len(nrow(comp))) {
    
    next_opt <- opt[opt[, "start"] > comp[j, "end"], , drop = FALSE]
    
    if(nrow(next_opt) == 0) next
    
    compulsory_text <- str_sub(
      text,
      comp[j, "end"] + 1,
      next_opt[1, "start"] - 1
    )
    
    modules <- compulsory_text |>
      str_split("\n") |>
      unlist()
    
    modules <- str_squish(modules)
    modules <- modules[modules != ""]
    
    modules <- modules[!modules %in% c("Compulsory modules", "Optional modules")]
    
    all_compulsory <- c(all_compulsory, modules)
  }
  
  all_compulsory <- str_squish(all_compulsory)
  all_compulsory <- all_compulsory[all_compulsory != ""]
  
  if(length(all_compulsory) == 0) return(NA_character_)
  
  result = str_c(unique(all_compulsory), collapse = " | ")
  return(result)
}
ucl_raw3 = ucl_raw2 %>%
  mutate(
    all_compulsory_modules = sapply(seq_len(nrow(ucl_raw2)), getCompModules)
  ) %>%
  mutate(all_compulsory_modules = replace_na(all_compulsory_modules, "No compulsory modules listed (Refer to optional modules!)"))

# View(ucl_raw3)
# head(ucl_raw3$all_compulsory_modules)
# skimr::skim(ucl_raw3)

# 03: About + What gives --------------------------------------------------

# ucl_raw3$about[1]
# ucl_raw3$what_gives_you[1]


ucl_raw4 <- ucl_raw3 %>%
  mutate(
    about_main = str_extract(
      about,
      "(?<=About this course\\n\\n)[\\s\\S]*?(?=\\n\\nWho this course is for)"
    ),
    
    about_who_for = str_extract(
      about,
      "(?<=Who this course is for\\n\\n)[\\s\\S]*$"
    ),
    
    graduate_attributes = str_extract(
      what_gives_you,
      "(?<=Graduate attributes\\n\\n)[\\s\\S]*?(?=\\n\\nGraduate destinations)"
    ),
    
    graduate_destinations = str_extract(
      what_gives_you,
      "(?<=Graduate destinations\\n\\n)[\\s\\S]*?(?=\\n\\nIndustries)"
    ),
    
    industries = str_extract(
      what_gives_you,
      "(?<=Industries\\n)[\\s\\S]*$"
    )
  )

# View(ucl_raw4)
# skimr::skim(ucl_raw4)

# 04: Clean-up ------------------------------------------------------------
UCL.v1 <- ucl_raw4 %>%
  mutate(
    about_section = paste0(
      ifelse(!is.na(about_main), paste0("About:\n", about_main), ""),
      ifelse(!is.na(about_who_for), paste0("\n\nWho this course is for:\n", about_who_for), "")
    ),
    graduate_section =  paste0(
      ifelse(!is.na(graduate_attributes), paste0("Graduate attributes:\n", graduate_attributes), ""),
      ifelse(!is.na(graduate_destinations), paste0("\n\nGraduate destinations:\n", graduate_destinations), ""),
      ifelse(!is.na(industries), paste0("\n\nIndustries:\n", industries), ""))
    )

# UCL.v1$about_section[1]
# UCL.v1$graduate_section[1]

UCL.v1 = UCL.v1 %>%
  select(
    title, degree_type, faculty, degree_url, A_Level_Grades, A_Level_Subjects, 
    all_compulsory_modules, about_section, graduate_section
    ) 

UCL.v1$university = "University College London"
# View(UCL.v1)

# 05: Save for presenting -------------------------------------------------
saveRDS(UCL.v1, "data/UCL.v1.rds")