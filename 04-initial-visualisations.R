# Load Packages -----------------------------------------------------------
pacman::p_load(rvest, 
               tidyverse, 
               stringr, 
               netstat,
               lubridate
)

# 00: Load Data and Inspect Structure -------------------------------------
UCL.v1 = read_rds("data/UCL.v1.rds")

# 01: Visualisations for Overall Uni --------------------------------------

# Most common modules by department
faculty_totals <- UCL.v1 %>%
  count(faculty, name = "total_courses")

module_percentages <- UCL.v1 %>%
  separate_rows(all_compulsory_modules, sep = " \\| ") %>%
  filter(!str_detect(all_compulsory_modules, "No compulsory modules")) %>%
  distinct(faculty, title, all_compulsory_modules) %>%
  count(faculty, all_compulsory_modules, name = "courses_taking_module") %>%
  left_join(faculty_totals, by = "faculty") %>%
  mutate(
    percentage = round(courses_taking_module / total_courses * 100, 2)
  ) %>%
  arrange(faculty, desc(percentage))

top_modules_faculty = module_percentages %>%
  group_by(faculty) %>%
  slice_max(percentage, n = 1)

# View(top_modules_faculty)

# Most common entry requirements by department
faculty_totals <- UCL.v1 %>%
  count(faculty, name = "total_courses")

alg_percentages <- UCL.v1 %>%
  count(faculty, A_Level_Grades, name = "entry_grades_faculty") %>%
  left_join(faculty_totals, by = "faculty") %>%
  mutate(
    percentage = round(entry_grades_faculty / total_courses * 100, 1)
  ) %>%
  arrange(faculty, desc(percentage))

toptwo_entry_grades_faculty = alg_percentages %>%
  group_by(faculty) %>%
  slice_max(percentage, n = 2)

# View(toptwo_entry_grades_faculty)

# 02: Entry Requirement Competitiveness Ranking ----------------------------
grade_score <- c(
  "E" = 1,
  "D" = 2,
  "C" = 3,
  "B" = 4,
  "A" = 5,
  "A*" = 6
)

score_grades <- function(x) {
  grades <- str_extract_all(x, "A\\*|[A-E]")[[1]]
  sum(grade_score[grades])
}

lookup_table <- tibble(
  A_Level_Grades = unique(UCL.v1$A_Level_Grades)
) %>%
  mutate(
    grade_points = sapply(A_Level_Grades, score_grades)
  )

UCL.v2 <- UCL.v1 %>%
  left_join(lookup_table, by = "A_Level_Grades") %>%
  group_by(faculty) %>%
  mutate(
    faculty_competitiveness_rank = dense_rank(desc(grade_points))
  ) %>%
  ungroup() %>%
  mutate(
    ucl_competitiveness_rank = dense_rank(desc(grade_points))
  )

View(UCL.v2)

glimpse(UCL.v2)

saveRDS(UCL.v2, "data/UCL.v2.rds")