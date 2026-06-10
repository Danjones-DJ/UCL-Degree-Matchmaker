# Load Packages -----------------------------------------------------------
pacman::p_load(RSelenium, 
               rvest, 
               tidyverse, 
               stringr, 
               netstat,
               lubridate
)


# 00: Set up Selenium -----------------------------------------------------

# As using Selenium, ensure before running that no drivers are running such that
# scraping begins on a free port.

if (exists("rD")) try(rD$server$stop(), silent = TRUE)
try(
  system("pkill -f selenium", ignore.stdout = TRUE, ignore.stderr = TRUE),
  silent = TRUE
)


# Set-up driver on Firefox

rD = rsDriver(
  browser    = "firefox",
  port       = netstat::free_port(),
  phantomver = NULL,
  chromever  = NULL
)
# Set up client/driver object.

remDr = rD$client


UCL.UG.URL = "https://www.ucl.ac.uk/study/prospective-students/undergraduate/courses"
# 01: Load UG page --------------------------------------------------------

remDr$navigate(UCL.UG.URL)

Sys.sleep(4)  # let Cloudflare / JS load

html = remDr$getPageSource()[[1]]
page = read_html(html)

# Time for cookies to load
Sys.sleep(1.5)

cookie_button = remDr$findElement(
  using = "css selector",
  value = "button.ucl-privacy-banner__button:nth-child(1)"
)

# Accept cookies
cookie_button$clickElement()

# Wait to finish clicking / loading
Sys.sleep(1)



# 02: Handle pagination ---------------------------------------------------
html = remDr$getPageSource()[[1]]
page = read_html(html)

# Instead of manually checking page count and course counts...
# Use Selenium, click last page, get url, bosh

last_page_num <- page %>%
  html_element("a.pager__item--last__link") %>%
  html_attr("href") %>%
  str_extract("\\d+") %>%
  as.integer()

# Build vector of URLs 
urls = c(
  UCL.UG.URL,
  paste0(UCL.UG.URL, "?page=", 1:last_page_num)
)

length(urls) == last_page_num + 1 # If true: le bosh

# 03: Extract Course Info -------------------------------------------------
# Initialise dataframe for all courses, then will use the links in the urls 
# vector to get general info for all courses


extractCourseInfo = function(pageURL) {
  
  # Be polite 
  Sys.sleep(1) 
  
  # Keeping the above below as a reminder to not be silly. 
  # Don't use selenium if the pages are static!!!!!
  # # Navigate to page
  # remDr$navigate(pageURL)
  # html = remDr$getPageSource()[[1]]
  # page = read_html(html)
  
  # Get static page...
  page = read_html(pageURL)
  
  # Extract courses
  
  df = page %>%
    html_elements("article.course-feed-listing-item") %>%
    map_dfr(~ {
      tibble(
        
        # Get Title
        title = .x %>%
          html_element("h2") %>%
          html_text2(), 
        
        # Get Degree Type
        degree_type = .x %>%
          html_element(".course-feed-listing-item__degree-level") %>%
          html_text2(),
        
        # Get Faculty/Department
        faculty = .x %>%
          html_element(".course-feed-listing-item__faculty") %>%
          html_text2(),
        
        # Get URL for further scraping
        raw_url = .x %>%
          html_element("h2 a") %>% html_attr("href")
      )
    }) %>%
    drop_na(raw_url) %>%
    mutate(
      degree_url = paste0("https://www.ucl.ac.uk", raw_url)
    ) %>%
    select(-raw_url) 
  print("done")
  return(df)
}

# print(extractCourseInfo(urls[1]))
UCL_Courses_DF <- map_dfr(urls, extractCourseInfo)

# # View and ensure correct number of courses (as of June 2026, 429)
dim(UCL_Courses_DF)
# View(UCL_Courses_DF)


# 04: End Selenium and Save -----------------------------------------------
remDr$close() # Close Selenium
saveRDS(UCL_Courses_DF, "data/UCL_Courses_DF.rds")


