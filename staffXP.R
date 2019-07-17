
library(here)
library(tidyverse)
library(ggthemes)
library(plotly)




staff.school <- read.delim(here("data","StaffSchoolFTE17.txt")) 

student.school <- read.delim(here("data","filesenr.asp.txt")) %>% # 2017-18 Student Enrollment
  filter(str_detect(COUNTY, "Monterey")) %>% # only Monterey County
  select(1:6,ends_with("TOTAL")) %>% # Remove the grade level enrollment
  group_by(SCHOOL) %>% 
  mutate(total = sum(ENR_TOTAL)) %>% # find total school enrollment
  filter(ETHNIC == 7) %>% # show white enrollment
  mutate(white = sum(ENR_TOTAL),  # calculated total white enrollment
         perc.white = white/total) %>% # calculate percentage
  select(-ETHNIC,-GENDER, -ENR_TOTAL) %>% # clean up
  distinct() # remove gender duplicate lines
  

staff <- read.delim(here("data","StaffDemo17.txt")) %>% # 2017-18 staff 
  filter(str_detect(CountyName, "MONTEREY")  ) %>% # limits to Monterey County
  left_join(staff.school, by = c("RecID", "DistrictName") ) %>% #adds school name to teacher
  filter(FTE.Teaching > 1,
         !str_detect(SchoolName, "District Office"))  # Must be teaching some, eliminates admin only and pupil services only, and not at "District Office"


lunch <- readxl::read_xlsx(here("data","frpm1718.xlsx"), sheet = "FRPM School-Level Data ", range = "A2:AB10475") %>%
  filter(str_detect(`County Name`, "Monterey")) %>% 
  select(starts_with("School"), starts_with("Percent")) %>%
  select(starts_with("School"), contains("FRPM")) %>%
  select(contains("Name"), contains("K-12")) %>% 
  `colnames<-`(c("SchoolName","perc.lunch" ))


graphit <- function(compiled, ylab){
  
  joint <-  compiled %>%
    group_by(DistrictName, SchoolName) %>%
    mutate(PercNew = mean(NewTeacher)) %>%
    select(DistrictName, PercNew) %>%
    distinct() %>%
    arrange(PercNew) %>%
    left_join(student.school, by = c("SchoolName" = "SCHOOL")) %>%
    left_join(lunch)
  
  
  ggplot(joint, aes(x = perc.white, y = PercNew)) + 
    geom_point() + 
    geom_text( aes( label = SchoolName), size = 2, color = "grey" , position = "dodge" ) +
    geom_smooth(method=lm , color="red", se=TRUE) +
    theme_hc() +
    labs(
      x = "Percent of White Students",
      y = ylab
    )
}



# New Teachers
compiled <- staff %>% 
  filter(str_detect(DistrictName,"Salinas City")) %>%  # Filter for a single district
  mutate(NewTeacher = ifelse(YearsTeaching <= 2, 1,0) ) 

graphit(compiled, "Percent of New Teachers")

ggplotly() 

# Female Teachers
compiled <- staff %>% 
  mutate(NewTeacher = ifelse(GenderCode == "F", 1,0) ) 

graphit(compiled, "Percent of Female Teachers")


# Master's or Doctorate 
compiled <- staff %>% 
  mutate(NewTeacher = ifelse(EducationLevel %in% c("D","V","M"), 1,0) ) 

graphit(compiled, "Percent of Teachers with Masters or Doctorate")

# Tenured 
compiled <- staff %>% 
  mutate(NewTeacher = ifelse(EmploymentStatusCode == "T", 1,0) ) 

graphit(compiled, "Percent of Teachers Tenured")

# White, Non-Hispanic
compiled <- staff %>% 
  mutate(NewTeacher = ifelse(EthnicGroup == 7, 1,0) ) 

graphit(compiled, "Percent of White Teachers")
