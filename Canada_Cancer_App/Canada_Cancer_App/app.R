#### Canadian Cancer Application Using HMD Intermediate Age-specific Death Rates 1950-2022 ####
#libraries
library(readr)
library(tidyverse)
library(ggplot2)
library(shiny)
library(bslib)
library(shinyWidgets)
library(plotly)

can_int_dat_raw <- read_csv("CAN_m_interm_orig.csv",
col_types = cols(
  country = col_character(),
  year = col_double(),
  sex = col_character(),  
  agf = col_double(),
  cause = col_character(),
  m0 = col_character(), m1 = col_character(), m5 = col_character(), m10 = col_character(),
  m15 = col_character(), m20 = col_character(), m25 = col_character(), m30 = col_character(),
  m35 = col_character(), m40 = col_character(), m45 = col_character(), m50 = col_character(),
  m55 = col_character(), m60 = col_character(), m65 = col_character(), m70 = col_character(),
  m75 = col_character(), m80 = col_character(), m85p = col_character(), m85 = col_character(),
  m90p = col_character(), m90 = col_character(), m95p = col_character(), m95 = col_character(),
  m100p = col_character()
))

#add more rows with pivot longer to remove the raw age groups
can_int_dat <- can_int_dat_raw %>% 
  pivot_longer(
    cols = starts_with("m"),
    names_to = "age",
    values_to = "deaths"
  ) %>% 
  filter(!age %in% c("m85", "m90", "m95", "m90p", "m95p", "m100p"))

can_int_dat <- can_int_dat %>% 
  mutate(
    age_group = case_when(
      age == "m0"  ~ "<1",
      age == "m1"  ~ "1-4",
      age == "m5"  ~ "5-9",
      age == "m10" ~ "10-14",
      age == "m15" ~ "15-19",
      age == "m20" ~ "20-24",
      age == "m25" ~ "25-29",
      age == "m30" ~ "30-34",
      age == "m35" ~ "35-39",
      age == "m40" ~ "40-44",
      age == "m45" ~ "45-49",
      age == "m50" ~ "50-54",
      age == "m55" ~ "55-59",
      age == "m60" ~ "60-64",
      age == "m65" ~ "65-69",
      age == "m70" ~ "70-74",
      age == "m75" ~ "75-79",
      age == "m80" ~ "80-84",
      age == "m85p" ~ "85-100",
      TRUE ~ NA_character_
    ),
    age_start = case_when(
      age == "m0" ~ 0,
      age == "m85p" ~ 85,
      TRUE ~ as.numeric(stringr::str_extract(age, "\\d+"))
    ),
    age_width = case_when(
      age == "m0" ~ 1,
      age == "m1" ~ 4,
      age == "m85p" ~ 15,
      TRUE ~ 5
    ),
    deaths_numeric = as.numeric(deaths)
  ) %>%
  filter(
    !is.na(age_group),
    !cause %in% c(
      "I000","I001","I002","I003","I004","I005","I006",
      "I024","I025","I026","I027","I028","I029","I030","I031","I032",
      "I033","I034","I035","I036","I037","I038","I039","I040","I041",
      "I042","I043","I044","I045","I046","I047","I048","I049","I050",
      "I051","I052","I053","I054","I055","I056","I057","I058"
    )
  )

# Creating death proportion (px)
can_int_tots <- can_int_dat %>%
  group_by(year, age, sex) %>%
  summarise(total_deaths = sum(deaths_numeric, na.rm = TRUE))

can_int_dat_final <- can_int_dat %>%
  left_join(can_int_tots, by = c("year", "age", "sex")) %>%
  mutate(px = deaths_numeric / total_deaths) 

#Labeling causes of death
causes_int <- c(
  "I007" =  "Lip, oral cavity and pharynx",
  "I008" =  "Esophagus",
  "I009" =  "Stomach",
  "I010" =  "Colon, rectum and anus ",
  "I011" =  "Pancreas",
  "I012" =  "Digestive System",
  "I013" =  "Larynx, trachea, bronchus and lung ",
  "I014" =  "Skin",
  "I015" =  "Breast",
  "I016" =  "Uterus",
  "I017" =  "Ovary",
  "I018" =  "Prostate",
  "I019" =  "Genital organs",
  "I020" =  "Bladder",
  "I021" =  "Kidney and urinary organs",
  "I022" =  "Leukemia",
  "I023" =  "Other/unknown/benign neoplasms"
  
)
can_int_dat_final <- can_int_dat_final %>%
  mutate(
    causes_int_label = factor(causes_int[cause], levels = unname(causes_int))
  )


#### Shiny App Code ####
ui <-
  fluidPage(
    titlePanel("Cancer Mortality in Canada: An Analysis of Age, Sex, and Proportion of Death"),
    fluidRow(
      #Left side
      column(
        width = 6,
        selectInput(
          "cause_left", "Select cancer type:",
          choices = setNames(names(causes_int), causes_int),
          selected = "I015"
        ),
        selectInput(
          "sex_left", "Select sex:",
          choices = c("Male" = 1, "Female" = 2, "Both" = 3),
          selected = 2
        ),
        plotlyOutput("heatmap1", height = "600px")
        
      ),
      #Right side
      column(
          width = 6,
          selectInput(
            "cause_right", "Select cancer type:",
            choices = setNames(names(causes_int), causes_int),
            selected = "I018"
          ),
          selectInput(
            "sex_right", "Select sex:",
            choices = c("Male" = 1, "Female" = 2, "Both" = 3),
            selected = 1
          ),
          plotlyOutput("heatmap2", height = "600px")
    )
  )
)
server <- function (input, output) {
  #left side
  output$heatmap1 <- renderPlotly({
   can_int_sm_left <- can_int_dat_final %>% 
     filter(
       cause== input$cause_left,
       sex== input$sex_left)
   plot_ly(
     data = can_int_sm_left, 
     x = ~year, 
     y = ~age_start, 
     z = ~px, 
     customdata = ~sex,
     type = "heatmap",
     colors = rev(viridisLite::viridis(100)),
     hovertemplate = paste(
       "Year: %{x}<br>", 
       "Age: %{y}<br>",
       "Sex: %{customdata}<br>",
       "Px: %{z:.3f}<extra></extra>")
   ) %>% 
     layout(
       xaxis = list(title = "Year", tickvals = seq(1950, 2022, 10)),
       yaxis = list(title = "Age", tickvals = seq(0, 99, 10))
     )
  })
  
  #right side
  output$heatmap2 <- renderPlotly({
    can_int_sm_right <- can_int_dat_final %>% 
      filter(
        cause== input$cause_right,
        sex== input$sex_right)
    plot_ly(
      data = can_int_sm_right, 
      x = ~year, 
      y = ~age_start, 
      z = ~px, 
      customdata = ~sex,
      type = "heatmap",
      colors = rev(viridisLite::viridis(100)),
      hovertemplate = paste(
        "Year: %{x}<br>", 
        "Age: %{y}<br>",
        "Sex: %{customdata}<br>",
        "Px: %{z:.3f}<extra></extra>")
    ) %>% 
      layout(
        xaxis = list(title = "Year", tickvals = seq(1950, 2022, 10)),
        yaxis = list(title = "Age", tickvals = seq(0, 99, 10))
      )
  })
  
}
shinyApp(ui = ui, server = server)
