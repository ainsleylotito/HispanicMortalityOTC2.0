library(dplyr)
library(ggplot2) 


# -----------------------------
# CLEANING FUNCTIONS
# -----------------------------

# MCD --------------------


#NH white and black data
white_black_clean <- function(df, cod_character) {
  
  df <- df %>% 
    select(
      -any_of(c("Notes",
                "Five.Year.Age.Groups.Code",
                "Year.Code",
                "Sex.Code",
                "Race.Code",
                "Single.Race.6.Code",
                "Population"))
    )
  
  if ("Race" %in% names(df)) {
    df <- df %>% rename(race_eth = Race)
  } else if ("Single.Race.6" %in% names(df)) {
    df <- df %>% rename(race_eth = Single.Race.6)
  }
  
  df %>% 
    rename(age_group = Five.Year.Age.Groups) %>% 
    rename_with(tolower) %>% 
    mutate(
      cod = cod_character,
      deaths = as.numeric(deaths),
      year = as.integer(year)
    ) %>% 
    filter(!is.na(year))
}


#hispanic all race data
hispanic_clean <- function(df, cod_character) {
  
  df %>% 
    select(
      -any_of(c("Notes",
                "Five.Year.Age.Groups.Code",
                "Year.Code",
                "Sex.Code",
                "Population"))
    ) %>% 
    rename(age_group = Five.Year.Age.Groups) %>% 
    rename_with(tolower) %>% 
    mutate(
      cod = cod_character,
      race_eth = "Hispanic",
      deaths = as.numeric(deaths),
      year = as.integer(year)
    ) %>% 
    filter(!is.na(year))
}

# UCD ----------- 
#NH white and black data
NH_ucd_clean <- function(df, cod_character) {
  
  df <- df %>% 
    select(
      -any_of(c("Notes",
                "Five-Year Age Groups Code",
                "Year Code",
                "Sex Code",
                "Race Code",
                "Population",
                'Crude Rate'))
    )
  
  if ("Race" %in% names(df)) {
    df <- df %>% rename(race_eth = Race)
  } else if ("Single.Race.6" %in% names(df)) {
    df <- df %>% rename(race_eth = Single.Race.6)
  }
  
  df %>% 
    rename(age_group = 'Five-Year Age Groups') %>% 
    rename_with(tolower) %>% 
    mutate(
      cod = cod_character,
      deaths = as.numeric(deaths),
      year = as.integer(year)
    ) %>% 
    filter(!is.na(year))
}


#hispanic all race data
H_ucd_clean <- function(df, cod_character) {
  
  df %>% 
    select(
      -any_of(c("Notes",
                "Five-Year Age Groups Code",
                "Year Code",
                "Sex Code",
                "Population",
                "Crude Rate"))
    ) %>% 
    rename(age_group = "Five-Year Age Groups") %>% 
    rename_with(tolower) %>% 
    mutate(
      cod = cod_character,
      race_eth = "Hispanic",
      deaths = as.numeric(deaths),
      year = as.integer(year)
    ) %>% 
    filter(!is.na(year))
} 

#By Age Group Hispanic 
ucd_age_clean_h <- function(df, cod_character) {
  
  df %>% 
    filter(is.na(Notes) | Notes == "") %>%   # keep only rows with no notes
    select(
      -any_of(c("Notes",
                "Five-Year Age Groups Code",
                "Population",
                "Crude Rate",
                "Race",
                "Race Code"))
    ) %>% 
    rename(age_group = "Five-Year Age Groups") %>% 
    rename_with(tolower) %>% 
    mutate(
      cod = cod_character,
      race_eth = "hispanic",
      deaths = as.numeric(deaths)
    ) 
}


#By Age Group NH 
ucd_age_clean_nh <- function(df, cod_character) {
  
  df %>% 
    filter(is.na(Notes) | Notes == "") %>%   # keep only rows with no notes
    select(
      -any_of(c("Notes",
                "Five-Year Age Groups Code",
                "Population",
                "Crude Rate",
                "Race Code"))
    ) %>% 
    rename(age_group = "Five-Year Age Groups",
           race_eth = "Race") %>% 
    rename_with(tolower) %>% 
    mutate(
      cod = cod_character,
      deaths = as.numeric(deaths)
    ) 
} 

#Formatting race and age 
race_age_format <- function(df) {
  
  df %>%
    filter(age_group != "Not Stated") %>% 
    mutate(
      race_eth = case_when(
        race_eth == "Black or African American" ~ "black",
        race_eth == "White" ~ "white",
        race_eth == "Hispanic" ~ "hispanic",
        TRUE ~ race_eth
      ), 
      age_group = stringr::str_trim(age_group),
      age_group = case_when(
        age_group %in% c("< 1 year", "1-4 years") ~ "0-4",
        age_group == "5-9 years"   ~ "5-9",
        age_group == "10-14 years" ~ "10-14",
        age_group == "15-19 years" ~ "15-19",
        age_group == "20-24 years" ~ "20-24",
        age_group == "25-29 years" ~ "25-29",
        age_group == "30-34 years" ~ "30-34",
        age_group == "35-39 years" ~ "35-39",
        age_group == "40-44 years" ~ "40-44",
        age_group == "45-49 years" ~ "45-49",
        age_group == "50-54 years" ~ "50-54",
        age_group == "55-59 years" ~ "55-59",
        age_group == "60-64 years" ~ "60-64",
        age_group == "65-69 years" ~ "65-69",
        age_group == "70-74 years" ~ "70-74",
        age_group == "75-79 years" ~ "75-79",
        age_group == "80-84 years" ~ "80-84",
        age_group == "85-89 years" ~ "85-89",
        age_group %in% c("90-94 years", "95-99 years", "100+ years") ~ "90+",
        TRUE ~ NA_character_
      )
    ) %>%  
    group_by(race_eth, age_group, cod) %>%
    summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop")
}

#By Year Hispanic 

#By Year NH



# -----------------------------
# GLOBAL CONSTANTS
# -----------------------------

age_levels <- c(
  "0-4","5-9","10-14","15-19","20-24",
  "25-29","30-34","35-39","40-44",
  "45-49","50-54","55-59","60-64",
  "65-69","70-74","75-79","80-84",
  "85-89","90+"
)

set_age_order <- function(df){
  df %>% mutate(age_group=factor(age_group, levels=age_levels, ordered=TRUE))
}


# -----------------------------
# CORE RATE CALCULATION
# -----------------------------

calc_rates <- function(data, years, sex_val, cod_val){
  
  data %>%
    filter(year %in% years,
           sex == sex_val,
           cod == cod_val) %>%
    group_by(race_eth, age_group) %>%
    summarise(
      deaths = sum(deaths, na.rm=TRUE),
      pop    = sum(population, na.rm=TRUE),
      .groups="drop"
    ) %>%
    mutate(rate = deaths/pop*100000) %>%
    set_age_order()
}



# -----------------------------
# RATE RATIOS
# -----------------------------

calc_rr <- function(df, ref="white"){
  df %>%
    group_by(age_group) %>%
    mutate(
      ref_rate  = rate[race_eth==ref],
      ref_count = deaths[race_eth==ref],
      rr = rate/ref_rate,
      rr_lcl = rr*exp(-1.96*sqrt(1/deaths+1/ref_count)),
      rr_ucl = rr*exp( 1.96*sqrt(1/deaths+1/ref_count))
    ) %>%
    ungroup() %>%
    filter(race_eth!=ref)
}



# -----------------------------
# CI FUNCTIONS
# -----------------------------

ci_log <- function(rate,count){
  tibble(
    lcl = rate*exp(-1.96*sqrt(1/count)),
    ucl = rate*exp( 1.96*sqrt(1/count))
  )
}

ci_exact <- function(deaths,pop){
  tibble(
    lcl=(qchisq(.025,2*deaths)/2)/pop*100000,
    ucl=(qchisq(.975,2*(deaths+1))/2)/pop*100000
  )
}



# -----------------------------
# PLOTTING FUNCTIONS
# -----------------------------

plot_rr <- function(df, title){
  
  ggplot(df,
         aes(age_group, rr, color=race_eth, group=race_eth))+
    
    geom_hline(yintercept=1, linetype="dashed")+
    geom_line(linewidth=1)+
    geom_point(size=2)+
    geom_errorbar(aes(ymin=rr_lcl, ymax=rr_ucl), width=.15)+
    
    labs(title=title,
         x="Age group",
         y="Rate Ratio",
         color="Race/Ethnicity")+
    
    scale_color_discrete(labels = c(
      "hispanic" = "Hispanic",
      "white" = "Non-Hispanic White",
      "black" = "Non-Hispanic Black"
    ))+
    
    theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(1.2, "cm"),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1)
  )
}



plot_rate <- function(df,title,ylabel="Rate per 100,000"){
  
  ggplot(df,
         aes(age_group, rate, color=race_eth, group=race_eth))+
    
    geom_line(linewidth=1)+
    geom_point(size=2)+
    geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.15)+
    
    labs(title=title,
         x="Age group",
         y=ylabel,
         color="Race/Ethnicity")+
    
    scale_color_discrete(labels = c(
      "hispanic" = "Hispanic",
      "white" = "Non-Hispanic White",
      "black" = "Non-Hispanic Black"
    ))+
    
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_text(size = 14),
          legend.text  = element_text(size = 12),
          legend.key.size = unit(1.2, "cm"),
          legend.position = c(0.98, 0.98),
          legend.justification = c(1, 1)
    )  
}



plot_yearly <- function(df,title){
  
  pd <- position_dodge(.3)
  
  ggplot(df,aes(year, rate, color=race_eth))+
    
    geom_line(linewidth=1)+
    geom_point(size=2,position=pd)+
    geom_errorbar(aes(ymin=lcl,ymax=ucl),
                  width=.2,position=pd)+
    
    labs(title=title,
         x="Year",
         y="Rate per 100,000",
         color="Race/Ethnicity")+
    
    
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 1))+
    
    scale_color_discrete(labels = c(
      "hispanic" = "Hispanic",
      "white" = "Non-Hispanic White",
      "black" = "Non-Hispanic Black"
    ))+
    
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_text(size = 14),
          legend.text  = element_text(size = 12),
          legend.key.size = unit(1.2, "cm"),
          legend.position = c(0.98, 0.98),
          legend.justification = c(1, 1)
    )  
}

# ---------------------------- 
# OVERLAY FUNCTIONS 
# ---------------------------- 
plot_rate_overlay <- function(new, old, title){
  
  ggplot(new, aes(age_group, rate, color=race_eth, group=race_eth)) +
    
    geom_line(size=1) +
    geom_point(size=2) +
    geom_errorbar(aes(ymin=lcl,ymax=ucl), width=.15) +
    
    geom_line(data=old, linetype="dashed", size=1) +
    geom_point(data=old, shape=1, size=2) +
    geom_errorbar(data=old,
                  aes(ymin=lcl,ymax=ucl),
                  linetype="dashed",
                  width=.15) +
    
    labs(title=title,
         subtitle="Solid = 2007–2020, Dashed = 1999–2006",
         x="Age", y="Rate per 100k") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
} 


plot_rr_overlay <- function(new, old, title){
  
  ggplot(new, aes(age_group, rr, color=race_eth, group=race_eth)) +
    geom_hline(yintercept=1, linetype="dashed", color="gray50") +
    
    geom_line(size=1) +
    geom_point(size=2) +
    geom_errorbar(aes(ymin=rr_lcl, ymax=rr_ucl), width=.15) +
    
    geom_line(data=old, linetype="dashed", size=1) +
    geom_point(data=old, shape=1, size=2) +
    geom_errorbar(data=old,
                  aes(ymin=rr_lcl,ymax=rr_ucl),
                  linetype="dashed",
                  width=.15) +
    
    labs(title=title,
         subtitle="Solid = 2007–2020, Dashed = 1999–2006",
         x="Age", y="Rate Ratio") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

#comparison data for RRs
comparison_data <- function(df1, df2) {df1 %>%
  left_join(df2,
            by=c("age_group","race_eth"),
            suffix=c("_new","_old")) %>%
  mutate(diff = rr_new - rr_old,
         pct_change = ((rr_new - rr_old)/rr_old * 100),
         rr_ratio = rr_new / rr_old)
}

#making heatmap to compare old/new rrs
heatmap_rr_comparison <- function(df){ 
  ggplot(df, aes(age_group, race_eth, fill = rr_ratio)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 1) +
  labs(title="Change in Disparity (RR Ratio)",
       x="Age Group",
       y="Race/Ethnicity")
}

#comparing rates by age group
rate_comparison_data <- function(df_new, df_old){
  
  df_new %>%
    left_join(df_old,
              by = c("age_group","race_eth"),
              suffix = c("_new","_old")) %>%
    mutate(
      rate_diff = rate_new - rate_old,
      pct_change = ((rate_new - rate_old)/rate_old) * 100,
      rate_ratio = rate_new / rate_old
    )
}

#heatmap for rate changes by age group
heatmap_rate_comparison <- function(df){
  
  ggplot(df,
         aes(age_group, race_eth, fill = rate_ratio)) +
    geom_tile() +
    scale_fill_gradient2(midpoint = 1) +
    labs(title = "Change in Mortality Rates (Rate Ratio)",
         x = "Age Group",
         y = "Race/Ethnicity")
}

