# Wrangle Data

#check clinic attendance codes
clinic_attendance_codes <- all_appointments_raw |> 
  clean_names() |> 
  distinct(clinic_attendance)



# Wrangle -----------------------------------------------------------------

## all appoitnemnts for summar chart

# all_appointments <- all_appointments_total_raw |> 
#   clean_names() |> 
#   left_join(specialty_lookup) |> 
#   mutate(selections = case_when(specialty %in% select_specialties ~ "Mental Health",
#                                 TRUE ~ "Other"))
# 
# all_specialties_chart <- plot_ly(data = all_appointments,
#                                  x = ~total_appointments,
#                                  y = ~specialty_description,
#                                  color = ~selections,
#                                  colors = chart_cols,
#                                  type = 'bar') %>%
#   layout(xaxis = list(title = "Appointments"),
#          yaxis = list(title = "Specialty", 
#                       categoryorder = "total ascending"),
#          hovermode = "y unified")


# create filter for specialties with over 20 appointments
keep_specialties_list <- all_appointments_raw |> 
  clean_names() |> 
  group_by(specialty) %>%
  summarise(total_appointments = sum(total_appointments)) %>%
  ungroup() |> 
  filter(total_appointments >= 20) |> 
  pull(specialty)


## all appointments by Year and SIMD
all_appointments_simd <- all_appointments_raw |> 
  bind_rows(all_appointments_raw_borders) |>
  bind_rows(all_appointments_raw_scotland) |> 
  clean_names() |> 
  left_join(
    simd_lookup,
    by = "postcode"
  ) |> 
  left_join(specialty_lookup) |> 
  mutate(date_year = year(clinic_date)) |> 
  mutate(selections = case_when(specialty %in% select_specialties ~ "Mental Health",
                                TRUE ~ "Other"),
         simd_quintile = factor(simd_quintile)) |> 
  filter(specialty %in% keep_specialties_list)







# Practice DNA Rate
all_dna_summary <- all_appointments_simd %>%
  group_by(location) %>%
  summarise(total_appointments =  sum(total_appointments),
            dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dna_percent = percent(dna_count / total_appointments, .1)) %>%
  arrange(desc(dna_percent)) 

#specialty
#mental health group
all_spec_simd_summary_mh <- all_appointments_simd %>%
  filter(location == "The Tweed") |> 
  filter(specialty %in% select_specialties) |> 
  mutate(specialty_description = "Mental Health (Grouped)",
         selections = "Mental Health (Grouped)") |> 
  group_by(location, specialty_description, selections) %>%
  summarise(total_appointments = sum(total_appointments),
            dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dna_percent_chart = dna_count / total_appointments,
         dna_percent = percent(dna_count / total_appointments, .1)) %>%
  arrange(desc(dna_percent)) 

#all specialties group
all_spec_simd_summary_all <- all_appointments_simd %>% 
  mutate(specialty_description = "All Specialties",
         selections = "All Specialties") |> 
  group_by(location, specialty_description, selections) %>%
  summarise(total_appointments = sum(total_appointments),
            dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dna_percent_chart = dna_count / total_appointments,
         dna_percent = percent(dna_count / total_appointments, .1)) %>%
  arrange(desc(dna_percent)) 

#standard names group
all_spec_simd_summary <- all_appointments_simd %>%
  group_by(location, specialty_description, selections) %>%
  summarise(total_appointments = sum(total_appointments),
            dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dna_percent_chart = dna_count / total_appointments,
         dna_percent = percent(dna_count / total_appointments, .1)) %>%
  arrange(desc(dna_percent)) |> 
  bind_rows(all_spec_simd_summary_mh, all_spec_simd_summary_all) |> 
  filter(location == "The Tweed") 

all_specialties_chart <- all_spec_simd_summary |> 
  filter(!specialty_description %in% c("All Specialties", "Mental Health (Grouped)")) |> 
  plot_ly(x = ~total_appointments,
          y = ~specialty_description,
          color = ~selections,
          colors = chart_cols,
          type = 'bar') %>%
  layout(xaxis = list(title = "Appointments"),
         yaxis = list(title = "Specialty", 
                      categoryorder = "total ascending"),
         hovermode = "y unified")

# all spec - DNAs
all_specialties_dna_chart <- all_spec_simd_summary |> 
  filter(!specialty_description %in% c("All Specialties", "Mental Health (Grouped)")) |> 
  plot_ly(x = ~dna_count,
          y = ~specialty_description,
          color = ~selections,
          colors = chart_cols,
          type = 'bar') %>%
  layout(xaxis = list(title = "Appointments"),
         yaxis = list(title = "Specialty", 
                      categoryorder = "total ascending"),
         hovermode = "y unified")


# DNA Percentage 
all_specialties_dnaper_chart <- plot_ly(data = all_spec_simd_summary,
                                        x = ~dna_percent_chart,
                                        y = ~specialty_description,
                                        color = ~selections,
                                        colors = chart_cols,
                                        type = 'bar') %>%
  layout(xaxis = list(title = "Appointments",
                      tickformat = ".0%" ),
         yaxis = list(title = "Specialty", 
                      categoryorder = "total ascending"),
         hovermode = "y unified")


# Specialty + Year + SIMD
all_spec_simd_year_mh <- all_appointments_simd %>%
  filter(specialty %in% select_specialties) |> 
  mutate(specialty_description = "Mental Health (Grouped)") |> 
  group_by(location, date_year, specialty_description, simd_quintile) %>%
  summarise(total_appointments = sum(total_appointments),
            dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dna_percent = percent(dna_count / total_appointments, .1)) %>%
  arrange(desc(dna_percent)) 

all_spec_simd_year_all <- all_appointments_simd %>%
  mutate(specialty_description = "All Specialties") |> 
  group_by(location, date_year, specialty_description, simd_quintile) %>%
  summarise(total_appointments = sum(total_appointments),
            dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dna_percent = percent(dna_count / total_appointments, .1)) %>%
  arrange(desc(dna_percent)) 


all_spec_simd_year <- all_appointments_simd %>%
  group_by(location, date_year, specialty_description, simd_quintile) %>%
  summarise(total_appointments = n(),
            dna_count = sum(clinic_attendance == 8, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(dna_percent = percent(dna_count / total_appointments, .1)) %>%
  arrange(desc(dna_percent)) |> 
  bind_rows(all_spec_simd_year_all, all_spec_simd_year_mh) 


mh_group_selection <- all_appointments_simd |> 
  filter(selections == "Mental Health") |> 
  distinct(specialty_description) |> 
  pull()

#test table to check formatting
all_spec_simd_dnaper_pivot <- all_spec_simd_year |>
  # filter(specialty_description == "General Psychiatry") |>
  select(location, specialty_description, date_year, simd_quintile, dna_percent) |>
  pivot_wider(names_from = date_year, values_from = dna_percent) |>
  arrange(location, specialty_description, simd_quintile) |>
  filter(!is.na(simd_quintile)) |>
  select(location, specialty_description, simd_quintile, `2023`, `2024`, `2025`) |> 
  rename(`Specialty Description` = specialty_description,
         `SIMD Quintile` = simd_quintile)


all_spec_simd_appt_pivot <- all_spec_simd_year |>
  # filter(specialty_description == "General Psychiatry") |>
  select(location, specialty_description, date_year, simd_quintile, total_appointments) |>
  pivot_wider(names_from = date_year, values_from = total_appointments) |>
  arrange(location, specialty_description, simd_quintile) |>
  filter(!is.na(simd_quintile)) |>
  select(location, specialty_description, simd_quintile, `2023`, `2024`, `2025`) |> 
  rename(`Specialty Description` = specialty_description,
         `SIMD Quintile` = simd_quintile)



# SIMD Chart --------------------------------------------------------------

simd_mh_spec_data <- all_spec_simd_year |> 
  filter(specialty_description %in% c("All Specialties", "Mental Health (Grouped)"),
         location == "The Tweed") |> 
  group_by(specialty_description, simd_quintile) |> 
  summarise(total_appointments = sum(total_appointments),
            dna_count = sum(dna_count),
            .groups = "drop") %>%
  mutate(dna_percent = percent(dna_count / total_appointments, .1),
         dna_percent_chart = dna_count / total_appointments)

simd_mh_spec_chart <- plot_ly(data = simd_mh_spec_data,
                                        x = ~specialty_description,
                                        y = ~dna_percent_chart ,
                                        color = ~simd_quintile,
                                        colors = chart_cols,
                                        type = 'bar') %>%
  layout(xaxis = list(title = "",
                      categoryorder = "total ascending"),
         yaxis = list(title = "DNA Percentage",
                      tickformat = ".1%" ),
         legend = list(
           title = list(
             text = "SIMD Quintile"
           )
         ),
         hovermode = "x unified")


# Mode of Clinic Attendance -----------------------------------------------
colnames(all_appointments_mode_raw)

all_appointments_mode_atten <- all_appointments_mode_raw |> 
  clean_names() |> 
  filter(specialty %in% keep_specialties_list) |> 
  left_join(specialty_lookup) |> 
  mutate(date_year = year(clinic_date)) |> 
  group_by(specialty_description, clinic_attendance, mode_of_clinical_interaction) |> 
  summarise(value = sum(total_appointments)) |> 
  ungroup() |> 
  filter(clinic_attendance == 1) |> 
  group_by(specialty_description) |> 
  mutate(total_appt = sum(value)) |>
  ungroup() |> 
  mutate(perc = value / total_appt) |> 
  mutate(mode_of_clin = case_when(mode_of_clinical_interaction == 1 ~ "Face to Face",
                                  mode_of_clinical_interaction == 2 ~ "Telephone",
                                  mode_of_clinical_interaction == 3 ~ "Videolink",
                                  mode_of_clinical_interaction == 4 ~ "Written")) 

specialty_order <- all_appointments_mode_atten %>%
  filter(mode_of_clin == "Face to Face") %>%
  arrange(perc) %>%
  pull(specialty_description)

spec_mode_of_contact <- plot_ly(data = all_appointments_mode_atten,
                                        x = ~perc,
                                        y = ~specialty_description,
                                        color = ~mode_of_clin,
                                        colors = chart_cols,
                                        type = 'bar') %>%
  layout(xaxis = list(title = "Percentage",
                      tickformat = ".0%"),
         yaxis = list(title = "", 
                      categoryorder = "array",
                      categoryarray = specialty_order),
         barmode = 'stack',
         hovermode = "y unified")



  
  
  
  
  
