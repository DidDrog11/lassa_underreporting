data <- readxl::read_xlsx(here("data", "suspected_confirmed_deaths.xlsx"))

cfr_1 <- data %>%
  dplyr::select(year, country, region, suspected_cases, confirmed_cases, deaths_among_confirmed) %>%
  drop_na(deaths_among_confirmed) %>%
  group_by(year, country) %>%
  mutate(records = n()) %>%
  filter(!(country == "nigeria" & region == "all" & records > 2)) %>% # remove nigeria all if by state is available
  ungroup() %>%
  mutate(cases = case_when(is.na(confirmed_cases) ~ suspected_cases,
                           deaths_among_confirmed > confirmed_cases ~ suspected_cases,
                           TRUE ~ confirmed_cases)) %>%
  rowwise() %>%
  mutate(cfr = case_when(is.nan(deaths_among_confirmed/cases) ~ 0,
                         TRUE ~ deaths_among_confirmed/cases),
         cfr_type = "summary_cfr") %>%
  ungroup()

weighted_cfr_1 <- cfr_1 %>%
  filter(cfr != 0 & cfr != 1) %>%
  summarise(total_cases = sum(cases),
            total_deaths = sum(deaths_among_confirmed),
            summary_cfr = wtd.mean(cfr, weights = cases),
            sd_cfr = sqrt(wtd.var(cfr, cases))) %>%
  mutate(cfr_type = "summary_cfr")

cfr_2 <- data %>%
  dplyr::select(year, country, region, suspected_cases, confirmed_cases, deaths_among_confirmed) %>%
  drop_na(deaths_among_confirmed) %>%
  group_by(year, country) %>%
  mutate(records = n()) %>%
  filter(!(country == "nigeria" & region == "all" & records > 2)) %>%
  ungroup() %>%
  filter(year < 2021 & country == "nigeria") %>%
  mutate(cases = case_when(is.na(confirmed_cases) ~ suspected_cases,
                           deaths_among_confirmed > confirmed_cases ~ suspected_cases,
                           TRUE ~ confirmed_cases)) %>%
  rowwise() %>%
  mutate(cfr = case_when(is.nan(deaths_among_confirmed/cases) ~ 0,
                         TRUE ~ deaths_among_confirmed/cases),
         cfr_type = "ncdc_cfr") %>%
  ungroup()

weighted_cfr_2 <- cfr_2 %>%
  filter(cfr != 0 & cfr != 1) %>%
  summarise(total_cases = sum(cases),
            total_deaths = sum(deaths_among_confirmed),
            summary_cfr = wtd.mean(cfr, cases),
            sd_cfr = sqrt(wtd.var(cfr, cases))) %>%
  mutate(cfr_type = "ncdc_cfr")

cfr_3 <- data %>%
  dplyr::select(year, country, region, suspected_cases, confirmed_cases, deaths_among_confirmed) %>%
  drop_na(deaths_among_confirmed) %>%
  group_by(year, country) %>%
  mutate(records = n()) %>%
  filter(!(country == "nigeria" & region == "all" & records > 2)) %>%
  ungroup() %>%
  filter(year < 2021 & year >= 2017 & region %in% c("edo", "ondo")) %>%
  mutate(cases = case_when(is.na(confirmed_cases) ~ suspected_cases,
                           deaths_among_confirmed > confirmed_cases ~ suspected_cases,
                           TRUE ~ confirmed_cases)) %>%
  rowwise() %>%
  mutate(cfr = case_when(is.nan(deaths_among_confirmed/cases) ~ 0,
                         TRUE ~ deaths_among_confirmed/cases),
         cfr_type = "ncdc_sub_cfr") %>%
  ungroup()

weighted_cfr_3 <- cfr_3 %>%
  filter(cfr != 0 & cfr != 1) %>%
  summarise(total_cases = sum(cases),
            total_deaths = sum(deaths_among_confirmed),
            summary_cfr = wtd.mean(cfr, cases),
            sd_cfr = sqrt(wtd.var(cfr, cases))) %>%
  mutate(cfr_type = "ncdc_sub_cfr") 

combined_cfr <- bind_rows(cfr_1, cfr_2, cfr_3) %>%
  filter(cfr != 0 & cfr != 1) %>%
  mutate(cfr_name = case_when(cfr_type == "summary_cfr" ~ "Method 1\nCombined CFR",
                              cfr_type == "ncdc_cfr" ~ "Method 2\nNCDC CFR",
                              cfr_type == "ncdc_sub_cfr" ~ "Method 3\nEdo and Ondo CFR"))

combined_weighted_cfr <- bind_rows(weighted_cfr_1, weighted_cfr_2, weighted_cfr_3) %>%
  mutate(cfr_name = case_when(cfr_type == "summary_cfr" ~ "Method 1\nCombined CFR",
                              cfr_type == "ncdc_cfr" ~ "Method 2\nNCDC CFR",
                              cfr_type == "ncdc_sub_cfr" ~ "Method 3\nEdo and Ondo CFR"))

fig_1a <- combined_cfr %>%
  ggplot(aes(x = cfr, y = cfr_name, fill = cfr_name)) +
  stat_slab(aes(thickness = stat(pdf * n)),
            scale = 0.7) +
  stat_dots(side = "bottom", 
            position = "dodgejust",
            scale = 0.4, 
            slab_size = NA) +
  annotate("rect", xmin = 0.254, xmax = 0.258, ymin = 1, ymax = 1.8) +
  annotate("rect", xmin = 0.250, xmax = 0.254, ymin = 2, ymax = 2.8) +
  annotate("rect", xmin = 0.163, xmax = 0.167, ymin = 3, ymax = 3.8) +
  labs(x = "Case Fatality Rate (%)",
       y = element_blank()) + 
  scale_fill_manual(values = c("#e41a1c", "#377eb8", "#984ea3")) +
  theme_bw() +
  theme(legend.position = "none")

estimated_cases <- data %>%
  dplyr::select(year, country, region, suspected_cases, confirmed_cases, deaths_among_confirmed) %>%
  group_by(year, country) %>%
  mutate(records = n()) %>%
  filter(!(region != "all")) %>% # only keep nigeria country totals
  ungroup() %>%
  mutate(cases = case_when(is.na(confirmed_cases) ~ suspected_cases,
                           deaths_among_confirmed > confirmed_cases ~ suspected_cases,
                           TRUE ~ confirmed_cases)) %>%
  mutate(estimated_cases_1 = round(deaths_among_confirmed / combined_weighted_cfr$summary_cfr[combined_weighted_cfr$cfr_type == "summary_cfr"], 0),
         estimated_cases_2 = round(deaths_among_confirmed / combined_weighted_cfr$summary_cfr[combined_weighted_cfr$cfr_type == "ncdc_cfr"], 0),
         estimated_cases_3 = round(deaths_among_confirmed / combined_weighted_cfr$summary_cfr[combined_weighted_cfr$cfr_type == "ncdc_sub_cfr"], 0)) %>%
  pivot_longer(cols = c(contains("estimated"), "cases"), names_to = "estimate", values_to = "cases") %>%
  mutate(sd = case_when(estimate == "estimated_cases_1" ~ round(cases * combined_weighted_cfr$sd_cfr[combined_weighted_cfr$cfr_type == "summary_cfr"], 0),
                        estimate == "estimated_cases_2" ~ round(cases * combined_weighted_cfr$sd_cfr[combined_weighted_cfr$cfr_type == "ncdc_cfr"], 0),
                        estimate == "estimated_cases_3" ~ round(cases * combined_weighted_cfr$sd_cfr[combined_weighted_cfr$cfr_type == "ncdc_sub_cfr"], 0))) %>%
  dplyr::select(year, country, estimate, cases, sd, deaths_among_confirmed)

country_underreporting <- estimated_cases %>%
  drop_na(deaths_among_confirmed) %>%
  group_by(country, estimate) %>%
  summarise(cases = sum(cases)) %>%
  pivot_wider(names_from = estimate, values_from = cases) %>%
  mutate(underreporting_3 = cases/estimated_cases_3)

under_reporting <- estimated_cases %>%
  filter(!country %in% c("burkina faso", "mali")) %>%
  group_by(country, estimate) %>%
  summarise(cases = sum(cases)) %>%
  filter(estimate %in% c("cases", "estimated_cases_3")) %>%
  pivot_wider(names_from = estimate, values_from = cases) %>%
  mutate(estimated_cases_3 = case_when(is.na(estimated_cases_3) ~ round(cases / country_underreporting$underreporting_3[country_underreporting$country == country], 0),
                                       TRUE ~ estimated_cases_3),
         difference = cases/estimated_cases_3)

country_order <- data %>%
  group_by(country) %>%
  summarise(cases = sum(confirmed_cases, na.rm = TRUE)) %>%
  arrange(-cases)

fig_1b <- under_reporting %>%
  mutate(country = factor(str_to_sentence(country), levels = str_to_sentence(country_order$country)),
         difference = paste0(round(difference * 100, 0), "%")) %>%
  pivot_longer(cols = c("cases", "estimated_cases_3"), names_to = "measure", values_to = "cases") %>%
  mutate(name = factor(case_when(measure == "estimated_cases_3" ~ "Expected cases\nMethod 3 CFR",
                                 measure == "cases" ~ "Reported cases")),
         country = factor(paste0(country, ": ", difference)),
         country = fct_reorder(country, cases, min)) %>%
  ggplot() +
  geom_col(aes(x = country, y = cases, fill = fct_rev(name)), position = "dodge") +
  facet_wrap(~ country, scales = "free", ncol = 2) +
  scale_fill_manual(values = c("#4daf4a", "#984ea3")) +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  labs(y = "Cases",
       x = element_blank(),
       fill = element_blank()) +
  theme_bw()

shift_legend2 <- function(p) {
  # ...
  # to grob
  gp <- ggplotGrob(p)
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  
  # establish name of empty panels
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  names <- empty.facet.panels$name
  # example of names:
  #[1] "panel-3-2" "panel-3-3"
  
  # now we just need a simple call to reposition the legend
  lemon::reposition_legend(p, 'center', panel=names)
}

fig_1b <- shift_legend2(fig_1b)

fig_1 <- plot_grid(plotlist = list(fig_1a, fig_1b), labels = c("A", "B"))

save_plot(plot = fig_1, filename = here("report", "fig_1.png"), base_height = 10, base_width = 8)
save_plot(plot = fig_1, filename = here("report", "fig_1.pdf"), base_height = 10, base_width = 8)
