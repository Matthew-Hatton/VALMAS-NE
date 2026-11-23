# script to handle whaling data from IWC.
rm(list = ls()) #reset

library(tidyverse)
library(rnaturalearth)
library(lubridate)
library(scales)

land <- ne_countries(scale = "medium", returnclass = "sf")

source("./Code/R Scripts/@_Region file.R")

domain <- readRDS("./Objects/domain/Domains.RDS")

## whaling csvs - data is split pre and post moratorium so need to combine.
pre <- read.csv("./Objects/fishing/whaling/catches-preMoratorium.csv") %>% 
  subset(select = -c(X)) %>%  # delete eronious column
  mutate(Moratorium = "Pre")
post <- read.csv("./Objects/fishing/whaling/catches-postMoratorium.csv") %>% 
  mutate(Moratorium = "Post")

whaling <- rbind(pre,post)

areas <- read.csv("./Objects/fishing/whaling/summary_catch.csv") %>% 
  st_as_sf(wkt = "wkt",crs = crs)
st_geometry(areas) <- "geometry"

ggplot() +
  geom_sf(data = areas, aes(fill = AreaName)) +
  geom_sf(data = land, fill = "grey80", colour = NA) +
  coord_sf(xlim = st_bbox(areas)[c("xmin", "xmax")],
           ylim = st_bbox(areas)[c("ymin", "ymax")]) # whole world - all data

ggsave("./Figures/fishing/IWCAreas.png",width = 12,height = 7)

# crop to our domain
domain_in <- st_intersection(areas,domain)
ggplot() +
  geom_sf(data = domain_in, aes(fill = AreaName)) +
  geom_sf(data = land, fill = "grey80", colour = NA) +
  coord_sf(xlim = st_bbox(domain_in)[c("xmin", "xmax")],
           ylim = st_bbox(domain_in)[c("ymin", "ymax")]) # whole world - all data
ggsave("./Figures/fishing/IWCAreasInterest.png",width = 12,height = 7)

areas_of_interest <- domain_in$AreaName

whaling_cropped <- whaling %>% 
  filter(Area %in% areas_of_interest) %>% 
  mutate(
    Syear  = as.integer(as.character(Syear)),
    Smonth = match(tolower(trimws(Smonth)), tolower(month.abb)),
    Sday   = suppressWarnings(as.integer(as.character(Sday))),
    Sday   = if_else(is.na(Sday) | Sday < 1 | Sday > 31, 1L, Sday),
    date   = make_date(Syear, Smonth, Sday)
  ) %>%  # make date
  pivot_longer(cols = TBlue:Oth,names_to = "Species",values_to = "Catch")

ggplot(
  data = whaling_cropped %>% filter(Species == "Total"),
  aes(x = date, y = Catch, colour = Moratorium)
) +
  geom_line(linewidth = 0.6, alpha = 0.7) +
  geom_point(size = 1, alpha = 0.9) +
  annotate("rect",
           xmin = as.Date("2010-01-01"), xmax = as.Date("2017-12-31"),
           ymin = -Inf, ymax = Inf,
           fill = "grey50", alpha = 0.2) +
  scale_x_date(
    breaks = function(x) c(breaks_width("10 years")(x), as.Date("2017-01-01")),
    date_labels = "%Y",
    expand = expansion(mult = 0.02)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_colour_brewer(palette = "Set1", name = "Moratorium") +
  labs(
    title = "Whale catch in the North Sea",
    subtitle = "Total catch over time, by moratorium status",
    x = NULL,
    y = "Catch (number of whales)",
    caption = "Source: IWC catch database"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey35"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(colour = "grey40"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(colour = "grey50", hjust = 0)
  )

ggsave("./Figures/fishing/WhalingAllYears.png",width = 12,height = 7)
write.csv(whaling_cropped,"./Objects/fishing/whaling/WhalesInDomainAllYears.csv")

whaling_recent <- whaling_cropped %>%
  filter(Species == "Total",
         date >= as.Date("2011-01-01"),
         date <= as.Date("2017-12-31"))

ggplot(whaling_recent, aes(x = date, y = Catch, colour = Moratorium)) +
  geom_line(linewidth = 0.6, alpha = 0.7) +
  geom_point(size = 1, alpha = 0.9) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = 0.02)) +
  scale_y_continuous(labels = label_comma(),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_colour_brewer(palette = "Set1", name = "Moratorium") +
  labs(
    title = "Whale catch in the North Sea, 2011-2017",
    subtitle = "Total catch over time, by moratorium status",
    x = NULL,
    y = "Catch (number of whales)",
    caption = "Source: IWC catch database"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey35"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(colour = "grey40"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(colour = "grey50", hjust = 0)
  )
ggsave("./Figures/fishing/WhalingCroppedYears.png",width = 12,height = 7)

## IWC only report whale catch in the entire area, not individual polygons. Scale total whale catch in region down to just the domain using the ratio of their areas.
# In fact, let's scale each area
area_tbl <- areas %>%
  mutate(area = as.numeric(st_area(geometry))) %>%
  st_drop_geometry() %>%
  group_by(AreaName) %>%
  summarise(area = sum(area), .groups = "drop")

domain_tbl <- domain_in %>%
  mutate(domain_area = as.numeric(st_area(geometry))) %>%
  st_drop_geometry() %>%
  group_by(AreaName) %>%
  summarise(domain_area = sum(domain_area), .groups = "drop")

area_ratios <- area_tbl %>%
  left_join(domain_tbl, by = "AreaName") %>%
  mutate(domain_area = coalesce(domain_area, 0),
         ratio = domain_area / area) %>% 
  filter(AreaName %in% areas_of_interest)

# scale total whaling by these ratios
whaling_scaled <- whaling_cropped %>%
  left_join(area_ratios %>% dplyr::select(AreaName, ratio),
            by = c("Area" = "AreaName")) %>%
  mutate(Catch_domain = if_else(Species == "Total", Catch * ratio, NA_real_))

whaling_domain <- whaling_scaled %>%
  filter(Species == "Total", !is.na(date)) %>%
  group_by(date, Moratorium) %>%
  summarise(Catch_domain = sum(Catch_domain, na.rm = TRUE), .groups = "drop")

ggplot(whaling_domain, aes(x = date, y = Catch_domain, colour = Moratorium)) +
  geom_line(linewidth = 0.6, alpha = 0.7) +
  geom_point(size = 1, alpha = 0.9) +
  annotate("rect",
           xmin = as.Date("2010-01-01"), xmax = as.Date("2017-12-31"),
           ymin = -Inf, ymax = Inf,
           fill = "grey50", alpha = 0.2) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y",
               expand = expansion(mult = 0.02)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_brewer(palette = "Set1", name = "Moratorium") +
  labs(
    title = "Whale catch within the model domain",
    subtitle = "Total catch scaled by the fraction of each IWC area inside the domain",
    x = NULL,
    y = "Catch in domain (number of whales)",
    caption = "Source: IWC catch database, area-scaled"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey35"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(colour = "grey40"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(colour = "grey50", hjust = 0)
  ) +
  NULL
ggsave("./Figures/fishing/WhalingAllYearsInDomain.png",width = 12,height = 7)


ggplot(whaling_domain %>% filter(between(year(date),2011,2019)), aes(x = date, y = Catch_domain, colour = Moratorium)) +
  geom_line(linewidth = 0.6, alpha = 0.7) +
  geom_point(size = 1, alpha = 0.9) +
  scale_y_continuous(expand = c(0,0)) +
  scale_colour_brewer(palette = "Set1", name = "Moratorium") +
  labs(
    title = "Whale catch within the model domain",
    subtitle = "Total catch scaled by the fraction of each IWC area inside the domain",
    x = NULL,
    y = "Catch in domain (number of whales)",
    caption = "Source: IWC catch database, area-scaled"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey35"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(colour = "grey40"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(colour = "grey50", hjust = 0)
  ) +
  NULL
ggsave("./Figures/fishing/WhalingAllYearsInDomain.png",width = 12,height = 7)

write.csv(whaling_domain,"./Objects/fishing/whaling/WhalingInDomain.csv",row.names = F)