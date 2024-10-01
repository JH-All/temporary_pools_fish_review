# Packages and data ---------------
library(readxl)
library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(segmented)

data <- read_excel("raw_data.xlsx")

# Figure 1 ---------------------------
year_count <- data %>% 
  count(year)

Figure_1 <- year_count %>% 
  arrange(year) %>%  
  mutate(cumulative_n = cumsum(n)) %>%  
  ggplot(aes(x = year, y = cumulative_n)) +
  geom_line(color = "black", size = 1.5)+
  geom_point(size = 7, pch = 21, bg = "black",
             col = "white", alpha = 1)+
  scale_y_continuous(expand = c(0.01,0), breaks = seq(0, 110, by = 10),
                     limits = c(0,115))+
  scale_x_continuous(breaks = seq(1980, 2023, by = 1),
                     limits = c(1980, 2023.5),
                     expand = c(0,0))+
  theme_linedraw(base_size = 15)+
  theme(axis.text.x=element_text( 
    angle = 45, hjust = 1))+
  labs(x = NULL, y = "Cumulative number of studies")


ggsave("Figure_1.tiff", width = 11, height = 8)

# Figure 2 ---------------------------
## A)
country_count <- data %>% 
  count(country)

country_df <- country_count[-19,]
world_map <- map_data(map = "world")

country_df$country <- as.factor(country_df$country)
levels(country_df$country)[levels(country_df$country) == "United States"] <- "USA"

f2a <- ggplot(country_df) +
  geom_map(aes(map_id = country, 
               fill = n), map = world_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat,
                                     group = group),
               colour = 'black', fill = NA) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_viridis_c(name = "Number \nof studies")+
  coord_fixed()+
  theme_map()

## B)
country_year_count <- data %>% 
  count(country, year)

country_year_count <- country_year_count[-c(65:69),]

top_countries <- country_year_count %>%
  group_by(country) %>%
  summarise(total_studies = sum(n)) %>%
  arrange(desc(total_studies)) %>%
  slice_head(n = 5) %>%
  pull(country)

country_year_count <- country_year_count %>%
  mutate(country = if_else(country %in% top_countries, as.character(country), "Others")) %>%
  filter(country %in% c(top_countries, "Others"))

country_year_count <- country_year_count %>%
  mutate(decade = case_when(
    year %in% 1981:1990 ~ "1981-1990",
    year %in% 1991:2000 ~ "1991-2000",
    year %in% 2001:2010 ~ "2001-2010",
    year %in% 2011:2020 ~ "2011-2020",
    year %in% 2021:2023 ~ "2021-2023"
  ))

result <- country_year_count %>%
  group_by(country, decade) %>%
  summarise(total_studies = sum(n))

result$country <- factor(result$country,
                         levels = c("Brazil", "Mozambique",
                                    "Uruguay", "United States", 
                                    "Mexico", "Others"))

f2b <- result %>% 
  ggplot(aes(x = country, y = total_studies,
             fill = decade)) +
  geom_bar(stat = "identity", position="stack",
           color = "black") +
  theme_linedraw(base_size = 18)+
  theme(axis.text.x=element_text( 
    angle = 45, hjust = 1,
  )
  )+
  labs(x = NULL, y = "Number of studies",
       fill = "Years")+
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0, 45, by = 5),
                     limits = c(0,45))+
  scale_fill_brewer(palette="BrBG",
                    direction = -1)

Figure_2 <- plot_grid(f2a, f2b,
                      nrow = 2, labels = "AUTO")

ggsave("Figure_2.jpg", width = 15, height = 13)

# Figure 3 -----------------------
## Groups
family_count <- data %>% 
  count(family)

family_count$perc <- (family_count$n / sum(family_count$n)) *100
family_count$relative <- family_count$n / sum(family_count$n)
sum(family_count$perc)

p1 <- family_count %>% 
  mutate(perc = round(perc, 2)) %>%
  ggplot(aes(x = reorder(family, perc), y = perc))+
  geom_bar(stat = "identity", width = 0.8,
           fill = "darkslategrey", color = "black")+
  labs(x = NULL, y = "Percentage of studies (%)")+
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 70, by = 10),
                     limits = c(0,70))+
  theme_classic(base_size = 18)+
  coord_flip()

p1

## Topics
topic_count <- data %>% 
  count(topic)

topic_count$perc <- (topic_count$n / sum(topic_count$n)) *100
topic_count$relative <- topic_count$n / sum(topic_count$n)
sum(topic_count$perc)

p2 <- topic_count %>% 
  mutate(perc = round(perc, 2)) %>%
  ggplot(aes(x = reorder(topic, perc), y = perc))+
  geom_bar(stat = "identity", width = 0.8,
           fill = "darkslategrey", color = "black")+
  labs(x = NULL, y = "Percentage of studies (%)")+
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 35, by = 5),
                     limits = c(0,35))+
  theme_classic(base_size = 18)+
  coord_flip()

Figure_3 <- plot_grid(p1, p2, nrow = 2, labels = "AUTO")

ggsave("Figure_3.tiff", width = 10, height = 12)

# Table 2 -----------------------
## Topics by family

df <- data %>%
  group_by(family, topic) %>%
  summarise(count = n()) %>%
  ungroup()


# Figure 4 -----------------------
## Group per year

specific_families <- c("Rivulidae",
                       "Nothobranchiidae", 
                       "Community")

family_counts <- data %>%
  filter(family %in% specific_families) %>% 
  group_by(family, year) %>%
  summarise(count = n()) %>%
  ungroup()

family_counts$family <- as.factor(family_counts$family)
levels(family_counts$family)
family_counts$family <- factor(family_counts$family, 
                               levels = rev(levels(family_counts$family)) )


Figure_4 = family_counts %>% 
  arrange(year) %>% 
  group_by(family) %>%
  mutate(cumulative_n = cumsum(count)) %>% 
  ggplot(aes(x = year, y = cumulative_n,
             color = family, group = family,
             fill = family)) +
  geom_line(size = 1.5,
            linetype = 21,
            alpha = 0.9, show.legend = FALSE)+
  geom_point(size = 5, pch = 21,
             col = "black",alpha = 0.9)+
  ylim(0,70)+
  labs(x = "Year", y = "Cumulative number of studies", 
       fill = "Fish clade") +
  #facet_wrap(~family, nrow = 3,
  #          scales = "free")+
  scale_fill_viridis_d(end = 0.5, direction = -1) +  
  scale_color_viridis_d(end = 0.5, direction = -1) +
  scale_x_continuous(breaks = seq(1980, 2023, by = 5))+
  theme_linedraw(base_size = 16)+
  theme(strip.text = element_text(size = 15),
        legend.position = c(0.02, 0.96),  
        legend.justification = c(0, 1),  
        legend.background = element_blank(), 
        legend.key = element_blank()) 

ggsave("Figure_4.tiff", width = 7, height = 6)

# Figure 5 -----------------------
## Topic per year

data$topic
topic_counts <- data %>%
  group_by(topic, year) %>%
  summarize(count = n(), .groups = "drop") %>% 
  complete(topic, year, fill = list(n = 0))

topic_counts$count <- replace(topic_counts$count, 
                              is.na(topic_counts$count), 0)

topic_counts$topic <- factor(topic_counts$topic,
                             levels = c("Reproduction",
                                        "Taxonomy",
                                        "Community ecology",
                                        "Genetics",
                                        "Physiology",
                                        "Trophic ecology",
                                        "Species interactions",
                                        "Behavior",
                                        "New occurrences",
                                        "Ecotoxicology"))

topic_counts$topic <- factor(topic_counts$topic, 
                             levels = rev(levels(topic_counts$topic)) )


textcol <- "black"

Figure_5 <- topic_counts %>%
  ggplot(aes(x = factor(year), y = factor(topic), 
             fill = count)) +
  geom_tile(color = "black", size = 1.1) +
  scale_fill_gradient(low = "white", high = "darkorchid4")+
  scale_y_discrete(expand=c(0, 0))+
  scale_x_discrete(expand = c(0,0))+
  theme_grey(base_size=18)+
  theme(legend.position="right", legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0, "cm")),
        legend.text=element_text(colour=textcol, size=20, face="bold"),
        legend.key.height=grid::unit(1.5, "cm"),
        legend.key.width=grid::unit(0.5, "cm"),
        axis.text.x=element_text(size=18, colour=textcol, 
                                 angle = 45, hjust = 1),
        axis.text.y=element_text(vjust=0.2, colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7, 0.4, 0.1, 0.2, "cm"),
        plot.title=element_text(colour=textcol, hjust=0, size=14, face="bold")
  )+ 
  labs(fill = NULL, x = NULL, y = NULL)

ggsave("Figure_5.jpg", width = 14, height = 8)

# Exponential regression ---------
year_count <- data %>% 
  count(year)

model <- lm(log(n) ~ year, data = year_count)
summary(model)

# Spearman correlations ---------------------
authors = read_excel("authors_per_year.xlsx")
authors$authors_per_paper_mean = as.numeric(authors$authors_per_paper_mean)
cor.test(authors$single_authors, authors$n_papers_year,
         method = "spearman")
cor.test(authors$authors_per_paper_mean, authors$n_papers_year,
         method = "spearman")

# Figure S1 ----------------------
aut1 = authors %>% 
  ggplot(aes(x = single_authors, y = n_papers_year))+
  geom_smooth(method = "glm",
              se = F, linetype = "dashed",
              color = "black")+
  geom_point(size = 4, shape = 21, fill = "cyan4")+
  theme_classic(base_size = 15)+
  scale_y_continuous(limits = c(0, 12),
                     breaks = seq(0, 12, by = 2))+
  labs(x = "Number of unique authors per year", 
       y = "Number of papers for each year")

aut2 = authors %>% 
  ggplot(aes(x = authors_per_paper_mean, y = n_papers_year))+
  geom_smooth(method = "glm",
              se = F, linetype = "dashed",
              color = "black")+
  geom_point(size = 4, shape = 21, fill = "cyan4")+
  theme_classic(base_size = 15)+
  scale_y_continuous(limits = c(0, 12),
                     breaks = seq(0, 12, by = 2))+
  labs(x = "Average number of authors per paper", 
       y = "Number of papers for each year")

aut3 = plot_grid(aut1, aut2, labels = "AUTO",
                 nrow = 1)
aut3
ggsave("Figure_S1.jpg", aut3, width = 10)

# Figure S2 --------------------
sp_year = read_excel("description_year_Rivulidae.xlsx")


sp_year_df = sp_year %>%
  group_by(description_year) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

min(sp_year_df$description_year)

fig_s2 = sp_year_df %>%
  ggplot(aes(x = description_year, y = count))+
  geom_bar(stat = "identity", width = 1,
           color = "black",
           fill = "cyan4")+
  scale_x_continuous(limits = c(1860, 2024),
                     breaks = seq(1860, 2024, by = 5))+
  scale_y_continuous(limits = c(0,4),
                     breaks = seq(0, 4, by = 1),
                     expand = c(0,0))+
  theme_classic(base_size = 12)+
  theme(axis.text.x=element_text( 
    angle = 90, hjust = 1))+
  labs(x = "Years", y =  "Number of Rivulidae species described")

ggsave("Figure_S2.jpg", fig_s2)

# Figure S3 ----------------------------
subfamily = read_excel("subfamily.xlsx")

subfamily %>% 
  count(subfamily)

subfamily_count <- subfamily %>%
  group_by(subfamily, topic) %>%
  summarize(count = n(), .groups = "drop") %>% 
  group_by(subfamily) %>%
  mutate(percentage = (count / sum(count)) * 100)

subfamily_count$topic = as.factor(subfamily_count$topic )
subfamily_count$topic = factor(subfamily_count$topic ,
                               levels = c("Taxonomy", "Reproduction", "Genetics",
                                          "Physiology", "Behavior", "Trophic ecology",
                                          "Community ecology", "New occurrences",
                                          "Species interactions"))

fig_s3 = subfamily_count %>% 
  ggplot(aes(x= subfamily, y = percentage,
             fill = topic))+
  geom_bar(stat = "identity", width = 0.6,
           color = "black")+
  scale_fill_brewer(palette = "Paired")+
  scale_y_continuous(expand = c(0,0))+
  labs(x = NULL, y = "Percentage (%)",
       fill = "Study topic")+
  theme_classic(base_size = 12)

ggsave("Figure_S3.jpg", fig_s3)

# Segmented regression ---------------------
rivulidae_data <- data %>% filter(family == "Rivulidae")
artigos_por_ano <- rivulidae_data %>% count(year)
modelo_linear <- lm(n ~ year, data = artigos_por_ano)

modelo_segmentado <- segmented(modelo_linear, seg.Z = ~year, psi = 2013)

modelo_segmentado_2012 <- segmented(modelo_linear, seg.Z = ~year, 
                                         psi = c(2012))
summary(modelo_segmentado)
summary(modelo_segmentado_2012)
plot(modelo_segmentado)
points(artigos_por_ano$year, artigos_por_ano$n)

modelo_segmentado_v2 <- segmented(modelo_linear, seg.Z = ~year)
summary(modelo_segmentado_v2)
plot(modelo_segmentado_v2,
     xlab = "Year", lwd = 2)
points(artigos_por_ano$year, artigos_por_ano$n,
       pch = 21, cex = 1.6, col = "black",
       bg = "grey"
)
?points

# Figure S4 ----------------
png("Figure_S4.png", width = 8, height = 6, units = "in", res = 300)

plot(modelo_segmentado_v2, xlab = "Year", lwd = 2)
points(artigos_por_ano$year, artigos_por_ano$n, 
       pch = 21, cex = 2, col = "black", bg = "grey")

dev.off()
