
univ <- read.csv("C:\\Users\\HP\\Downloads\\cwurData (1).csv")
head(univ)


str(univ)


summary(univ)


colnames(univ)


dim(univ)


library(dplyr)
library(readr)
library(tidyr)
library(skimr)
library(ggplot2)


colSums(is.na(univ))


data_clean <-univ %>% drop_na()


sum(duplicated(univ))


univ %>% summarise(across(everything(), ~ sum(is.na(.))))


univ %>% duplicated() %>% sum()


univ_clean <- univ %>% distinct()


univ_clean <- univ_clean %>% filter(!is.na(score))


univ_clean <- univ_clean %>%
  mutate(patents = ifelse(is.na(patents), mean(patents, na.rm = TRUE), patents))
head(univ_clean)


univ_sel <- univ_clean %>% select(institution, country, quality_of_education, publications, quality_of_faculty, score)
head(univ_sel)


univ_top100 <- univ_clean %>% filter(score <= 100)
head(univ_top100)


univ_selected <- univ_top100 %>% 
  select(world_rank, institution, country, score, year)
head(univ_selected)


univ_new <- univ_sel %>%
  mutate(Rank_Category = ifelse(score <= 100, "Top 100", "Beyond 100"))
head(univ_new)


univ %>%
  group_by(publications) %>%
  summarise(
    count = n(),
    mean_score = mean(score),
    sd_score = sd(score),
    
    
  )


IQR(univ$education)

univ_filtered <- univ %>% filter(quality_of_education > 50)


univ_select <- univ_filtered %>% select(quality_of_education,quality_of_faculty,publications)

univ_mutated <- univ_select %>%
  mutate(University_Standard =  quality_of_faculty/publications)

univ_filtered <-univ_mutated  %>% filter(University_Standard  > 4)






summary(univ_clean %>% select(world_rank, score, year))


skim(univ_clean %>% select(world_rank, score))

pairs(univ [, 5:8], main = "Scatterplot Matrix of univ Data", col = univ$score)


ggplot(univ_clean, aes(x = world_rank)) +
  geom_histogram(binwidth = 100, fill = "steelblue", color = "white") +
  labs(title = "Distribution of World Rankings",
       x = "World Rank", y = "Number of Universities")



ggplot(univ_clean, aes(x = score)) +
  geom_histogram(binwidth = 5, fill = "darkgreen", color = "white") +
  labs(title = "Distribution of Scores",
       x = "Score", y = "Number of Universities")



ggplot(univ_clean, aes(x = publications)) +
  geom_histogram(binwidth = 5, fill = "darkgreen", color = "white") +
  labs(title = "Distribution of Scores",
       x = "Publications", y = "Number of Universities")





ggplot(univ_clean, aes(x = patents)) +
  geom_histogram(binwidth = 5, fill = "darkgreen", color = "white") +
  labs(title = "Distribution of Scores",
       x = "Patents", y = "Number of Universities")





ggplot(univ_clean, aes(x = world_rank, y = score)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Relationship Between World Rank and Score",
       x = "World Rank", y = "Score")

univ_clean %>%
  filter(world_rank <= 100) %>%
  count(country, sort = TRUE) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(country, n), y = n)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(title = "Top Countries by Number of Universities in Top 100",
       x = "Country", y = "Number of Universities")



ggplot(univ, aes(x = quality_of_education, y = quality_of_faculty)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Education vs Faculty", x = "Quality of Edu", y = "Quality of Fac")

ggplot(univ, aes(x = quality_of_education, y = publications)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Education vs Publications", x = "Quality of Edu", y = "publications")

ggplot(univ, aes(x = publications, y = score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Publications VS Score", x = "publications", y = "score")

ggplot(univ, aes(y = patents)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot of patents", y = "Patents")


numeric_data <- univ_clean %>%
  select(publications, influence, citations, patents, score)


cor_df <- as.data.frame(as.table(cor(numeric_data, use = "complete.obs")))


ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlation Heatmap of University Metrics",
       x = "", y = "", fill = "Correlation") +
  theme_minimal()


univ_clean %>%
  filter(country %in% c("USA", "United Kingdom", "Canada")) %>%  # choose some countries
  group_by(country, year) %>%
  summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = factor(year), y = mean_score, fill = country)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average Score by Country and Year",
       x = "Year", y = "Average Score", fill = "Country") +
  theme_minimal()

