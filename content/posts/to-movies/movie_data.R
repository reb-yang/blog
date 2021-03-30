library(tidyverse)
library(ggrepel)
library(ggimage)

movies_raw <- read_csv("content/posts/to-movies/inputs/toronto_movies.csv")


movies <- movies_raw %>% 
  select(title, actors, awards, director, genre, imdb_rating, 
         imdb_votes, metascore, language, plot, rated, released, runtime, writer, year, production) %>% 
  mutate_if(is.character, list(~na_if(., "N/A"))) %>%
  mutate(imdb_votes = as.numeric(gsub("," ,"", imdb_votes)),
         imdb_rating = as.numeric(imdb_rating),
         metascore = as.numeric(metascore),
         oscar = ifelse(str_detect(awards, "Oscar"), "yes", "no"))

# compare imdb rating to metascore 
movie_ratings <- movies %>% 
  filter(!is.na(metascore)) %>% 
  filter(!is.na(imdb_rating)) %>% 
  mutate(scale_meta = metascore/10)

movie_ratings_avg <- movie_ratings %>% 
  mutate(log_votes = log(imdb_votes),
         meta_avg = ifelse(scale_meta > mean(movie_ratings$scale_meta), "above", "below"),
         rating_avg = ifelse(imdb_rating > mean(movie_ratings$imdb_rating), "above", "below"),
         agree = ifelse(meta_avg == rating_avg, "agree", "disagree"),
         diff = abs(scale_meta - imdb_rating))

fit <- lm(scale_meta ~ imdb_rating, data = movie_ratings)
purple_green <- colorRampPalette(colors = c("#14631e", "grey50", "#781f87"))
critics_avg <- round(mean(movie_ratings_avg$scale_meta), 2)
user_avg <- round(mean(movie_ratings_avg$imdb_rating), 2)
movie_ratings_avg$residuals <- fit$residuals

oscar_img <- "content/posts/to-movies/inputs/oscar_image.png"
movie_ratings_avg$image <- oscar_img

plot1 <- movie_ratings_avg %>% 
  ggplot(aes(x = imdb_rating, y = scale_meta)) +
  geom_hline(yintercept = mean(movie_ratings$scale_meta), lwd = 0.3, lty = 2) +
  geom_vline(xintercept = mean(movie_ratings$imdb_rating), lwd = 0.3, lty = 2) +
  geom_point(aes(color = residuals), size = 4, alpha = 0.7) +
  geom_image(data = movie_ratings_avg %>% filter(oscar == "yes"),
             aes(image=image), size=.017) + 
  scale_color_gradientn(colors = c('#007a00', '#6eb87c', '#a3c6a8', 
                                   '#d4d4d4', '#cf9db6', '#c46299', '#b4007c'),
                        breaks = (c(-3.7, 0, 2.5)),
                        labels = c("Fans liked better", "Agree", "Critics liked better")) + 
  theme_minimal(base_family = "Sans") + 
  scale_y_continuous(limits = c(1, 10), breaks = seq(1,10, by = 1)) + 
  scale_x_continuous(limits = c(1, 10), breaks = seq(1,10, by = 1)) + 
  labs(x = "IMDb User Rating", y = "Critic's Metascore (scaled)", 
       title = "Do Critics and Fans Agree on the Best and Worst Movies\nFilmed in Toronto?",
       caption = "Source: Wikipedia & IMDb",
       color = "") +
theme(panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 24),
      text = element_text(family = "Lobster"),
      panel.background = element_rect(fill = "grey99"),
      plot.background = element_rect(fill = "grey99"),
      legend.position = c(0.1, .85)) 



plot2 <- plot1 +
  geom_label_repel(data=movie_ratings_avg %>% filter(residuals > 2.5), # Filter data first
    aes(label=title,
        family = "mono"),
    nudge_y = 0.3, nudge_x = -0.3, size = 3) +
  geom_label_repel(data=movie_ratings_avg %>% filter(residuals < -3), # Filter data first
                   aes(label=title,
                       family = "mono"),
                   nudge_y = -0.5, size = 3) +
  geom_label_repel(data=movie_ratings_avg %>% filter(imdb_rating < 4 & scale_meta < 2),
                   aes(label=title,
                       family = "mono"),
                   nudge_y = -0.3, nudge_x = -0.4, size = 3) +
  geom_label_repel(data=movie_ratings_avg %>% filter(imdb_rating > 8),
                   aes(label=title,
                       family = "mono"),
                   nudge_y = 0.4, nudge_x = 0.5, size = 3) +
  geom_label_repel(data=movie_ratings_avg %>% filter(scale_meta >= 9),
                   aes(label=title,
                       family = "mono"),
                   nudge_y = 0.4, nudge_x = 0.5, size = 3)



plot3 <- plot2 +
  geom_text(aes(label = "These movies were received\nbetter by critics than fans",
                x = 4, y= 8.5, family = "mono")) + 
  geom_text(aes(label = paste("Average\n", critics_avg),
                y = critics_avg, x = 1, family = "mono"), size = 3) + 
  geom_text(aes(label = paste("Average\n", user_avg),
                x = user_avg, y = 10, family = "mono"), size = 3) +
  geom_text(aes(label = "These fan favorites\nwere a flop with\ncritics",
                x = 9, y= 2, family = "mono")) +
  geom_label_repel(data = movie_ratings_avg %>% filter(title == "Suicide Squad"),
                   aes(label = "Suicide Squad won\nan Oscar for best\nMakeup and Hairstyling",
                family = "mono"), size = 2.5, nudge_y = 2.5, nudge_x = -2.3) +
  geom_label_repel(data = movie_ratings_avg %>% filter(title == "Silver Streak"),
                   aes(label = "Silver Streak was\nnominated for\nbest Sound",
                       family = "mono"), size = 2.5, nudge_y = -0.4, nudge_x = 1.7) +
  geom_image(aes(x = 1, y = 7, image=oscar_img), size=.018) +
  geom_text(aes(label = "= Oscar nominee/winner",
                y = 7, x = 1.8, family = "Lobster"), size = 3) 


plot3


