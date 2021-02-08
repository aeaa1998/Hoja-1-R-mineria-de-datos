library("ggpubr")
library("ggplot2")
theme_set(theme_pubr())

library(dplyr)



movies <- read.csv("./movie.csv")
view(movies)
nrow(movies) #cantidad de filas
ncol(movies) #cantidad e columnas
str(movies)
summary(movies)


budget <- movies$budget 
order(budget, decreasing = F)
str(budget)
#1
summary(movies)
plot(movies$popularity, type = "l")


#medias

ggdensity(movies$vote_average, 
          main = "votes avg",
          xlab = "avg")

hist(movies$popularity)
order(movies$revenue,na.last = TRUE,decreasing = TRUE)
head(sort(movies$revenue,decreasing = T),10)

#4.2
ganancias <- head(sort(movies$revenue,decreasing = T),10)

x<-movies[movies$revenue %in% ganancias,"original_title"]
print(x)

#4.1 
preup <- head(sort(movies$budget,decreasing = T),10)

y<-movies[movies$budget %in% preup,"original_title"]
print(y)

#4.3

votes <- head(sort(movies$vote_count,decreasing = T),1)

vcount<-movies[movies$vote_count %in% votes,"original_title"]
print(vcount)

#4.4

avg <- head(sort(movies$vote_average,decreasing = F),1)

worst<-movies[movies$vote_average %in% avg,"original_title"]
print(worst)

#4.5
# ggplot(movies,aes(release_year) ) +
#   geom_bar(fill = "#0073C2FF") +
#   theme_pubclean()

df <- movies %>%
  group_by(release_year) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = release_year, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()
