setwd("~/R")
library(dplyr)
library(stringr)
library(tidyr)
library("ggpubr")
library("ggplot2")
theme_set(theme_pubr())
movies  <- read.csv("./excel/tmdb-movies.csv")

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
ggdensity(movies$budget, 
          main = "votes avg",
          xlab = "avg")
ggdensity(movies$puplarity, 
          main = "votes avg",
          xlab = "avg")
ggdensity(movies$revenue, 
          main = "votes avg",
          xlab = "avg")
ggdensity(movies$runtime, 
          main = "votes avg",
          xlab = "avg")
ggdensity(movies$release_year, 
          main = "votes avg",
          xlab = "avg")
ggdensity(movies$vote_count, 
          main = "votes avg",
          xlab = "avg")

hist(movies$popularity)
order(movies$revenue,na.last = TRUE,decreasing = TRUE)
head(sort(movies$revenue,decreasing = T),10)

##SECCION $

#4.1 
preup <- head(sort(movies$budget,decreasing = T),10)

y<-movies[movies$budget %in% preup,"original_title"]
print(y)

#4.2
ganancias <- head(sort(movies$revenue,decreasing = T),10)

x<-movies[movies$revenue %in% ganancias,"original_title"]
print(x)

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


#4.7

mostRepeatedGenre <- tail(names(sort(table(movies$genres))), 1)

genresIndividualList = unlist(strsplit(movies$genres, "\\|"), recursive = FALSE)
mostRepeatedGenresTable = sort(table(genresIndividualList))
mostRepeatedGenre <- tail(names(mostRepeatedGenresTable), 1)
print(mostRepeatedGenre)
print("Grafica 4.6")
plot(tail(mostRepeatedGenresTable, 5))

#4.8

dataGroupedByGenre <- separate_rows(movies,genres,sep="\\|") %>%
  group_by(genres) %>%
  summarize(
    revenue = sum(revenue),
    ingresos_netos = sum(revenue) - sum(budget),
    budget = sum(budget)
    )
peliculasConMayoresGanancias <- dataGroupedByGenre[order(-dataGroupedByGenre$ingresos_netos),]
print("Top 5 generos")
print(head(peliculasConMayoresGanancias$genres, 5))

#4.9
peliculasConMayorPresupuesto <- dataGroupedByGenre[order(-dataGroupedByGenre$budget),]
print("Top 5 generos con mayor presupuesto")
print(head(peliculasConMayorPresupuesto$genres, 5))
#4.10
moviesSeparatedByDirector = separate_rows(movies,director,sep="\\|")
dataGroupedByDirector <- moviesSeparatedByDirector %>%
  group_by(director) %>%
  summarize(
    vote_average = mean(vote_average),
  )

print("Top 20 directores con mejores calificaciones")
bestDirectors <- dataGroupedByDirector[order(-dataGroupedByDirector$vote_average),]

print(head(bestDirectors$director, 20))

#4.11
moviesByBudget <- movies %>%
  group_by(budget) %>%
  summarize(
    revenue = mean(revenue),
  )

moviesByBudgetOrdered = moviesByBudget[order(-moviesByBudget$budget),]
plot(head(moviesByBudgetOrdered$budget, 30), head(moviesByBudgetOrdered$revenue, 30), main="Gráfica 11", xlab="Presupuesto", ylab="Ingresos")
lines(head(moviesByBudgetOrdered,30))
abline(lm(head(moviesByBudgetOrdered$revenue, 30)~head(moviesByBudgetOrdered$budget, 30)), col="green")
#4.12
moviesWithDates <- movies %>%
  separate(release_date, c("month", "day", "year"), "/")



moviesByMonth <- moviesWithDates %>%
  group_by(month) %>%
  summarize(
    revenue = mean(revenue),
  )
print(moviesByMonth$month)
moviesByMonthOrdered = moviesByMonth[order(moviesByMonth$month),]
plot(moviesByMonthOrdered$month, moviesByMonthOrdered$revenue, main="Gráfica 12", xlab="Mes", ylab="Ingresos")
mappedMonth = data.frame(moviesByMonthOrdered$revenue, row.names = moviesByMonthOrdered$month)

#4.13
moviesByMonthOrderedRev = moviesByMonth[order(-moviesByMonth$revenue),]
print("Top 3")
print(head(moviesByMonthOrderedRev, 3))

#4.14
moviesByVoteAvg  <- filter(movies, revenue != 0 & budget != 0) %>%
  group_by(vote_average) %>%
  summarize(
    revenue = mean(revenue),
    
  )
print("Top 3 donde revnue y budget != a 0")
moviesByVoteAvgDec = moviesByVoteAvg[order(-moviesByVoteAvg$vote_average),]
moviesByVoteAvgDecR = moviesByVoteAvg[order(-moviesByVoteAvg$revenue),]

plot(moviesByVoteAvgDec$vote_average, moviesByVoteAvgDec$revenue, main="Gráfica 13", xlab="Calificacion", ylab="Ingresos")
abline(lm(moviesByVoteAvgDec$revenue~moviesByVoteAvgDec$vote_average), col="green")
print("Top 5 Peliculas ordenadas por su calificación")
print(head(moviesByVoteAvgDec, 5))
print("Top 5 Peliculas ordenadas por sus ingresos")
print(head(moviesByVoteAvgDecR, 5))

#4.15
moviesBygenreWithRuntime = separate_rows(movies,genres,sep="\\|") %>%
  group_by(genres) %>%
  summarize(
    length = mean(runtime)
  )
moviesBygenreWithRuntimeDesc = moviesBygenreWithRuntime[order(-moviesBygenreWithRuntime$length),]
print("Top generos mas largo")
print(head(moviesBygenreWithRuntimeDesc$genres, 1))
print("Top 5 generos mas largos en promedio")

print(head(moviesBygenreWithRuntimeDesc$genres, 5))
barplot(head(moviesBygenreWithRuntimeDesc$length, 5),xlab="Genero",ylab="Tiempo",main="Tiempo por genero", 
        names.arg = head(moviesBygenreWithRuntimeDesc$genres, 5))



