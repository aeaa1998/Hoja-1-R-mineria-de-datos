setwd("~/R")
library(dplyr)
library(stringr)
library(tidyr)

#4.7
movies  <- read.csv("Hoja-1/excel/tmdb-movies.csv")


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




