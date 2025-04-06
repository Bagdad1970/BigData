df <- read.csv("~/study/R/lab3/datasets/summer.csv")

# Получаем последние 6 Олимпиад
last_6_olympics <- tail(sort(unique(df$Year)), 6)
df_last_6_olympics <- subset(df, Year %in% last_6_olympics)

# Считаем общее количество медалей по странам
medal_counts <- aggregate(Medal ~ Country, data=df_last_6_olympics, FUN=function(x) sum(x %in% c("Gold", "Silver", "Bronze")))
colnames(medal_counts) <- c("Country", "Total")

# Выбираем 7 лучших стран
the_best_7_countries <- medal_counts[order(-medal_counts$Total), "Country"][1:7]
the_best_7_countries_data <- subset(df_last_6_olympics, Country %in% the_best_7_countries)

# Считаем количество золотых медалей по годам и странам
gold_summary <- aggregate(Medal ~ Year + Country, data=subset(the_best_7_countries_data, Medal == "Gold"), FUN=length)
colnames(gold_summary) <- c("Year", "Country", "GoldCount")

# Устанавливаем цвета для графиков
colors <- rainbow(length(the_best_7_countries))

# Создаем график
png("changes_in_gold_medals.png", width=1000, height=800)
plot(NULL,
     xlim = range(gold_summary$Year),
     ylim = c(0, max(gold_summary$GoldCount) + 20),
		 main = "Изменение количества золотых медалей по странам (последние 6 Олимпиад)",
     xlab = "Год Олимпиады",
     ylab = "Количество золотых медалей",
		 xaxt = "n",
     yaxt = "n",
     cex.main = 1.5,  # Увеличиваем размер заголовка
     cex.lab = 1.3    # Увеличиваем размер подписей осей
)
axis(1, at=last_6_olympics, labels=last_6_olympics, cex.axis=1.3)  # Исправлено: используем фактические годы
axis(2, at=seq(0, max(gold_summary$GoldCount), by=10), cex.axis=1.3)  # Улучшаем разметку оси Y

# Добавляем линии для каждой страны
for (i in seq_along(the_best_7_countries)) {
  country_data <- subset(gold_summary, Country == the_best_7_countries[i])
  lines(country_data$Year, country_data$GoldCount, type = "o", col = colors[i], lwd=3, pch=19, cex=1.5)
}

# Добавляем легенду
legend("right", 
       legend=the_best_7_countries, 
       col=colors, 
       lwd=2, 
       pch=19, 
       cex=1.3,
       title="Страны")

dev.off()
