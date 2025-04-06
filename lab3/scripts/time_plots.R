df <- read.csv("~/study/R/lab3/datasets/russian_atletics_summer_data.csv")


df_summary <- aggregate(Medal ~ Year, data = df, FUN=length)
colnames(df_summary) <- c("Year", "Total")

gold_medals <- subset(df, Medal == "Gold")
gold_medals_summary <- aggregate(Medal ~ Year, data = gold_medals, FUN = length)
colnames(gold_medals_summary) <- c("Year", "Gold")

df_gender <- aggregate(Medal ~ Year + Gender, data = df, FUN = length)
colnames(df_gender) <- c("Year", "Gender", "Total")

y_breaks <- seq(0, max(df_summary$Total) + 1, by=1)

png("plot1.png", width=800, height=650)
barplot(df_summary$Total, names.arg = df_summary$Year,
        main = "Количество мест 1-8 Швеции в фигурном катании",
        xlab = "Год Олимпиады", ylab = "Количество мест", col = "steelblue",
        ylim = c(0, max(df_summary$Total) + 2),
        yaxt = "n")
axis(2, at = y_breaks, labels = y_breaks)
dev.off()

gold_medals <- subset(df, Medal == "Gold")
medals_by_year = table(gold_medals$Year)

png("number_gold_medals_by_year.png", width=1000, height=800)
pie(medals_by_year,
    main="Количество золотых медалей России в легкой атлетике по Олимпиадам",
    labels=paste(medals_by_year, "медалей"),
    col=rainbow(8),
    cex=1.6,
    clockwise=TRUE,
)
legend("topright",
       legend = names(medals_by_year),
       fill = rainbow(length(medals_by_year)),
       title="Год Олимипиады",
       cex=1.5
)
dev.off()


create_medal_trend_plot <- function(medal_data, gender_name, years_range = "1996-2024") {
  medal_counts <- as.data.frame(table(Year = medal_data$Year, Medal = medal_data$Medal))
  colnames(medal_counts) <- c("Year", "Medal", "Count")
  
  medal_counts$Year <- as.numeric(as.character(medal_counts$Year))
  medal_counts$Count <- as.numeric(medal_counts$Count)
  
  gold <- subset(medal_counts, Medal == "Gold")
  silver <- subset(medal_counts, Medal == "Silver")
  bronze <- subset(medal_counts, Medal == "Bronze")
  
  y_max <- max(c(gold$Count, silver$Count, bronze$Count), na.rm = TRUE)
  x_lim <- range(medal_counts$Year)
  
  plot(NA,
       xlim = x_lim,
       ylim = c(0, y_max),
       xlab = "Год Олимпиады", 
       ylab = "Количество наград",
       main = paste("Динамика наград среди", gender_name, years_range),
       cex.lab = 1.4,
       cex.main = 1.8
  )
  
  axis(1, at = seq(x_lim[1], x_lim[2], by = 4), cex.axis = 1.3)
  axis(2, at = seq(0, y_max, by = 1), cex.axis = 1.3)
  
  if (nrow(gold) > 0) {
    lines(gold$Year, gold$Count, type = "o", col = "gold", lwd = 4, pch = 19)
  }
  if (nrow(silver) > 0) {
    lines(silver$Year, silver$Count, type = "o", col = "#c0c0c0", lwd = 4, pch = 19)
  }
  if (nrow(bronze) > 0) {
    lines(bronze$Year, bronze$Count, type = "o", col = "#cd7f32", lwd = 4, pch = 19)
  }
  
  legend("topright", 
         legend = c("Золото", "Серебро", "Бронза"),
         col = c("gold", "#c0c0c0", "#cd7f32"),
         lwd = 2, pch = 19,
         cex = 1.5)
}

men_medals <- subset(df, Gender == "Men" & Year >= (2025-30))
women_medals <- subset(df, Gender == "Women" & Year >= (2025-30))

png("trend_quantity_change.png", width=1200, height=800)
par(mfrow = c(2,1))
create_medal_trend_plot(men_medals, "мужчин")
create_medal_trend_plot(women_medals, "женщин")
dev.off()
