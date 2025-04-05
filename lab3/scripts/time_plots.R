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
        ylim = c(0, max(df_summary$Total) + 1),
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


count_type_medals <- function(df) {
  df$Year <- as.numeric(as.character(df$Year))
  as.data.frame(table(Year=df$Year, Medal=df$Medal))
}

men_medals <- subset(df, Gender == "Men" & Year >= (2025-30))

men_counts <- count_type_medals(men_medals)
colnames(men_counts) <- c("Year", "Medal", "Count")

men_counts$Year <- as.numeric(as.character(men_counts$Year))
men_counts$Count <- as.numeric(men_counts$Count)

gold_men <- subset(men_counts, Medal == "Gold")
silver_men <- subset(men_counts, Medal == "Silver")
bronze_men <- subset(men_counts, Medal == "Bronze")

y_max_men <- max(c(gold_men$Count, silver_men$Count, bronze_men$Count), na.rm = TRUE)

x_lim = sort(unique(men_medals$Year))


png("trend_quantity_change.png", width=1200, height=800)
par(mfrow = c(2,1))

plot(NA,
		 xlim=c(min(x_lim), max(x_lim)),
		 ylim = c(0, y_max_men),
     xlab = "Год Олимпиады", cex.lab=1.4,
     ylab = "Количество наград",
     main = "Динамика наград среди мужчин (1996-2024)", cex.main = 1.8
)
axis(1, at=seq(min(x_lim), max(x_lim), by=4), cex.axis=1.3)
axis(2, at=seq(0, y_max_men, by=1), cex.axis=1.3)

if (nrow(gold_men) > 0) {
  lines(gold_men$Year, gold_men$Count, type = "o", col = "gold", lwd=4, pch = 19)
}
if (nrow(silver_men) > 0) {
  lines(silver_men$Year, silver_men$Count, type = "o", col = "#c0c0c0", lwd=4, pch = 19)
}
if (nrow(bronze_men) > 0) {
  lines(bronze_men$Year, bronze_men$Count, type = "o", col = "#cd7f32", lwd=4, pch = 19)
}
legend("topright", 
       legend = c("Золото", "Серебро", "Бронза"),
       col = c("gold", "#c0c0c0", "#cd7f32"),
       lwd = 2, pch = 19,
			 cex=1.5)


women_medals <- subset(df, Gender == "Women" & Year >= (2025-30))
women_counts <- count_type_medals(women_medals)
colnames(women_counts) <- c("Year", "Medal", "Count")

women_counts$Year <- as.numeric(as.character(women_counts$Year))
women_counts$Count <- as.numeric(women_counts$Count)

gold_women <- subset(women_counts, Medal == "Gold")
silver_women <- subset(women_counts, Medal == "Silver")
bronze_women <- subset(women_counts, Medal == "Bronze")

y_max_women <- max(c(gold_women$Count, silver_women$Count, bronze_women$Count), na.rm = TRUE)

x_lim = sort(unique(women_medals$Year))

plot(NA,
		 xlim=c(min(x_lim), max(x_lim)),
		 ylim = c(0, y_max_women),
     xlab = "Год Олимпиады", cex.lab=1.4,
     ylab = "Количество наград",
     main = "Динамика наград среди женщин (1996-2024)", cex.main = 1.8
)
axis(1, at=seq(min(x_lim), max(x_lim), by=4), cex.axis=1.3)
axis(2, at=seq(0, y_max_women, by=1), cex.axis=1.3)

if (nrow(gold_women) > 0) {
  lines(gold_women$Year, gold_women$Count, type = "o", col = "gold", lwd=4, pch = 19)
}
if (nrow(silver_women) > 0) {
  lines(silver_women$Year, silver_women$Count, type = "o", col = "#c0c0c0", lwd=4, pch = 19)
}
if (nrow(bronze_women) > 0) {
  lines(bronze_women$Year, bronze_women$Count, type = "o", col = "#cd7f32", lwd=4, pch = 19)
}

legend("topright", 
       legend = c("Золото", "Серебро", "Бронза"),
       col = c("gold", "#c0c0c0", "#cd7f32"),
       lwd = 2, pch = 19,
			 cex=1.5)
dev.off()
