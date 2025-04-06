df <- read.csv("~/study/R/lab3/datasets/summer.csv")

last_6_olympics <- tail(sort(unique(df$Year)), 6)
df_athletics_last_6_olympics <- subset(df, Country == "RUS" & Sport == "Athletics" & Year %in% last_6_olympics)

total_prizes_athletics_last_6_olympics <- aggregate(Medal ~ Year + Gender,
																										data=df_athletics_last_6_olympics,
																										FUN=length)
colnames(total_prizes_athletics_last_6_olympics) <- c("Year", "Gender", "Total")


men_athletics_last_6_olympics <- subset(total_prizes_athletics_last_6_olympics, Gender == "Men")
women_athletics_last_6_olympics <- subset(total_prizes_athletics_last_6_olympics, Gender == "Women")

png("plot_men_women_prizes.png", width=1000, height=800)
plot(men_athletics_last_6_olympics$Year, men_athletics_last_6_olympics$Total,
		 type="o", col="blue",
		 xlab = "Год Олимпиады",
		 ylab = "Количество медалей",
     main = "Динамика медалей России по легкой атлетике (последние 6 Олимпиад)",
     ylim = c(0, max(total_prizes_athletics_last_6_olympics$Total)+5),
     xaxt = "n",
		 cex.main=1.7,
		 cex.lab=1.4,
		 cex=1.5,
		 lwd=3,
		 pch=19
)
lines(women_athletics_last_6_olympics$Year, women_athletics_last_6_olympics$Total,
			type = "o",
			col = "red",
			lwd=3, cex=1.5, pch=19)
axis(1, at=last_6_olympics, labels=last_6_olympics)
legend("topright", 
       legend = c("Мужчины", "Женщины"), 
       col = c("blue", "red"), 
			 cex=1.3,
			 lwd=2)
dev.off()


total_prizes <- aggregate(Total ~ Gender, 
                               data = total_prizes_athletics_last_6_olympics, 
                               FUN = sum)

colors = c("blue", "pink")
png("pie_men_women_prizes.png", width=1000, height=800)
pie(total_prizes$Total,
    labels = paste0(total_prizes$Total, " медалей"),
		cex=1.6,
		cex.main=1.7,
    col = colors,
    main = "Распределение медалей России по легкой атлетике (последние 6 Олимпиад)")
legend("topright",
			 legend=c("Мужчины", "Женщины"),
			 fill=colors,
			 title="Пол", cex=1.5)

dev.off()


medal_matrix <- with(total_prizes_athletics_last_6_olympics, {
  tapply(Total, list(Gender, Year), sum)
})
medal_matrix[is.na(medal_matrix)] <- 0

png("barplot_men_women_prizes.png", width=1000, height=700)
barplot(medal_matrix, beside=TRUE,
        col = c("blue", "pink"),
        main = "Медали России по легкой атлетике по полу (последние 6 Олимпиад)",
        xlab = "Год Олимпиады", ylab = "Количество медалей",
        legend.text = rownames(medal_matrix),
				ylim=c(0, max(medal_matrix)+5),
        args.legend = list(x = "topright", title = "Пол", legend=c("Мужчины", "Женщины")))
dev.off()
