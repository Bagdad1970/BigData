summer_data <- read.csv("~/study/R/lab3/datasets/summer.csv")


russian_atletics_summer_data <- subset(
	summer_data,
	tolower(Sport) == "athletics" & toupper(Country) == "RUS"
)

