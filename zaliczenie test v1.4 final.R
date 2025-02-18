#zainstaluj pakiet "readr"
install.packages("readr")
#zainstaluj pakiet "tidyverse"
install.packages("tidyverse")

install.packages("caret")

#wymagane pakiety
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(nnet)  
library(randomForest)  
library(xgboost) 
library(ggplot2)  

library(readr)
Obesity_prediction <- read_csv("jezyk R/otyłość/Obesity prediction.csv")
View(Obesity_prediction)

library(dplyr)
glimpse(Obesity_prediction)

# Definiowanie Problemu
# 1. Celem projektu jest przewidywanie poziomu otyłości na podstawie dostępnych danych dotyczących stylu życia i czynników zdrowotnych

# 2. Przygotowanie danych:

# pokaż dane 
Obesity_prediction
glimpse(Obesity_prediction)
dim(Obesity_prediction)

# sprawdź jakość danych
str(Obesity_prediction)

# przeprowadź podsumowanie danych
summary(Obesity_prediction)

# sprawdź czy są duplikaty danych
sum(duplicated(Obesity_prediction))

# sprawdź czy są wartości ujemne w kolumnach numerycznych Age, Height, Weight, FCVC, NCP, CH2O, FAF, TUE
kolumny <- c("Age", "Height", "Weight", "FCVC", "NCP", "CH2O", "FAF", "TUE")
ujemne_wartosci <- sapply(Obesity_prediction[kolumny], function(x) any(x < 0))
ujemne_wartosci

# Sprawdzenie braków danych
if (is.data.frame(Obesity_prediction)) {
  sum(is.na(Obesity_prediction))
} else {
  stop("Obiekt 'data' nie jest ramką danych. Sprawdź wczytywanie danych.")
}

# Usunięcie lub imputacja brakujących wartości
Obesity_prediction_clean <- na.omit(Obesity_prediction)

# 3. Eksploracyjna analiza danych (EDA)

# Liczność kategorii zmiennej Obesity
table(Obesity_prediction$Obesity)

# Wykres kołowy przedstawiający rozkład zmiennej 'Obesity' (odpowiednik plt.pie())
library(ggplot2)
obesity_counts <- table(Obesity_prediction$Obesity)
df_obesity <- data.frame(Category = names(obesity_counts), Count = as.vector(obesity_counts))
ggplot(df_obesity, aes(x = "", y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("Rozkład zmiennej 'Obesity'")

# Histogramy dla zmiennych numerycznych
ggplot(Obesity_prediction_clean, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram zmiennej 'Age'")

# Wykres pudełkowy dla wieku w zależności od otyłości
# Wykres pudełkowy dla wieku nieznacznie się pokrywa, można go wykorzystać do modelowania.
ggplot(Obesity_prediction_clean, aes(x = Obesity, y = Age)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot zmiennej 'Age' w zależności od 'Obesity'")

# plt.figure(figsize=(12,5)) sns.boxplot(x="Obesity", y="Height", data=df)
# Wykres pudełkowy dotyczący wzrostu w dużym stopniu się pokrywa, dlatego nie można go wykorzystać do klasyfikowania poziomów otyłości.
ggplot(Obesity_prediction_clean, aes(x = Obesity, y = Height)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot zmiennej 'Height' w zależności od 'Obesity'")

# plt.figure(figsize=(12,5) sns.boxplot(x="Obesity", y="Weight", data=df)
# Biorąc pod uwagę masę ciała, możemy wyraźnie określić poziom otyłości.
ggplot(Obesity_prediction_clean, aes(x = Obesity, y = Weight)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot zmiennej 'Weight' w zależności od 'Obesity'")

#plt.figure(figsize=(12,5)) sns.boxplot(x="Obesity", y="FCVC", data=df)
# Wykres pudełkowy dla FCVC w dużym stopniu się pokrywa i nie może być wykorzystany jako cecha do klasyfikacji poziomów otyłości.
# W przypadku zmiennej FCVC, nie można wyraźnie określić poziomu otyłości.
ggplot(Obesity_prediction_clean, aes(x = Obesity, y = FCVC)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot zmiennej 'FCVC' w zależności od 'Obesity'")

#plt.figure(figsize=(12,5)) sns.boxplot(x="Obesity", y="NCP", data=df)
# Wykres pudełkowy dla nakładania się NCP dla kilku klas, ale można wyróżnić masę ciała prawidłową, niedowagę i otyłość typu 3.
# Wykres pudełkowy dla NCP w dużym stopniu się pokrywa i nie może być wykorzystany jako cecha do klasyfikacji poziomów otyłości.
# W przypadku zmiennej NCP, nie można wyraźnie określić poziomu otyłości.
ggplot(Obesity_prediction_clean, aes(x = Obesity, y = NCP)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot zmiennej 'NCP' w zależności od 'Obesity'")

#plt.figure(figsize=(12,5)) sns.boxplot(x="Obesity", y="CH2O", data=df)
# Wykres pudełkowy nakładania się CH2O na poziom otyłości nie może być wykorzystany jako cecha do klasyfikacji poziomów otyłości.
# Wykres pudełkowy dla CH2O w dużym stopniu się pokrywa i nie może być wykorzystany jako cecha do klasyfikacji poziomów otyłości.
# W przypadku zmiennej CH2O, nie można wyraźnie określić poziomu otyłości.
ggplot(Obesity_prediction_clean, aes(x = Obesity, y = CH2O)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot zmiennej 'CH2O' w zależności od 'Obesity'")

# plt.figure(figsize=(12,5)) sns.boxplot(x="Obesity", y="FAF", data=df)
# Wykres pudełkowy dla FAF w dużym stopniu się pokrywa i nie może być wykorzystany jako cecha do klasyfikowania poziomów otyłości.
# Wykres pudełkowy dla FAF w dużym stopniu się pokrywa i nie może być wykorzystany jako cecha do klasyfikacji poziomów otyłości.
# W przypadku zmiennej FAF, nie można wyraźnie określić poziomu otyłości.
ggplot(Obesity_prediction_clean, aes(x = Obesity, y = FAF)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot zmiennej 'FAF' w zależności od 'Obesity'")

# plt.figure(figsize=(12,5)) sns.boxplot(x="Obesity", y="TUE", data=df)
# Wykresy pudełkowe dla TUE w dużym stopniu się pokrywają i nie mogą być wykorzystane jako cecha do klasyfikowania poziomów otyłości.
# Wykres pudełkowy dla TUE w dużym stopniu się pokrywa i nie może być wykorzystany jako cecha do klasyfikacji poziomów otyłości.
# W przypadku zmiennej TUE, nie można wyraźnie określić poziomu otyłości.
ggplot(Obesity_prediction_clean, aes(x = Obesity, y = TUE)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot zmiennej 'TUE' w zależności od 'Obesity'")

#plt.figure(figsize = (13, 5)) sns.countplot(x = "Obesity", hue = "Gender", data = df) plt.show()
# Poziom otyłości u mężczyzn i kobiet wygląda na prawie zrównoważony.
# Liczba kobiet i mężczyzn w różnych kategoriach otyłości
ggplot(Obesity_prediction_clean, aes(x = Obesity, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba kobiet i mężczyzn w różnych kategoriach otyłości")
                                       
#plt.figure(figsize = (13, 5)) sns.countplot(x = "Obesity", hue = "family_history", data = df) plt.show()
#Jeśli otyłość jest w historii rodzinnej, to ludzie prawdopodobnie mają nadwagę. Tak więc historia rodzinna jest ważną cechą
# Liczba osób z historią otyłości w rodzinie w różnych kategoriach otyłości
ggplot(Obesity_prediction_clean, aes(x = Obesity, fill = family_history)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób z historią otyłości w rodzinie w różnych kategoriach otyłości")

#plt.figure(figsize = (13, 5)) sns.countplot(x = "Obesity", hue = "CAEC", data = df) plt.show()
# Osoby, które mają tendencję do podjadania między posiłkami, są bardziej narażone na nadwagę
# W przypadku zmiennej CAEC, nie można wyraźnie określić poziomu otyłości.
# Liczba osób w różnych kategoriach CAEC w różnych kategoriach otyłości
ggplot(Obesity_prediction_clean, aes(x = Obesity, fill = CAEC)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób w różnych kategoriach CAEC w różnych kategoriach otyłości")

#plt.figure(figsize = (13, 5)) sns.countplot(x = "Obesity", hue = "SMOKE", data = df) plt.show()
# Palenie nie wydaje się mieć dużego wpływu na otyłość
# Osoby palące są bardziej narażone na nadwagę
# Liczba osób palących w różnych kategoriach otyłości
ggplot(Obesity_prediction_clean, aes(x = Obesity, fill = SMOKE)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób palących w różnych kategoriach otyłości")

#plt.figure(figsize = (13, 5)) sns.countplot(x = "Obesity", hue = "FAVC", data = df) plt.show()
# Osoby, które jedzą dużo fast foodów, są bardziej narażone na nadwagę
# Liczba osób jedzących dużo fast foodów w różnych kategoriach otyłości
ggplot(Obesity_prediction_clean, aes(x = Obesity, fill = FAVC)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób jedzących dużo fast foodów w różnych kategoriach otyłości")

#plt.figure(figsize = (13, 5)) sns.countplot(x = "Obesity", hue = "SCC", data = df) plt.show()
# Monitorowanie spożywanych kalorii nie ma wpływu na otyłość
# Liczba osób w różnych kategoriach SCC w różnych kategoriach otyłości
ggplot(Obesity_prediction_clean, aes(x = Obesity, fill = SCC)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób w różnych kategoriach SCC w różnych kategoriach otyłości")

#plt.figure(figsize = (13, 5)) sns.countplot(x = "Obesity", hue = "CALC", data = df) plt.show()
# Picie alkoholu nie wydaje się mieć związku z otyłością
# Liczba osób w różnych kategoriach CALC w różnych kategoriach otyłości
ggplot(Obesity_prediction_clean, aes(x = Obesity, fill = CALC)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób w różnych kategoriach CALC w różnych kategoriach otyłości")

#plt.figure(figsize = (13, 5)) sns.countplot(x = "Obesity", hue = "MTRANS", data = df) plt.show()
# Środek transportu nie ma wpływu na poziom otyłości.
# Osoby, które podróżują samochodem, są bardziej narażone na nadwagę
# Liczba osób podróżujących różnymi środkami transportu w różnych kategoriach otyłości
ggplot(Obesity_prediction_clean, aes(x = Obesity, fill = MTRANS)) +
  geom_bar(position = "dodge") +
  labs(title = "Liczba osób podróżujących różnymi środkami transportu w różnych kategoriach otyłości")

# Obliczenie BMI
Obesity_prediction_clean$BMI <- Obesity_prediction_clean$Weight / (Obesity_prediction_clean$Height^2)

# Usunięcie nieistotnych cech
Obesity_prediction_clean <- select(Obesity_prediction_clean, -"Height",-"FCVC",-"CH2O",-"TUE",-"Gender",
                                   -"FAVC",-"SMOKE",-"SCC",-"CALC",-"MTRANS", -"FAF")

# Konwersja zmiennej docelowej na faktor
Obesity_prediction_clean$Obesity <- as.factor(Obesity_prediction_clean$Obesity)

# Podział danych na zbiór treningowy i testowy
set.seed(123)
split <- createDataPartition(Obesity_prediction_clean$Obesity, p = 0.7, list = FALSE)
train <- Obesity_prediction_clean[split, ]
test <- Obesity_prediction_clean[-split, ]

# **1. Model drzewa decyzyjnego**
model_tree <- rpart(Obesity ~ ., data = train, method = "class")

# Wizualizacja drzewa decyzyjnego
rpart.plot(model_tree, extra = 101)

# Predykcja na zbiorze testowym
predictions_tree <- predict(model_tree, test, type = "class")

# Ocena modelu drzewa decyzyjnego
conf_matrix_tree <- confusionMatrix(predictions_tree, test$Obesity)
print(conf_matrix_tree)

# **2. Model regresji logistycznej**
model_logistic <- multinom(Obesity ~ ., data = train)

# Predykcja na zbiorze testowym
predictions_logistic <- predict(model_logistic, test)

# Ocena modelu regresji logistycznej
conf_matrix_logistic <- confusionMatrix(predictions_logistic, test$Obesity)
print(conf_matrix_logistic)

# **3. Model Random Forest**
set.seed(123)
model_rf <- randomForest(Obesity ~ ., data = train, ntree = 100, importance = TRUE)

# Predykcja na zbiorze testowym
predictions_rf <- predict(model_rf, test)

# Ocena modelu Random Forest
conf_matrix_rf <- confusionMatrix(predictions_rf, test$Obesity)
print(conf_matrix_rf)

# **4. Model XGBoost**
train_xgb <- model.matrix(Obesity ~ . -1, data = train)
test_xgb <- model.matrix(Obesity ~ . -1, data = test)

train_labels <- as.numeric(train$Obesity) - 1
test_labels <- as.numeric(test$Obesity) - 1

dtrain <- xgb.DMatrix(data = train_xgb, label = train_labels)
dtest <- xgb.DMatrix(data = test_xgb, label = test_labels)

params <- list(objective = "multi:softmax", num_class = length(levels(train$Obesity)), eval_metric = "mlogloss")
model_xgb <- xgb.train(params = params, data = dtrain, nrounds = 100)

# Predykcja na zbiorze testowym
predictions_xgb <- predict(model_xgb, dtest)
predictions_xgb <- factor(predictions_xgb, levels = 0:(length(levels(train$Obesity)) - 1), labels = levels(train$Obesity))

# Ocena modelu XGBoost
conf_matrix_xgb <- confusionMatrix(predictions_xgb, test$Obesity)
print(conf_matrix_xgb)

# **📊 Porównanie dokładności modeli**
accuracy_data <- data.frame(
  Model = c("Drzewo Decyzyjne", "Regresja Logistyczna", "Random Forest", "XGBoost"),
  Accuracy = c(conf_matrix_tree$overall["Accuracy"],
               conf_matrix_logistic$overall["Accuracy"],
               conf_matrix_rf$overall["Accuracy"],
               conf_matrix_xgb$overall["Accuracy"])
)

ggplot(accuracy_data, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal() +
  ggtitle("Porównanie dokładności modeli") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# **📉 Wizualizacja macierzy błędów dla Random Forest**
rf_table <- as.data.frame(conf_matrix_rf$table)
ggplot(rf_table, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  ggtitle("Macierz błędów dla Random Forest") +
  theme_minimal()

# **📉 Wizualizacja macierzy błędów dla XGBoost**
xgb_table <- as.data.frame(conf_matrix_xgb$table)
ggplot(xgb_table, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  ggtitle("Macierz błędów dla XGBoost") +
  theme_minimal()

# **Zapisz modele**
saveRDS(model_tree, "model_tree.rds")
saveRDS(model_logistic, "model_logistic.rds")
saveRDS(model_rf, "model_rf.rds")
saveRDS(model_xgb, "model_xgb.rds")


