import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from scipy.stats import beta
from scipy.stats import norm

df = pd.read_csv("C:\\Users\\santi\\Downloads\\SuperMarketData.csv")

plt.hist(df["Rating"], bins=20, color='skyblue', edgecolor='black', alpha=0.7)
plt.xlabel("Rating")
plt.title("Distribución de Ratings")
plt.show()

# Primeras filas del dataframe y tamaño
print(df.head())
print(f"Cantidad de registros en el dataframe: {len(df)}")

# Normalización de ratings
ratings = df["Rating"].values
min_rating, max_rating = ratings.min(), ratings.max()
ratings_normalized = (ratings - min_rating) / (max_rating - min_rating)

# Ajuste beta
alpha, beta_param, _, _ = beta.fit(ratings)
print("Parámetros alpha y beta:", alpha, beta_param)

# Calcular media y varianza de la distribución normalizada
media_norm = alpha / (alpha + beta_param)
varianza_norm = (alpha * beta_param) / ((alpha + beta_param) ** 2 * (alpha + beta_param + 1))
desviacion_norm = np.sqrt(varianza_norm)

# Convertir media y varianza a la escala original
media = media_norm * (max_rating - min_rating) + min_rating
varianza = varianza_norm * (max_rating - min_rating) ** 2
desviacion = np.sqrt(varianza)

print("Media normalizada y varianza normalizada:", media_norm, varianza_norm)
print("Media y varianza en la escala original:", media, varianza)

# Calcular probabilidad de que la calificación promedio supere 8.5
threshold = 8.5
probabilidad_superior = 1 - norm.cdf(threshold, media, desviacion)
print(f"Probabilidad de que el promedio de los ratings supere {threshold}:", probabilidad_superior)
