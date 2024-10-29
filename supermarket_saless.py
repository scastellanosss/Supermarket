import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from scipy.stats import beta
from scipy.stats import norm

df = pd.read_csv("C:\\Users\\santi\\Downloads\\SuperMarketData.csv")

df["Sales"].plot(kind="hist", bins=20, edgecolor="black", alpha=0.7)
plt.xlabel("Sales")
plt.title("Distribución de Sales")
plt.show()

print(df.head())

print('Longitud del dataframe: ' + str(len(df)))

sales = np.array(df["Sales"])*19.88

max_sales = max(sales)
min_sales = min(sales)
sales_norm = 1/(max_sales - min_sales) * (sales - min_sales)

a,b,_,_=beta.fit(sales)
print("alpha y beta:", a,b)

mu_norm = a / (a + b)

var_norm = (a * b) / ((a + b) ** 2 * (a+b+1)) 
desv_norm = np.sqrt(var_norm)

mu = (max_sales - min_sales) * mu_norm + min_sales
var = (max_sales - min_sales) ** 2 * var_norm
sigma = np.sqrt(var)

print("mu normalizada, varianza normalizada:", mu_norm, var_norm)

#------Salarios------
fact = 1.15

sal_cajeros = 258.25
num_cajeros = 30
dias_t = 24

tot_sal_caj = sal_cajeros * num_cajeros*dias_t*fact

sal_conserjes = 5000
num_conserjes = 20

tot_sal_conserjes = sal_conserjes* num_conserjes*fact

gerente = 100000

sub_gerentes = 45000
num_subgerentes = 4
tot_sal_sub = sub_gerentes*num_subgerentes

sal_almacenista = 262.13
num_almacenista = 40
tot_sal_alm = sal_almacenista*num_almacenista*dias_t*fact

g_pasillo = 264.65
num_pasillos = 40
tot_pasillo = g_pasillo*num_pasillos*fact


nomina_tot = tot_sal_caj + tot_sal_conserjes + tot_sal_sub + gerente + tot_sal_sub + tot_sal_alm + tot_pasillo


print("nómina total:", nomina_tot)
gasto_luz = 120*2000*24*3.134*30
print("Gasto luz:", gasto_luz)
gastos_tot = gasto_luz+nomina_tot
ingreso = gastos_tot + 1500000
print("Gastos totales:", gastos_tot)
print("Ingresos requeridos:", ingreso)

omega = norm.ppf(0.1)
a_  = mu**2
b_ = -2*mu*ingreso - omega**2 * sigma**2
c_ = ingreso**2

N1 = (-b_+np.sqrt(b_**2-4*a_*c_))/(2*a_)
N2 = (-b_-np.sqrt(b_**2-4*a_*c_))/(2*a_)


# print("N1:", N1)
# print("N2:", N2)

if (ingreso/N1-mu > 0) :
    N = N1
else:
    N = N2

porc_pob = N/160000
print("Porcentaje de aprobación:", porc_pob)
