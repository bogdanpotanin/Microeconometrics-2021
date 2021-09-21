# --------
# Потанин Богдан Станиславович
# Микроэконометрика в R :)
# Семинар 3. Тестирование гипотез о параметрах в пробит модели
# --------

# Отключим scientific notation
options(scipen = 999)

# Симулируем данные, содержащую информацию
# о характеристиках заемщика, а также о том,
# наступил ли у него дефолт по ипотеке.
# Все переменные измерены в условных единицах
set.seed(12345)                                            # для воспроизводимости
n <- 10000                                                 # число индивидов в выборке
h <- data.frame(ind = rep(1:n))                            # датафрейм для хранения данных
h$inc <- runif(n, 0, 1)                                    # доход в условных единицах
h$pay <- runif(n, 0, 1)                                    # ежемесячный платеж
h$age <- runif(n, 0, 1)                                    # возраст
h$ins <- rbinom(n, 1, 0.7)                                 # факт наличия страховки
h$chl <- rbinom(n, 1, 0.6)                                 # факт наличия детей
eps <- rnorm(n)                                            # случайная ошибка 

beta <- c(0.6, -3, 2, -5, 3.5, -0.8, 0, 0.8)               # оцениваемые регрессионные 
                                                           # коэффициенты
             

def_li <- beta[1] +                                        # линейный индекс,
          beta[2] * h$inc +                                # отражающий вклад наблюдаемых
          beta[3] * h$pay +                                # факторов в вероятность дефолта
          beta[4] * h$age +
          beta[5] * h$age ^ 2 +                                            
          beta[6] * h$ins +                          
          beta[7] * h$chl +
          beta[8] * (h$chl * h$inc)

def_star <- def_li + eps                                   # латентная переменная,
                                                           # отражающая склонность
                                                           # к дефолту
h$def <- as.numeric(def_star >= 0)                         # наблюдаемое значение переменной
mean(h$def)                                                # доля дефолтов

h$ind <- NULL                                              # уберем индексы
head(h)                                                    # посмотрим на данные

#---------------------------------------------------
# Часть 1. Тестирование гипотез о
#          нескольких параметрах
#---------------------------------------------------

library("lmtest")                                          # дополнительные функции для 
                                                           # тестирования гипотез

# Оценим полную модель
model.probit.F <- glm(formula = def ~ inc + pay +
                                      age + I(age ^ 2) +
                                      ins + chl +
                                      chl * inc,
                      data = h,                                  
                      family = binomial(link = "probit")) 
summary(model.probit.F)

# Пример №1. Проверим гипотезу:
# H0: beta age     = 0
#          age ^ 2 = 0
model.probit.R <- glm(formula = def ~ inc + pay +          # ограниченная модель
                                      ins + chl +          # без переменных
                                      chl * inc,           # на возраст
                      data = h,                                  
                      family = binomial(link = "probit")) 
summary(model.probit.R)

# Проверим гипотезу при помощи разных тестов,
# где p-value указан как "Pr(>Chisq)"
lrtest(model.probit.F, model.probit.R)                     # тест отношения 
                                                           # правдоподобия                        
waldtest(model.probit.F, model.probit.R, test = "Chisq")   # теста Вальда

# Пример №2. Проверим гипотезу:
# H0: beta age     = -5
#          age ^ 2 = 3

# Оценим ограниченную модель, учитывая ограничения за 
# счет использования фиксированных значений коэффициентов
# для возраста и возраста в квадрате, с помощью аргумента 
# offset в функции glm()
model.probit.R <- glm(formula = def ~ inc + pay +          # ограниченная модель
                                      ins + chl +
                                      chl * inc,
                      offset = I(-5 * age) +               # фиксированная часть 
                               I(3 * age ^ 2),             # формулы, где 
                                                           # на коэффициенты 
                                                           # накладывается задаваемые
                                                           # нулевой гипотезой
                                                           # ограничения на параметры
                      data = h,                                  
                      family = binomial(link = "probit")) 
summary(model.probit.R)

# Проверим гипотезу LR тестом
lrtest(model.probit.F, model.probit.R)

# Пример №3. Проверим гипотезу: 
# H0: beta inc = -2 * pay
#          age = -5

model.probit.R <- glm(formula = def ~ I(inc - pay / 2) +   # объединяем две 
                                                           # переменные в одну      
                                      ins + chl +
                                      I(age ^ 2) +
                                      chl * inc,
                      offset = I(-5 * age),                # фиксированная часть 
                                                           # формулы, где 
                                                           # на коэффициенты 
                                                           # накладывается задаваемые
                                                           # нулевой гипотезой
                                                           # ограничения на параметры
                      data = h,                                  
                      family = binomial(link = "probit")) 
summary(model.probit.R)

# Проверим гипотезу LR тестом, поскольку функция
# waldtest() не учитывает ограничения, введенные 
# за счет аргумента offset в фукнции glm()
lrtest(model.probit.F, model.probit.R)

#---------------------------------------------------
# Часть 2. Тестирование гипотез о возможности
#          оценивания общей модели для групп
#---------------------------------------------------

# Проверим, можно ли оценивать совместную модели
# для людей с детьми и без них
# H0: коэффициенты в моделях для людей с детьми
#     и без - не различаются

# Оценим ограниченную модель
model.probit.R <- glm(formula = def ~ inc + pay +
                                      age + I(age ^ 2) +
                                      ins + chl,
                       data = h,
                       family = binomial(link = "probit"))

# Оценим полную модель как комбинацию двух моделей
  # модель только для людей с детьми
model.probit.F1 <- glm(formula = def ~ inc + pay +
                                       age + I(age ^ 2) +
                                       ins,
                       data = h[h$chl == 1, ],
                       family = binomial(link = "probit"))
  # модель только для людей без детей
model.probit.F0 <- glm(formula = def ~ inc + pay +
                                       age + I(age ^ 2) +
                                       ins,
                       data = h[h$chl == 0, ],
                       family = binomial(link = "probit"))

# Считаем логарифмы правдоподобия 
# полной и ограниченной моделей
lnL.F <- logLik(model.probit.F1) + logLik(model.probit.F0) # логарифм правдоподобия 
                                                           # полной модели
lnL.R <- logLik(model.probit.R)                            # логарифм правдоподобия 
                                                           # ограниченной модели

# Тестируем гипотезу
r <- length(model.probit.R$coefficients) - 2               # число ограничений
t <- 2 * (lnL.F - lnL.R)                                   # статистика теста
p.value <- as.numeric(1 - pchisq(t, df = r))               # p-value теста

# Альтернатинвый вариант тестирования
model.probit.F <- glm(formula = def ~ inc + pay +
                                      age + I(age ^ 2) +
                                      ins + chl +
                                      I(inc * chl) + 
                                      I(pay * chl) +
                                      I(age * chl) +
                                      I(age ^ 2 * chl) +
                                      I(ins * chl),
                      data = h,
                      family = binomial(link = "probit"))
lnL.F <- logLik(model.probit.F)
r <- length(model.probit.R$coefficients) - 2               # число ограничений
t <- 2 * (lnL.F - lnL.R)                                   # статистика теста
p.value <- as.numeric(1 - pchisq(t, df = r))               # p-value теста


#---------------------------------------------------
# Часть 3. Учет гетероскедастичности в моделях
#          бинарного выбора
#---------------------------------------------------

# Кратко о предполагаемом процессе генерации данных
# в пробит модели с гетероскедастичной случайной
# ошибкой:
# y_latent_i = x_i * beta + e_i          основное уравнение
# e_i ~ N(0, sigma_i ^ 2)                случайная ошибка
# sigma_i = h(w_i * tau)                 уравнение дисперсии
# w_i * tau                              линейный индекс
#                                        уравнения дисперсии
# w_i                                    независимые переменные
#                                        влияющие на дисперсию
# h(0) = 1                               обычно накладываемые
# h'(0) != 0                             условия на функцию h()
# h(t) = exp(t)                          некоторые примеры
#        |t + 1|                         функции h(), удовтелвтяорющей
#        (t + 1) ^ 2                     данным условиям
# В данной модели в качестве оцениваемых параметров
# выступают векторы коэффициентов beta и tau

# Внесем некоторые корректировки в процесс 
# генерации данных

# Симулируем гетероскедастичные случайные ошибки, 
# предполагая, что их дисперсия может зависеть от
# платежей и факта наличия детей
set.seed(123)
tau <- c(0.6, -0.3)
eps.var <- exp(tau[1] * h$pay +                           # дисперсия случайной ошибки
               tau[2] * h$chl) ^ 2                        # зависит от некоторых регрессоров
head(eps.var, 5)
eps <- rnorm(n, mean = 0, sd = sqrt(eps.var))             # случайные ошибки с
                                                          # различными дисперсия

# Симулируем зависимую переменную
def_star <- def_li + eps                                  # латентная переменная,
                                                          # отражающая склонность
                                                          # к дефолту
h$def <- as.numeric(def_star >= 0)                        # наблюдаемое значение переменной
mean(h$def)                                               # доля дефолтов

# Оценим параметры пробит модели с 
# гетероскедастичной случайной ошибкой

library("glmx")                                           # пакет, позволяющий оценивать пробит
                                                          # модель с гетероскдестичной 
                                                          # случайной ошибкой
# Оценим пробит модель без
# учета гетероскедастичности
model.probit <- glm(formula = def ~ inc + pay +
                                    age + I(age ^ 2) +
                                    ins + chl +
                                    chl * inc,
                    data = h,                                  
                    family = binomial(link = "probit")) 
summary(model.probit)

# Оценим пробит модель без
# с учетом гетероскедастичности
model.hetprobit <- hetglm(formula = def ~ inc + pay +        # линейный индекс
                                          age + I(age ^ 2) + # основного уравнения
                                          ins + chl +
                                          chl * inc |
                                          pay + chl,         # линейный индекс
                                                             # уравнения дисперсии
                          data = h,                                 
                          family = binomial(link = "probit"),
                          link.scale = "log")
summary(model.hetprobit)
# В функции hetglm() link.scale указывает, 
# в каком виде представлена ошибка
# Имеются следующие варианты:
# 1. identity  ---  sigma_i        =  w_i * tau
# 2. log       ---  log(sigma_i)   =  w_i * tau  =>  sigma_i = exp(w_i * tau_i)
# 3. sqrt      ---  sqrt(sigma_i)  =  w_i * tau  =>  sigma_i = (w_i * tau_i) ^ 2

# Достанем полученные оценки
beta.est <- model.hetprobit$coefficients$mean              # оценки коэффициентов при переменных
                                                           # основного уравнения
tau.est <- model.hetprobit$coefficients$scale              # оценки коэффициентов при переменных
                                                           # в уравнении дисперсии

# Сравним истинные значения и оценки
# коэффициентов линейного индекса
# уравнения дисперсии
rbind(true = tau,
      est = tau.est)

# Достанем оценки стандартных отклонений
# случайных ошибок
sigma.est <- predict(model.hetprobit, type = "scale")
head(sigma.est, 5)

# Осуществим тест на гомоскедастичность:
# H0: tau = 0
lrtest(model.hetprobit, model.probit)

# Предскажем вероятности и 
# скорректированный линейный индекс
prob.est <- predict(model.hetprobit, type = "response")    # вероятности
head(prob.est, 5)
y.li.est <- predict(model.hetprobit, type = "link")        # скорректированный 
head(y.li.est, 5)                                          # линейный индекс

# Рассчитаем предельные эффекты
# для индивида
Boris <- data.frame(inc = 0.2,                             # характеристики Бориса
                    pay = 0.1,
                    age = 0.3,
                    ins = 1,
                    chl = 1)

# Предварительные расчеты
prob.Boris <- predict(model.hetprobit, newdata = Boris,    # оценка вероятности 
                      type = "response")                   # дефолта Бориса
li.Boris.adj <- predict(model.hetprobit, newdata = Boris,  # оценка отношения линейного
                        type = "link")                     # индекса Бориса к стнадртному
                                                           # отклонению случайно ошибки
sigma.Boris <- predict(model.hetprobit, newdata = Boris,   # оценка стандартного
                       type = "scale")                     # отклонения случайной
                                                           # ошибки Бориса
li.Boris <- li.Boris.adj * sigma.Boris                     # оценка линейного
                                                           # индекса Бориса

# Используем встроенную функцию
library("margins")
ME.Boris <- margins(model.hetprobit, 
                    data = Boris)
summary(ME.Boris)

# Считаем предельный эффект аналитически   
ME.pay.1 <- dnorm(li.Boris, sd = sigma.Boris) * 
            (beta.est["pay"] - 
             li.Boris * tau.est["pay"])

# Считаем предельный эффект с помощью
# численного дифференцирования
delta <- 1e-6                                              # приращение                                                 
Boris.delta <- Boris
Boris.delta$pay <- Boris$pay + delta                       # приращение по возрасту
prob.Boris.delta <- predict(model.hetprobit, 
                            newdata = Boris.delta,
                            type = "response")
ME.pay.2 <- (prob.Boris.delta - prob.Boris) / delta

# ЗАДАНИЯ (* - средне, ** - сложно, *** - очень сложно)
# 3.1. Используя встроенные данные Mroz87 из библиотеки
#      sampleSelection и пробит модель с гетероскедастичной
#      случайно ошибкой определите, как на вероятность
#      занятости (lfp) влияют возраст (age), образование (educ),
#      факт проживания в городе (city) и число несовершеннолетних
#      детей (kids5 и kids618). При этом предполагается, что
#      гетероскедастичность может быть обусловлена возрастом
#      и уровнем образования. Далее, для 28-летнего индивида 
#      без высшего образования и с доходом 20000 оцените:
#      1)    вероятность занятости
#      2)    предельный эффект возраста на вероятность занятости
#      3)    предельный эффект проживания в городе на вероятность занятости
#      4*)   предельный эффект возраста на вероятность занятости, если
#            возраст входит в линейный индекс квадратично
#      5)    повторите предыдущие пункты, используя различные подходы
#            к определению формы уравнения дисперсии: см. аргумент link.scale
#      6**)  стандартную ошибку оценки вероятности занятости

#---------------------------------------------------
# Часть 4. Проверка гипотезы о гомоскедастичности
#          при помощи LM теста
#---------------------------------------------------

library("numDeriv")                                        # библиотека для
                                                           # численного дифференцирования
# Оценим модель с гетероскедастичной
# случайной ошибкой
model.hetprobit <- hetglm(formula = def ~ inc + pay +
                                          age + I(age ^ 2) +
                                          ins + chl +
                                          I(chl * inc) |
                                          pay + chl,
                          data = h,                                 
                          family = binomial(link = "probit"))
beta.est <- model.hetprobit$coefficients$mean
tau.est <- model.hetprobit$coefficients$scale

# Проверим гипотезу о гомоскедастичности
# при помощи LM теста, преимущество которого
# заключается в том, что нет необходимости
# предполагать конкретную форму функции h(t):
# достаточно наложить на нее лишь пару ограничений:
# 1) h(0) = 1                              
# 2) h'(0) != 0                             
HetprobitLnL <- function(x,                                # коэффициенты
                         y,                                # зависима переменная
                         X,                                # регрессоры основного уравнения
                         W,                                # регрессоры уравнения дисперсии
                         scale_fn = exp,                   # функция уравнения дисперсии h()
                         is_aggregate = TRUE)              # возвращаем функцию правдоподобия (TRUE)   
                                                           # или отдельные вклады (FALSE)          
{
  m_X <- ncol(X)
  m_W <- ncol(W)
  
  beta <- matrix(x[1:m_X], ncol = 1)                       # вектор beta коэффициентов и
  tau <- matrix(x[(m_X + 1):(m_X + m_W)], ncol = 1)        # вектор дополнительных параметров  
                                                           # переводим в матрицу с одним столбцом
  
  y_li_mean <- X %*% beta                                  # оценка линейного индекса
  y_li_scale <- W %*% tau                                  # латентной переменной
  y_li_scale_fn <- scale_fn(y_li_scale)
  
  n_obs <- nrow(X)                                         # количество наблюдений
  
  L_vec <- matrix(NA, nrow = n_obs,                        # вектор столбец вкладов наблюдений
                  ncol = 1)                                # в функцию правдоподобия
  
  is_y_0 <- (y == 0)                                       # вектор условий y = 0
  is_y_1 <- (y == 1)                                       # вектор условий y = 1
  
  L_vec[is_y_1] <- pnorm(y_li_mean[is_y_1], 
                         sd = y_li_scale_fn[is_y_1])       # вклад наблюдений для которых yi = 1
  L_vec[is_y_0] <- 1 - pnorm(y_li_mean[is_y_0],
                             sd = y_li_scale_fn[is_y_0])   # вклад наблюдений для которых yi = 0
  
  lnL_vec <- log(L_vec)                                    # логарифмы вкладов
  
  if(!is_aggregate)                                        # возвращаем вклады
  {                                                        # при необходимости
    return(lnL_vec)
  }
  
  lnL <- sum(lnL_vec)                                      # логарифм функции правдоподобия
  
  return(lnL)
}

# Достанем данные
df.hetprobit <- model.frame(model.hetprobit)               # все регрессоры
X.mat <- cbind(1, as.matrix(df.hetprobit[                  # регрессоры основного
  names(df.hetprobit) %in% names(beta.est)]))              # уравнения
head(X.mat , 5)
W.mat <- as.matrix(df.hetprobit[                           # регрессоры уравнения
  names(df.hetprobit) %in% names(tau.est)])                # дисперсии
head(W.mat , 5)

# Достанем оценки ограниченной модели
x.est.R <- c(model.probit$coefficients, 
             rep(0, ncol(W.mat)))
n.R <- length(x.est.R)                                     # добавим имена
names(x.est.R)[(n.R - 1):n.R] <- paste(colnames(W.mat),    # для красоты
                                       "sigma")
print(x.est.R)

# Рассчитаем правдоподобие полной модели в точке,
# определяемой оценками, полученными по ограниченной
# модели
lnL.R <- HetprobitLnL(x.est.R, 
                      y = df.hetprobit[, 1],
                      X.mat, W.mat, exp)
lnL.R.grad <- grad(func = HetprobitLnL,                    # считаем градиент данной функции
                   x = x.est.R,                            # численным методом
                   y = df.hetprobit[, 1], 
                   X = X.mat, W = W.mat,
                   scale_fn = exp)                         # замените exp на function(x)
                                                           #                 {
                                                           #                   return(abs(x + 1)})
                                                           #                 }
                                                           # и убедитесь, что результат не изменится
lnL.R.grad <- matrix(lnL.R.grad, ncol = 1)                 # градиент как матрица с одним столбцом
lnL.R.Jac <- jacobian(func = HetprobitLnL,                 # оцениваем асимптотическую ковариационную
                      x = x.est.R,                         # матрицу при помощи Якобиана, расcчитанного
                      y = df.hetprobit[, 1],               # численным методом, поскольку численно
                      X = X.mat, W = W.mat,                # рассчитать Гессиан достаточно точным 
                      scale_fn = exp,                      # образом не получается
                      is_aggregate = FALSE,
                      method.args = list(r = 8))

# Реализуем тест
LM.value <- t(lnL.R.grad) %*%                              # считаем статистику теста
            solve(t(lnL.R.Jac) %*% lnL.R.Jac) %*%          # множителей Лагранжа
            lnL.R.grad
p.value <- 1 - pchisq(LM.value, df = ncol(W.mat))          # рассчитываем p-value теста