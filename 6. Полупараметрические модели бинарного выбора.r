# --------
# Потанин Богдан Станиславович
# Микроэконометрика в R :)
# Семинар 6. Полупараметрические модели бинаргого выбора
# --------

# Отключим scientific notation
options(scipen = 999)

# Подключим дополнительные библиотеки
library("mvtnorm")                                       # симуляции из многомерного
                                                         # нормального распределения
library("numDeriv")                                      # численное дифференцирование
library("GJRM")                                          # оценивание систем
                                                         # бинарных уравнений
library("pbivnorm")                                      # двумерное нормальное распределение
library("hpa")                                           # полу-непараметрический подход
library("ggplot2")                                       # красивые графики
library("np")                                            # ядерное оценивание параметров
                                                         # моделей бинарного выбора
library("Ecdat")                                         # встроенные данные
library("margins")                                       # расчет предельных эффектов

#---------------------------------------------------
# Часть 1. Оценивание неизвестной функции плотности
#---------------------------------------------------

# Симулируем выборку из распределения Стьюдента
set.seed(777)
n <- 5000
df <- 5
ncp <- 10
x <- rt(n, df, ncp)                                              
x <- matrix(x, ncol = 1)
# Посмотрим на гистограмму
hist(x, breaks = 100)

# Сгенерируем значения от квантили уровня 0.001
# до квантили уровне 0.999 нецентрированного
# распределения Стьюдента, используя шаг 0.01
test_values <- seq(from = qt(0.001, df, ncp), 
                   to = qt(0.999, df, ncp), 
                   by = 0.01)
test_values <- matrix(test_values, ncol = 1)
n0 <- length(test_values)

# Посчитаем значение функции плотности распределения
# Стьюдента в соответствующих точках
true_pred <- dt(test_values, df, ncp)

# Построим график истинной функции плотности
plot(test_values, true_pred, 
     xlab = "x - values", 
     ylab = "f(x) - density")

# Апроксимируем неизвестную плотность при помощи
# ядерного метода оценивания с использованием функции
# стандартного нормального распределения
# в качестве ядра
bw <- 0.9 * min(sd(x), IQR(x) / 1.34) * n ^ (-0.2)       # выбираем ширину окна
                                                         # по методу Сильвермана
kr_pred_train <- rep(NA, n)                              # вектор, в который будут
                                                         # сохраняться ядерные
                                                         # оценки плотности по
                                                         # исходной выборке
kr_pred <- rep(NA, n0)                                   # аналогичный вектор для
                                                         # тестовой выборки
  # полуачаем оценки по исходной выборке
for(i in 1:n)
{
  kr_pred_train[i] <- sum(dnorm((x[i] - x) / bw)) /      # по формуле
                      (bw * n)
}
  # получаем оценки по тестовой выборке
for(i in 1:n0)
{
  kr_pred[i] <- sum(dnorm((test_values[i] - x) / bw)) /  # по формуле
                (bw * n)
}

# Визуализируем ядерную оценки плотности
plot(test_values, kr_pred)                                 # строим график оценки плотности
lines(test_values, true_pred, col="green")                 # накладываем поверх истинную
                                                           # функцию плотности для сравнения

# Получим оценки функции плотности при помощи
# полу-непараметрического метода, максимизируя
# функцию квази правдоподобия с использованием 
# полинома в форме Эрмита
  # второй степени
model_2 <- hpaML(data = x,                                 # выборка
                 pol_degrees = 2,                          # степень полинома
                 tr_left = min(x),                         # усечение снизу
                 tr_right = max(x))                        # усечение сверху
summary(model_2)
AIC(model_2)
plot(model_2)
  # четвертой степени
model_4 <- hpaML(data = x, 
                 pol_degrees = 4, 
                 tr_left = min(x), 
                 tr_right = max(x))
summary(model_4)
AIC(model_4)
plot(model_4)
  # восьмой степени
model_8 <- hpaML(data = x, 
                 pol_degrees = 8, 
                 tr_left = min(x), 
                 tr_right = max(x))
summary(model_8)
AIC(model_8)
plot(model_8)

# Оценим значения функции плотности
# при помощи ядра с использованием
# встроенной функции
model_kernel <- density(x,                              # выборка
                        bw = "SJ",                      # ширина, может быть установлена
                                                        # автоматической процедурой "nrd0"
                                                        # или "SJ"
                        kernel = "epanechnikov")        # используемое ядро Епанечникова                      
plot(model_kernel)                                      # строим график
kernel_fn <- approxfun(model_kernel)                    # получаем ядерные оценки плотности
kernel_pred <- kernel_fn(test_values)                   # в новых точках

# Получим предсказанные значения функций плотности, 
# рассчитанные с помощью полиномов
model_2_pred <- predict(object = model_2, newdata = test_values)
model_4_pred <- predict(object = model_4, newdata = test_values)
model_8_pred <- predict(object = model_8, newdata = test_values)

# Обеспечим графическую репрезентацию результата

  # Подготовим данные
h <- data.frame("values" = rep(test_values, 6),
                "predictions" = c(model_2_pred, model_4_pred,
                                  model_8_pred, kr_pred,
                                  kernel_pred, true_pred), 
                "Density" = as.factor(c(
                  rep("K=2", n0), rep("K=4", n0),
                  rep("K=8", n0), rep("kernel1", n0),
                  rep("kernel2", n0),
                  rep("noncentral t-distribution", n0))))
h$Density = factor(h$Density, levels = c("K=2", "K=4", "K=8", 
                                         "kernel1", "kernel2", 
                                         "noncentral t-distribution"))

  # Построим график
ggplot(h, aes(values, predictions)) + geom_point(aes(color = Density)) +
  theme_minimal() + theme(legend.position = "top", 
                          text = element_text(size = 26),
                          legend.title=element_text(size = 20), 
                          legend.text=element_text(size = 28)) +
  guides(colour = guide_legend(override.aes = list(size = 10)))

# Сравним точность по RMSE
data.frame("K=2" = sqrt(mean((model_2_pred - true_pred) ^ 2)),
           "K=4" = sqrt(mean((model_4_pred - true_pred) ^ 2)),
           "K=8" = sqrt(mean((model_8_pred - true_pred) ^ 2)),
           "kernel1" = sqrt(mean((kr_pred - true_pred) ^ 2)),
           "kernel2" = sqrt(mean((kernel_pred - true_pred) ^ 2)))

# ЗАДАНИЯ (* - непросто, ** - сложно, *** - брутально)
# 1.1.    Воспроизведите результат используя
#         1)    распределение стьюдента с 10-ю степенями свободы
#         2)    логистическое распределение
#         3**)  Смесь распределений.
#               Пусть t1 и t2 - независимые случайные величины, имеющие 
#               распределение Стьюдента с df1 и df2 степенями свободы
#               соответственно. Также, имеется независимая от них бернулиевская
#               случайная величина V, такая, что P(V = 1) = p. Рассмотрим
#               распределение следующей случайной величины:
#               G = V * (t5 - a) + (1 - V) * (t10 + b).
#               Пусть df1 = 5, df2 = 10, a = 2, b = 3, p = 0.5.
# 1.2.   Запрограмируйте собственную функцию, которая позволяет
#        осуществлять ядерное оценивание функции плотности:
#        1*)    используя по крайней мере одно из  трех различных видов ядра,
#               например, Епачникова, логистическое или равномерное
#        2*)    используя произвольное значение ширины окна
#               или рассчитываемое по методу Сильвермана

#---------------------------------------------------
# Часть 2. Метод Галланта и Нички
#---------------------------------------------------

# По мотивам статьи:
# Parametric and Semi-Parametric Estimation of the 
# Binary Response Model of Labour Market

# Рассмотрим пример на данных, отражающих 
# занятость индивидов
help(Participation)

# Загрузим данные
data("Participation")
h <- Participation
h$lfp <- as.numeric(h$lfp == "yes")
h$foreign <- as.numeric(h$foreign == "yes")
h$age <- h$age * 10

# Пробит модель, описывающая занятость
model_pr <- glm(formula = lfp ~ lnnlinc +     # логарифм нетрудового дохода
                          age + I(age ^ 2) +  # возраст   
                          educ +              # образование в годах
                          nyc +               # к-во маленьких детей
                          noc +               # к-во взрослых детей
                          foreign,            # иностранец 
                data = h,                                     
                family = binomial(link = "probit"))  
summary(model_pr)
coef_pr <- coef(model_pr)

# Как и в оригинальной работе будем использовать
# полином третьей степени
model_hpa <- hpaBinary(formula = lfp ~ I(-lnnlinc) +        # нормализуем коэффициент при 
                                       age + I(age ^ 2) +   # регрессоре lnnlinc к -1
                                       educ +                      
                                       nyc +                       
                                       noc +                       
                                       foreign,                   
                       data = h, 
                       K = 3,                               # степень полинома
                       cov_type = "sandwich")               # тип ковариационной матрицы
summary(model_hpa)
coef_hpa <- model_hpa$coefficients                          # достанем оценки коэффициентов

# Визуализируем результат
plot(model_hpa)                                             # оценка функции плотности
                                                            # случайной ошибки
# Сравним модели по AIC
AIC(model_hpa)    
AIC(model_pr)

# сравним оценки со стандартизированными 
# коэффициентами пробит модели
data.frame("Galland.Nychka" = coef_hpa, 
           "Probit" = coef_pr[-1] / (-coef_pr[2]))

# Рассмотрим пример на данных, отражающих готовность
# людей платить за сохранение парка
help(Kakadu)

# Загрузим данные
data("Kakadu")
h <- Kakadu
h$wtp <- as.numeric(h$answer != "nn")                   # переменная принимает значение 1,
                                                        # если индивид готов заплатить за
                                                        # сохранение парка больше некоторой суммы

# Модель, описывающая готовность индивида
# заплатить более некоторой суммы
model_pr <- glm(formula = wtp ~ mineparks +            # открытие производства в парковых
                                                       # зонах существенно уменьшает
                                                       # привлекательность парка
                                age +                  # возраст
                                sex +                  # пол (мужчина)
                                income +               # доход в тысячах долларов
                                moreparks +            # нужно больше парков
                                wildlife +             # важно сохранять дикую природу
                                aboriginal +           # важно учитывать интересы 
                                                       # коренных жителей
                                finben,                # важно руководствоваться соображениями
                                                       # финансовой выгоды при использовании
                                                       # природных ресурсов
                 data = h,                                    
                 family = binomial(link = "probit"))          
summary(model_pr)

# Применим полу-непараметрическую модель
model_hpa <- hpaBinary(formula = formula(model_pr),                   
                       data = h, 
                       K = 6,                               # степень полинома
                       cov_type = "sandwich")               # тип ковариационной матрицы
summary(model_hpa)
# ВАЖНО:
# В зависимости от типа ковариационной матрицы
# можно получить различный подход к интерпретации
# оценок:
# cov_type = "sandwich" - квази-максимальное правдоподобие
# cov_type = "bootstrap" - полу-непараметрический подход
# cov_type = "hessian" - параметрический подход


# Визуализируем результат
plot(model_hpa)

# Сравним модели по AIC
AIC(model_pr)
AIC(model_hpa)

# Сравним предсказанные вероятности
p_pr <- predict(model_pr, type = "response")
p_hpa <- predict(model_hpa, is_prob = TRUE)
head(cbind(probit = p_pr, GN = p_hpa))
plot(p_pr, p_hpa,                                          # визуально сравнение оценок
     xlab = "Gallant and Nychka", ylab = "Probit")         # вероятностей

# Сравним предельные эффекты пробит модели
# и полученные при помощи метода Галланта и Нички
ME_hpa <- model_hpa$marginal_effects                       # Галланта и Ничка
ME_probit <- margins(model_pr, type = "response")          # Пробит модель
plot(ME_hpa[, "age"], ME_probit$dydx_age,                  # визуально сравнение оценок
     xlab = "Gallant and Nychka", ylab = "Probit")         # предельного эффекта возраста

# ЗАДАНИЯ (* - непросто, ** - сложно, *** - брутально)
# 2.1.    Подберите оптимальную степень полинома K
#         руководствуясь соображениями минимизации
#         информационного критерия Акаике
# 2.2.    Сравните оценки асимптотической ковариационной
#         матрицы, получаемые различными методами
# 2.3.    Используя функцию dhpa аналитически оцените:
#         1*)   вероятность того, что индивид с вашими
#               характеристиками будет готов платить
#               за сохранение парка
#         2**)  предельный эффект возраста на
#               соответствующую вероятность

#---------------------------------------------------
# Часть 3. Метод Ичимуры и Метод Клейна и Спади
#---------------------------------------------------

# Загрузим данные
data("Participation")
h <- Participation
h$lfp <- as.numeric(h$lfp == "yes")
h$foreign <- as.numeric(h$foreign == "yes")
h$age <- h$age * 10

# Пробит модель, описывающая занятость
# Спецификация сокращена для ускорения расчетов
model_pr <- glm(formula = lfp ~ foreign +
                                lnnlinc,
                data = h,                                     
                family = binomial(link = "probit"))  
summary(model_pr)
coef_pr <- coef(model_pr)

# Воспользуемся методом Клейна и Спади
model_ks <- npindexbw(lfp ~ foreign + lnnlinc,
                      method = "kleinspady",                # метод, который можно заменить
                                                            # на "ichimura"
                      data = h,
                      optim.method = "Nelder-Mead",         # метод оптимизации
    optim.reltol = sqrt(.Machine$double.eps) * 0.1,         # условия остановки
    optim.abstol = sqrt(.Machine$double.eps) * 0.1,         # оптимизационного алгоритма
    optim.maxit = 1000000, random.seed = 123,               # число итераций лучше поставить
                                                            # как можно больше, но это может
                                                            # привести к крайне долгим расчетам
    nmult = 1)                                              # число попыток найти максимум функции
                                                            # квази максимального правдоподобия
                                                            
model_ks <- npindex(bws = model_ks,                         # переводим модель в более удобный формат
                    gradients = TRUE)                       # стандартные ошибки
summary(model_ks)

# Посмотрим информацию об оценках
coef_ks <- coef(model_ks)                                   # оценки коэффициентов
cov_ks <- vcov(model_ks)                                    # оценка асимптотической ковариационной
                                                            # матрицы оценок коэффициентов
std_ks <- sqrt(diag(cov_ks))                                # асимптотические стандартные ошибки
z <- coef_ks / std_ks                                       # тестовая статистика
p_value <- 2 * pmin(pnorm(z), 1 - pnorm(z))                 # p-value теста о значимости коэффициентов при
                                                            # допущении о нормальном распределении оценок
data.frame("Estimate" = coef_ks,                            # репрезентация результата
           "Std Error" = std_ks,
           "p_value" = p_value)

# сравним оценки со стандартизированными 
# коэффициентами пробит модели
data.frame("KS" = coef_ks, 
           "Probit" = coef_pr[-1] / coef_pr[2])

# Получим предсказанные значения вероятностей
# для индивида с конкретными характеристиками
Boris <- data.frame("lnnlinc" = 11,
                    "foreign" = 0)
p_pr <- predict(model_pr, newdata = Boris,                  # пробит модель
                type = "response")
p_ks <- predict(model_ks, newdata = Boris)                  # модель клейна и спади

# Оценим предельный эффект логарифма дохода
# на вероятность занятости, используя
# численное дифференцирование
delta <- 0.0001                                             # берем малое приращение
Boris_delta <- Boris                                        # создаем копию Бориса с
Boris_delta$lnnlinc <- Boris_delta$lnnlinc + delta          # учетом малого приращения возраста
p_pr_delta <- predict(model_pr, newdata = Boris_delta,      # вероятность с учетом приращения
                      type = "response")                    # оцененная по пробит модель
p_ks_delta <- predict(model_ks, newdata = Boris_delta)      # вероятность с учетом приращения
                                                            # оцененная по модели Клейна и Спади
ME_lnnlinc_pr <- (p_pr_delta - p_pr) / delta                # предельный эффект в пробит модели               
ME_lnnlinc_ks <- (p_ks_delta - p_ks) / delta                # предельный эффект в модели Клейна и Спади

# Визуализируем результат
df <- data.frame("Probit" = c(p_pr, ME_lnnlinc_pr), 
                 "KS" = c(p_ks, ME_lnnlinc_ks))
rownames(df) = c("P(lfp = 1)", 
                 "dP(lfp = 1) / dlnnlinc")
print(df)

# ЗАДАНИЯ (* - непросто, ** - сложно, *** - брутально)
# 3.1.    Оцените параметры модели с помощью метода
#         Клейна и Спади зафиксировав на единице
#         коэффициент при переменной foreign
# 3.2.    Сравните предельные эффекты возраста, оцененные
#         при помощи пробит модели и метода Клейна и Спади
# 3.3.    Воспроизведите расчеты и сравнения, произведенные
#         в данном разделе, используя:
#         1)    данные Kakadu и произвольную зависимую и
#               независимые переменные
#         2**)  симулированные данные, предполагая, что
#               случайные ошибки следует нецентрированному
#               распределению Стьюдента с параметрами
#               df = 5 и ncp = 10