# //////////////////////////////////////////////////////////////////////////////
# TEMA: EDA de Marcel
# AUTOR: Henry P. Zumaeta Lozano
# CORREO: henry.zumaeta.l@uni.pe
# VERSION: 1.0
# FECHA VALIDACION: 02/08/2023
# //////////////////////////////////////////////////////////////////////////////

# **************
# Librerías ----
# **************
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

packages <- c("DataExplorer",         # EDA
              "tidyverse",            # for everything good in R ;)
              "SmartEDA",             # EDA
              "dlookr",               # EDA
              "funModeling",          # EDA
              "ISLR",                 # for the Wage dataset
              "ggstatsplot",          # publication ready visualizations with statistical details
              "flextable",            # beautifying tables
              "summarytools",         # EDA
              "psych",                # psychological research: descr. stats, FA, PCA etc.
              "skimr",                # summary stats
              "gtsummary",            # publication ready summary tables
              "moments",              # skewness, kurtosis and related tests
              "ggpubr",               # publication ready data visualization in R
              "PerformanceAnalytics", # econometrics for performance and risk analysis
              "fastStat",             # well :) you've guessed it
              "performance")          # Assessment of Regression Models Performance (for outliers here)
ipak(packages)


resistencia <- c(2450,3300,3400,3650,3800,2650,3150,
                 3100,3500,2850,3050,4300,3300,
                 3300,3150,2100,3300,3650,3150,
                 3550,2900,3250,3000,3400,3750,
                 3900,3150,3600,3000,4200,3700,
                 3050,3300,2350,4150,2950,3200,
                 3900,3200,3450,2500,3050,2650,
                 3050,2800,2700,3450,3400,3200,
                 3600)
table(resistencia)

breks_data <- seq(from = min(resistencia), to = max(resistencia), length = 11)
breks <- seq(from = 2000, to = 4500, length = 11)
pop_freq <- cut(resistencia, breaks = breks, right = TRUE, include.lowest = TRUE)
data.frame(table(pop_freq))
hist(resistencia, freq = F, breaks = 10)
lines(density(resistencia), lwd = 3, col = "blue")
str(resistencia)

df <- as.data.frame(resistencia)


# Creación de informes
# {DataExplorer}
library(DataExplorer) # for exploratory data analysis
library(tidyverse)    # for data, data wrangling and visualization

# report without a response variable
create_report(df)

# report with a response variable
create_report(df, y = "price")

# {SmartEDA}
library(SmartEDA) # for exploratory data analysis

ExpReport(df, op_file = 'smarteda.html')

# {dlookr}
library(dlookr) # for exploratory data analysis

# report without a response variable
diagnose_report(diamonds) # pdf by default

# report with a response variable and some dplyr sintax
diamonds %>%
    eda_report(
        target        = cut, 
        output_format = "html", 
        output_file   = "EDA_diamonds.html")

# example with missing values
transformation_report(airquality, target = Temp)
# tinytex::reinstall_tinytex(repository = "illinois") # Instalando latex

# example with outliers
transformation_report(diamonds, target = price)


# Panorama general de sus datos
# {DataExplorer}
introduce(df) %>% t() 

plot_intro(airquality)

# {funModeling}
library(funModeling)    # EDA

status(df) %>% flextable()


# Explore variables categóricas (discretas)
# {DataExplorer}
plot_bar(df)
plot_bar(diamonds, by = "cut")

# {SmartEDA}
ExpCatViz(df, Page = c(1,3))

library(ISLR)      # for the Wage dataset
ExpCatViz(
    Wage %>% 
        select(education, jobclass), 
    target="education")

# {ggstatsplot}
library(ggstatsplot)     # visualization with statistical details
ggbarstats(
    data  = Wage, 
    x     = jobclass, 
    y     = education, 
    label = "both")

# Explore variables numéricas con estadísticas descriptivas
# {dlookr}
library(flextable)        # beautifying tables
dlookr::describe(df) %>% flextable()

iris %>% 
    group_by(Species) %>% 
    univar_numeric() %>% 
    knitr::kable()

df %>% 
    diagnose_numeric() %>% 
    flextable()

# {SmartEDA}
ExpNumStat(df, by="A", Outlier=TRUE, Qnt = c(.25, .75), round = 2) %>%
    flextable()

ExpNumStat(df, by="G", gp="Species", Outlier=TRUE, Qnt = c(.25, .75),
           round = 2) %>%
    flextable()

ExpNumStat(df, by="GA", gp="Species", Outlier=TRUE, Qnt = c(.25, .75),
           round = 2) %>%
    flextable()

# {summarytools} y {psych}
library(summarytools)
iris %>% 
    group_by(Species) %>% 
    descr()

library(psych)
describeBy(iris,
           iris$Species)

# {summarytools}
library(summarytools)
dfSummary(diamonds)

# {skimr}
library(skimr)
skim(df)

# {gtsummary}
library(gtsummary)

mtcars %>% 
    select(mpg, hp, am, gear, cyl) %>% 
    tbl_summary(by = am) %>% 
    add_p()

Wage %>%
    select(age, wage, education, jobclass) %>% 
    tbl_summary(by = education) %>% 
    add_p()


# Explorar la distribución de variables numéricas
# {DataExplorer}
plot_histogram(df)

plot_density(df)

# works perfectly with dplyr!
airquality %>% 
    select(Ozone, Wind) %>% 
    plot_density()

# {momentos}
# ASIMETRÍA
library(moments)
skewness(df, na.rm = T) 

skewness(df, na.rm = T) 

# Test de asimetría
agostino.test(df$resistencia)
agostino.test(df$resistencia)

# CURTOSIS
# El valor de curtosis para una distribución normal es de alrededor de tres.
# valor p que diga si el resultado de la curtosis está significativamente
# lejos de tres

# Test de Anscombe-Glynn
anscombe.test(df$resistencia)
anscombe.test(df$resistencia)


# Comprobar la normalidad de la distribución
# {DataExplorer}
plot_qq(df$resistencia)
plot_qq(iris, by = "Species")

# {dlookr} visualización
iris %>%
    group_by(Species) %>%
    plot_normality(Petal.Length)

# {ggpubr}
library(ggpubr)
ggqqplot(iris, "Sepal.Length", facet.by = "Species")

# {dlookr} Pruebas de normalidad de Shapiro-Wilk
normality(airquality) %>%
    mutate_if(is.numeric, ~round(., 3)) %>% 
    flextable()

diamonds %>%
    group_by(cut, color, clarity) %>%
    normality()

bla <- Wage %>%
    filter(education == "1. < HS Grad") %>% 
    select(age)

normality(df) %>% flextable()

plot_density(df)

agostino.test(df$resistencia)

anscombe.test(df$resistencia)

ggqqplot(df$resistencia)


# Explore variables categóricas y numéricas con diagramas de caja
# {DataExplorer}
plot_boxplot(iris, by = "Species")

# {ggstatsplot}
ggbetweenstats(
    data = iris, 
    x    = Species, 
    y    = Sepal.Length, 
    type = "np")

# {SmartEDA}
ExpNumViz(iris, target = "Species", Page = c(2,2))


# Explore las correlaciones
# {dlookr} - correlación
correlate(airquality, Ozone)

plot_correlate(airquality, method = "kendall")

diamonds %>%
    filter(cut %in% c("Premium", "Ideal")) %>% 
    group_by(cut) %>%
    plot_correlate()

# {ggstatsplot}
ggcorrmat(data = iris)

ggcorrmat(
    data   = iris,
    type   = "np",
    output = "dataframe"
) %>% 
    mutate_if(is.numeric, ~round(., 2)) %>% 
    flextable()

ggscatterstats(
    data = airquality,
    x = Ozone,
    y = Temp,
    type = "np" # try the "robust" correlation too! It might be even better here
    #, marginal.type = "boxplot"
)

# {PerformanceAnalytics}
library(PerformanceAnalytics)
chart.Correlation(iris %>% select(-Species), method = "kendall") 

# {fastStat}
library(fastStat)
iris %>% select_if(is.numeric) %>% cor_sig_star(method = "kendall")

# {dlookr} - modelos lineales
bla <- compare_numeric(iris) 

bla$linear %>% 
    mutate_if(is.numeric, ~round(.,2)) %>% 
    flextable()

plot(bla)


# Modelización exploratoria
ggplot(airquality, aes(Solar.R, Temp))+
    geom_point()+
    geom_smooth()+
    facet_wrap(~Month)


# Explore los valores que faltan
# {dlookr}
plot_na_intersect(airquality)
plot(imputate_na(airquality, Ozone, Temp, method = "knn"))


# Explorar valores atípicos
# {performance}
library(performance)
plot(check_outliers(airquality$Wind, method = "zscore"))

check_outliers(airquality$Wind, method = "iqr")

# {dlookr}
diagnose_outlier(diamonds) %>% flextable()

airquality %>% 
    dplyr::select(Ozone, Wind) %>% 
    plot_outlier()

# Visualize variables with a ratio of outliers greater than 5%
diamonds %>%
    plot_outlier(diamonds %>%
                     diagnose_outlier() %>%
                     filter(outliers_ratio > 5) %>%
                     select(variables) %>%
                     pull())


# Imputar valores atípicos
bla <- imputate_outlier(diamonds, carat, method = "capping")
plot(bla)

