# Saiu um livro novo sobre R e estatística, o Moderndive. 
# A ideia aqui é trabalhar alguns exemplos dele.
# Pacotes utilizados.

library(ggplot2)
library(dplyr)
library(forcats)
library(moderndive)
library(gapminder)
library(skimr)


# Por que alguns professores e instrutores de universidades e faculdades
# recebem altas avaliações de ensino de alunos, enquanto outros não? 
# Quais fatores podem explicar essas diferenças? 
# Será que o grau de beleza explica estas diferenças?
# Para este fim foram coletadas informações sobre 463 professores.
# O tibble é evals

glimpse(evals)
class(evals)

# Vamos olhar inicialmente 3 colunas, variáveis: score, a nota do professor; 
# bty_avg, a avaliação da beleza do professor; e, age, sua idade.
# Primeiro vou passar os nomes para o português.
# Selecionar as 3 colunas trabalhadas agora e olhar 5 linhas 
# escolhidas por amostragem.

evals_ch6 <- evals %>% rename(nota_prof = score, beleza_media = bty_avg, idade = age) %>% 
  select(nota_prof, beleza_media, idade)

# Olhando mais.
glimpse(evals_ch6)


# Usando o skim no evals_ch6
skim(evals_ch6)

# Olhando a correlação entre a nota_prof e a beleza_media.

evals_ch6 %>% 
  get_correlation(formula = nota_prof ~ beleza_media)

# ou 

cor(x = evals_ch6$nota_prof, y = evals_ch6$beleza_media)

# Vamos agora olhar isso no gráfico.

ggplot(evals_ch6, aes(x = beleza_media, y = nota_prof)) +
  geom_point() +
  labs(x = "Avaliação da beleza do Professor", y = "Avaliação da qualidade da Aula", 
       title = "Relação entre a beleza e a qualidade da aula do professor") +  
  geom_smooth(method = "lm")

# Agora vou repetir este estudo porém usando como variávl explanatória a idade do professor. 

# Olhando a correlação entre a nota_prof e a idade.

evals_ch6 %>% 
  get_correlation(formula = nota_prof ~ idade)

# ou 

cor(x = evals_ch6$nota_prof, y = evals_ch6$idade)

# Vamos agora olhar isso no gráfico.

ggplot(evals_ch6, aes(x = idade, y = nota_prof)) +
  geom_point() +
  labs(x = "Avaliação da beleza do Professor", y = "Avaliação da qualidade da Aula", 
       title = "Relação entre a beleza e a qualidade da aula do professor") +  
  geom_smooth(method = "lm")

# O lm (linear model) acha os valores que definem a reta que relaciona 
# a qualidade da aula à beleza do professor.
# nota_prof = 3.880 + 0.06664 x beleza_media
# para beleza_media = 0 a nota_prof = 3.88034.
# para cada aumento de 1 na beleza_media a nota_prof umenta de 0.06664

lm(formula = nota_prof ~ beleza_media, data = evals_ch6)

# Regressão um pouco mais completa.
# Adiante vamos ver o significado de cada tópico desta tabela.
# Fit regression model:
score_model <- lm(nota_prof ~ beleza_media, data = evals_ch6)
# Get regression table:
get_regression_table(score_model)

# Chama-se residual (em inglês) a nota_prof de um professor específio,
# subtraída do valor na reta de regressão, ou a distância entre a nota-prof e a 
# reta de regressão.
# a função do moderndive que dá isso é a get_regression_points().
# nota_prof_hat é a nota_prof que está sobre a reta ajustada.
# residual é a nota_prof menos a nota_prof_hat. 
# ci significa intervalo de confiança.

get_regression_points(score_model)

gapminder2007 <- gapminder %>%
  filter(year == 2007) %>% 
  select(country, continent, lifeExp, gdpPercap)

glimpse(gapminder2007)

# Vamos dar uma olhada mais detalhada sobre as colunas
# continent e lifeExp



gapminder2007 %>% 
  select("continent", "lifeExp") %>% 
  skim()

# Olhando as expectativas de vida num facet_wratp.
ggplot(gapminder2007, aes(x = lifeExp)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(x = "Expectativa de vida", y = "Quantidade de países", 
       title = "Expectativa de vida por continente") +
  facet_wrap(~ continent, nrow = 2) +
  theme_bw()

ggplot(gapminder2007, aes(x = continent, y = lifeExp)) +
  geom_boxplot() +
  labs(x = "Continente", y = "Expectativa de vida", 
       title = "Expectativa de vida por continente") +
  theme_bw()

# Repetir a análise acima usando as colunas
# continente e gdpPercap.

gapminder2007 %>% 
  select("continent", "gdpPercap") %>% 
  skim()

table(gapminder2007$continent)

ggplot(gapminder2007, aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  labs(x = "Continente", y = "Renda percapita", 
       title = "Renda percapita por continente") +
  theme_bw()

# Usando a função get_regression_table.
# 
lifeExp_model <- lm(lifeExp ~ continent, data = gapminder2007)
get_regression_table(lifeExp_model)

