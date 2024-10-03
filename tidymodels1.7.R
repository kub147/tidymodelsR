install.packages("rstanarm") 
library(tidyverse)
library(rstanarm)

# Helper packages
library(readr)       # import danych
library(broom.mixed) # konwersja 
library(dotwhisker)  # wizualizacja

urchins <-
  read_csv("https://tidymodels.org/start/models/urchins.csv") |> 
  setNames(c("food_regime", "initial_volume", "width")) |> 
  mutate(food_regime = factor(food_regime, 
                              levels = c("Initial", "Low", "High")))
urchins  |> 
  ggplot(aes(
    x = initial_volume,
    y = width,
    col = food_regime,
    group = food_regime
  )) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  scale_color_viridis_d(option = "C", end = .9)


width ~ initial_volume * food_regime

lm_mod <- 
  linear_reg() |> 
  set_engine("lm")

lm_fit <-  
  lm_mod |>
  fit(width ~ initial_volume * food_regime, data = urchins)

print(lm_fit, digits = 5)

lm_fit$fit |> summary()


lm_fit |> 
  tidy() |> 
  dwplot(vline = geom_vline(xintercept = 0, color = "grey50", linetype = 2), 
         dot_args = list(size = 2, color = "black"), 
         whisker_args = list(color = "black")) +
  theme_bw()


new_points <- expand.grid(initial_volume = seq(5,45,5), 
                          food_regime = c("Initial", "Low", "High"))

# Prognoza średniej wartości
mean_pred <- predict(object = lm_fit, new_data = new_points)

# Prognoza przedizału ufności
conf_pred <- predict(object = lm_fit, new_data = new_points, type = "conf_int")

# Łączenie danych
lm_pred <- 
  new_points |> 
  bind_cols(mean_pred) |> 
  bind_cols(conf_pred)

# WYkres danych

lm_pred |>
  ggplot(aes(x = food_regime,
             y = .pred)) +
  geom_point() +
  geom_errorbar(aes(ymin = .pred_lower,
                    ymax = .pred_upper),
                width = 0.2) +
  facet_wrap(~ initial_volume) +
  theme_bw() +
  labs(y = "urchni size")


prior_dist <- rstanarm::student_t(df = 1)

set.seed(123)

# Ustawiamy metodę estymacji za pomocą parsnip

bayes_mod <-
  linear_reg() |>
  set_engine(engine = "stan",
             prior_intercept = prior_dist,
             prior = prior_dist)

# Estymacja modelu

bayes_fit <- 
  bayes_mod |> 
  fit(width ~ initial_volume * food_regime, data = urchins)


bayes_fit |> tidy(conf.int = T)


bayes_pred <- 
  new_points |> 
  bind_cols(predict(bayes_fit, new_data = new_points)) |> 
  bind_cols(predict(bayes_fit, new_data = new_points, type = "conf_int"))

bayes_pred |>
  ggplot(aes(x = food_regime,
             y = .pred)) +
  geom_point() +
  geom_errorbar(aes(ymin = .pred_lower,
                    ymax = .pred_upper),
                width = 0.2) +
  facet_wrap(~ initial_volume) +
  theme_bw() +
  labs(y = "urchni size")
