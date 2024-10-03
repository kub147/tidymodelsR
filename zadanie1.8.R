colnames(airquality) <- tolower(colnames(airquality))

air <-
  airquality |>
  as_tibble() |>
  na.omit() |> 
  select(-day) |> 
  mutate(month = factor(month)) 


# relacje między zmiennymi
ggpairs(air)


lm_model <- lm(ozone ~ solar.r + wind + temp + month, data = air)
# Wyniki modelu regresji liniowej
summary(lm_model)

# np month6 (Czerwiec): Wartość współczynnika -14.75895 sugeruje, że
#w porównaniu z miesiącem bazowym (maj), poziom ozonu w czerwcu jest 
#średnio o 14.76 jednostek niższy.