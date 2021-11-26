# Calculate cars per person
library(sf)
library(tidyr)
library(dplyr)

all = readRDS("data/LSOA_cars_per_person_2002_2018_long.Rds")

all_split <- all %>%
  group_by(code) %>%
  group_split()

res <- pbapply::pblapply(all_split, function(x){
  nm <- x$code[1]
  x$population_scale <- x$population / x$population[1]
  x$year_scale <- x$year - x$year[1]
  if(is.na(x$cars_per_person[1])){
    x$carspp_scale <- x$cars_per_person  / min(x$cars_per_person, na.rm = TRUE)
  } else {
    x$carspp_scale <- x$cars_per_person  / x$cars_per_person[1]
  }
  
  fit_pop <- lm(population_scale  ~   year_scale, data = x)
  fit_cars <- lm(carspp_scale  ~   year_scale, data = x)
  
  # Gradient for bits of the line
  fit1_pop <- lm(population_scale  ~  year_scale, data = x[1:5,])
  fit2_pop <- lm(population_scale  ~  year_scale, data = x[6:10,])
  fit3_pop <- lm(population_scale  ~  year_scale, data = x[11:16,])
  
  fit1_cars <- lm(carspp_scale  ~  year_scale, data = x[1:5,])
  fit2_cars <- lm(carspp_scale  ~  year_scale, data = x[6:10,])
  fit3_cars <- lm(carspp_scale  ~  year_scale, data = x[11:16,])
  
  
  # plot(x$year_scale, x$population_scale)
  # abline(fit_pop, col = "black")
  # abline(fit1_pop, col = "red")
  # abline(fit2_pop, col = "green")
  # abline(fit3_pop, col = "blue")
  # 
  # plot(x$year_scale, x$carspp_scale)
  # abline(fit_cars, col = "black")
  # abline(fit1_cars, col = "red")
  # abline(fit2_cars, col = "green")
  # abline(fit3_cars, col = "blue")
  
  x$gradient_pop <- fit_pop$coefficients[2]
  x$rsquare_pop <- summary(fit_pop)$r.squared
  
  x$gradient1_pop <- fit1_pop$coefficients[2]
  x$rsquare1_pop <- summary(fit1_pop)$r.squared
  
  x$gradient2_pop <- fit2_pop$coefficients[2]
  x$rsquare2_pop <- summary(fit2_pop)$r.squared
  
  x$gradient3_pop <- fit3_pop$coefficients[2]
  x$rsquare3_pop <- summary(fit3_pop)$r.squared
  
  x$gradient_cars <- fit_cars$coefficients[2]
  x$rsquare_cars <- summary(fit_cars)$r.squared
  
  x$gradient1_cars <- fit1_cars$coefficients[2]
  x$rsquare1_cars <- summary(fit1_cars)$r.squared
  
  x$gradient2_cars <- fit2_cars$coefficients[2]
  x$rsquare2_cars <- summary(fit2_cars)$r.squared
  
  x$gradient3_cars <- fit3_cars$coefficients[2]
  x$rsquare3_cars <- summary(fit3_cars)$r.squared
  
  sd_after_pop <- sapply(1:16, function(i,pop){
    sd(c(pop[i],pop[i+1]))
  }, pop = x$population_scale)
  sd_before_pop <- sapply(2:17, function(i,pop){
    sd(c(pop[i],pop[i-1]))
  }, pop = x$population_scale)
  x$sd_after_pop <- c(sd_after_pop,NA)
  x$sd_before_pop <- c(NA,sd_before_pop)
  x$sd_bna_pop <- x$sd_after_pop + x$sd_before_pop
  
  sd_after_cars <- sapply(1:16, function(i,pop){
    sd(c(pop[i],pop[i+1]))
  }, pop = x$population_scale)
  sd_before_cars <- sapply(2:17, function(i,pop){
    sd(c(pop[i],pop[i-1]))
  }, pop = x$population_scale)
  x$sd_after_cars <- c(sd_after_cars,NA)
  x$sd_before_cars <- c(NA,sd_before_cars)
  x$sd_bna_cars <- x$sd_after_cars + x$sd_before_cars
  
  return(x)
})
res <- bind_rows(res)
summary(res)

# Categorise areas a population rising/steady/falling
# Steady has gradient between -0.001 and 0.001

foo <- res[res$gradient_pop > -0.001 & res$gradient_pop < 0.001 , ]
foo <- foo[1:170,]

ggplot(foo, aes(year, population, color = code)) +
  geom_line(lwd = 2)

res$gradient_pop_class <- "steady"
res$gradient_pop_class <- ifelse(res$gradient_pop > 0.001, "rising", res$gradient_pop_class)
res$gradient_pop_class <- ifelse(res$gradient_pop < -0.001, "falling", res$gradient_pop_class)

table(res$gradient_pop_class)

res$gradient_cars_class <- "steady"
res$gradient_cars_class <- ifelse(res$gradient_cars > 0.001, "rising", res$gradient_cars_class)
res$gradient_cars_class <- ifelse(res$gradient_cars < -0.001, "falling", res$gradient_cars_class)

table(res$gradient_cars_class)

# Categorise as linear or non-linear

foo <- res[res$rsquare_pop > 0.65 , ]
foo <- foo[1:340, ]

ggplot(foo, aes(year, population, color = code)) +
  geom_line(lwd = 2)

res$linear_pop_class <- "slightly-linear"
res$linear_pop_class <- ifelse(res$rsquare_pop > 0.95, "linear", res$linear_pop_class)
res$linear_pop_class <- ifelse(res$rsquare_pop < 0.65, "non-linear", res$linear_pop_class)

table(res$linear_pop_class)

res$linear_cars_class <- "slightly-linear"
res$linear_cars_class <- ifelse(res$rsquare_cars > 0.95, "linear", res$linear_cars_class)
res$linear_cars_class <- ifelse(res$rsquare_cars < 0.65, "non-linear", res$linear_cars_class)

table(res$linear_cars_class)


res_summary <- res %>%
  group_by(code) %>%
  summarise(pop_min = min(population),
            pop_max = max(population),
            pop_sd = sd(population),
            gradient_pop = gradient_pop[1],
            gradient_cars = gradient_cars[1],
            rsquare_pop  = rsquare_pop[1],
            gradient1_pop = gradient1_pop[1],
            rsquare1_pop  = rsquare1_pop[1],
            gradient2_pop = gradient2_pop[1],
            rsquare2_pop  = rsquare2_pop[1],
            gradient3_pop = gradient3_pop[1],
            rsquare3_pop  = rsquare3_pop[1],
            gradient_pop_class = gradient_pop_class[1],
            linear_pop_class = linear_pop_class[1],
            bna_pop_min = min(sd_bna_pop, na.rm = TRUE),
            bna_pop_max = max(sd_bna_pop, na.rm = TRUE),
            bna_pop_mean = mean(sd_bna_pop, na.rm = TRUE),
            rsquare_cars  = rsquare_cars[1],
            gradient1_cars = gradient1_cars[1],
            rsquare1_cars  = rsquare1_cars[1],
            gradient2_cars = gradient2_cars[1],
            rsquare2_cars  = rsquare2_cars[1],
            gradient3_cars = gradient3_cars[1],
            rsquare3_cars  = rsquare3_cars[1],
            gradient_cars_class = gradient_cars_class[1],
            linear_cars_class = linear_cars_class[1],
            bna_cars_min = min(sd_bna_cars, na.rm = TRUE),
            bna_cars_max = max(sd_bna_cars, na.rm = TRUE),
            bna_cars_mean = mean(sd_bna_cars, na.rm = TRUE),
            
  )

res_summary$bna_range_pop <- res_summary$bna_pop_max - res_summary$bna_pop_min
res_summary$bna_range_cars <- res_summary$bna_cars_max - res_summary$bna_cars_min

# large bna range should indicate step chagne??

res_summary$bna_pop_class <- "slight-step"
res_summary$bna_pop_class <- ifelse(res_summary$bna_range_pop > 200, "stepped", res_summary$bna_pop_class)
res_summary$bna_pop_class <- ifelse(res_summary$bna_range_pop < 50, "no-step", res_summary$bna_pop_class)

res_summary$bna_cars_class <- "slight-step"
res_summary$bna_cars_class <- ifelse(res_summary$bna_range_cars > 200, "stepped", res_summary$bna_cars_class)
res_summary$bna_cars_class <- ifelse(res_summary$bna_range_cars < 50, "no-step", res_summary$bna_cars_class)


# Combine classes
res_summary$all_pop_class <- paste0(res_summary$gradient_pop_class," ",
                                res_summary$linear_class," ",
                                res_summary$bna_class)

types = as.data.frame(table(res_summary$all_class))

for(i in 1:nrow(types)){
  type = types$Var1[i]
  
  foo <- res[res$code %in% res_summary$code[res_summary$all_class == type] , ]
  if(length(unique(foo$code)) > 20){
    ids <- sample(unique(foo$code), 20)
    foo <- foo[foo$code %in% ids,]
  }
  ggplot(foo, aes(year, population, color = code)) +
    geom_line(lwd = 2) +
    ggtitle(type)
  ggsave(paste0("plots/pop_type_",gsub(" ","_",type),".png"))
}


