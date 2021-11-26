# COnvert Cars into cars per person
library(dplyr)
library(tidyr)
library(ggplot2)

# Population data

pop_all <- readRDS("data/population/LSOA_population_2002_2018.Rds")

# Classify Population Change
pop_all <- pivot_longer(pop_all, cols = names(pop_all)[2:18], names_to = "year")
pop_all$year <- as.numeric(gsub("pop_","", pop_all$year))
names(pop_all) <- c("code","year","population")

pop_split <- pop_all %>%
  group_by(code) %>%
  group_split()

res <- pbapply::pblapply(pop_split, function(x){
  nm <- x$code[1]
  x$population_scale <- x$population / x$population[1]
  x$year_scale <- x$year - x$year[1]
  
  #intercept <- x$population[1]
  fit <- lm(population_scale  ~   year_scale, data = x)

  # Gradient for bits of the line
  fit1 <- lm(population_scale  ~  year_scale, data = x[1:5,])
  fit2 <- lm(population_scale  ~  year_scale, data = x[6:10,])
  fit3 <- lm(population_scale  ~  year_scale, data = x[11:16,])
  
  # plot(x$year_scale, x$population_scale)
  # abline(fit, col = "black")
  # abline(fit1, col = "red")
  # abline(fit2, col = "green")
  # abline(fit3, col = "blue")
  
  x$gradient <- fit$coefficients[2]
  x$rsquare <- summary(fit)$r.squared
  
  x$gradient1 <- fit1$coefficients[2]
  x$rsquare1 <- summary(fit1)$r.squared
  
  x$gradient2 <- fit2$coefficients[2]
  x$rsquare2 <- summary(fit2)$r.squared
  
  x$gradient3 <- fit3$coefficients[2]
  x$rsquare3 <- summary(fit3)$r.squared
  
  sd_after <- sapply(1:16, function(i,pop){
    sd(c(pop[i],pop[i+1]))
  }, pop = x$population_scale)
  sd_before <- sapply(2:17, function(i,pop){
    sd(c(pop[i],pop[i-1]))
  }, pop = x$population_scale)
  x$sd_after <- c(sd_after,NA)
  x$sd_before <- c(NA,sd_before)
  x$sd_bna <- x$sd_after + x$sd_before
  
  return(x)
})
res <- bind_rows(res)
#row.names(res) <- 1:nrow(res)

summary(res)

# Categorise areas a population rising/steady/falling
# Steady has gradient between 5 and -5

foo <- res[res$gradient > -0.001 & res$gradient < 0.001 , ]
foo <- foo[1:170,]

ggplot(foo, aes(year, population, color = code)) +
  geom_line(lwd = 2)

res$gradient_class <- "steady"
res$gradient_class <- ifelse(res$gradient > 0.001, "rising", res$gradient_class)
res$gradient_class <- ifelse(res$gradient < -0.001, "falling", res$gradient_class)

table(res$gradient_class)

# Categorise as linear or non-linear

foo <- res[res$rsquare > 0.65 , ]
foo <- foo[1:340, ]

ggplot(foo, aes(year, population, color = code)) +
  geom_line(lwd = 2)

res$linear_class <- "slightly-linear"
res$linear_class <- ifelse(res$rsquare > 0.95, "linear", res$linear_class)
res$linear_class <- ifelse(res$rsquare < 0.65, "non-linear", res$linear_class)

table(res$linear_class)

res_summary <- res %>%
  group_by(code) %>%
  summarise(pop_min = min(population),
            pop_max = max(population),
            pop_sd = sd(population),
            gradient = gradient[1],
            rsquare  = rsquare[1],
            gradient1 = gradient1[1],
            rsquare1  = rsquare1[1],
            gradient2 = gradient2[1],
            rsquare2  = rsquare2[1],
            gradient3 = gradient3[1],
            rsquare3  = rsquare3[1],
            gradient_class = gradient_class[1],
            linear_class = linear_class[1],
            bna_min = min(sd_bna, na.rm = TRUE),
            bna_max = max(sd_bna, na.rm = TRUE),
            bna_mean = mean(sd_bna, na.rm = TRUE)
            )

res_summary$bna_range <- res_summary$bna_max - res_summary$bna_min

# large bna range should indicate step chagne??

res_summary$bna_class <- "slight-step"
res_summary$bna_class <- ifelse(res_summary$bna_range > 200, "stepped", res_summary$bna_class)
res_summary$bna_class <- ifelse(res_summary$bna_range < 50, "no-step", res_summary$bna_class)


# foo <- res_summary$code[res_summary$bna_range > 200]
# foo <- res[res$code %in% foo, ]
# foo <- foo[1:340,]
# unique(foo$linear_class)
# 
# ggplot(foo[foo$linear_class == "linear", ], aes(year, population, color = code)) +
#   geom_line(lwd = 2)
# 
# ggplot(foo[foo$linear_class == "slightly-linear", ], aes(year, population, color = code)) +
#   geom_line(lwd = 2)
# 
# ggplot(foo[foo$linear_class == "non-linear", ], aes(year, population, color = code)) +
#   geom_line(lwd = 2)
# 
# foo <- res_summary$code[res_summary$bna_range < 50]
# foo <- res[res$code %in% foo, ]
# foo <- foo[1:340,]
# unique(foo$linear_class)
# 
# ggplot(foo[foo$linear_class == "linear", ], aes(year, population, color = code)) +
#   geom_line(lwd = 2)
# 
# ggplot(foo[foo$linear_class == "slightly-linear", ], aes(year, population, color = code)) +
#   geom_line(lwd = 2)
# 
# ggplot(foo[foo$linear_class == "non-linear", ], aes(year, population, color = code)) +
#   geom_line(lwd = 2)


# Combine classes


res_summary$all_class <- paste0(res_summary$gradient_class," ",
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


# Rising Linear No-Step
