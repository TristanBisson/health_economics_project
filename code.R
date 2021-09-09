library(haven)
library(tidyverse)
library(broom)
library(stargazer)

df <- read_dta("C:/Users/Tristan/Desktop/ECN 6983/ipumsi_00003.dta")

df$disemp <- as.numeric(df$disemp)

df$disemp_bin <- ifelse(df$disemp == "1", 1,
                        ifelse(df$disemp == "2", 0, NA_integer_))

table(df$disemp_bin)

# EMPSTAT

table(df$age, df$empstat)

# Moyenne invalidité par rapport à l'âge

# travail variable age2

df$age.gp <- as.factor(df$age2)

table(df$age2)


df <- df %>%
  filter(age.gp %in% c(12:20)) %>%
  mutate(age.gp = recode(age.gp, "12" = "20 - 24",
                         "13" = "25 - 29",
                         "14" = "30 - 34", 
                         "15" = "35 - 39",
                         "16" = "40 - 44",
                         "17" = "45 - 49",
                         "18" = "50 - 54",
                         "19" = "55 - 59",
                         "20" = "60 - 64"))


# Figure distribution âge

library(sjmisc)

t <- data.frame(frq(df$age.gp))

t <- t %>%
  filter(frq != 0)

ggplot(t, aes(x = val, y = frq))+
  geom_bar(stat = "identity")+
  labs(x = "Âge", y = "Fréquence")+
  theme_classic()

ggsave("p2.png", plot = last_plot(), width = 15, height = 9, units = "cm")


# Figure distribution invalidité

t1 <- data.frame(frq(df$disemp_bin))

t1 <- t1 %>%
  filter(frq != 0)%>%
  mutate(inval = recode(val, "1" = "Invalide", "0" = "Non-invalide"))

ggplot(t1, aes(x = inval, y = frq))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "Fréquence")+
  theme_classic()+
  scale_y_continuous(labels = scales::comma,  limits = c(0, 350000))+
  geom_text(aes(label = frq), size = 4, hjust = 0.5, vjust = -1)
  

ggsave("p3.png", plot = last_plot(), width = 15, height = 9, units = "cm")

# distribution invalidité par âge

t <- data.frame(prop.table(table(df$age.gp, df$disemp_bin),2)*100)

t <- t %>%
  filter(Freq != 0)%>%
  mutate(inval = recode(Var2, "1" = "Invalide", "0" = "Non-invalide"))
  

ggplot(t, aes(x = Var1, y = Freq, fill = inval)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(fill = "Invalidité du travail", x = "Groupe d'âge", y = "Pourcentage")+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  theme_classic()

ggsave("p4.png", plot = last_plot(), width = 20, height = 9, units = "cm")

# Figure moyenne par âge

p1<- ggplot(df, aes(x = age.gp, y = disemp_bin))+
  stat_summary(fun = mean)+
  theme_classic()+
  labs(x ="Âge", y = "Invaliditié liée au travail")+
  guides(colour = guide_legend(override.aes = list(size=1)))

p1

ggsave("p1.png", plot = last_plot(), width = 15, height = 9, units = "cm")


# régression discontie

df <- df %>%
  mutate(age_1 = ifelse(age %in% c(25:41), age, NA)) %>%
  mutate(age_2 = ifelse(age %in% c(28:38), age, NA)) %>%
  mutate(age_3 = ifelse(age %in% c(30:36), age, NA)) %>%
  mutate(birth_year = 1991 - age_1) 

## list d'année pour les graph

x <- 1950
z <- NULL 

while (x <= 1966)
{
  z <- c(z, x)
  x <- x+2
}

z

# Année de naissance 

df <- df %>%
  mutate(D = as.numeric(ifelse(birth_year >= 1958,1,0)))


ggplot(subset(df, !is.na(birth_year)), aes(x = birth_year , y = disemp_bin, color = D)) +
  geom_smooth(method = "lm")+
  geom_vline(xintercept = 1958, linetype = "longdash")+
  labs(y = "Probabilités d'avoir un handicap lié au travail", x = "Année de naissance", color = "Légende")+
  theme_classic()+
  scale_x_continuous(breaks = z)

ggsave("p5.png", plot = last_plot(), width = 15, height = 9, units = "cm")


# moyenne inval

ggplot(df, aes(x = birth_year, y = disemp_bin))+
  stat_summary(fun = mean)+
  stat_summary(geom = "errorbar", width = 0.5)+
  geom_vline(xintercept = 1958, linetype = "longdash")+
  theme_classic()+
  labs(x ="Année de naissance", y = "Invaliditié liée au travail")+
  guides(colour = guide_legend(override.aes = list(size=1)))+
  scale_x_continuous(breaks = z)

ggsave("p6.png", plot = last_plot(), width = 15, height = 9, units = "cm")

# moyenne inval homme

gh <- ggplot(subset(df, sex == 1), aes(x = birth_year, y = disemp_bin))+
  stat_summary(fun = mean)+
  stat_summary(geom = "errorbar", width = 0.5)+
  geom_vline(xintercept = 1958, linetype = "longdash")+
  theme_classic()+
  labs(x ="", y = "Invaliditié liée au travail")+
  guides(colour = guide_legend(override.aes = list(size=1)))+
  scale_x_continuous(breaks = z)+
  ggtitle("Hommes")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))
  
gh

# moyenne inval femme

gf <- ggplot(subset(df, sex == 2), aes(x = birth_year, y = disemp_bin))+
  stat_summary(fun = mean)+
  stat_summary(geom = "errorbar", width = 0.5)+
  geom_vline(xintercept = 1958, linetype = "longdash")+
  theme_classic()+
  labs(x ="Année de naissance", y = "Invaliditié liée au travail")+
  guides(colour = guide_legend(override.aes = list(size=1)))+
  scale_x_continuous(breaks = z)+
  ggtitle("Femmes")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))

# grid arrange

library(gridExtra)

grid.arrange(gh,gf)

ggsave("ghgf.png", plot=grid.arrange(gh,gf), width = 15, height = 18, units = "cm")

##### Niveau d'étude ##### 

df$educuk <- as.numeric(df$educuk)
df$educuk[df$educuk == "99"] <- NA

df <- df %>%
  mutate(educuk_gp = recode(educuk, "10" = 1,
                            "11" = 1,
                            "12" = 0)) %>%
  mutate(educuk_gp2 = recode(educuk, "10" = 2,
                            "11" = 1,
                            "12" = 0))

ggplot(subset(df, educuk_gp != 99), aes(x = birth_year, y = educuk_gp))+
  stat_summary(fun = mean)+
  stat_summary(geom = "errorbar", width = 0.5)+
  geom_vline(xintercept = 1958, linetype = "longdash")+
  theme_classic()+
  labs(x ="Année de naissance", y = "Niveau d'éducation")+
  scale_x_continuous(breaks = z)

ggsave("p7.png", plot = last_plot(), width = 15, height = 9, units = "cm")



# Niveau d'étude par sex

# homme
geh <- ggplot(subset(df, educuk_gp != 99 & sex ==1), aes(x = birth_year, y = educuk_gp))+
  stat_summary(fun = mean)+
  stat_summary(geom = "errorbar", width = 0.5)+
  geom_vline(xintercept = 1958, linetype = "longdash")+
  theme_classic()+
  labs(x ="", y = "Niveau d'éducation")+
  scale_x_continuous(breaks = z)+
  ggtitle("Moyenne pour les hommes")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))

# femme
gef <- ggplot(subset(df, educuk_gp != 99 & sex ==2), aes(x = birth_year, y = educuk_gp))+
  stat_summary(fun = mean)+
  stat_summary(geom = "errorbar", width = 0.5 )+
  geom_vline(xintercept = 1958, linetype = "longdash")+
  theme_classic()+
  labs(x ="Année de naissance", y = "Niveau d'éducation")+
  scale_x_continuous(breaks = z)+
  ggtitle("Moyenne pour les femmes")+
  theme(plot.title = element_text(hjust = 0.5, size = 10))

grid.arrange(geh,gef)

ggsave("gehgef.png", plot=grid.arrange(geh,gef), width = 15, height = 18, units = "cm")

##### régression #####

## travail sur les variables ##

# age

df$age <- as.numeric(df$age)
df$age[df$age < 24] <- NA


# sex (femme)

df$sex <- as.numeric(df$sex)

df <- df %>%
  mutate(femme = recode(sex, "1" = 0, "2" = 1))

table(df$femme)

# race (white/non-white)

df <- df %>%
  mutate(n_white = ifelse(race == "10", 0, 1))

###### regression education ######

me1 <- lm(educuk_gp ~ D, data = df)
me2<- lm(educuk_gp ~ D * I(age_1 - 33), data = df)


stargazer(me1, me2, type = "text")

###### regression employment disabilty ######

#### OLS par age ####

# age_1 (25 - 41)

m1a <- lm(disemp_bin ~ D, data = df)

m4a <- lm(disemp_bin ~ D * I(age_1-33) + D * I((age_1-33)^2) + D * I((age_1-33)^3), data= df)

df$age_1 <- df$age_2

# age_2 (28 - 38)

m1b <- lm(disemp_bin ~ D, data = subset(df, !is.na(age_1)))

m4b <- lm(disemp_bin ~ D * I(age_1-33) + D * I((age_1-33)^2) + D * I((age_1-33)^3), data= df)


stargazer(m1a, m4a, m1b, m4b, type = "latex",
          keep.stat = "n", align = TRUE, no.space=TRUE)

#### OLS par sexe et 28-38 ####

# homme

m1a <- lm(disemp_bin ~ D, data = subset(df, femme == 0))

m4a <- lm(disemp_bin ~ D * I(age_1-33) + D * I((age_1-33)^2) + D * I((age_1-33)^3), data = subset(df, femme == 0))


#femme

m1b <- lm(disemp_bin ~ D, data= subset(df, femme ==1))

m4b <- lm(disemp_bin ~ D * I(age_1-33) + D * I((age_1-33)^2) + D * I((age_1-33)^3), data= subset(df, femme ==1))


stargazer(m1a, m4a, m1b, m4b, type = "latex",
          keep.stat = "n", align = TRUE, no.space=TRUE)

#### 2SLS et OLS ####

library("ivreg")

ols1<- lm(disemp_bin ~ educuk_gp, data= df)
ols2<- lm(disemp_bin ~ educuk_gp * I(age_1-33) + educuk_gp * I((age_1-33)^2) + educuk_gp * I((age_1-33)^3), data= df)

tsls1 <- ivreg(disemp_bin ~ educuk_gp| D, data= df)
tsls2 <- ivreg(disemp_bin ~ educuk_gp * I(age_1-33) + educuk_gp * I((age_1-33)^2) + educuk_gp * I((age_1-33)^3)
             | D * I(age_1-33) + D * I((age_1-33)^2) + D * I((age_1-33)^3), data= df)


stargazer(ols1, ols2, tsls1, tsls2, type = "text",
          keep.stat = "n", align = TRUE, no.space = TRUE)

### tableau de statistique descriptive ####



t <- data.frame(df$age, df$disemp_bin, df$educuk_gp)

t <- t %>%
  filter(df$age %in% c(20:64))

table(t$age)

stargazer(t, type = "latex", omit.summary.stat = c("p25", "p75"), covariate.labels = c("Âge", "Invalidité"))
