library(dplyr)
library(tidyverse)
library(plotly)
library(ggrepel)


#CARICO IL DATASET
sonno = read_csv("C:/Users/crist/OneDrive/Desktop/Università/DATA_SCIENCE/Progetto Finito/sleep_health_and_lifestyle_dataset.csv")

View(sonno)

#TIDY
colnames(sonno)[colnames(sonno)=="Blood Pressure"] <- "BloodPressure"
sonno1 = separate(sonno,BloodPressure, into = c("max_pressure","min_pressure"), sep = "/", convert = TRUE)
View(sonno1)

sonno2 = sonno1 %>% select(Age, `Quality of Sleep`)


#GRAFICI
sonno3= sonno2 %>% group_by(Age) %>%
  summarise(n=n(), avg_quality = mean(`Quality of Sleep`,na.rm=TRUE)) %>%
  arrange(desc(avg_quality))

age_quality = ggplot(data=sonno3,mapping = aes(x=Age,y= avg_quality)) +
  geom_point()+
  geom_line()+
  labs(
    title= "La qualità del sonno alle diverse età",
    x= "Qualità del sonno",
    y= "Età"
  )+
  theme_classic()
age_quality
AGEQ <- ggplotly(age_quality)


#CALO DRASTICO A 34 ANNI --> 2 DONNE SOVRAPPESO CHE SOFFRONO DI SLEEP APNEA

sonno4 = sonno1 %>% select(Age,`Sleep Duration`) %>% 
  group_by(Age) %>%
  summarise(n=n(), durata_media = mean(`Sleep Duration`,na.rm=TRUE)) %>%
  arrange(desc(durata_media))
sonno4

age_duration = ggplot(data=sonno4,mapping = aes(x=Age,y= durata_media)) +
  geom_point()+
  geom_line()+
  labs(
    title= "Età e durata del sonno",
    x= "Durata del sonno",
    y= "Età"
  )+
  theme_classic()
age_duration
AGED <- ggplotly(age_duration)

sonno5 = sonno1 %>% select(Age,`Quality of Sleep`,`Sleep Duration`)%>%
  group_by(Age) %>%
  summarise(n=n(), sleep_index = mean(`Quality of Sleep`*`Sleep Duration`, na.rm=TRUE))%>%
  arrange(desc(sleep_index))
sonno5

age_index = ggplot(data=sonno5,mapping = aes(x=Age,y= sleep_index)) +
  geom_point()+
  geom_line()+
  labs(
    title= "Grafico età/Indice del sonno",
    x= "Indice del sonno",
    y= "Età"
  )+
  theme_classic()
age_index
AGEIN <- ggplotly(age_index)

age34 = sonno1 %>% select(Age, `Quality of Sleep`, `Sleep Disorder`) %>%
  filter(Age == 34)

age34

#Lavoro e sleep index 

sonno9= sonno1 %>% select(Occupation,`Quality of Sleep`, `Sleep Duration`)%>%
  group_by(Occupation) %>%
  summarise(n=n(), sleep_index = mean(`Quality of Sleep`*`Sleep Duration`,na.rm=TRUE))%>%
  arrange(desc(sleep_index))
sonno9
prof_index = ggplot(data=sonno9) +
  geom_col(mapping = aes(x=Occupation,y= sleep_index,fill=Occupation))+
  coord_flip()+
  labs(
    title= "Professione e Indice del sonno",
    x= "Professione",
    y= "Indice del sonno",
    fill="Professione"
  )+
  theme_minimal()
PROFIN <- ggplotly(prof_index)



sonno10= sonno1 %>% select(Age,Gender,`Quality of Sleep`, `Sleep Duration`)%>%
  group_by(Gender,Age) %>%
  summarise(n=n(), sleep_index = mean(`Quality of Sleep`*`Sleep Duration`,na.rm=TRUE))%>%
  arrange(desc(sleep_index))

gender_index = ggplot(data=sonno10) +
  geom_point(mapping = aes(x=Age,y= sleep_index, color=Gender))+
  scale_color_manual(values = c(Male = "blue", Female = "deeppink3"))+
  labs(
    title= "Genere e Indice del sonno",
    x= "Età",
    y= "Indice del sonno",
    fill="Genere",
  )+
  theme_minimal()
gender_index


sonno11= sonno1 %>% select(Gender,`Quality of Sleep`, `Sleep Duration`)%>%
  group_by(Gender) %>%
  summarise(n=n(), sleep_index = mean(`Quality of Sleep`*`Sleep Duration`,na.rm=TRUE))%>%
  arrange(desc(sleep_index))

gender_bar = ggplot(data=sonno11) +
  geom_col(mapping = aes(x=Gender,y= sleep_index,fill=Gender))+
  labs(
    title= "Genere e Indice del sonno",
    x= "Genere",
    y= "Indice del sonno",
    fill="Genere"
  )+
  theme_minimal()
GENDERBAR <- ggplotly(gender_bar)



sonno12 = sonno1 %>% select(`Physical Activity Level`, `Quality of Sleep`, `Sleep Duration`,Age) %>%
  group_by(Age) %>%
  summarise(n=n(),sleep_index = mean(`Quality of Sleep`*`Sleep Duration`,na.rm=TRUE), avg_phy = mean(`Physical Activity Level`,na.rm=TRUE)) %>%
  arrange(desc(avg_phy))                   
view(sonno12)


physical = ggplot(data=sonno12,mapping = aes(x=avg_phy,y=sleep_index, color=Age))    +
  geom_point()+
  labs(
    title= "Attività fisica e Indice del sonno",
    x= "Attività fisica",
    y= "Indice del sonno",
  )+
  theme_minimal()
PHYS = ggplotly(physical)
PHYS



sonno13= sonno1 %>% select(Age,`BMI Category`,`Sleep Disorder`,`Quality of Sleep`, `Sleep Duration`)%>%
  group_by(`Sleep Disorder`) %>%
  summarise(n=n(),sleep_index = mean(`Quality of Sleep`*`Sleep Duration`,na.rm=TRUE))%>%
  arrange(desc(sleep_index))
view(sonno13)

dist_index = ggplot(data=sonno13) +
  geom_col(mapping = aes(x=`Sleep Disorder`,y= sleep_index, fill=`Sleep Disorder`))+
  scale_fill_manual(values = c(None="red", `Sleep Apnea`= "green",Insomnia="blue"))+
  labs(
    title= "Disturbi del sonno e Indice del sonno ",
    x= "Disturbi del sonno",
    y= "Indice del sonno",
    fill="Tipo di disturbo",
  )+
  theme_minimal()


dist_index



sonno14= sonno1 %>% select(Age,`BMI Category`,`Sleep Disorder`,`Quality of Sleep`, `Sleep Duration`)%>%
  group_by(`BMI Category`) %>%
  summarise(n=n(),sleep_index = mean(`Quality of Sleep`*`Sleep Duration`,na.rm=TRUE))%>%
  arrange(desc(sleep_index))


BMI_index = ggplot(data=sonno14) +
  geom_col(mapping = aes(x=`BMI Category`,y= sleep_index, fill=`BMI Category`))+
  scale_fill_manual(values = c(Normal="cyan", `Normal Weight`= "green",Overweight="orange",Obese="red"))+
  labs(
    title= "Categria BMI e Indice del sonno ",
    x= "Categoria BMI",
    y= "Indice del sonno",
    fill="Categorie" )


BMI_index
BMI <- ggplotly(BMI_index)
BMI