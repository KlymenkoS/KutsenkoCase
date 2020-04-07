library(haven)
df <- read_sav("C:/Users/bksta/OneDrive/Документи/STUDYING/3 КУРС/Соціальна структура суспільства/Завдання до тижня самостійної роботи/2 тиждень/R_Files/US2016.sav")

df_e5 <- subset(df, select = c(V110,V111,V112,V113,V114,V115,V116,V117,V118,V119,V120,V121,V122,
                                 V123,V124,V125,V126))# формуєм змінну Е5 в окремий масив (char);
df_e5_n <- subset(df, select = c(V110,V111,V112,V113,V114,V115,V116,V117,V118,V119,V120,V121,V122,
                               V123,V124,V125,V126))# формуєм змінну Е5 в окремий масив (numeric);
#___________________________________________________________________
#створюємо датафрейм df_e5_g з середнім значенням по 7-бальній шкалі;

df_e5_g <- data.frame(Nationality = c("Американці", "Араби", "Білоруси", "Угорці",
                              "Грузини", "Євреї", "Кримські татари", "Молдавани",
                              "Німці", "Поляки","Росіяни", "Румуни", "Турки",
                              "Українці", "Українці, що проживають в інших країнах",
                              "Цигани", "Чеченці"))
df_e5_g$SD <- ifelse(df_e5_g$Nationality == "Американці", mean(df_e5_n$V110, na.rm = T,digits = 2),
        ifelse(df_e5_g$Nationality == "Араби", mean(df_e5_n$V111, na.rm = T),
        ifelse(df_e5_g$Nationality == "Білоруси", mean(df_e5_n$V112, na.rm = T),
        ifelse(df_e5_g$Nationality == "Угорці", mean(df_e5_n$V113, na.rm = T),
        ifelse(df_e5_g$Nationality == "Грузини", mean(df_e5_n$V114, na.rm = T),
        ifelse(df_e5_g$Nationality == "Євреї", mean(df_e5_n$V115, na.rm = T),
        ifelse(df_e5_g$Nationality == "Кримські татари", mean(df_e5_n$V116, na.rm = T),
        ifelse(df_e5_g$Nationality == "Молдавани", mean(df_e5_n$V117, na.rm = T),
        ifelse(df_e5_g$Nationality == "Німці", mean(df_e5_n$V118, na.rm = T),
        ifelse(df_e5_g$Nationality == "Поляки", mean(df_e5_n$V119, na.rm = T),
        ifelse(df_e5_g$Nationality == "Росіяни", mean(df_e5_n$V120, na.rm = T),
        ifelse(df_e5_g$Nationality == "Румуни", mean(df_e5_n$V121, na.rm = T),
        ifelse(df_e5_g$Nationality == "Турки", mean(df_e5_n$V122, na.rm = T),
        ifelse(df_e5_g$Nationality == "Українці", mean(df_e5_n$V123, na.rm = T),
        ifelse(df_e5_g$Nationality == "Українці, що проживають в інших країнах",mean(df_e5_n$V124, na.rm = T),
        ifelse(df_e5_g$Nationality == "Цигани", mean(df_e5_n$V125, na.rm = T),
        ifelse(df_e5_g$Nationality == "Чеченці", mean(df_e5_n$V126, na.rm = T), df_e5_g$Nationality)))))))))))))))))
        

#створюємо візуалізацію із показником середньої національної дистанції;

df_e5_g <- df_e5_g[order(df_e5_g$SD),] #сортуємо за зростанням SD

library(ggplot2)
p_e5<- ggplot(df_e5_g, aes(x= reorder(Nationality, -SD), y= SD))+
  geom_bar(stat="identity", width = 0.5, col = "Black", fill = "Blue")+
  theme_light()+
  theme(text = element_text(size = 14), axis.text.x = element_text(angle=90, vjust = 1))+
  ylab("")+
  xlab("Індекс соціальної дистанційованості українців")
 




#перекодовуємо з числових показників 1,2,3... в підписи;_________________________________________________

df_e5$V110 <- ifelse(df_e5$V110 == 1,"Членів моєї сім'ї",
                     ifelse(df_e5$V110 == 2,"Близьких друзів",
                     ifelse(df_e5$V110 == 3,"Сусідів",
                     ifelse(df_e5$V110 == 4,"Колег по роботі",
                     ifelse(df_e5$V110 == 5,"Мешканців України",
                     ifelse(df_e5$V110 == 6,"Відвідувачів України, туристів",
                     ifelse(df_e5$V110 == 7,"Взагалі не допускав би в Україну",df_e5$V110)))))))

df_e5$V111 <- ifelse(df_e5$V111 == 1,"Членів моєї сім'ї",
                     ifelse(df_e5$V111 == 2,"Близьких друзів",
                     ifelse(df_e5$V111 == 3,"Сусідів",
                     ifelse(df_e5$V111 == 4,"Колег по роботі",
                     ifelse(df_e5$V111 == 5,"Мешканців України",
                     ifelse(df_e5$V111 == 6,"Відвідувачів України, туристів",
                     ifelse(df_e5$V111 == 7,"Взагалі не допускав би в Україну",df_e5$V110)))))))
df_e5$V112 <- ifelse(df_e5$V112 == 1,"Членів моєї сім'ї",
                     ifelse(df_e5$V112 == 2,"Близьких друзів",
                     ifelse(df_e5$V112 == 3,"Сусідів",
                     ifelse(df_e5$V112 == 4,"Колег по роботі",
                     ifelse(df_e5$V112 == 5,"Мешканців України",
                     ifelse(df_e5$V112 == 6,"Відвідувачів України, туристів",
                     ifelse(df_e5$V112 == 7,"Взагалі не допускав би в Україну",df_e5$V110)))))))
df_e5$V113 <- ifelse(df_e5$V113 == 1,"Членів моєї сім'ї",
                     ifelse(df_e5$V113 == 2,"Близьких друзів",
                     ifelse(df_e5$V113 == 3,"Сусідів",
                     ifelse(df_e5$V113 == 4,"Колег по роботі",
                     ifelse(df_e5$V113 == 5,"Мешканців України",
                     ifelse(df_e5$V113 == 6,"Відвідувачів України, туристів",
                     ifelse(df_e5$V113 == 7,"Взагалі не допускав би в Україну",df_e5$V110)))))))
df_e5$V114 <- ifelse(df_e5$V114 == 1,"Членів моєї сім'ї",
                            ifelse(df_e5$V114 == 2,"Близьких друзів",
                            ifelse(df_e5$V114 == 3,"Сусідів",
                            ifelse(df_e5$V114 == 4,"Колег по роботі",
                            ifelse(df_e5$V114 == 5,"Мешканців України",
                            ifelse(df_e5$V114 == 6,"Відвідувачів України, туристів",
                            ifelse(df_e5$V114 == 7,"Взагалі не допускав би в Україну",df_e5$V110)))))))
df_e5$V115 <- ifelse(df_e5$V115 == 1,"Членів моєї сім'ї",
                     ifelse(df_e5$V115 == 2,"Близьких друзів",
                     ifelse(df_e5$V115 == 3,"Сусідів",
                     ifelse(df_e5$V115 == 4,"Колег по роботі",
                     ifelse(df_e5$V115 == 5,"Мешканців України",
                     ifelse(df_e5$V115 == 6,"Відвідувачів України, туристів",
                     ifelse(df_e5$V115 == 7,"Взагалі не допускав би в Україну",df_e5$V110)))))))
df_e5$V116 <- ifelse(df_e5$V116 == 1,"Членів моєї сім'ї",
                     ifelse(df_e5$V116 == 2,"Близьких друзів",
                     ifelse(df_e5$V116 == 3,"Сусідів",
                     ifelse(df_e5$V116 == 4,"Колег по роботі",
                     ifelse(df_e5$V116 == 5,"Мешканців України",
                     ifelse(df_e5$V116 == 6,"Відвідувачів України, туристів",
                     ifelse(df_e5$V116 == 7,"Взагалі не допускав би в Україну",df_e5$V110)))))))
df_e5$V117 <- ifelse(df_e5$V117 == 1,"Членів моєї сім'ї",
                     ifelse(df_e5$V117 == 2,"Близьких друзів",
                     ifelse(df_e5$V117 == 3,"Сусідів",
                     ifelse(df_e5$V117 == 4,"Колег по роботі",
                     ifelse(df_e5$V117 == 5,"Мешканців України",
                     ifelse(df_e5$V117 == 6,"Відвідувачів України, туристів",
                     ifelse(df_e5$V117 == 7,"Взагалі не допускав би в Україну",df_e5$V110)))))))
df_e5$V118 <- ifelse(df_e5$V118 == 1,"Членів моєї сім'ї",
                     ifelse(df_e5$V118 == 2,"Близьких друзів",
                     ifelse(df_e5$V118 == 3,"Сусідів",
                     ifelse(df_e5$V118 == 4,"Колег по роботі",
                     ifelse(df_e5$V118 == 5,"Мешканців України",
                     ifelse(df_e5$V118 == 6,"Відвідувачів України, туристів",
                     ifelse(df_e5$V118 == 7,"Взагалі не допускав би в Україну",df_e5$V110)))))))
df_e5$V119 <- ifelse(df_e5$V119 == 1,"Членів моєї сім'ї",
                     ifelse(df_e5$V119 == 2,"Близьких друзів",
                     ifelse(df_e5$V119 == 3,"Сусідів",
                     ifelse(df_e5$V119 == 4,"Колег по роботі",
                     ifelse(df_e5$V119 == 5,"Мешканців України",
                     ifelse(df_e5$V119 == 6,"Відвідувачів України, туристів",
                     ifelse(df_e5$V119 == 7,"Взагалі не допускав би в Україну",df_e5$V110)))))))
df_e5$V120 <- ifelse(df_e5$V120 == 1,"Членів моєї сім'ї",
                     ifelse(df_e5$V120 == 2,"Близьких друзів",
                     ifelse(df_e5$V120 == 3,"Сусідів",
                     ifelse(df_e5$V120 == 4,"Колег по роботі",
                     ifelse(df_e5$V120 == 5,"Мешканців України",
                     ifelse(df_e5$V120 == 6,"Відвідувачів України, туристів",
                     ifelse(df_e5$V120 == 7,"Взагалі не допускав би в Україну",df_e5$V110)))))))
df_e5$V121 <- ifelse(df_e5$V121 == 1,"Членів моєї сім'ї",
                     ifelse(df_e5$V121 == 2,"Близьких друзів",
                     ifelse(df_e5$V121 == 3,"Сусідів",
                     ifelse(df_e5$V121 == 4,"Колег по роботі",
                     ifelse(df_e5$V121 == 5,"Мешканців України",
                     ifelse(df_e5$V121 == 6,"Відвідувачів України, туристів",
                     ifelse(df_e5$V121 == 7,"Взагалі не допускав би в Україну",df_e5$V110)))))))
df_e5$V122 <- ifelse(df_e5$V122 == 1,"Членів моєї сім'ї",
                     ifelse(df_e5$V122 == 2,"Близьких друзів",
                     ifelse(df_e5$V122 == 3,"Сусідів",
                     ifelse(df_e5$V122 == 4,"Колег по роботі",
                     ifelse(df_e5$V122 == 5,"Мешканців України",
                     ifelse(df_e5$V122 == 6,"Відвідувачів України, туристів",
                     ifelse(df_e5$V122 == 7,"Взагалі не допускав би в Україну",df_e5$V110)))))))

df_e5$V123 <- ifelse(df_e5$V123 == 1,"Членів моєї сім'ї",
                     ifelse(df_e5$V123 == 2,"Близьких друзів",
                     ifelse(df_e5$V123 == 3,"Сусідів",
                     ifelse(df_e5$V123 == 4,"Колег по роботі",
                     ifelse(df_e5$V123 == 5,"Мешканців України",
                     ifelse(df_e5$V123 == 6,"Відвідувачів України, туристів",
                     ifelse(df_e5$V123 == 7,"Взагалі не допускав би в Україну",df_e5$V110)))))))
df_e5$V124 <- ifelse(df_e5$V124 == 1,"Членів моєї сім'ї",
                     ifelse(df_e5$V124 == 2,"Близьких друзів",
                     ifelse(df_e5$V124 == 3,"Сусідів",
                     ifelse(df_e5$V124 == 4,"Колег по роботі",
                     ifelse(df_e5$V124 == 5,"Мешканців України",
                     ifelse(df_e5$V124 == 6,"Відвідувачів України, туристів",
                     ifelse(df_e5$V124 == 7,"Взагалі не допускав би в Україну",df_e5$V110)))))))
df_e5$V125 <- ifelse(df_e5$V125 == 1,"Членів моєї сім'ї",
                     ifelse(df_e5$V125 == 2,"Близьких друзів",
                     ifelse(df_e5$V125 == 3,"Сусідів",
                     ifelse(df_e5$V125 == 4,"Колег по роботі",
                     ifelse(df_e5$V125 == 5,"Мешканців України",
                     ifelse(df_e5$V125 == 6,"Відвідувачів України, туристів",
                     ifelse(df_e5$V125 == 7,"Взагалі не допускав би в Україну",df_e5$V110)))))))
df_e5$V126 <- ifelse(df_e5$V126 == 1,"Членів моєї сім'ї",
                     ifelse(df_e5$V126 == 2,"Близьких друзів",
                     ifelse(df_e5$V126 == 3,"Сусідів",
                     ifelse(df_e5$V126 == 4,"Колег по роботі",
                     ifelse(df_e5$V126 == 5,"Мешканців України",
                     ifelse(df_e5$V126 == 6,"Відвідувачів України, туристів",
                     ifelse(df_e5$V126 == 7,"Взагалі не допускав би в Україну",df_e5$V110)))))))

#створюємо візуалізацію окремо по кожній національності;______________________________________
library("ggplot2")
ggplot(df_e5,aes(x=V110))+# для американців;
  geom_bar(width = 0.5)+
  theme_light()+
  ggtitle("Я згоден допустити американців як…", subtitle = ("(Абсолютні показники)"))+
  ylab("Кількість відповідей")+
  xlab("")+
  theme(text = element_text(size = 14), axis.text.x = element_text(angle=90, vjust = 1))+
  scale_x_discrete(na.translate = FALSE)# прибирає NA із графіка, але не з масиву;

#_______________________________________________________________________________________________
                                    
                                               #ФАКТОРНИЙ АНАЛІЗ Е5

#перевіряємо на наявність пропущених значень;

which(is.na(df_e5_n)==T)

#замінюємо пропущені значення середнім значенням у змінній;

df_e5_n$V110 <- replace(df_e5_n$V110, is.na(df_e5_n$V110)==T, mean(df_e5_n$V110, na.rm = T))
df_e5_n$V111 <- replace(df_e5_n$V111, is.na(df_e5_n$V111)==T, mean(df_e5_n$V111, na.rm = T))
df_e5_n$V112 <- replace(df_e5_n$V112, is.na(df_e5_n$V112)==T, mean(df_e5_n$V112, na.rm = T))
df_e5_n$V113 <- replace(df_e5_n$V113, is.na(df_e5_n$V113)==T, mean(df_e5_n$V113, na.rm = T))
df_e5_n$V114 <- replace(df_e5_n$V114, is.na(df_e5_n$V114)==T, mean(df_e5_n$V114, na.rm = T))
df_e5_n$V115 <- replace(df_e5_n$V115, is.na(df_e5_n$V115)==T, mean(df_e5_n$V115, na.rm = T))
df_e5_n$V116 <- replace(df_e5_n$V116, is.na(df_e5_n$V116)==T, mean(df_e5_n$V116, na.rm = T))
df_e5_n$V117 <- replace(df_e5_n$V117, is.na(df_e5_n$V117)==T, mean(df_e5_n$V117, na.rm = T))
df_e5_n$V118 <- replace(df_e5_n$V118, is.na(df_e5_n$V118)==T, mean(df_e5_n$V118, na.rm = T))
df_e5_n$V119 <- replace(df_e5_n$V119, is.na(df_e5_n$V119)==T, mean(df_e5_n$V119, na.rm = T))
df_e5_n$V120<- replace(df_e5_n$V120, is.na(df_e5_n$V120)==T, mean(df_e5_n$V120, na.rm = T))
df_e5_n$V121 <- replace(df_e5_n$V121, is.na(df_e5_n$V121)==T, mean(df_e5_n$V121, na.rm = T))
df_e5_n$V122 <- replace(df_e5_n$V122, is.na(df_e5_n$V122)==T, mean(df_e5_n$V122, na.rm = T))
df_e5_n$V123 <- replace(df_e5_n$V123, is.na(df_e5_n$V123)==T, mean(df_e5_n$V123, na.rm = T))
df_e5_n$V124 <- replace(df_e5_n$V124, is.na(df_e5_n$V124)==T, mean(df_e5_n$V124, na.rm = T))
df_e5_n$V125 <- replace(df_e5_n$V125, is.na(df_e5_n$V125)==T, mean(df_e5_n$V125, na.rm = T))
df_e5_n$V126 <- replace(df_e5_n$V126, is.na(df_e5_n$V126)==T, mean(df_e5_n$V126, na.rm = T))



f_a <- factanal(df_e5_n,4, rotation = "varimax") #проводимо факт
plot(f_a$loadings)




#_______________________________________________________________________________________________________________

                                   #РЕГРЕСІЙНИЙ АНАЛІЗ (ЛІНІЙНА РЕГРЕСІЯ)

# готуємо датафрейм для подальшого регресійного аналізу (лінійна регресія), перетворюючи в факторні в numeric;

df_rg_e5 <- data.frame(sd = as.numeric(df_e5_g$SD), sex = as.numeric(df$V278), age = as.numeric(df$V279),
                  education = as.numeric(df$V281), religion = as.numeric(df$V286), place = as.numeric(df$V290),
                  empl = as.numeric(df$V214), opport = as.numeric(df$V248))

cor(df_rg_e5, use = "complete.obs")         #створюємо кореляційну матрицю для створеного масиву (use = "complete.obs" - не берем до уваги пропущені значення)

cor.test(df_rg_e5$sd, df_rg_e5$religion)    #перевіряємо, чи є кореляція між sd та віросповіданням;




#перетворюємо змінну religion на факторну для подальшого регресійного аналізу;

df_rg_e5$religion <- factor(df_rg_e5$religion, labels = c("Не релігійний", "Православ'я",
                                                          "Католицизм", "Греко-католицизм", "Протестантизм",
                                                          "Іслам", "Іудаїзм")) 

cor.test(df_rg_e5$sd, df_rg_e5$religion)              #перевіряємо, чи є кореляція між sd та віросповіданням;

summary(lm(df_rg_e5$sd ~ df_rg_e5$religion)) #будуємо лінійну регресію для цих змінних;

  # SD= 4.59 +  0.63512 * Іудаїзм

ggplot(df_rg_e5, aes(religion,sd))+
  geom_point(size = 5)+

df_rg_e5$religion <- factor(df_rg_e5$religion, labels = c("Не релігійний", "Православ'я",
                                                          "Католицизм", "Греко-католицизм", "Протестантизм",
                                                          "Іслам", "Іудаїзм"))
aggregate(sd ~ religion ,df_rg_e5, mean)

p_e5

str(iris)
install.packages("devtools")
help(package = devtools)
ses