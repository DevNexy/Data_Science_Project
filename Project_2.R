install.packages(c("tidyverse", "data.table", "devtools", "ggpubr", "PerformanceAnalytics", "caret"))
library(tidyverse)
library(data.table)
library(devtools)
library(survival)
library(ggpubr)
data("colon", package = "survival")
colon <- as_tibble(colon)

#데이터 전처리
clean_colon <- na.omit(colon)
clean_colon <- clean_colon %>% filter(etype == 1) %>% select(-rx)

View(clean_colon)

#train / test (비율 9:1)
library(caret)
set.seed(31)
index <- createDataPartition(y = clean_colon$status, p = 0.9, list = FALSE)

train <- clean_colon[index, ]
test <- clean_colon[-index, ]

m <- glm(status ~., data = train, family = "binomial")
summary(m)

m_2 = step(m, direction = "backward")
summary(m_2)

#원본데이터
p1 <- m_2 %>% ggplot(aes(id, status))+geom_jitter(aes(col = factor(status)), height = 0.1, width = 0.2)+
  ggtitle("id-status")+theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))

p2 <- m_2 %>% ggplot(aes(time, status))+geom_jitter(aes(col = factor(status)), height = 0.1, width = 0.2)+
  ggtitle("time-status")+theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))

ggarrange(p1, p2, labels = c("A", "B"), ncol = 2, nrow = 1)

#예측 데이터
m_pre <- predict(m_2, newdata = test %>% select(-status), type = "response") %>%
  tibble(predict_status = .) %>% bind_cols(test, .)
str(m_pre)
View(m_pre)
