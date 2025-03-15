

moda_função <- function(x) {
  frequências <- na.omit(unique(x))
  frequências[which.max(tabulate(match(x, frequências)))]
}


set.seed(100)
index_1 <- createDataPartition(dados_clean$Credit_Score, p = 2/3, list = FALSE)
treino_1 <- dados_clean[index_1, ]
teste_1 <- dados_clean[-index_1, ]


colunas_integer <- c('Age', 'Num_Bank_Accounts', 'Num_Credit_Card', 'Num_of_Delayed_Payment', 'Num_Credit_Inquiries', 'Credit_History_Age')


colunas_numeric <- c('Interest_Rate', 'Changed_Credit_Limit')

colunas_char <- c('Occupation', 'Credit_Mix', 'Payment_of_Min_Amount', 'Spent_Behaviour', 'Value_Payments_Behaviour')

treino_1$Payment_of_Min_Amount <- as.character(treino_1$Payment_of_Min_Amount)
teste_1$Payment_of_Min_Amount <- as.character(teste_1$Payment_of_Min_Amount)


for (coluna in names(treino_1)) {
  if (coluna %in% colunas_integer ) {
    treino_1 <- treino_1 %>%
      mutate(
        !!coluna := ifelse(
          is.na(!!sym(coluna)),
          median(!!sym(coluna), na.rm=TRUE), 
          !!sym(coluna)
        )
      )
  } else  if (coluna %in% colunas_char) {
    treino_1 <- treino_1 %>%
      mutate(
        !!coluna := ifelse(
          is.na(!!sym(coluna)),
          moda_função(!!sym(coluna)), 
          !!sym(coluna)
        )
      )
  } else if (coluna %in% colunas_numeric) {
    treino_1 <- treino_1 %>%
      mutate(
        !!coluna := ifelse(
          is.na(!!sym(coluna)), 
          mean(!!sym(coluna), na.rm = TRUE),
          !!sym(coluna)
        )
      )
  }
}

treino_1$Payment_of_Min_Amount <- as.factor(treino_1$Payment_of_Min_Amount)

colunas_vetor_occ  <- c()
for (i in sort(unique(teste_1$Occupation))[-1]) {
  coluna <- paste0('Occupation_', gsub(' ', '_', i))
  colunas_vetor_occ <- c(colunas_vetor_occ, coluna)
  treino_1 <- treino_1 %>%
    mutate(!!coluna := as.numeric(str_detect(Occupation, i)))
}

occupation_index <- grep('Occupation', colnames(treino_1))
treino_1 <- treino_1 %>%
  dplyr::select(c(1:occupation_index), colunas_vetor_occ, everything())


treino_1 <- treino_1 %>% rename(Payment_of_Min_Amount_Yes = Payment_of_Min_Amount)
treino_1$Payment_of_Min_Amount_Yes <- ifelse(treino_1$Payment_of_Min_Amount_Yes == 'Yes', 1, 0)

média_emi <- mean(treino_1$Total_EMI_per_month, na.rm=TRUE)
testar_emi <- treino_1 %>%
  dplyr::select(ID, Customer_ID, Monthly_Inhand_Salary, Total_EMI_per_month) %>%
  filter(is.na(Total_EMI_per_month)) %>%
  group_by(Customer_ID) %>%
  mutate(
    verificar = média_emi/Monthly_Inhand_Salary*100
  )
treino_1$Total_EMI_per_month <- ifelse(is.na(treino_1$Total_EMI_per_month),
                                       média_emi,
                                       treino_1$Total_EMI_per_month)

média_invested <- mean(treino_1$Amount_invested_monthly, na.rm=TRUE)
testar_invested <- treino_1 %>%
  dplyr::select(ID, Customer_ID, Monthly_Inhand_Salary, Amount_invested_monthly) %>%
  filter(is.na(Amount_invested_monthly)) %>%
  group_by(Customer_ID) %>%
  mutate(
    verificar = média_invested/Monthly_Inhand_Salary*100
  )

treino_1$Amount_invested_monthly <- ifelse(is.na(treino_1$Amount_invested_monthly),
                                           média_invested,
                                           treino_1$Amount_invested_monthly)

média_balance <- mean(treino_1$Monthly_Balance, na.rm=TRUE)
testar_balance <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Monthly_Inhand_Salary, Amount_invested_monthly, Total_EMI_per_month, Monthly_Balance) %>%
  filter(is.na(Monthly_Balance)) %>%
  mutate(
    diferença = Monthly_Inhand_Salary-(Amount_invested_monthly+Total_EMI_per_month),
    relação = (média_balance<=diferença)
  )%>%
  filter(relação==FALSE)

treino_1$Monthly_Balance <- ifelse(is.na(treino_1$Monthly_Balance),
                                   média_balance,
                                   treino_1$Monthly_Balance)


for (coluna in names(teste_1)) {
  if (coluna %in% colunas_integer ) {
    teste_1 <- teste_1 %>%
      mutate(
        !!coluna := ifelse(
          is.na(!!sym(coluna)),
          median(!!sym(coluna), na.rm=TRUE), 
          !!sym(coluna)
        )
      )
  } else  if (coluna %in% colunas_char) {
    teste_1 <- teste_1 %>%
      mutate(
        !!coluna := ifelse(
          is.na(!!sym(coluna)),
          moda_função(!!sym(coluna)), 
          !!sym(coluna)
        )
      )
  } else if (coluna %in% colunas_numeric) {
    teste_1 <- teste_1 %>%
      mutate(
        !!coluna := ifelse(
          is.na(!!sym(coluna)), 
          mean(!!sym(coluna), na.rm = TRUE),
          !!sym(coluna)
        )
      )
  }
}


teste_1$Payment_of_Min_Amount <- as.factor(teste_1$Payment_of_Min_Amount)


colunas_vetor_occ  <- c()

for (i in sort(unique(treino_1$Occupation))[-1]) { 
  coluna <- paste0('Occupation_', gsub(' ', '_', i))
  colunas_vetor_occ <- c(colunas_vetor_occ, coluna)
  teste_1 <- teste_1 %>%
    mutate(!!coluna := as.numeric(str_detect(Occupation, i)))
}

occupation_index <- grep('Occupation', colnames(treino_1))
teste_1 <- teste_1 %>%
  dplyr::select(c(1:occupation_index), colunas_vetor_occ, everything())


teste_1 <- teste_1 %>% rename(Payment_of_Min_Amount_Yes = Payment_of_Min_Amount)
teste_1$Payment_of_Min_Amount_Yes <- ifelse(teste_1$Payment_of_Min_Amount_Yes == 'Yes', 1, 0)


média_emi <- mean(teste_1$Total_EMI_per_month, na.rm=TRUE)
testar_emi <- teste_1 %>%
  dplyr::select(ID, Customer_ID, Monthly_Inhand_Salary, Total_EMI_per_month) %>%
  filter(is.na(Total_EMI_per_month)) %>%
  group_by(Customer_ID) %>%
  mutate(
    verificar = média_emi/Monthly_Inhand_Salary*100
  )
teste_1$Total_EMI_per_month <- ifelse(is.na(teste_1$Total_EMI_per_month),
                                      média_emi,
                                      teste_1$Total_EMI_per_month)

média_invested <- mean(teste_1$Amount_invested_monthly, na.rm=TRUE)
testar_invested <- teste_1 %>%
  dplyr::select(ID, Customer_ID, Monthly_Inhand_Salary, Amount_invested_monthly) %>%
  filter(is.na(Amount_invested_monthly)) %>%
  group_by(Customer_ID) %>%
  mutate(
    verificar = média_invested/Monthly_Inhand_Salary*100
  )
teste_1$Amount_invested_monthly <- ifelse(is.na(teste_1$Amount_invested_monthly),
                                          média_invested,
                                          teste_1$Amount_invested_monthly)

média_balance <- mean(teste_1$Monthly_Balance, na.rm=TRUE)
testar_balance <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Monthly_Inhand_Salary, Amount_invested_monthly, Total_EMI_per_month, Monthly_Balance) %>%
  filter(is.na(Monthly_Balance)) %>%
  mutate(
    diferença = Monthly_Inhand_Salary-(Amount_invested_monthly+Total_EMI_per_month),
    relação = (média_balance<=diferença)
  )%>%
  filter(relação==FALSE)

teste_1$Monthly_Balance <- ifelse(is.na(teste_1$Monthly_Balance),
                                  média_balance,
                                  teste_1$Monthly_Balance)

sum(is.na(treino_1)) == 0
sum(is.na(teste_1)) == 0


rm(list = setdiff(ls(), c('treino_1', 'teste_1', 'dados_clean')))



treino_1$Credit_Score <- factor(treino_1$Credit_Score, levels = c('Poor','Standard', 'Good'), ordered = TRUE)
teste_1$Credit_Score <- factor(teste_1$Credit_Score, levels = c('Poor','Standard', 'Good'), ordered = TRUE)

treino_1 <- treino_1[,-c(1,2,3,4,6,7)]
treino_1 <- treino_1[, -which(colnames(treino_1) == 'Type_of_Loan')]
teste_1 <- teste_1[,-c(1,2,3,4,6,7)]
teste_1 <- teste_1[, -which(colnames(teste_1) == 'Type_of_Loan')]

treino_1 <- treino_1[, -c(2:15, 37)]
teste_1 <- teste_1[, -c(2:15, 37)]

treino_1 <- treino_1[, -which(colnames(treino_1) == 'Annual_Income')] 
treino_1 <- treino_1[, -which(colnames(treino_1) == 'Credit_Mix')]
teste_1 <- teste_1[, -which(colnames(teste_1) == 'Annual_Income')] 
teste_1 <- teste_1[, -which(colnames(teste_1) == 'Credit_Mix')]

treino_1 <- treino_1[, -c(7:15)]
treino_1 <- treino_1[, -which(colnames(treino_1) == 'Total_EMI_per_month')]
treino_1 <- treino_1[, -which(colnames(treino_1) == 'Spent_Behaviour')]
treino_1 <- treino_1[, -which(colnames(treino_1) == 'Value_Payments_Behaviour')]

teste_1 <- teste_1[, -c(7:15)]
teste_1 <- teste_1[, -which(colnames(teste_1) == 'Total_EMI_per_month')]
teste_1 <- teste_1[, -which(colnames(teste_1) == 'Spent_Behaviour')]
teste_1 <- teste_1[, -which(colnames(teste_1) == 'Value_Payments_Behaviour')]

rm(list = setdiff(ls(), c('treino_1', 'teste_1', 'dados_clean')))


 mean(treino_1$Credit_Score == 'Poor')*100
 mean(teste_1$Credit_Score == 'Poor')*100

 mean(treino_1$Credit_Score == 'Standard')*100
 mean(teste_1$Credit_Score == 'Standard')*100 

mean(treino_1$Credit_Score == 'Good')*100
mean(teste_1$Credit_Score == 'Good')*100

barplot(table(treino_1$Credit_Score), col="blue")
barplot(table(teste_1$Credit_Score), col="red")
