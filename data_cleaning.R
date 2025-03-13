library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)
library(stringr)
library(caret)


moda_função <- function(x) {
  frequências <- na.omit(unique(x))
  frequências[which.max(tabulate(match(x, frequências)))]
}


dados <- fread('D:\\Ana\\Universidade\\Mestrado\\1º Ano\\1º Semestre\\MTC + MP\\CreditDataset.csv')


dados_clean <- data.frame(dados)

payment_behaviour_index <- grep('Payment_Behaviour', colnames(dados_clean))
dados_clean[,-c(2,payment_behaviour_index)] <- lapply(dados_clean[,-c(2,payment_behaviour_index)], FUN = function(x){
  if(is.character(x)){
    x <- gsub('_','',x)
    x <- gsub('"', '', x)
    return(x)
  }else{
    return(x)
  }
})

dados_clean$Customer_ID <- as.factor(dados_clean$Customer_ID)

dados_clean$Month <- match(dados_clean$Month, month.name)
dados_clean$Month <- as.factor(dados_clean$Month)

dados_clean$Age <- as.numeric(dados_clean$Age)
tabela_análise <- dados_clean %>%
  count(Age)

ggplot(dados_clean, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = 'blue', color = 'black', alpha = 0.7, na.rm = TRUE) +
  scale_x_continuous(limits = c(10, 60)) +
  labs(title = 'Histograma',
       x = 'MIS',
       y = 'Frequencia')

dados_clean$Age <- ifelse(
  dados_clean$Age < 14 | dados_clean$Age > 56, 
  NA, 
  dados_clean$Age)

age_NA_todas <- dados_clean %>%
  dplyr::select(Customer_ID, Age) %>%
  group_by(Customer_ID) %>%
  filter(all(is.na(Age))) %>%
  ungroup()

while (sum(is.na(dados_clean$Age)) > nrow(age_NA_todas)) { 
  dados_clean <- dados_clean %>% 
    arrange(ID) %>%
    group_by(Customer_ID) %>%
    fill(Age, .direction = 'downup') %>%
    ungroup()
}

dados_clean$Occupation <- ifelse(dados_clean$Occupation=='', NA, dados_clean$Occupation)

occ_NA_todas <- dados_clean %>%
  dplyr::select(Customer_ID, Occupation) %>%
  group_by(Customer_ID) %>%
  filter(all(is.na(Occupation))) %>%
  ungroup()

while (sum(is.na(dados_clean$Occupation)) > nrow(occ_NA_todas)) {
  dados_clean <- dados_clean %>% 
    arrange(ID) %>%
    group_by(Customer_ID) %>%
    fill(Occupation, .direction = 'downup') %>%
    ungroup()
}


dados_clean$Annual_Income <- as.numeric(dados_clean$Annual_Income)

plot(dados_clean$Annual_Income, ylim = c(0,1000000))
abline(h = 200000, col = 'red')

annual_customer <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Annual_Income) %>%
  group_by(Customer_ID) %>%
  mutate(distintos = n_distinct(Annual_Income))%>%
  summarise(
    todos_annual_distintos = list(unique(Annual_Income)),
    n_distintos = first(distintos))%>%
  ungroup()

annual_dist_sem_outlier <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Annual_Income) %>%
  group_by(Customer_ID) %>%
  filter(Annual_Income<=200000) %>%
  mutate(distintos = n_distinct(Annual_Income)) %>%
  summarise(distintos = first(distintos))

dados_clean <- dados_clean %>%
  group_by(Customer_ID) %>%
  mutate(Annual_Income = ifelse(Annual_Income > 200000,
                                NA,
                                Annual_Income)
         )%>%
  ungroup()

Annual_Income_NA_todas <- dados_clean %>%
  dplyr::select(Customer_ID, Annual_Income) %>%
  group_by(Customer_ID) %>%
  filter(all(is.na(Annual_Income))) %>%
  ungroup()

dados_clean <- dados_clean %>%
  group_by(Customer_ID) %>%
  mutate(Annual_Income = moda_função(Annual_Income)) %>%
  ungroup()

annual_monthly <- dados_clean %>%
  dplyr::select(Customer_ID, Annual_Income, Monthly_Inhand_Salary) %>%
  filter(!is.na(Annual_Income)) %>%
  mutate(
    Relação_monthly_anual = Annual_Income / Monthly_Inhand_Salary)

summary(annual_monthly$Relação_monthly_anual)
hist(annual_monthly$Relação_monthly_anual)

dados_clean <- dados_clean %>%
  mutate(Annual_Income = ifelse(is.na(Annual_Income),
                                Monthly_Inhand_Salary*12,
                                Annual_Income))

monthly_customer <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Monthly_Inhand_Salary) %>%
  group_by(Customer_ID) %>%
  arrange(ID) %>%
  filter(!is.na(Monthly_Inhand_Salary)) %>%
  mutate(distintos = n_distinct(Monthly_Inhand_Salary))%>%
  summarise(
    monthly_array = list(Monthly_Inhand_Salary),
    monthly_array_dist = list(unique(Monthly_Inhand_Salary)),
    n_distintos = first(distintos),
    min_max = max(Monthly_Inhand_Salary) - min(Monthly_Inhand_Salary)
    )

analise_annual_n <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Annual_Income) %>%
  group_by(Customer_ID) %>%
  summarise(n_distintos = n_distinct(Annual_Income)) %>%
  filter(n_distintos>1)

monthly_annual_2 <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Monthly_Inhand_Salary,Annual_Income) %>%
  group_by(Customer_ID) %>%
  arrange(ID) %>%
  filter(!is.na(Monthly_Inhand_Salary)) %>%
  filter(n_distinct(Monthly_Inhand_Salary)==2) %>%
  mutate(relação = Annual_Income/Monthly_Inhand_Salary,
    relação_list = list(unique(relação)),
    dif = abs(unique(relação)[1]-unique(relação)[2])
    )

monthly_outliers_dist_2 <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Monthly_Inhand_Salary) %>%
  filter(!is.na(Monthly_Inhand_Salary)) %>%  
  group_by(Customer_ID) %>% 
  arrange(ID) %>% 
  filter(n_distinct(Monthly_Inhand_Salary) == 2) %>% 
  mutate(
    monthly_list = list(Monthly_Inhand_Salary),
    dif_monthly = ifelse(is.na(lag(Monthly_Inhand_Salary)), 
                         0, 
                         abs(Monthly_Inhand_Salary - lag(Monthly_Inhand_Salary)))
  ) %>%
  filter(dif_monthly > 0) %>%
  group_by(Customer_ID, dif_monthly) %>%
  filter(n() > 1) %>% 
  ungroup()


monthly_NA_todas <- dados_clean %>%
  dplyr::select(Customer_ID, Monthly_Inhand_Salary) %>%
  group_by(Customer_ID) %>%
  filter(all(is.na(Monthly_Inhand_Salary))) %>%
  ungroup() 
monthly_customer_1 <- monthly_customer %>% filter(n_distintos==1)
monthly_customer_2 <- monthly_customer %>% filter(n_distintos==2)


dados_clean <- dados_clean %>%
  group_by(Customer_ID) %>%
  mutate(
    Monthly_Inhand_Salary = ifelse(Customer_ID %in% monthly_customer_1$Customer_ID & is.na(Monthly_Inhand_Salary),
                                   moda_função(Monthly_Inhand_Salary),
                                   Monthly_Inhand_Salary)
    )%>%
  ungroup()


while(sum(is.na(dados_clean$Monthly_Inhand_Salary)) > nrow(monthly_NA_todas)){
dados_clean <- dados_clean %>% 
  group_by(Customer_ID)%>%
  arrange(ID) %>%
  mutate(
    Monthly_Inhand_Salary = ifelse(
      is.na(Monthly_Inhand_Salary) & !is.na(lag(Monthly_Inhand_Salary)),
      lag(Monthly_Inhand_Salary),
      ifelse(
        is.na(Monthly_Inhand_Salary) & !is.na(lead(Monthly_Inhand_Salary)),
        lead(Monthly_Inhand_Salary),
        Monthly_Inhand_Salary
      ))) %>%
  ungroup()
}


dados_clean$Num_Bank_Accounts <- ifelse(
  dados_clean$Num_Bank_Accounts < 0 | dados_clean$Num_Bank_Accounts > 11, 
  NA, 
  dados_clean$Num_Bank_Accounts)

n_accounts_customerID <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Num_Bank_Accounts) %>%
  filter(!is.na(Num_Bank_Accounts)) %>%
  group_by(Customer_ID) %>%
  arrange(ID) %>%
  mutate(distintos = n_distinct(Num_Bank_Accounts))%>%
  filter(distintos > 1) %>%
  summarise(
    Num_Bank_Accounts = list(Num_Bank_Accounts),
    distintos = first(distintos)
  )%>%
  ungroup()

n_account_outliers_dist_2 <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Num_Bank_Accounts) %>%
  filter(!is.na(Num_Bank_Accounts)) %>%  
  group_by(Customer_ID) %>% 
  arrange(ID) %>% 
  filter(n_distinct(Num_Bank_Accounts) == 2) %>% 
  mutate(
    lista = list(Num_Bank_Accounts),
    dif = ifelse(is.na(lag(Num_Bank_Accounts)), 
                         0, 
                         abs(Num_Bank_Accounts - lag(Num_Bank_Accounts)))
  ) %>%
  filter(dif > 0) %>%
  group_by(Customer_ID, dif) %>%
  filter(n() > 1) %>% 
  ungroup()

num_accounts_NA_todas <- dados_clean %>%
  dplyr::select(Customer_ID, Num_Bank_Accounts) %>%
  group_by(Customer_ID) %>%
  filter(all(is.na(Num_Bank_Accounts))) %>%
  ungroup() 

while (sum(is.na(dados_clean$Num_Bank_Accounts)) > nrow(num_accounts_NA_todas)) {
  dados_clean <- dados_clean %>% 
    arrange(ID) %>%
    group_by(Customer_ID) %>%
    fill(Num_Bank_Accounts, .direction = 'downup') %>%
    ungroup()
}


tabela_análise <- dados_clean %>%
  count(Num_Credit_Card)

dados_clean$Num_Credit_Card <- ifelse(
  dados_clean$Num_Credit_Card > 11, 
  NA, 
  dados_clean$Num_Credit_Card)

num_credit_card_customerID <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Num_Credit_Card) %>%
  filter(!is.na(Num_Credit_Card)) %>%
  group_by(Customer_ID) %>%
  arrange(ID) %>%
  mutate(distintos = n_distinct(Num_Credit_Card))%>%
  filter(distintos > 1) %>%
  summarise(
    Num_Credit_Card = list(Num_Credit_Card),
    distintos = first(distintos)
  )%>%
  ungroup()

num_credit_outliers_dist_2 <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Num_Credit_Card) %>%
  filter(!is.na(Num_Credit_Card)) %>%  
  group_by(Customer_ID) %>% 
  arrange(ID) %>% 
  filter(n_distinct(Num_Credit_Card) > 1) %>% 
  mutate(
    lista = list(Num_Credit_Card),
    dif = ifelse(is.na(lag(Num_Credit_Card)), 
                 0, 
                 abs(Num_Credit_Card - lag(Num_Credit_Card)))
  ) %>%
  filter(dif > 0) %>%
  group_by(Customer_ID, dif) %>%
  mutate(
    contar = n()
  ) %>%
  filter(contar > 1) %>% 
  ungroup()

num_credit_card_NA_todas <- dados_clean %>%
  dplyr::select(Customer_ID, Num_Credit_Card) %>%
  group_by(Customer_ID) %>%
  filter(all(is.na(Num_Credit_Card))) %>%
  ungroup() 


while (sum(is.na(dados_clean$Num_Credit_Card)) > nrow(num_credit_card_NA_todas)) {
  dados_clean <- dados_clean %>% 
    arrange(ID) %>%
    group_by(Customer_ID) %>%
    fill(Num_Credit_Card, .direction = 'downup') %>%
    ungroup()
}


tabela_análise <- dados_clean %>%
  count(Interest_Rate)

dados_clean$Interest_Rate <- ifelse(
  dados_clean$Interest_Rate > 34, 
  NA, 
  dados_clean$Interest_Rate)

interest_customerID <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Interest_Rate) %>%
  filter(!is.na(Interest_Rate)) %>%
  group_by(Customer_ID) %>%
  arrange(ID) %>%
  mutate(distintos = n_distinct(Interest_Rate))%>%
  filter(distintos > 1) %>%
  summarise(
    Interest_Rate = list(Interest_Rate),
    distintos = first(distintos)
  )%>%
  ungroup()

interest_NA_todas <- dados_clean %>%
  dplyr::select(Customer_ID, Interest_Rate) %>%
  group_by(Customer_ID) %>%
  filter(all(is.na(Interest_Rate))) %>%
  ungroup()

while (sum(is.na(dados_clean$Interest_Rate)) > nrow(interest_NA_todas)) {
  dados_clean <- dados_clean %>% 
    arrange(ID) %>%
    group_by(Customer_ID) %>%
    fill(Interest_Rate, .direction = 'downup') %>%
    ungroup()
}


dados_clean$Num_of_Loan <- as.numeric(dados_clean$Num_of_Loan)

dados_clean$Type_of_Loan <- gsub('and ', '', dados_clean$Type_of_Loan)
dados_clean$Type_of_Loan <- ifelse(dados_clean$Type_of_Loan=='', 'No Loan', dados_clean$Type_of_Loan)

loans_lista <- dados_clean$Type_of_Loan %>%
  str_split(',') %>%
  unlist() %>%
  trimws() %>% 
  unique()
loans_lista <- loans_lista[loans_lista != 'No Loan']

colunas_vetor  <- c()
for (i in loans_lista) {
  coluna <- paste0('Type_Loan_', gsub(' ', '_', i))
  coluna <- gsub('-', '_', coluna)
  colunas_vetor  <- c(colunas_vetor, coluna)
  dados_clean <- dados_clean %>%
    mutate(!!coluna := str_count(Type_of_Loan, i))
}

type_of_loan_index <- grep('Type_of_Loan', colnames(dados_clean))
dados_clean <- dados_clean %>%
  dplyr::select(c(1:type_of_loan_index), colunas_vetor, everything())

dados_clean <- dados_clean %>%
  mutate(Num_of_Loan = rowSums(dados_clean[, colunas_vetor]))

tabela_análise <- dados_clean %>%
  count(Delay_from_due_date)

delay_customer <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Delay_from_due_date) %>%
  filter(!is.na(Delay_from_due_date)) %>%
  arrange(ID) %>%
  group_by(Customer_ID) %>%
  mutate(
    diferença = abs(Delay_from_due_date - lag(Delay_from_due_date))
    )

delay_max_mi <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Delay_from_due_date) %>%
  filter(!is.na(Delay_from_due_date)) %>%
  arrange(ID) %>%
  group_by(Customer_ID) %>%
  mutate(
    diferença = max(Delay_from_due_date) - min(Delay_from_due_date)
  )


dados_clean$Num_of_Delayed_Payment <- as.numeric(dados_clean$Num_of_Delayed_Payment)

dados_clean$Num_of_Delayed_Payment <- ifelse(
  dados_clean$Num_of_Delayed_Payment < 0 | dados_clean$Num_of_Delayed_Payment > 28, 
  NA, 
  dados_clean$Num_of_Delayed_Payment)

delay_customerID <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Num_of_Delayed_Payment) %>%
  filter(!is.na(Num_of_Delayed_Payment)) %>%
  group_by(Customer_ID) %>%
  arrange(ID) %>%
  mutate(distintos = n_distinct(Num_of_Delayed_Payment))%>%
  filter(distintos > 1) %>%
  summarise(
    Num_of_Delayed_Payment = list(Num_of_Delayed_Payment),
    distintos = first(distintos)
  )%>%
  ungroup()

num_delay_NA_todas <- dados_clean %>%
  dplyr::select(Customer_ID, Num_of_Delayed_Payment) %>%
  group_by(Customer_ID) %>%
  filter(all(is.na(Num_of_Delayed_Payment))) %>%
  ungroup() 

while (sum(is.na(dados_clean$Num_of_Delayed_Payment)) > nrow(num_delay_NA_todas)) {
  dados_clean <- dados_clean %>% 
    arrange(ID) %>%
    group_by(Customer_ID) %>%
    fill(Num_of_Delayed_Payment, .direction = 'downup') %>%
    ungroup()
}


dados_clean$Changed_Credit_Limit <- as.numeric(dados_clean$Changed_Credit_Limit)

credit_customerID <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Changed_Credit_Limit) %>%
  filter(!is.na(Changed_Credit_Limit)) %>%
  group_by(Customer_ID) %>%
  arrange(ID) %>%
  mutate(distintos = n_distinct(Changed_Credit_Limit))%>%
  filter(distintos > 1) %>%
  summarise(
    Changed_Credit_Limit = list(Changed_Credit_Limit),
    distintos = first(distintos)
  )%>%
  ungroup()

dados_clean <- dados_clean %>%
  group_by(Customer_ID) %>%
  mutate(
    Changed_Credit_Limit = ifelse(
      is.na(Changed_Credit_Limit),
      mean(Changed_Credit_Limit, na.rm = TRUE),
      Changed_Credit_Limit)
  ) %>%
  ungroup()


dados_clean$Num_Credit_Inquiries <- ifelse(
  dados_clean$Num_Credit_Inquiries > 17, 
  NA, 
  dados_clean$Num_Credit_Inquiries)

num_inquiries_customerID <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Num_Credit_Inquiries) %>%
  filter(!is.na(Num_Credit_Inquiries)) %>%
  group_by(Customer_ID) %>%
  arrange(ID) %>%
  mutate(distintos = n_distinct(Num_Credit_Inquiries))%>%
  summarise(
    Num_Credit_Inquiries = list(Num_Credit_Inquiries),
    distintos = first(distintos)
  )%>%
  ungroup()

num_inquiries_outliers_dist_2 <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Num_Credit_Inquiries) %>%
  filter(!is.na(Num_Credit_Inquiries)) %>%  
  group_by(Customer_ID) %>% 
  arrange(ID) %>% 
  filter(n_distinct(Num_Credit_Inquiries) == 2) %>% 
  mutate(
    lista = list(Num_Credit_Inquiries),
    dif = ifelse(is.na(lag(Num_Credit_Inquiries)), 
                 0, 
                 abs(Num_Credit_Inquiries - lag(Num_Credit_Inquiries)))
  ) %>%
  filter(dif > 0) %>%
  group_by(Customer_ID, dif) %>%
  filter(n() > 1) %>% 
  ungroup()

num_inquiries_NA_todas <- dados_clean %>%
  dplyr::select(Customer_ID, Num_Credit_Inquiries) %>%
  group_by(Customer_ID) %>%
  filter(all(is.na(Num_Credit_Inquiries))) %>%
  ungroup() 

while (sum(is.na(dados_clean$Num_Credit_Inquiries)) > nrow(num_inquiries_NA_todas)) {
  dados_clean <- dados_clean %>% 
    arrange(ID) %>%
    group_by(Customer_ID) %>%
    fill(Num_Credit_Inquiries, .direction = 'downup') %>%
    ungroup()
}


dados_clean$Credit_Mix <- as.factor(dados_clean$Credit_Mix)
dados_clean$Credit_Mix <- factor(dados_clean$Credit_Mix, levels = c('Good', 'Standard', 'Bad'))

credit_mix_customerID <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Credit_Mix) %>%
  filter(!is.na(Credit_Mix)) %>%
  group_by(Customer_ID) %>%
  arrange(ID) %>%
  mutate(distintos = n_distinct(Credit_Mix))%>%
  summarise(
    Credit_Mix = list(Credit_Mix),
    distintos = first(distintos)
  )%>%
  ungroup()

credit_mix_NA_todas <- dados_clean %>%
  dplyr::select(Customer_ID, Credit_Mix) %>%
  group_by(Customer_ID) %>%
  filter(all(is.na(Credit_Mix))) %>%
  ungroup()

while (sum(is.na(dados_clean$Credit_Mix)) > nrow(credit_mix_NA_todas)) {
  dados_clean <- dados_clean %>% 
    arrange(ID) %>%
    group_by(Customer_ID) %>%
    fill(Credit_Mix, .direction = 'downup') %>%
    ungroup()
}


dados_clean$Outstanding_Debt <- as.numeric(dados_clean$Outstanding_Debt)

Outstanding_Debt_customerID <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Outstanding_Debt) %>%
  filter(!is.na(Outstanding_Debt)) %>%
  group_by(Customer_ID) %>%
  arrange(ID) %>%
  mutate(distintos = n_distinct(Outstanding_Debt))%>%
  summarise(
    Outstanding_Debt = list(Outstanding_Debt),
    distintos = first(distintos)
  )%>%
  ungroup()

plot(dados_clean$Credit_Utilization_Ratio)
summary(dados_clean$Credit_Utilization_Ratio)
hist(dados_clean$Credit_Utilization_Ratio, breaks = 20)


dados_clean$Credit_History_Age <- sapply(dados_clean$Credit_History_Age, function(x) {
  if (!is.na(x)) {
    separação <- strsplit(x, ' ')[[1]]
    Anos <- as.numeric(separação[1])  
    Meses <- as.numeric(separação[4]) 
    Meses_Total <- Anos * 12 + Meses
    return(Meses_Total)
  } else {
    return(NA)
  }
})

dados_clean$Credit_History_Age <- as.numeric(gsub(" Years.*", "", unname(dados_clean$Credit_History_Age)))

month_history <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Month, Credit_History_Age) %>%
  filter(!is.na(Credit_History_Age)) %>%
  group_by(Customer_ID) %>%
  arrange(ID) %>%
  mutate(
    Month = as.numeric(Month),
    dif_month = ifelse(
      is.na(lag(Month)),
      '1º',
      Month - lag(Month)),
    dif_hist = ifelse(
      is.na(lag(Credit_History_Age)),
      '1º',
      Credit_History_Age - lag(Credit_History_Age)),
    relação = (dif_month == dif_hist)
    ) %>%
  filter(relação = FALSE)

credit_hist_NA_todas <- dados_clean %>%
  dplyr::select(Customer_ID, Credit_History_Age) %>%
  group_by(Customer_ID) %>%
  filter(all(is.na(Credit_History_Age))) %>%
  ungroup() 

while (sum(is.na(dados_clean$Credit_History_Age)) > nrow(credit_hist_NA_todas)) {
  dados_clean <- dados_clean %>%
    arrange(ID) %>%
    group_by(Customer_ID) %>%
    mutate(
      Credit_History_Age = ifelse(
        is.na(Credit_History_Age) & !Customer_ID%in%credit_hist_NA_todas$Customer_ID & !is.na(lag(Credit_History_Age)),
        lag(Credit_History_Age) + (as.numeric(Month) - lag(as.numeric(Month))),
        ifelse(
          is.na(Credit_History_Age) & !Customer_ID%in%credit_hist_NA_todas$Customer_ID & !is.na(lead(Credit_History_Age)),
          lead(Credit_History_Age) + (lead(as.numeric(Month)) - as.numeric(Month)),
          Credit_History_Age
        )
      ))%>%
    ungroup()
}


dados_clean$Payment_of_Min_Amount <- as.factor(dados_clean$Payment_of_Min_Amount)

payment_customerID <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Payment_of_Min_Amount) %>%
  filter(!is.na(Payment_of_Min_Amount)) %>%
  filter(!Payment_of_Min_Amount=='NM') %>%
  group_by(Customer_ID) %>%
  arrange(ID) %>%
  mutate(distintos = n_distinct(Payment_of_Min_Amount))%>%
  summarise(
    Payment_of_Min_Amount = list(Payment_of_Min_Amount),
    distintos = first(distintos)
  )%>%
  ungroup()

num_loan_customerID <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Num_of_Loan) %>%
  filter(!is.na(Num_of_Loan)) %>%
  group_by(Customer_ID) %>%
  arrange(ID) %>%
  mutate(distintos = n_distinct(Num_of_Loan))%>%
  summarise(
    Num_of_Loan = list(Num_of_Loan),
    distintos = first(distintos)
  )%>%
  ungroup()

dados_clean$Payment_of_Min_Amount <- as.character(dados_clean$Payment_of_Min_Amount)
dados_clean$Payment_of_Min_Amount[dados_clean$Payment_of_Min_Amount == 'NM'] <- NA

payment_min_NA_todas <- dados_clean %>%
  dplyr::select(Customer_ID, Payment_of_Min_Amount) %>%
  group_by(Customer_ID) %>%
  filter(all(is.na(Payment_of_Min_Amount))) %>%
  ungroup()

while (sum(is.na(dados_clean$Payment_of_Min_Amount)) > nrow(payment_min_NA_todas)) {
  dados_clean <- dados_clean %>%
    arrange(ID) %>%
    group_by(Customer_ID) %>%
    fill(Payment_of_Min_Amount, .direction = 'downup') %>%
    ungroup()
}
dados_clean$Payment_of_Min_Amount <- factor(dados_clean$Payment_of_Min_Amount, 
                                            levels = c('No', 'Yes'))


dados_clean_relação <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Monthly_Inhand_Salary, Total_EMI_per_month) %>%
  group_by(Customer_ID) %>%
  mutate(relção_correta = Total_EMI_per_month <= Monthly_Inhand_Salary,
         percentagem = Total_EMI_per_month/Monthly_Inhand_Salary*100)

plot(dados_clean_relação$percentagem, ylim=c(0,100))
abline(h = 25, col = 'red')

dados_clean <- dados_clean %>%
  mutate(Total_EMI_per_month = ifelse(Total_EMI_per_month/Monthly_Inhand_Salary*100>25,
                                      NA,
                                      Total_EMI_per_month))
sum(is.na(dados_clean$Total_EMI_per_month))
emi_NA_todas <- dados_clean %>%
  dplyr::select(Customer_ID, Total_EMI_per_month) %>%
  group_by(Customer_ID) %>%
  filter(all(is.na(Total_EMI_per_month))) %>%
  ungroup()

emi_customerID <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Total_EMI_per_month) %>%
  filter(!is.na(Total_EMI_per_month)) %>%
  group_by(Customer_ID) %>%
  arrange(ID) %>%
  mutate(distintos = n_distinct(Total_EMI_per_month))%>%
  summarise(
    Total_EMI_per_month = list(Total_EMI_per_month),
    distintos = first(distintos)
  )%>%
  ungroup()

while (sum(is.na(dados_clean$Total_EMI_per_month)) > nrow(emi_NA_todas)) { 
  dados_clean <- dados_clean %>% 
    arrange(ID) %>%
    group_by(Customer_ID) %>%
    fill(Total_EMI_per_month, .direction = 'downup') %>%
    ungroup()
}


dados_clean$Amount_invested_monthly <- as.numeric(dados_clean$Amount_invested_monthly)

dados_clean$Amount_invested_monthly <- ifelse(
  dados_clean$Amount_invested_monthly == 10000,
  NA,
  dados_clean$Amount_invested_monthly
)

invested_monthly <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Monthly_Inhand_Salary, Amount_invested_monthly) %>%
  mutate(
    relação = (Amount_invested_monthly/Monthly_Inhand_Salary)
  )

invested_customerID <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Amount_invested_monthly) %>%
  filter(!is.na(Amount_invested_monthly)) %>%
  group_by(Customer_ID) %>%
  arrange(ID) %>%
  mutate(distintos = n_distinct(Amount_invested_monthly))%>%
  summarise(
    Amount_invested_monthly = list(Amount_invested_monthly),
    distintos = first(distintos)
  )%>%
  ungroup()

dados_clean <- dados_clean %>%
  group_by(Customer_ID) %>%
  mutate(
    Amount_invested_monthly = ifelse(
      is.na(Amount_invested_monthly),
      mean(Amount_invested_monthly, na.rm = TRUE),
      Amount_invested_monthly
    )
  ) %>%
  ungroup()


dados_clean$Payment_Behaviour <- ifelse(
  dados_clean$Payment_Behaviour == '!@9#%8',
  NA,
  dados_clean$Payment_Behaviour)

payment_behaviour_NA_todas <- dados_clean %>%
  dplyr::select(Customer_ID, Payment_Behaviour) %>%
  group_by(Customer_ID) %>%
  filter(all(is.na(Payment_Behaviour))) %>%
  ungroup()

dados_clean <- dados_clean %>%
  separate(Payment_Behaviour, into = c('Spent_Behaviour', 'Delete1', 'Value_Payments_Behaviour', 'Delete2'), sep = '_', extra = 'merge') 
dados_clean$Delete1 <- NULL
dados_clean$Delete2 <- NULL

dados_clean$Spent_Behaviour <-as.factor(dados_clean$Spent_Behaviour)
dados_clean$Value_Payments_Behaviour <-as.factor(dados_clean$Value_Payments_Behaviour)

payment_behaviour_customerID <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Spent_Behaviour, Value_Payments_Behaviour) %>%
  filter(!is.na(Spent_Behaviour), !is.na(Value_Payments_Behaviour)) %>%
  group_by(Customer_ID) %>%
  arrange(ID) %>%
  mutate(distintos_spent = n_distinct(Spent_Behaviour),
         distintos_value = n_distinct(Value_Payments_Behaviour))%>%
  summarise(
    Spent_Behaviour = list(Spent_Behaviour),
    Value_Payments_Behaviour = list(Value_Payments_Behaviour),
    distintos_spent = first(distintos_spent),
    distintos_value = first(distintos_value)
  )%>%
  ungroup()

while (sum(is.na(dados_clean$Spent_Behaviour)) > nrow(payment_behaviour_NA_todas)) {
  dados_clean <- dados_clean %>%
    arrange(ID) %>%
    group_by(Customer_ID) %>%
    fill(Spent_Behaviour,  .direction = 'downup') %>%
    fill(Value_Payments_Behaviour,  .direction = 'downup') %>%
    ungroup()
}


dados_clean$Monthly_Balance <- as.numeric(dados_clean$Monthly_Balance)

dados_clean$Monthly_Balance <- ifelse(
  dados_clean$Monthly_Balance < 0,
  NA,
  dados_clean$Monthly_Balance
)

balance_monthly <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Monthly_Inhand_Salary, Monthly_Balance) %>%
  mutate(
    relação = (Monthly_Balance<=Monthly_Inhand_Salary)
  )%>%
  filter(relação==FALSE)
balance_monthly_invested <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Monthly_Inhand_Salary, Monthly_Balance, Amount_invested_monthly) %>%
  mutate(
    relação = (Monthly_Balance<=Monthly_Inhand_Salary-Amount_invested_monthly)
  )%>%
  filter(relação==FALSE)

balance_monthly_invested_emi <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Monthly_Inhand_Salary, Amount_invested_monthly, Total_EMI_per_month, Monthly_Balance) %>%
  mutate(
    diferença = Monthly_Inhand_Salary-(Amount_invested_monthly+Total_EMI_per_month),
    relação = (Monthly_Balance<=diferença)
  )%>%
  filter(relação==FALSE)

balance_customer <- dados_clean %>%
  dplyr::select(ID, Customer_ID, Monthly_Balance) %>%
  filter(!is.na(Monthly_Balance)) %>%
  group_by(Customer_ID) %>%
  arrange(ID) %>%
  mutate(distintos = n_distinct(Monthly_Balance))%>%
  summarise(
    Monthly_Balance = list(Monthly_Balance),
    distintos = first(distintos)
  )%>%
  ungroup()

dados_clean <- dados_clean %>%
  group_by(Customer_ID) %>%
  mutate(
    Monthly_Balance = ifelse(
      is.na(Monthly_Balance),
      mean(Monthly_Balance, na.rm = TRUE),
      Monthly_Balance
    )
  ) %>%
  ungroup()

monthly_com_annual <- dados_clean %>%
  dplyr::select(ID,Customer_ID, Annual_Income, Monthly_Inhand_Salary) %>%
  group_by(Customer_ID) %>%
  filter(is.na(Monthly_Inhand_Salary) & any(!is.na(Annual_Income)))

while (sum(is.na(dados_clean$Monthly_Inhand_Salary))>0) {
  dados_clean <- dados_clean %>%
    mutate(,
           Monthly_Inhand_Salary = ifelse(
             is.na(Monthly_Inhand_Salary),
             Annual_Income/12,
             Monthly_Inhand_Salary)
    )
}

rm(list = setdiff(ls(), c('dados_clean')))
