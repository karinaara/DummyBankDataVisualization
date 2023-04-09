# View a list of column namesnames(loan)
table(loan$loan_status)

# Selecting so that there are only a few important columns
loan_data <- loan[, c("member_id",  "home_ownership",  "loan_amnt", "total_acc",
                      "term" , "int_rate" , "addr_state", "emp_length", "annual_inc"
                      ,"loan_status")]

# Main reference column
loan_data$loan_status[loan_data$loan_status == "Fully Paid"] <- "Good Credit"
loan_data$loan_status[loan_data$loan_status == "Charged Off"] <- "Bad Credit"
loan_data <- loan_data[loan_data$loan_status %in% c("Bad Credit", "Good Credit"),]

# Membuat batasan berdasarkan kepemilikan rumah
loan_data <- loan_data[loan_data$home_ownership %in% c("OWN", "RENT","MORTGAGE"),]

# Delete data in an empty row
loan_data <- na.omit(loan_data)


# CREDIT LOAN VISUALIZATION
options(scipen = 999)
status <- table(loan_data$loan_status)
sorted_s <- rev(status[order(status)])
barplot(sorted_s, 
        horiz = TRUE, 
        col = c("blue", "lightblue"),
        main = "CREDIT LOAN STATUS",
        border = "black", # warna tepi batang
        axes = TRUE, # menampilkan sumbu x dan y
        cex.axis = 0.8, # ukuran teks sumbu x dan y
        font.axis = 2, # jenis huruf teks sumbu x dan y
        cex.lab = 1.2, # ukuran teks nama sumbu x dan y
        font.lab = 2, # jenis huruf teks nama sumbu x dan y
        mgp = c(2, 0.7, 0), # jarak antara sumbu x, label, dan judul
        col.axis = "black", # warna teks sumbu x dan y
        tck = 0.01 # panjang grid)
        )


# HOME OWNERSHIP STATUS VISUALIZATION
status_h <- table(loan_data$home_ownership) 
sorted_h <- rev(status_h[order(status_h)])

barplot(sorted_h, 
        horiz = FALSE, 
        col = c("cornsilk", "burlywood", "chocolate"),
        main = "HOME OWNERSHIP STATUS",
        border = "black", # warna tepi batang
        axes = TRUE, # menampilkan sumbu x dan y
        cex.axis = 0.8, # ukuran teks sumbu x dan y
        font.axis = 2, # jenis huruf teks sumbu x dan y
        cex.lab = 1.2, # ukuran teks nama sumbu x dan y
        font.lab = 2, # jenis huruf teks nama sumbu x dan y
        mgp = c(2, 0.7, 0), # jarak antara sumbu x, label, dan judul
        col.axis = "black", # warna teks sumbu x dan y
        tck = 0.01 # panjang grid)
)

status <- loan_data$loan_status
home <- loan_data$home_ownership
new_table <- table(status, home)
new_table <- as.data.frame(new_table)

ggplot(new_table, aes(x = home, y = Freq, fill = status)) + 
  geom_bar(position = position_dodge(), stat="identity") +
  labs(title = "HOME OWNERSHIP", x = "Home Ownership Category", y = "Frequency") +
  theme_light()


# LOAN AMMOUNT VISUALIZATION
status <- loan_data$loan_status
ammount <- loan_data$loan_amnt
amm <- data.frame(status, ammount)
amm1 <- amm[amm$status %in% c("Bad Credit"),]
amm2 <- amm[amm$status %in% c("Good Credit"),]

tot_amm <- list(amm1$ammount, amm2$ammount)

boxplot(tot_amm, main = "Loan Ammount", ylab="Frequency")


# ACCOUNT TOTAL VISUALIZATION
status <- loan_data$loan_status
acc <- loan_data$total_acc
new_table2 <- table(status, acc)
new_table2 <- as.data.frame(new_table2)
new_table2

new1 <- new_table2[new_table2$status %in% c("Bad Credit"),]
new2 <- new_table2[new_table2$status %in% c("Good Credit"),]
new1 <- subset(new1, select = c("acc","Freq"))
new2 <- subset(new2, select = c("acc","Freq"))

vec <- seq(from = 0, to = 120, length.out = 5)

barplot(new2$Freq, col="darkorange", 
        names.arg = levels(new2$acc), 
        xlab = "Total Credit Lines", 
        ylab = "Frequency",
        main = "CREDIT LINES")
barplot(new1$Freq, col="yellow", 
        names.arg = levels(new1$acc), 
        xlab = "Total Credit Lines", 
        ylab = "Frequency",
        main = "CREDIT LINES",
        add=TRUE)
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("yellow", "darkorange"))

summary(new1)


# LOAN TERM VISUALIZATION
status <- loan_data$loan_status
term <- loan_data$term
new_table3 <- table(status, term)
new_table3 <- as.data.frame(new_table3)
new_table3

ggplot(new_table3, aes(x = term, y = Freq, fill = status)) + 
  geom_bar(position = position_dodge(), stat="identity") +
  labs(title = "CREDIT TERM", x = "Credit Term", y = "Frequency") +
  theme_light()

 
# INTEREST RATE OF LOAN VISUALIZATION
status <- loan_data$loan_status
rate <- loan_data$int_rate
new_table4 <- data.frame(status, rate)
head(new_table4)
df1 <- new_table4
df1_1 <- df1[df1$status %in% c("Bad Credit"),]
df1_2 <- df1[df1$status %in% c("Good Credit"),]
interval <- seq(from = 0, to = 30, length.out = 10)
rate1 <- data.frame(table(cut(df1_1$rate, breaks = interval, include.lowest = TRUE)))
rate2 <- data.frame(table(cut(df1_2$rate, breaks = interval, include.lowest = TRUE)))
barplot(rate2$Freq, col="lightgreen", 
        names.arg = levels(rate2$Var1), 
        xlab = "Interest Rate", 
        ylab = "Frequency",
        main = "INTEREST RATE OF LOAN")
barplot(rate1$Freq, 
        col="yellowgreen", 
        names.arg = levels(rate1$Var1), 
        xlab = "Interest Rate", 
        ylab = "Frequency",
        add=TRUE,
        main = "INTEREST RATE OF LOAN")
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("yellowgreen", "lightgreen"))


# REGION LOAN MAP VISUALIZATION 
library(ggplot2)
library(maps)

maps <- data.frame(state = loan_data$addr_state, loan = loan_data$loan_amnt)
maps$loan <- as.numeric(as.character(maps$loan))
maps <- aggregate(loan ~ state, maps, sum)
map_data <- map_data("state")
state_lookup <- data.frame(state = state.abb, name = state.name, stringsAsFactors = FALSE)
state_lookup$name <- tolower(state_lookup$name)
maps$state <- state_lookup$name[match(maps$state, state_lookup$state)]
merged_data <- merge(map_data, maps, by.x = "region", by.y = "state")
ggplot(merged_data, aes(x = long, y = lat, group = group, fill = loan)) +
  geom_polygon() +
  scale_fill_gradient(low = "white", high = "lightsalmon") +
  theme_light() +
  labs(title = "LOAN MAP BY REGION IN US", x = "longitudinal", y = "latitude")


# EMPLOYMENT LENGTH VISUALIZATION
library(dplyr)
table(loan_data$emp_length)
status <- loan_data$loan_status
emp <- loan_data$emp_length
new_table5 <- data.frame(status, emp)
new_table5$emp[new_table5$emp == "10+ years"] <- "10 years +"
head(new_table5)
df2 <- as.data.frame(table(new_table5))
df2_1 <- df2[df2$status %in% c("Bad Credit"),]
df2_2 <- df2[df2$status %in% c("Good Credit"),]
barplot(df2_2$Freq, col="lightskyblue", 
        names.arg = levels(df2_2$emp), 
        xlab = "Employment Length", 
        ylab = "Frequency",
        main = "Employment Length")
barplot(df2_1$Freq, 
        col="lightseagreen", 
        names.arg = levels(df2_1$emp), 
        xlab = "Employment Length", 
        ylab = "Frequency",
        add=TRUE,
        main = "EMPLOYMENT LENGTH")
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("lightseagreen", "lightskyblue"))


# ANUAL INCOME VISUALIZATION
status <- loan_data$loan_status
inc <- loan_data$annual_inc
new_table6 <- data.frame(status, inc)
head(new_table6)
df3 <- new_table6
df3_1 <- df3[df3$status %in% c("Bad Credit"),]
df3_2 <- df3[df3$status %in% c("Good Credit"),]
options(scipen = 999)
interval <- seq(from = 0, to = 100000, length.out = 10)
inc1 <- data.frame(table(cut(df3_1$inc, breaks = interval, include.lowest = TRUE)))
inc2 <- data.frame(table(cut(df3_2$inc, breaks = interval, include.lowest = TRUE)))
max(loan_data$annual_inc)
barplot(inc2$Freq, col="plum", 
        names.arg = levels(inc1$Var1), 
        xlab = "Annual Income", 
        ylab = "Frequency",
        main = "ANNUAL INCOME",
        horiz=TRUE)
barplot(inc1$Freq, col="purple", 
        names.arg = levels(inc1$Var1), 
        xlab = "Interval", 
        ylab = "Frequency",
        add=TRUE,
        main = "Annual Income",
        horiz=TRUE)
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("purple", "plum"))

