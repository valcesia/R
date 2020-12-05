#Atualizando pacotes no R Studio usando o CRAN - Comprehensive R Archive Network
update.packages(ask = FALSE)

#Instalando pacotes necessários para análise dos dados
install.packages("devtools")
install.packages("ggpubr")
install.packages("PerformanceAnalytics")

#Carregando as bibliotecas necessárias para análise dos dados
library("ggpubr")
library("tidyverse")
library("PerformanceAnalytics")

#Definindo o Working Directory
setwd("~/Documents/R")

#data1 - CPU / MEM / DISCO - Provisionamento sem Open vSwitch
data1 <- read.csv("Provision-1.csv")
data1
head(data1, 6)
colnames(data1)

#data2 - CPU / MEM / DISCO - Provisionamento com Open vSwitch
data2 <- read.csv("Provision-2.csv")
data2
head(data2, 6)
colnames(data2)

#data1-net - NET - Provisionamento sem Open vSwitch
data1net <- read.csv("Provision-1-Net.csv")
data1net
head(data1net, 6)
colnames(data1net)

#data2-net - NET - Provisionamento com Open vSwitch
data2net <- read.csv("Provision-2-Net.csv")
data2net
head(data2net, 6)
colnames(data2net)

#Teste de correlação data1 - CPU / MEM / DISCO - Provisionamento sem Open vSwitch

cor.test(data1$kvm_cpu_utilization...., data1$virtual_cpu_utilization....)
cor.test(data1$kvm_ram_available.GB., data1$virtual_ram_total..GB.)
cor.test(data1$kvm_disk_reads,data1$virtual_io_read)
cor.test(data1$kvm_device_writes,data1$virtual_io_write)

#Teste de correlação data2 - CPU / MEM / DISCO - Provisionamento com Open vSwitch

cor.test(data2$kvm_cpu_utilization...., data2$virtual_cpu_utilization....)
cor.test(data2$kvm_ram_available.GB., data2$virtual_ram_total..GB.)
cor.test(data2$kvm_disk_reads,data2$virtual_io_read)
cor.test(data2$kvm_device_writes,data2$virtual_io_write)

#Teste de correlação data1-net - Provisionamento sem Open vSwitch

cor.test(data1net$kvm_net_rx..kB.s.,data1net$virtual_net_rx..kB.s.)
cor.test(data1net$kvm_net_tx..kB.s.,data1net$virtual_net_tx..kB.s.)

#Teste de correlação data2-net - Provisionamento com Open vSwitch

cor.test(data2net$kvm_net_rx..kB.s.,data2net$virtual_net_rx..kB.s.)
cor.test(data2net$kvm_net_tx..kB.s.,data2net$virtual_net_tx..kB.s.)

#Correlação de Pearson CPU - data1
library("ggpubr")
ggscatter(data1, x = "kvm_cpu_utilization....", y = "virtual_cpu_utilization....",
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "red", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "KVM - CPU Utilization (%)", ylab = "Virtual - CPU Utilization (%)")

#Correlação de Pearson CPU - data2
library("ggpubr")
ggscatter(data2, x = "kvm_cpu_utilization....", y = "virtual_cpu_utilization....",
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "KVM - CPU Utilization (%)", ylab = "Virtual - CPU Utilization (%)")

#Correlação de Pearson RAM - data1
library("ggpubr")
ggscatter(data1, x = "kvm_ram_available.GB.", y = "virtual_ram_total..GB.",
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "red", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "KVM - Available Memory (GB)", ylab = "Virtual - Total Memory (GB)") 

#Correlação de Pearson RAM - data2
library("ggpubr")
ggscatter(data2, x = "kvm_ram_available.GB.", y = "virtual_ram_total..GB.",
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "KVM - Available Memory (GB)", ylab = "Virtual - Total Memory (GB)") 

#Correlação de Pearson DISK-READ - data1
library("ggpubr")
ggscatter(data1, x = "kvm_disk_reads", y = "virtual_io_read",
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "red", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "KVM Disk Read (pps)", ylab = "Virtual I/O Read (pps)") 

#Correlação de Pearson DISK-READ - data2
library("ggpubr")
ggscatter(data2, x = "kvm_disk_reads", y = "virtual_io_read",
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "KVM Disk Read (pps)", ylab = "Virtual I/O Read (pps)") 

#Correlação de Pearson DISK-WRITE - data1
library("ggpubr")
ggscatter(data1, x = "kvm_device_writes", y = "virtual_io_write",
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "red", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "KVM Disk Write (pps)", ylab = "Virtual I/O Write (pps)") 

#Correlação de Pearson DISK-WRITE - data2
library("ggpubr")
ggscatter(data2, x = "kvm_device_writes", y = "virtual_io_write",
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "KVM Disk Write (pps)", ylab = "Virtual I/O Write (pps)") 

#Correlação de Pearson NET-RX - data1net
library("ggpubr")
ggscatter(data1net, x = "kvm_net_rx..kB.s.", y = "virtual_net_rx..kB.s.",
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "red", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "KVM RX (kBs)", ylab = "Virtual RX (kBs)") 

#Correlação de Pearson NET-RX - data2net
library("ggpubr")
ggscatter(data2net, x = "kvm_net_rx..kB.s.", y = "virtual_net_rx..kB.s.",
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "KVM RX (kBs)", ylab = "Virtual RX (kBs)") 

#Correlação de Pearson NET-TX -data1net
library("ggpubr")
ggscatter(data1net, x = "kvm_net_tx..kB.s.", y = "virtual_net_tx..kB.s.",
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "red", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "KVM TX (kBs)", ylab = "Virtual TX (kBs)") 

#Correlação de Pearson NET-TX -data2net
library("ggpubr")
ggscatter(data2net, x = "kvm_net_tx..kB.s.", y = "virtual_net_tx..kB.s.",
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "KVM TX (kBs)", ylab = "Virtual TX (kBs)") 

#Use chart.Correlation(): Draw scatter plots - usando PerformanceAnalytics

#Provisionamento sem Open vSwitch
my_data1 <- data1[, c(2,3,4,5,6,7,8,9)]
chart.Correlation(my_data1, histogram=TRUE, pch=19)

#Provisionamento com Open vSwitch
my_data2 <- data2[, c(2,3,4,5,6,7,8,9)]
chart.Correlation(my_data2, histogram=TRUE, pch=19)

#Provisionamento sem Open vSwitch
my_data1net <- data1net[, c(2,3,4,5)]  
chart.Correlation(my_data1net, histogram=TRUE, pch=19)

#Provisionamento com Open vSwitch
my_data2net <- data2net[, c(2,3,4,5)]  
chart.Correlation(my_data2net, histogram=TRUE, pch=19)

#Conclusão sobre CPU
cpu_kvm_1 <- data1[, c(2)]
cpu_virtual_1 <- data1[, c(3)]

cpu_kvm_2 <- data2[, c(2)]
cpu_virtual_2 <- data2[, c(3)]

mean(cpu_kvm_1)
sd(cpu_kvm_1)

mean(cpu_virtual_1)
sd(cpu_virtual_1)

mean(cpu_kvm_2)
sd(cpu_kvm_2)

mean(cpu_virtual_2)
sd(cpu_virtual_2)

#Conclusão sobre Rede - RX
rx_kvm_1 <- data1net[, c(2)]
rx_virtual_1 <- data1net[, c(3)]

rx_kvm_2 <- data2net[, c(2)]
rx_virtual_2 <- data2net[, c(3)]

mean(rx_kvm_1)
sd(rx_kvm_1)

mean(rx_kvm_2)
sd(rx_kvm_2)

mean(rx_virtual_1)
sd(rx_virtual_1)

mean(rx_virtual_2)
sd(rx_virtual_2)
