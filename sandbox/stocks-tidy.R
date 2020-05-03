# 0. Introduction 
#### Libraries #################################################################
library(tidyverse)
library(janitor)
library(magrittr)

#### Figure 1.1: Monthly stock returns #########################################
# Daily stock prices
stocks_tidy <- read_csv("stocks.csv") %>% 
  # Turn RET from factor to numeric
  mutate(RET = as.numeric(as.character(RET))) %>%
  # Turn date from int to Date
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
  # Remove rows with empty values
  filter(TICKER!="" & !is.na(RET)) %>%
  # Remove duplicates
  group_by(date, TICKER) %>%
  summarise_all(first) %>%
  ungroup() %>%
  # Create month
  mutate(month = paste0(format(date, "%Y-%m"), "-01")) %>%
  mutate(month = as.Date(month))

# Aggregate daily returns to monthly return
agg <- function(r) prod(1+r, na.rm=TRUE) - 1
mnthly_tidy <- stocks_tidy %>%
  group_by(TICKER, month) %>%
  summarize(RET = agg(RET), 
            SNP = agg(sprtrn)) %>%
  ungroup()

# Monthly returns of S&P500
# Since sprtrn is a column in stocks with duplicate entries for each stock,
# take only the first one for each month.
SNP_tidy <- mnthly_tidy %>%
  select(month, RET = SNP) %>%
  group_by(month) %>%
  summarise(RET = first(RET)) %>%
  ungroup() %>%
  mutate(TICKER = "SNP")

# Monthly return of US Treasury bills
tbills_tidy <- read_csv("tbills.csv") %>%
  mutate(date = as.Date(date)) %>%
  rename(month = date) %>%
  rename(RET = return) %>%
  mutate(TICKER = "Tbills")

# Combine returns
mnthly_tidy <- mnthly_tidy %>%
  select(-SNP) %>%
  rbind(SNP_tidy) %>%
  rbind(tbills_tidy) 

# Create dataframe for plot
mnthly_4plot <- mnthly_tidy %>%
  # Remove outlier
  filter(TICKER != "MPET") %>%
  # Add aesthetics
  mutate(alpha = ifelse(TICKER %in% c("SNP", "Tbills"), 1, 0.3)) %>%
  mutate(color = ifelse(TICKER %in% c("SNP", "Tbills"), TICKER, "Stocks")) %>%
  mutate(linetype = ifelse(TICKER %in% c("SNP", "Tbills"), TICKER, "Stocks"))

# Plot
ggplot(mnthly_4plot) +
  geom_line(aes(x = month, 
                y = RET, 
                group = TICKER, 
                alpha = alpha,
                color = color,
                linetype = linetype)) +
  scale_color_manual(values = c("red", "black", "gray")) +
  scale_linetype_manual(values = c("solid", "dotted", "solid")) +
  guides(alpha = FALSE, linetype = FALSE) +
  labs(x = "month", y = "return",
       title = "Monthly stock returns\nfor members of S&P 500 and their average")

#### Figure 1.3: Big stocks ####################################################
# Market capitalizations of big stocks
bigs_tidy <- read_csv("bigstocks.csv", col_names = FALSE)

# Excess returns of big stocks
ret_bigs <- mnthly_tidy %>%
  filter(TICKER %in% bigs_tidy$X1) %>%
  spread(TICKER, RET) %>%
  select(-month) %>%
  as.matrix()

exr <- aaa - tbills_tidy$RET

# Excess returns of the market
mkt <- SNP_tidy$RET - tbills_tidy$RET

# Capital Asset Pricing Model (CAPM)
capm <- lm(exr ~ mkt) %>%
  broom::tidy() %>%
  select(response)
spread(term, estimate)

# column 1: beta
# column 2: alpha
(ab <- t(coef(capm))[,2:1])

# Remove outlier: WMT (Walmart Inc)
ab <- ab[-9,]

# Plot
par(mai=c(.8,.8,0,0), xpd=FALSE)
plot(ab, type="n", bty="n", xlab="beta", ylab="alpha")
abline(v=1, lty=2, col=8)
abline(h=0, lty=2, col=8)
text(ab, labels=rownames(ab), cex=bigs[,2]/350, col="navy") 
