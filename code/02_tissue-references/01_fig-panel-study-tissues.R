library(ggplot2)
library(dplyr)
library(scales)

csv.name <- "decon-studies-tissues.csv"
csv <- read.csv(csv.name)

#------------
# format data
#------------
# year
csv$year <- gsub(".* ", "", csv$citation)

# set all tissues
csv$tissues <- paste0(csv$tissues, ";all_tissues")

# format tissues
unique.tissues <- strsplit(csv$tissues, ";") %>% unlist() %>% unique()
tx.filter <- c("")
unique.tissues <- unique.tissues[!unique.tissues %in% tx.filter]

#---------------------
# ref counts by tissue
#---------------------
tx.counts.list <- lapply(unique.tissues, function(tx){
  nrow(csv[grepl(paste0("(^|;)", tx, "(;|$)"), csv[,3]),])
})
tx.counts.table <- do.call(rbind, tx.counts.list) %>% as.data.frame()
colnames(tx.counts.table) <- "references"
tx.counts.table$tx <- unique.tissues

# plot
plot.data <- tx.counts.table
plot.data$references <- as.numeric(plot.data$references)
ref.order <- order(plot.data$references) %>% rev()
ref.levels <- plot.data$tx[ref.order]
plot.data$tx <- factor(plot.data$tx, levels = ref.levels)
plot.data <- plot.data[seq(15),]

ggplot(plot.data, aes(x = tx, y = references)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#------------------------
# cumulative refs by year
#------------------------
seq.year <- seq(min(csv$year), max(csv$year), 1)
ref.all.vector <- c()

df.cumul <- lapply(seq.year, function(year.iter){
  year.filter <- csv$year <= year.iter
  csvf <- csv[year.filter,]
  tx.counts.list <- lapply(unique.tissues, function(tx){
    tx.filter <- grepl(paste0("(^|;)", tx, "(;|$)"), csvf[,3])
    csvf[tx.filter,] %>% nrow()
  })
  tx.counts.table <- do.call(rbind, tx.counts.list) %>% as.data.frame()
  colnames(tx.counts.table) <- "references"
  tx.counts.table$tx <- unique.tissues
  tx.counts.table$year <- year.iter
  tx.counts.table
})
df.cumul <- do.call(rbind, df.cumul) %>% as.data.frame()

tx.filter <- c("all_tissues", "tumor", "brain", "immune_cell", "blood", "pancreas")
df.cumul <- df.cumul[df.cumul$tx %in% tx.filter,]

# convert all

df.cumul$year <- as.integer(df.cumul$year)
df.cumul$Tissue <- df.cumul$tx

ggplot(df.cumul, aes(x = year, y = references, 
                     color = Tissue, shape = Tissue, lty = Tissue)) + theme_bw() + 
  geom_point(size = 2) + geom_line(size = 1) + 
  scale_x_continuous(breaks=pretty_breaks()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Year") + ylab("References")


