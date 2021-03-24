library(data.table)
library(dbplyr)

n = 10000000

# let's create a large data.table

DT = data.table(ID = 1:n,
								Capacity = sample(100:(100*n), n, replace = T),
								Code = sample(LETTERS[1:4], n, replace = T),
								State = rep(c("PA", "NY", "CT", "OH", "DE"), n / 5))


print(object.size(DT), units = "auto")

# There are four ways we can save the data.table to disk:
# 	As a csv file:
# 		write.csv()
# 		fwrite()
# 	As a binary file:
# 		save()
# 		saveRDS()
	

{
	elapsed1 = proc.time()
		write.csv(DT, file = "dt.write.csv", row.names = FALSE)
	elapsed1 = proc.time() - elapsed1
	
	elapsed2 = proc.time()
		fwrite(DT, file = "dt.fwrite.csv")
	elapsed2 = proc.time() - elapsed2

	elapsed3 = proc.time()
		save(DT, file = "dt.rdata")
	elapsed3 = proc.time() - elapsed3
	
	elapsed4 = proc.time()
		saveRDS(DT, file = "dt.rds")
	elapsed4 = proc.time() - elapsed4
}

fileio = data.table(save.command = c("write.csv", "fwrite", "save", "saveRDS"),
										save.time.sec = c(elapsed1[3], elapsed2[3], elapsed3[3], elapsed4[3]))

fileio[, save.time.ratio := round(save.time.sec / min(save.time.sec), 2)]

fileio[, file.size.MB := c(file.size("dt.write.csv") %/% 1024^2,
                           file.size("dt.fwrite.csv") %/% 1024^2,
                           file.size("dt.rdata") %/% 1024^2,
                           file.size("dt.rds") %/% 1024^2)]



fileio
# Reading the files, we can use
# 	As a csv file:
# 		read.csv()
# 		fread()
# 	As a binary file:
# 		load()
# 		readRDS()

{
	elapsed1 = proc.time()
		DT1 = read.csv("dt.write.csv")
	elapsed1 = proc.time() - elapsed1
	
	elapsed2 = proc.time()
		DT1 = fread("dt.fwrite.csv")
	elapsed2 = proc.time() - elapsed2

	elapsed3 = proc.time()
		load("dt.rdata")
	elapsed3 = proc.time() - elapsed3

	elapsed4 = proc.time()
		DT1 = readRDS("dt.rds")
	elapsed4 = proc.time() - elapsed4
}

# Adding a new column

# method 1
fileio[, read.command := c("read.csv", "fread", "load", "readRDS")]

# method 2 using data.frame syntax
fileio$read.time.sec = c(elapsed1[3], elapsed2[3], elapsed3[3], elapsed4[3])

fileio[, read.time.ratio := round(read.time.sec / min(read.time.sec), 2)]


fileio

#fread and fwrite are FAST, but save and saveRDS are smaller.

#removing a column
fileio[, save.time.ratio := c()]
fileio[, read.time.ratio := c()]
fileio

#reorder the columns
setcolorder(fileio, c("file.size.MB", "save.command", "save.time.sec", "read.command", "read.time.sec"))
fileio

#save and saveRDS should be used when saving an object that is not in matrix format.



DT2 = data.table(z = runif(1000),
                 x = runif(1000),
                 y = runif(1000))

DT2[, o := as.numeric(z < 0.5)]

fit1 = lm(z ~ x + y, data = DT2)
fit2 = glm(o ~ x + y, family = binomial, data = DT2)

#with save, we can save the objects in individual files or in one file

save(fit1, file = "fit1.rdata")
save(fit2, file = "fit2.rdata")
save(fit1, fit2, file = "fit.rdata")

# with saveRDS, we must save the files in individual files
saveRDS(fit1, file = "fit1.rds")
saveRDS(fit2, file = "fit2.rds")

rm(list = c("fit1", "fit2"))
# load restores the names
load("fit1.rdata")
load("fit2.rdata")
rm(list = c("fit1", "fit2"))
load("fit.rdata")

rm(list = c("fit1", "fit2"))
#readRDS does not restore the names and you can assign new names to the objects
readRDS("fit1.rds")
fit1a = readRDS("fit1.rds")
fit2a = readRDS("fit2.rds")


