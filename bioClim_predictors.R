###########################################################################################

	#	MANNUALLY CALCULATE BIOCLIM VARIABLES	#

###########################################################################################

	#BIOCLIM 1: MEAN ANNUAL TEMPERATURE

a <- 1
b <- 12
site_mean_max <- matrix(0, nrow = 1, ncol = nrow(pts))
for ( k in 1:nrow(pts)){				
	quart1 <- matrix(0, nrow = 69, ncol = 1)
	for ( i in 1:69 ) {
		quart1[i, ] <- mean(temp_output[a:b, k], na.rm = T)
#		quart1 <- do.call('rbind', quart)
		a <- 1 + (12 * i)
		b <- 13 + (12 * i)
	}
	a <- 1
	b <- 12
	site_mean_max[, k] <- mean(quart1, na.rm = T)
}

bio1 <- site_mean_max


	#BIOCLIM 2: ANNUAL DIURNAL RANGE

a <- 1
b <- 12
site_mean_max <- matrix(0, nrow = 1, ncol = nrow(pts))
for ( k in 1:nrow(pts)){				
	quart1 <- matrix(0, nrow = 24, ncol = 1)
	for ( i in 1:24 ) {
		quart1[i, ] <- (sum(tmax[a:b, k]) - sum(tmin[a:b, k])) / 12
#		quart1 <- do.call('rbind', quart)
		a <- 1 + (12 * i)
		b <- 13 + (12 * i)
	}
	a <- 1
	b <- 12
	site_mean_max[, k] <- mean(quart1, na.rm = T)
}

bio2 <- site_mean_max


	#BIOCLIM 3: ISOTHERMALITY

# SEE BELOW BIO7 (BIO3 = (BIO2 / BIO7) * 100)


	#BIOCLIM 4: TEMPERATURE SEASONALITY

a <- 1
b <- 12
site_mean_max <- matrix(0, nrow = 1, ncol = nrow(pts))
for ( k in 1:nrow(pts)){				
	quart1 <- matrix(0, nrow = 69, ncol = 1)
	for ( i in 1:69 ) {
		quart1[i, ] <- sd(temp_output[a:b, k], na.rm = T)
#		quart1 <- do.call('rbind', quart)
		a <- 1 + (12 * i)
		b <- 13 + (12 * i)
	}
	a <- 1
	b <- 12
	site_mean_max[, k] <- mean(quart1, na.rm = T)
}

bio4 <- site_mean_max

	#BIOCLIM 5: MAXIMUM TEMPERATURE OF THE HOTTEST MONTH

a <- 1
b <- 12
site_mean_max <- matrix(0, nrow = 1, ncol = nrow(pts))
for ( k in 1:nrow(pts)){				
	quart1 <- matrix(0, nrow = 24, ncol = 1)
	for ( i in 1:24 ) {
		quart1[i, ] <- max(tmax[a:b, k], na.rm = T)
#		quart1 <- do.call('rbind', quart)
		a <- 1 + (12 * i)
		b <- 13 + (12 * i)
	}
	a <- 1
	b <- 12
	site_mean_max[, k] <- mean(quart1, na.rm = T)
}

bio5 <- site_mean_max

	#BIOCLIM 6: MINIMUM TEMPERATURE OF THE COLDEST MONTH

a <- 1
b <- 12
site_mean_max <- matrix(0, nrow = 1, ncol = nrow(pts))
for ( k in 1:nrow(pts)){				
	quart1 <- matrix(0, nrow = 24, ncol = 1)
	for ( i in 1:24 ) {
		quart1[i, ] <- min(tmin[a:b, k], na.rm = T)
#		quart1 <- do.call('rbind', quart)
		a <- 1 + (12 * i)
		b <- 13 + (12 * i)
	}
	a <- 1
	b <- 12
	site_mean_max[, k]<- mean(quart1, na.rm = T)
}

bio6 <- site_mean_max

	#BIOCLIM 7: ANNUAL TEMPERATURE RANGE

bio7 <- bio5 - bio6

bio3 <- (bio2 / bio7) * 100


	#BIOCLIM 8: MEAN TEMPERATURE WETTEST QUARTER

a <- 1
b <- 3
site_mean_max <- matrix(0, nrow = 1, ncol = nrow(pts))
for ( k in 1:nrow(pts)){				
	mean_max <- data.frame(1:69, 1:69)
	for (j in 1:69){
			quart_t <- list()
			quart_pr <- list()
			for ( i in 1:12 ) {
				quart_pr[[i]] <- mean(precip_output[a:b, k], na.rm = T)
				quart_t [[i]] <- mean(temp_output[a:b, k], na.rm = T)
				quart1 <- do.call('rbind', quart_pr)
				quart2 <- do.call('rbind', quart_t)
				a <- a+1
				b <- b+1
			}
		a <- 1 + (12 * j)
		b <- 3 + (12 * j)
		test <- data.frame(cbind(quart1, quart2))
		mean_max[j, ] <- test[test$X1 == max(test$X1), ]
#		mean_max <- do.call('rbind', outlist)
	}
	a <- 1
	b <- 3
	site_mean_max[, k] <- mean(mean_max[, 2], na.rm = T)
}

bio8 <- site_mean_max
str(bio8)

	#BIOCLIM 9: MEAN TEMPERATURE DRIEST QUARTER

a <- 1
b <- 3
site_mean_max <- matrix(0, nrow = 1, ncol = nrow(pts))
for ( k in 1:nrow(pts)){				
	mean_max <- data.frame(1:69, 1:69)
	for (j in 1:69){
			quart_t <- list()
			quart_pr <- list()
			for ( i in 1:12 ) {
				quart_pr[[i]] <- mean(precip_output[a:b, k], na.rm = T)
				quart_t [[i]] <- mean(temp_output[a:b, k], na.rm = T)
				quart1 <- do.call('rbind', quart_pr)
				quart2 <- do.call('rbind', quart_t)
				a <- a+1
				b <- b+1
			}
		a <- 1 + (12 * j)
		b <- 3 + (12 * j)
		test <- data.frame(cbind(quart1, quart2))
		mean_max[j, ] <- test[test$X1 == min(test$X1), ]
#		mean_max <- do.call('rbind', outlist)
	}
	a <- 1
	b <- 3
	site_mean_max[, k] <- mean(mean_max[, 2], na.rm = T)
}

bio9<- site_mean_max
str(bio9)

	#BIOCLIM 10: MEAN TEMPERATURE WARMEST QUARTER

a <- 1
b <- 3
site_mean_max <- matrix(0, nrow = 1, ncol = nrow(pts))
for ( k in 1:nrow(pts)){				
	mean_max <- matrix(0, nrow = 69, ncol = 1)
	for (j in 1:69){
			quart1 <- matrix(0, nrow = 12, ncol = 1)
			for ( i in 1:12 ) {
				quart1[i, ] <- mean(temp_output[a:b, k], na.rm = T)
#				quart1 <- do.call('rbind', quart)
				quart1_max <- max(quart1, na.rm = T)
				a <- a+1
				b <- b+1
			}
		a <- 1 + (12 * j)
		b <- 3 + (12 * j)
		mean_max[j, ] <- quart1_max
#		mean_max <- do.call('rbind', outlist)
	}
	a <- 1
	b <- 3
	site_mean_max[, k] <- mean(mean_max, na.rm = T)
}

bio10 <- site_mean_max

	#BIOCLIM 11: MEAN TEMPERATURE COLDEST QUARTER

a <- 1
b <- 3
site_mean_max <- matrix(0, nrow = 1, ncol = nrow(pts))
for ( k in 1:nrow(pts)){
	mean_max <- matrix (0, nrow = 69, ncol = 1)
	for (j in 1:69){
			quart1 <- matrix(0, nrow = 12, ncol = 1)
			for ( i in 1:12 ) {
				quart1[i, ] <- mean(temp_output[a:b, k], na.rm = T)
#				quart1 <- do.call('rbind', quart)
				quart1_max <- min(quart1, na.rm = T)
				a <- a+1
				b <- b+1
			}
		a <- 1 + (12 * j)
		b <- 3 + (12 * j)
		mean_max[j, ] <- quart1_max
#		mean_max <- do.call('rbind', outlist)
	}
	a <- 1
	b <- 3
	site_mean_max[, k] <- mean(mean_max, na.rm = T)
}

bio11 <- site_mean_max


	#BIOCLIM 12: ANNUAL PRECIPITATION

a <- 1
b <- 12
site_mean_max <- matrix(0, nrow = 1, ncol = nrow(pts))
for ( k in 1:nrow(pts)){				
	quart1 <- matrix(0, nrow = 69, ncol = 1)
	for ( i in 1:69 ) {
		quart1[i, ] <- sum(precip_output[a:b, k], na.rm = T)
#		quart1 <- do.call('rbind', quart)
		a <- 1 + (12 * i)
		b <- 13 + (12 * i)
	}
	a <- 1
	b <- 12
	site_mean_max[, k] <- mean(quart1, na.rm = T)
}

bio12 <- site_mean_max

	#BIOCLIM 13: PRECIPITATION OF THE WETTEST MONTH

a <- 1
b <- 12
site_mean_max <- matrix(0, nrow = 1, ncol = nrow(pts))
for ( k in 1:nrow(pts)){				
	quart1 <- matrix(0, nrow = 69, ncol = 1)
	for ( i in 1:69 ) {
		quart1[i, ] <- max(precip_output[a:b, k], na.rm = T)
#		quart1 <- do.call('rbind', quart)
		a <- 1 + (12 * i)
		b <- 13 + (12 * i)
	}
	a <- 1
	b <- 12
	site_mean_max[, k] <- mean(quart1, na.rm = T)
}

bio13 <- site_mean_max


	#BIOCLIM 14: PRECIPITATION OF THE DRIEST MONTH

a <- 1
b <- 12
site_mean_max <- matrix(0, nrow = 1, ncol = nrow(pts))
for ( k in 1:nrow(pts)){				
	quart1 <- matrix(0, nrow = 69, ncol = 1)
	for ( i in 1:69 ) {
		quart1[i, ] <- min(precip_output[a:b, k], na.rm = T)
#		quart1 <- do.call('rbind', quart)
		a <- 1 + (12 * i)
		b <- 13 + (12 * i)
	}
	a <- 1
	b <- 12
	site_mean_max[, k] <- mean(quart1, na.rm = T)
}

bio14 <- site_mean_max



	#BIOCLIM 15: PRECIPITATION SEASONALITY (CV)

a <- 1
b <- 12
site_mean_max <- matrix(0, nrow = 1, ncol = nrow(pts))
for ( k in 1:nrow(pts)){				
	quart1 <- matrix(0, nrow = 69, ncol = 1)
	for ( i in 1:69 ) {
		quart1[i, ] <- sd(precip_output[a:b, k], na.rm = T)
#		quart1 <- do.call('rbind', quart)
		a <- 1 + (12 * i)
		b <- 13 + (12 * i)
	}
	a <- 1
	b <- 12
	site_mean_max[, k] <- mean(quart1, na.rm = T)
}

bio15 <- (site_mean_max / (1 + (bio12 / 12))) * 100
str(bio15)


	#BIOCLIM 16: PRECIPITATION OF THE WETEST QUARTER

a <- 1
b <- 3
site_mean_max <- matrix(0, nrow = 1, ncol = nrow(pts))
for ( k in 1:nrow(pts)){		
	mean_max <- matrix (0, nrow = 69, ncol = 1)
	for (j in 1:69){
		quart1 <- matrix(0, nrow = 12, ncol = 1)
			for ( i in 1:12 ) {
				quart1[i, ] <- sum(precip_output[a:b, k], na.rm = T)
#				quart1 <- do.call('rbind', quart)
				quart1_max <- max(quart1, na.rm = T)
				a <- a+1
				b <- b+1
			}
		a <- 1 + (12 * j)
		b <- 3 + (12 * j)
		mean_max[j, ] <- quart1_max
#		mean_max <- do.call('rbind', outlist)
	}
	a <- 1
	b <- 3
	site_mean_max[, k] <- mean(mean_max, na.rm = T)
}

bio16 <- site_mean_max

	#BIOCLIM 17: PRECIPITATION OF THE DRIEST QUARTER

a <- 1
b <- 3
site_mean_max <- matrix(0, nrow = 1, ncol = nrow(pts))
for ( k in 1:nrow(pts)){		
	mean_max <- matrix (0, nrow = 69, ncol = 1)
	for (j in 1:69){
		quart1 <- matrix(0, nrow = 12, ncol = 1)
			for ( i in 1:12 ) {
				quart1[i, ] <- sum(precip_output[a:b, k], na.rm = T)
#				quart1 <- do.call('rbind', quart)
				quart1_max <- min(quart1, na.rm = T)
				a <- a+1
				b <- b+1
			}
		a <- 1 + (12 * j)
		b <- 3 + (12 * j)
		mean_max[j, ] <- quart1_max
#		mean_max <- do.call('rbind', outlist)
	}
	a <- 1
	b <- 12
	site_mean_max[, k] <- mean(mean_max, na.rm = T)
}

bio17 <- site_mean_max


	#BIOCLIM 18: PRECIPITATION OF THE WARMEST QUARTER

a <- 1
b <- 3
site_mean_max <- matrix(0, nrow = 1, ncol = nrow(pts))
for ( k in 1:nrow(pts)){				
	mean_max <- data.frame(1:69, 1:69)
	for (j in 1:69){
			quart1 <- matrix(0, nrow = 12, ncol = 1)
			quart2 <- matrix(0, nrow = 12, ncol = 1)
			for ( i in 1:12 ) {
				quart1[i, ] <- mean(precip_output[a:b, k], na.rm = T)
				quart2 [i, ] <- mean(temp_output[a:b, k], na.rm = T)
#				quart1 <- do.call('rbind', quart_pr)
#				quart2 <- do.call('rbind', quart_t)
				a <- a+1
				b <- b+1
			}
		a <- 1 + (12 * j)
		b <- 3 + (12 * j)
		test <- data.frame(cbind(quart1, quart2))
		mean_max[j, ] <- test[test$X2 == max(test$X2), ]
#		mean_max <- do.call('rbind', outlist)
	}
	a <- 1
	b <- 3
	site_mean_max[, k] <- mean(mean_max[, 1], na.rm = T)
}

bio18<- site_mean_max


	#BIOCLIM 19: PRECIPITATION OF THE COLDEST QUARTER

a <- 1
b <- 3
site_mean_max <- matrix(0, nrow = 1, ncol = nrow(pts))
for ( k in 1:nrow(pts)){				
	mean_max <- data.frame(1:69, 1:69)
	for (j in 1:69){
			quart1 <- matrix(0, nrow = 12, ncol = 1)
			quart2 <- matrix(0, nrow = 12, ncol = 1)
			for ( i in 1:12 ) {
				quart1[i, ] <- mean(precip_output[a:b, k], na.rm = T)
				quart2 [i, ] <- mean(temp_output[a:b, k], na.rm = T)
#				quart1 <- do.call('rbind', quart_pr)
#				quart2 <- do.call('rbind', quart_t)
				a <- a+1
				b <- b+1
			}
		a <- 1 + (12 * j)
		b <- 3 + (12 * j)
		test <- data.frame(cbind(quart1, quart2))
		mean_max[j, ] <- test[test$X2 == min(test$X2), ]
#		mean_max <- do.call('rbind', outlist)
	}
	a <- 1
	b <- 3
	site_mean_max[, k] <- mean(mean_max[, 1], na.rm = T)
}

bio19<- site_mean_max



#################################################################################################

										#	END!!!	#

#################################################################################################

