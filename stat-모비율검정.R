nc <- c(7093,6324,6653,3656,6229,6658,16859,1186,648,1000,2161)
lt <- c(8571,10719,6398,4940,7746,4835,1354,1274,1051,1861,4639)

?????? <- c(2020.05,2020.06,2020.07,2020.08,2020.09,2020.10,2020.11,2020.12,2021.01,2021.02,2021.03)
?????? <- ?????? %>% as.character()


nc_pos <- c(2.24,1.95,2.18,2.01,1.99,2.12,2.12,2.54,1.54,2.82,2.63)
nc_neg <- c(1.26,1.54,1.42,1.61,1.53,1.27,1.23,0.85,1.33,0.93,1.16)

lt_pos <- c(2.4,2.54,2.14,2.04,2.18,2.15,2.08,2,1.73,2.86,2.44)
lt_neg <- c(1.13,1.3,1.41,1.49,1.44,1.61,0.94,0.99,0.95,0.82,0.79)

nc_freq <- 58347
lt_freq <- 52014

nc_pos_freq <- round(nc * nc_pos * 0.01) %>% sum()
nc_neg_freq <- round(nc * nc_neg * 0.01) %>% sum()

lt_pos_freq <- round(lt * lt_pos * 0.01) %>% sum()
lt_neg_freq <- round(lt * lt_neg * 0.01) %>% sum()

nc_pos_ratio <- nc_pos_freq / nc_freq
nc_neg_ratio <- nc_neg_freq / nc_freq

lt_pos_ratio <- lt_pos_freq / lt_freq
lt_neg_ratio <- lt_pos_freq / lt_freq
  

prop.test(c(nc_pos_freq,lt_pos_freq), c(nc_freq,lt_freq), alternative = "two.sided")

prop.test(c(nc_neg_freq,lt_neg_freq), c(nc_freq,lt_freq), alternative = "two.sided")


# c(nc_pos_freq,lt_pos_freq) ??????????????????
# c(nc_freq,lt_freq) ??????????????????
