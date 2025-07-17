ccf(data_edited$ipp_ave, data_edited$retail_prices |> filter(type=="rmr"), lag.max = 12)




ippave_ts <- ts(data_edited$ipp_ave$price, start = c(2018,7), frequency = 12)

pricedata <- 
data_edited$ipp_ave |> 
  rename(ipp=price) |> 
  left_join(data_edited$retail_prices |> filter(type=="rmr") |> select(date,rprice=price)) |> 
  
  left_join(data_edited$wholesale_prices |> filter(type=="rmr") |> select(date,wprice=price)) |> 
  
  left_join(data_edited$farmgate_prices |> select(date,farmgate)) |> 
  left_join(data_edited$ipp |> filter(type=="Vietnam 25%") |> select(date,viet_price=price)) |> 
  left_join(data_edited$ipp |> filter(type=="Thai 25%") |> select(date,thai_price=price)) |> 
  left_join(data_edited$ipp |> filter(type=="Pakistan 25%") |> select(date,pak_price=price)) 


ccf(pricedata$viet_price,pricedata$rprice, lag.max=12)


library(tseries)
adf.test(pricedata_comb$ipp)

ccf(pricedata$rprice,pricedata$farmgate, lag.max=12)


library(lmtest)

grangertest(rprice ~ farmgate, order = 6, data = pricedata)

grangertest(ipp~rprice, order = 2, data = pricedata)
