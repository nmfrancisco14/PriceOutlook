library(plotly)
library(gt)

percent_ban <- 
data$pay_psa |> 
  filter(locCode==999,year>=2018,eco==2) |> 
  arrange(year) |> 
  dplyr::select(year,prod = estProduction) |> 
  mutate(riceProd = as.numeric(prod)*0.63,
         yr = as.numeric(year)) |> 
  dplyr::select(yr,riceProd) |> 
  left_join(
    data$imports |> 
      mutate(date = mdy(monthyr)) |> 
      dplyr::select(date,imports=imports_nrp) |> 
      mutate(mth = month(date),
             yr = year(date),
             ban_mths = if_else(mth %in% c(9,10), 1,0),
             imports = as.numeric(imports)) |> 
      group_by(yr,ban_mths) |>
      # group_by(yr) |> 
      summarise(imports = sum(imports,na.rm = T)) |> 
      filter(ban_mths==1) |>
      dplyr::select(-ban_mths)
  ) |> 
  mutate(perc = imports/riceProd) |> 
  ungroup() |> 
  summarise(perc = mean(perc,na.rm = T)) |> 
  pull()


demand_elasticity <- 
  list(
    KQCustodio2011  = -0.5046,
    MIBellezas2020 =  -0.585,
    trajectories2023 = -0.0729,
    NLNojor2023com = -0.071,
    NLNojor2023uncom = -0.88865,
    DDawe = -0.3)


supply_elasticity  <- 
  list(
    JEHinlo2002 = .0471,
    Trajectories2023_farmArea = 0.1818,
    Trajectories2023_retailImp = 1.0929
  )






PriceImpact <- function(shock,ed,es,shock_type = c("demand","supply")){
  if(shock_type =="demand"){
    shock/(ed-es)
  } else {
    shock/(es-ed)
  }
}


case1 <- PriceImpact(-percent_ban,demand_elasticity$trajectories2023,supply_elasticity$Trajectories2023_farmArea)
case2 <- PriceImpact(-percent_ban,demand_elasticity$trajectories2023,0)


baseprice <- 41.31

data.frame(
  rice_form = c("milled rice","dry palay"),
  price_date = c("July2025","June2025"),
  price = c(41.31,16.99),
  supplyshock = -percent_ban*100
) |> 
  expand_grid(
    data.frame(
      source = names(demand_elasticity),
      demand_elas =unlist(demand_elasticity,use.names=FALSE)
      )
  ) |> 
  mutate(pricechange = PriceImpact(-percent_ban, demand_elas,0, shock_type ="demand"),
         newprice = if_else(rice_form =="milled rice",
                            price*(1+pricechange),
                            ((price/0.63)*(1+pricechange))*0.63),
         pricegap = newprice - price
  ) |> gt()
