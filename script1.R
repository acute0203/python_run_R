library(quantmod)
#匯入資料
STK_name="STOCK_NAME"
STK=read.csv(paste0("STOCK_DIRECTORY",STK_name,".txt"))
rwe = RUN_WITHOUT_EX
timeCharVector=paste(STK[,1]) #取資料第一行
#timeVector=strptime(timeCharVector,"%Y-%m-%d",tz=Sys.timezone()) #轉換成時間序列(日期)，tz是時區
timeVector=strptime(timeCharVector,"%Y/%m/%d",tz=Sys.timezone()) #轉換成時間序列(日期)，tz是時區
#STK=cbind(STK$Open,STK$High,STK$Low,STK$Close,STK$Volume) #取資料的某幾行
STK=cbind(STK$Open,STK$High,STK$Low,STK$Close,STK$TotalVolume) #取資料的某幾行
STK=xts(STK,as.POSIXct(timeVector)) # 轉成時間序列資料
colnames(STK)=c("Open","High","Low","Close","Volume") #欄位命名
total_long_money = 10000

STK=as.matrix(STK) #轉矩陣，轉成矩陣後就不是時間序列，所以用rowname不是time
bband=BBands(STK[,"Close"],n = 20,sd = 1)
using_up_STD = 1.5
using_dn_STD = 1.5
up_bbands=bband[,'mavg']+using_up_STD * (bband[,'up']-bband[,'mavg'])
dn_bbands=bband[,'mavg']-using_dn_STD * (bband[,'mavg']-bband[,'dn'])
avg_bbands=bband[,'mavg']
total_service_charge = 0

#profit=setNames(rep(0,length(rownames(STK))),rownames(STK)) #可以去設定向量名稱
#long strategy
long=0
long_count=0
long_profit=0
long_day_index=20
long_loss_point=0.03
avg_long_cost=0
long_profit=setNames(rep(0,length(rownames(STK))),rownames(STK)) #可以去設定向量名稱
#1:long 2:sell long 3:short 4:buy short
long_trade_status = setNames(rep("不動作",length(rownames(STK))),rownames(STK)) #可以去設定向量名稱

repeat{
  is_touch_lower_edge = Cl(STK)[long_day_index] <= dn_bbands[long_day_index]
  is_touch_upper_edge = Cl(STK)[long_day_index] >= up_bbands[long_day_index] 
  is_ex_vol = Vo(STK)[long_day_index]/Vo(STK)[long_day_index-1] >= 2
  close_price_more_than = Cl(STK)[long_day_index] >= Cl(STK)[long_day_index-1] * 1.05
  close_price_less_than = Cl(STK)[long_day_index] <= Cl(STK)[long_day_index-1] * 0.95
  if(rwe){
  	short_ex_vol_price = FALSE#is_ex_vol && close_price_less_than
  	buy_ex_vol_price = FALSE#is_ex_vol && close_price_more_than
	}else{
    short_ex_vol_price = is_ex_vol && close_price_less_than
    buy_ex_vol_price = is_ex_vol && close_price_more_than
	}
  still_have_money = total_long_money - (long + as.numeric(Cl(STK)[long_day_index])) >=0
  #long strategy
  buy_bool = is_touch_lower_edge
  #if(!still_have_money){
    #print(paste0("No money in long index:",long_day_index,"倉口：",long_count,",money:",total_long_money-long,",stock price:",Cl(STK)[long_day_index]))
  #}
  
  function(){
    #for test
    if(buy_ex_vol_price){
      print(Vo(STK)[long_day_index])
      print(paste0(Vo(STK)[long_day_index]/Vo(STK)[long_day_index-1]))
      print(paste0(Cl(STK)[long_day_index],Cl(STK)[long_day_index-1]))
      break
    }
  }
  if (still_have_money&& !short_ex_vol_price &&(buy_ex_vol_price || buy_bool)){
    long = long + as.numeric(Cl(STK)[long_day_index])
    long_count = long_count + 1
    avg_long_cost=long/long_count
    long_trade_status[long_day_index] = "進場作多:布"
    if(buy_ex_vol_price){
      long_trade_status[long_day_index] = "進場作多:爆"
    }
    #print(paste0("多：",Cl(STK)[long_day_index],",多單總張數:",long_count,",多單平均成本:",avg_long_cost))
  }else if (long_count && (short_ex_vol_price || is_touch_upper_edge || as.numeric(Cl(STK)[long_day_index])<=avg_long_cost*(1 - long_loss_point))){
    service_charge = as.numeric(Cl(STK)[long_day_index]) * 0.006 * long_count
    total_service_charge = total_service_charge + service_charge
    trade_profit = as.numeric(Cl(STK)[long_day_index])*long_count-long
    total_long_money = total_long_money + trade_profit
    long_profit[long_day_index] = trade_profit
    long_trade_status[long_day_index] = "多單平倉"
    #if (long_count!=0){
    #print(paste0("平倉：",Cl(STK)[long_day_index],",多單總張數:",long_count,",多單平均成本:",avg_long_cost,",本次損益:",trade_profit,",多單總損益:",sum(long_profit),",Current total money:",total_long_money))
    #}
    long_count=0
    long=0
    avg_long_cost=0
  }
  #index check
  if(long_day_index < nrow(STK)){
    long_day_index = long_day_index + 1
  }else{
    if (long_count!=0){
      service_charge = as.numeric(Cl(STK)[long_day_index]) * 0.006 * long_count
      total_service_charge = total_service_charge + service_charge
      trade_profit = as.numeric(Cl(STK)[long_day_index])*long_count-long
      total_long_money =total_long_money + trade_profit
      long_profit[long_day_index] = trade_profit
      long_trade_status[long_day_index] = 2
      #long_profit=long_profit+((as.numeric(Cl(STK)[long_day_index]) * long_count)-long)
    }
    break
  }
}

#short strategy
short=0
short_count=0
short_profit=0
short_day_index=20
short_loss_point=0.03
avg_short_cost=0
short_trade_status = setNames(rep("不動作",length(rownames(STK))),rownames(STK)) #可以去設定向量名稱
short_profit=setNames(rep(0,length(rownames(STK))),rownames(STK))
total_short_money = total_long_money
repeat{
  is_touch_lower_edge = Cl(STK)[short_day_index] <= dn_bbands[short_day_index]
  is_touch_upper_edge = Cl(STK)[short_day_index] >= up_bbands[short_day_index] 
  is_ex_vol = Vo(STK)[short_day_index]/Vo(STK)[short_day_index-1] >= 2
  close_price_more_than = Cl(STK)[short_day_index] >= Cl(STK)[short_day_index-1] * 1.05
  close_price_less_than = Cl(STK)[short_day_index] <= Cl(STK)[short_day_index-1] * 0.95
  still_have_money = total_short_money - (short + as.numeric(Cl(STK)[short_day_index])) >=0
  #short strategy
  if(rwe){
  	short_ex_vol_price = FALSE#is_ex_vol && close_price_less_than
  	buy_ex_vol_price = FALSE#is_ex_vol && close_price_more_than
	}else{
    short_ex_vol_price = is_ex_vol && close_price_less_than
    buy_ex_vol_price = is_ex_vol && close_price_more_than
	}
  short_bool = is_touch_upper_edge
  #if(!still_have_money){
    #print(paste0("No money in short index:",short_day_index,",倉口：",short_count,",money:",total_short_money-short,",stock price:",Cl(STK)[short_day_index]))
  #}
  if (still_have_money && !buy_ex_vol_price && (short_ex_vol_price || short_bool)){
    short=short + as.numeric(Cl(STK)[short_day_index])
    short_count = short_count+1
    avg_short_cost=short/short_count
    short_trade_status[short_day_index] = "進場放空:布"
    if(short_ex_vol_price){
      short_trade_status[short_day_index] = "進場放空:爆"
    }
    #print(paste0("空：",Cl(STK)[short_day_index],",空單總張數:",short_count,",空單平均成本:",avg_short_cost))
  }else if(short_count && (buy_ex_vol_price||is_touch_lower_edge|| as.numeric(Cl(STK)[short_day_index])>=avg_short_cost*(1 + short_loss_point))){
    service_charge = as.numeric(Cl(STK)[short_day_index]) * 0.006 * short_count
    total_service_charge = total_service_charge + service_charge
    trade_profit=short-(as.numeric(Cl(STK)[short_day_index])*short_count)
    total_short_money = total_short_money + trade_profit
    short_profit[short_day_index]=trade_profit
    short_trade_status[short_day_index] = "空單平倉"
    #if (short_count!=0){
    #print(paste0("平倉：",Cl(STK)[short_day_index],",空單總張數:",short_count,",空單平均成本:",avg_short_cost,",本次損益:",trade_profit,",空單總損益:",sum(short_profit),",Current total money:",total_short_money))
    #}
    short_count=0
    short=0
    avg_short_cost=0
  }
  if(short_day_index < nrow(STK)){
    short_day_index = short_day_index + 1
  }else{
    if (short_count!=0){
      service_charge = as.numeric(Cl(STK)[short_day_index]) * 0.006 * short_count
      total_service_charge = total_service_charge + service_charge
      trade_profit=short-(as.numeric(Cl(STK)[short_day_index])*short_count)
      total_short_money = total_short_money + trade_profit
      short_trade_status[short_day_index] = 2
      short_profit[short_day_index]=trade_profit
    }
    break
  }
}
#View(cbind(Cl(STK),Vo(STK),up_bbands,dn_bbands,long_trade_status,short_trade_status))

#View(cbind(Cl(STK),up_bbands,dn_bbands,Cl(STK)>=up_bbands,Cl(STK)<=dn_bbands,short_profit))
#print(sum(short_profit))
profit = long_profit + short_profit
wintimes=length(profit[profit>0])/length(profit[profit!=0])#勝率
winloss=mean(profit[profit>0])/abs(mean(profit[profit<0]))#賺賠比
PF=sum(profit[profit>0])/abs(sum(profit[profit<0]))#獲利因子
print(paste0(wintimes,",",winloss,",",PF,",",sum(profit),",",total_service_charge,",",sum(profit) - total_service_charge))