install.packages("data.table")

"""Any stock greater than 50% ignore the stock PROBM
operating income/(PPE +Accum. Depr.) + (working cap)

##Lab 9 attempt
"""

data=readRDS("/Users/rohitraikhy/Downloads/DATA.rds")
pricedf=readRDS("/Users/rohitraikhy/Documents/IM/pricedf.RDS")

## Lets change the names of the columns to make  it easier to code

df=data[,c('Date','PRIME','Accounts Receivable','Gross Profit','Revenue','Total Assets','Total Current Assets','Gross Property, Plant And Equipment','Accumulated Depreciation','Depreciation, Depletion And Amortization','Selling, General, & Admin. Expense','Total Debt Per Share','Shares Outstanding (Eop)', 'Cash And Cash Equivalents','Total Liabilities')]

names(df)=c('Date','Tick','AR','GP','Rev','TA','TCA','GPPE','AD','DDA','SGAE','TD','SO','CCE','TL')

df$td=(df$TD*df$SO)

### use data.tables to calc the ratios

library(data.table)

setDT(df)

## DSRI - Days Sale in inventory

df=df[order(Tick,Date)]
df[,AR.y:=shift(AR,n=1,type="lag"),by=.(Tick)]
df[,DSRI:=(AR/AR.y)]

### Gross Margin Index

df[,GP.y:=shift(GP,n=1,type='lag'),by=.(Tick)]
df[,Rev.y:=shift(Rev,n=1,type='lag'),by=.(Tick)]
df[,GMI:=((GP.y/Rev.y)/(GP/Rev))]

###Asset Quality Index - AQI

df[,TA.y:=shift(TA,n=1,type='lag'),by=.(Tick)]

##default is lag
df[,TCA.y:=shift(TA,n=1),by=.(Tick)]
df[,GPPE.y:=shift(GPPE,n=1),by=.(Tick)]
df[,AD.y:=shift(AD,n=1),by=.(Tick)]

df[,AQI:=((TA-TCA-GPPE+AD)/TA)/((TA.y-TCA.y-GPPE.y+AD.y)/(TA.y))]

##SGI - Sales Growth Index

df[,Rev.y:=shift(Rev,n=1),by=.(Tick)]

df[,SGI:=(Rev/Rev.y)]

##DEPI - Depreciation 

df[,DDA.y:=shift(DDA,n=1),by=.(Tick)]
df[,DEPI:=DDA.y/DDA]

##SGAI 

df[,SGAE.y:=shift(SGAE,n=1),by=.(Tick)]
df[,SGAI:=SGAE/SGAE.y]

##LVGI

df[,TD.y:=shift(TD,n=1),by=.(Tick)]
df[,LVGI:=((TD/TA)/(TD.y/TA.y))]

##TATA

df[,DL.y:=shift(TL,n=1),by=.(Tick)]

df[,NOA:=(TA=CCE-TL+TD)]
df[,NOA.y:=shift(NOA,n=1),by=.(Tick)]

df[,TATA:=(((NOA-NOA.y))/((NOA+NOA.y)/2))]

##ROC

df$OP=data$`Operating Income`

df$TCL=data$`Total Current Liabilities`

df$PPE=data$`Gross Property, Plant And Equipment`

df[,ROC:=OP/((PPE+AD)+(TCA-TCL))]

###Now lets calc the PROBM

df[,PROBM:=(-4.84+0.92*DSRI+0.528*GMI+0.404*AQI+0.892*SGI+0.115*DEPI-0.172*SGAI+4.679*TATA-3.27*LVGI)]

#calc prob

df[,Prob:=pnorm(PROBM,mean=0,sd=1)]

df$CCE=data$`Cash And Cash Equivalents`
df$OP=data$`Operating Income`

## Lets get the prices

pricedf$Period=as.integer(format(pricedf$Date,'%Y%m'))

ratios=df[,c('Date','Tick','DSRI','GMI','AQI','SGI','DEPI','SGAI','LVGI','TATA','PROBM','Prob','ROC','SO','TD','CCE','OP')]

ratios$PS=data$`Preferred Stock`
ratios$MI=data$`Minority Interest`


setDT(pricedf)

##Calc. returns 

pricedf=pricedf[order(Ticker,Period)]

df2=merge(ratios,pricedf,by.x=c('Tick','Date'),by.y=c('Ticker','Period'))



##calc TEV

df2[,TEV:=((SO*Price)+TD-CCE+PS+MI)]

df[,TD.y:=shift(TD,n=1),by=.(Tick)]
df2[,OP1:=shift(OP,n=1),by=.(Tick)]
df2[,OP2:=shift(OP,n=2),by=.(Tick)]
df2[,OP3:=shift(OP,n=3),by=.(Tick)]

df2[,EY:=(((OP3+OP2+OP1+OP)/4)/TEV)]

df2[,c('ROC1','ROC2','ROC3','ROC4','ROC5','ROC6','ROC7'):=shift(ROC,n=(1:7)),by=.(Tick)]
df2[,ROC1:=shift(ROC,n=(1:7)),by=.(Tick)]

df3=subset(df2,Date>=200612 & Date<=201612)

df3[,GeoMean:=((ROC*ROC1*ROC2*ROC3*ROC4*ROC5*ROC6*ROC7)^(1/8))]

df3[,Price2:=shift(Price,n=4,type='lead'),by=.(Tick)]

test=df3[,.(Date,Price,Price2,Tick)]

df3[,AnnualReturn:=((Price2/Price)-1)]

resdfo=df3[order(Date,-AnnualReturn)]

resdfo2=resdfo[,head(.SD,10),by=.(Date)]


resdfo3=resdfo2[,.(Date,Tick,GeoMean,EY,AnnualReturn)]

resdfo4=resdfo3[is.finite(GeoMean)]

resdfo4[,AvgROC:=(sum(GeoMean)/(.N)),by=.(Date)]
resdfo4[,AvgEY:=(sum(EY)/(.N)),by=.(Date)]

resdfo4[,MaxROC:=max(GeoMean),by=.(Date)]
resdfo4[,MaxEY:=max(EY),by=.(Date)]

resdfo4[,MinROC:=min(GeoMean),by=.(Date)]
resdfo4[,MinEY:=min(EY),by=.(Date)]

resdf=resdfo4[,.(Date,AvgROC,AvgEY,MaxROC,MaxEY,MinROC,MinEY)]

resdf=resdf[,unique(.SD),by=.(Date)]


test=resdfo3[order(AnnualReturn)]

test[Date=='200612',head(.SD,2)]
test[Date=='200612']

test2=merge(test,pricedf,by.x=c('Tick','Date'),by.y=c('Ticker','Period'))

test=subset(df,Date=='201512'|Date=='201606'|Date=='201712',c(TA,Date))

value=which.max(df$TA)
df[value,'Date']

dfa=subset(df,TA>=50&TA<=100)
