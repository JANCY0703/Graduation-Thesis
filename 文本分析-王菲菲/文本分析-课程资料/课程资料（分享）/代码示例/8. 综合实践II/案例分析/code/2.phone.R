############说明：该程序用于处理手机数据，并与评论数据合并在一起###################

rm(list=ls())
##设置路径
setwd("C:\\Users\\star\\Desktop\\案例分析")  
####根据您文件存放位置相应改变
##读入手机指标数据
phone_infor=read.csv("./data/phone_infor_utf8.csv",header=T)


##数据中有很多的空值，这是因为网站上并没有公布相应的信息，所以需要对缺失值进行处理
####我们首先来看各个变量的空缺值情况
########找到每个变量填写信息的数量
fill=apply(phone_infor,2,function(x){length(na.omit(x))})
########计算每个变量的缺失比例
lack=(dim(phone_infor)[1]-fill)/dim(phone_infor)[1]
########将缺失比例降序排列
sort(round(lack,3),decreasing=T)
########可以看到，机身厚度的缺失比例是最大的，其次是机身重量和ROM内存，我们决定舍弃机身厚度、重量和ROM内存
phone_infor_2=phone_infor[,-c(5,6,8)]
########进一步，去掉所有变量的缺失值，即只要有一个每行的所有变量中有一个缺失，就剔除它
phone_infor_2=na.omit(phone_infor_2)


##将phone数据和comment数据合并到一起
####读取comments数据
com_reg=read.csv("./results/com_reg.csv",header=T)
####找到phone数据中的手机ID
phoneid1=phone_infor_2$手机编号
####找到comments数据中的手机ID
phoneid2=com_reg[,1]
####看phoneid2中包含哪些phoneid1
sel_num=unlist(apply(as.matrix(phoneid1),1,function(x){return(which(phoneid2==x))}))
####筛选phoneid2
phoneid=phoneid2[sel_num]
####筛选comments数据
com_reg2=com_reg[sel_num,]
####统计手机的数目
I=length(phoneid)
####筛选phone数据
########定义标号的存储变量
order1=0
########对每个手机ID进行操作
for(i in 1:I)
{
 theid=com_reg2[i,1]
 num1=which(phoneid1==theid)
 order1=c(order1,num1)
}
order1=order1[-1]
####筛选出phone数据
phone_infor_3=phone_infor_2[order1,]
####将两部分数据进行合并
data_reg=cbind(phone_infor_3,com_reg2)

##保存结果
write.csv(data_reg, file = "./results/data_reg.csv", quote = F, row.names = F)




