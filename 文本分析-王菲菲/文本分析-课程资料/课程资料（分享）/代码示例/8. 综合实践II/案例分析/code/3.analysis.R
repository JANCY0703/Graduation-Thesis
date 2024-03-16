############说明：该程序用于描述统计分析和建立线性回归模型###################


rm(list=ls())
##设置路径
setwd("C:\\Users\\star\\Desktop\\案例分析") 
####根据您文件存放位置相应改变
##加载程序包
library(ggplot2)
library(RColorBrewer)
library(car)


##读入整理好的数据集
data_reg=read.csv("./results/data_reg.csv",header=T)
####查看行列，共208行，39列
N=dim(data_reg)[1]
####将数据中的各列分别以它们的名字命名为单独的变量
attach(data_reg)
####提取phoneID
phoneid=data_reg[,1]


#########################1.描述分析
##好评率
####好评率的分布
good_freq=好评数/总评论数
####制作直方图
p=ggplot(data_reg, aes(x=good_freq))+geom_histogram(bins=20)+xlab("好评率")+ylab("频数")
p+ theme_set(theme_bw())+theme(panel.grid.major=element_line(colour=NA),panel.grid=element_blank(),legend.position='none')

##最低点作为异常值，去掉
####寻找好评率最低点
num=which.min(good_freq)
####在数据中去掉改点
good_freq=good_freq[-num]
data_reg=data_reg[-num,]
attach(data_reg)
####重新整理数据，提取phoneID，计算行列和好评率
phoneid=data_reg[,1]
N=dim(data_reg)[1]


##将价格调整为以千元为单位
data_reg$价格=data_reg$价格/1000


##品牌
####统计每个品牌的总评论数
total_sale=tapply(总评论数,品牌,sum)
####降序排列
total_sale=total_sale[order(total_sale,decreasing=T)]
####计算前8个品牌的占比
sum(total_sale[1:8])/sum(total_sale)
####找出前8个品牌的名字
topnames=names(total_sale[1:8])
####我们把出现次数最多的前8种合并到一起，然后把其他品牌列为“其他”
sel_num=which((品牌!=topnames[1])&(品牌!=topnames[2])&(品牌!=topnames[3])&(品牌!=topnames[4])&
(品牌!=topnames[5])&(品牌!=topnames[6])&(品牌!=topnames[7])&(品牌!=topnames[8]))
####重新定义品牌2
品牌2=as.character(品牌)
####令不在8大品牌众的其他品牌都是“其他”
品牌2[sel_num]="其他"
thename=c(topnames,"其他")
品牌2=factor(品牌2,order=TRUE,levels=c(topnames,"其他"))
####设置各个品牌为单独的示性变量
华为=小米=乐视=奇虎360=OPPO=三星=VIVO=nubia=rep(0,N)
华为[which(品牌2=="华为（HUAWEI）")]=1
小米[which(品牌2=="小米（MI）")]=1
乐视[which(品牌2=="乐视（Letv）")]=1
奇虎360[which(品牌2=="360")]=1
OPPO[which(品牌2=="OPPO")]=1
三星[which(品牌2=="三星（SAMSUNG）")]=1
VIVO[which(品牌2=="vivo")]=1
nubia[which(品牌2=="努比亚（nubia）")]=1


##前摄像头像素
####将前置摄像头划分为低、中、高
前置摄像头=rep("中(500~1000)",N)
前置摄像头[which(data_reg$前置摄像头像素>1000)]="高(>1000)"
前置摄像头[which(data_reg$前置摄像头像素<500)]="低(<500)"


##后摄像头像素
####将后置摄像头划分为低、中、高
后置摄像头=rep("中(1000~1500)",N)
后置摄像头[which(data_reg$后置摄像头像素>1500)]="高(>1500)"
后置摄像头[which(data_reg$后置摄像头像素<1000)]="低(<1000)"


##指纹识别设置
####将指纹识别的取值变为类别
指纹识别设置=rep("其他",N)
指纹识别设置[which(data_reg$指纹识别==0)]="不支持"
指纹识别设置[which(data_reg$指纹识别==1)]="支持"


##GPS
####将GPS的取值变为类别
GPS设置=rep("其他",N)
GPS设置[which(data_reg$GPS定位==0)]="不支持"
GPS设置[which(data_reg$GPS定位==1)]="支持"


##是否促销和好评率的关系
####将否促销的取值变为类别
data_reg$促销信息[which(data_reg$促销信息==0)]="无"
data_reg$促销信息[which(data_reg$促销信息==1)]="有"



#########################2.建立线性回归模型
##建立回归模型
####进行回归
model=lm(log(good_freq)~价格+华为+小米+乐视+奇虎360+三星+OPPO+VIVO+nubia
         +屏幕尺寸+前置摄像头+后置摄像头+指纹识别设置+GPS设置+促销信息
         +avg_words
         +速度+物流+送货+包装+客服
         +售后+发票+屏幕+电池+系统+性价比
         +质量+外观+功能+运行
         +充电+声音+耳机+信号+开机+软件
         ,data=data_reg)
####用BIC进行变量筛选
model.bic=step(model,k=log(N),trace=F)
####看回归结果
summary(model.bic)
####用VIF检查多重共线性
vif(model.bic)

