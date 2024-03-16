############说明：该程序用于深挖热评词###################


############################1.读入数据，并设置需要的基本函数（部分程序和1.comments.R相同）
rm(list=ls())
##设置路径
setwd("C:\\Users\\star\\Desktop\\案例分析")  
####根据您文件存放位置相应改变
##加载R包                                                                                                          
library(jiebaRD)
library(jiebaR)                                                       
library(RColorBrewer)


##读入comments数据
com_data=read.csv("./data/comments_infor_utf8.csv", header = T, stringsAsFactors = F)


##对com_data中的comments进行简单的文本统计
####提取评论所在的列
comments=com_data$评论内容
####统计词数
words=nchar(comments)
####看字数的分布情况
summary(words)
####发现字数中有0，所以我们去掉字数为0的评论
########找出哪些行是0字符的
num=which(words==0)
########在评论数据中去掉字数为0的行
com_data=com_data[-num,]
####更新com_data数据
comments=com_data$评论内容
words=words[-num]
ratings=com_data$评论得分
phoneid=com_data$手机编号
####统计句子，我们按照“，。！？”来表明一个句子
sentence=strsplit(comments,"，|。|！|？")


##分词
####设置停用词所在目录
stoppath="./data/stopwords_utf8.txt"
####初始化分词器，即能分词，又可以进行词性标注，并且在分词的时候去停用词
cutter = worker('tag',bylines = TRUE,stop_word=stoppath)


##对分词结果按照词性进行筛选
####读入选取的词性
choose_tags=read.csv("./data/choose_tags_utf8.csv", header = F, stringsAsFactors = F) 
####找到词性所在列
choose_tags=as.matrix(choose_tags[,1])
####定义筛选函数，对每个分词结果，找出其符合choose_tags里面词性的全部词
select=function(theres)
{
 thename=names(theres) #提取词性
 num=unlist(apply(choose_tags,1,function(x){return(which(thename==x))})) #返回符合choose_tags里面词性的下标
 return(theres[num]) #返回对应的词
}


##定义查找关键词的函数，x为查找的comments，word为查找的词
find_word=function(x,word)
{
 num=grep(word,x)  #判断x中有哪些包含word
 if(length(num)>0) #如果包含word，则返回这个comment，否则不返回
 {return(x[num])}
}


############################2.物流

##找到所有包含“物流”的句子
####定义关键词为“物流”
target="物流"
####在sentence中返回所有包含target的短句
aa=lapply(sentence,find_word,word=target)
####去掉list的形式
aa=unlist(aa)
####对这些短句进行分词
res=cutter[aa]
####将分词后的结果从list转换为matrix格式
res=lapply(res,as.matrix)


##寻找关注点
####将每行文本的分词结果逐一排列起来
res = as.data.frame(do.call(rbind, res), stringsAsFactors = F) 
freq = as.data.frame(table(res),stringsAsFactors = F) 
####按词频个数降序排列           
freq = freq[order(-freq[,2]),]  
####挑选前100个高频词，有时可能需要列出给多的词                               
top100 = freq[1:100,]
####提炼关注点
select_words=c("速度","服务","包装")


##整合相近的词汇
####提取所有包含target的评论
tnum=grep(target,comments)
comments2=comments[tnum]
####统计个数
N=length(tnum)
####将相近的词汇进行替代，可能是相近的名词，也可能是该名词的形容词
comments2=gsub(pattern = "快", replacement = "速度", comments2)
comments2=gsub(pattern = "慢", replacement = "速度", comments2)
comments2=gsub(pattern = "第二天", replacement = "速度", comments2)
comments2=gsub(pattern = "给力", replacement = "速度", comments2)
comments2=gsub(pattern = "神速", replacement = "速度", comments2)
comments2=gsub(pattern = "态度", replacement = "服务", comments2)


##统计在出现target的评论中，出现某个关注点的评论
####统计在出现target的评论中，出现某个关注点的评论标号
num1=grep("速度",comments2)
num2=grep("服务",comments2)
num3=grep("包装",comments2)
####统计出现target+关注点的评论数比例
n1=length(num1)/N
n2=length(num2)/N
n3=length(num3)/N
####将各个比例放入一个向量中，并命名
wuliu=c(n1,n2,n3)
names(wuliu)=select_words
####计算所有包含这些关注点的评论占总评论的个数
length(unique(c(num1,num2,num3)))/N
####作图
barplot(wuliu,col=brewer.pal(3, 'Blues'),ylim=c(0,1),ylab="在物流评论中占比")


##计算每个关注点的得分
####所有包含target的评论的平均分
theratings=ratings[tnum]
####包含target+某个关注点的评论的平均分
rr1=mean(theratings[num1])
rr2=mean(theratings[num2])
rr3=mean(theratings[num3])
####将所有平均分放入一个向量，并命名
rr=mean(theratings)
ruse=c(rr1,rr2,rr3,rr)
names(ruse)=c("物流+速度","物流+服务","物流+包装","物流")
####计算行业标准，即所有评论的平均分
themean=mean(ratings)
####作图进行对比
barplot(ruse,col=c(brewer.pal(3, 'Greens'),"dark red"),ylab="平均分",ylim=c(0,5))
abline(h=themean,lwd=2)
####最后，将需要的结果用物流的名字进行保存，以便和之后的结果进行区分
ruse_wuliu=ruse
com_wuliu=comments2
num_wuliu=tnum
rat_wuliu=theratings



############################3.客服（这部分程序的思路和物流部分相同，差别仅在于提取的关注点）
target="客服"
####在sentence中返回所有包含target的短句
aa=lapply(sentence,find_word,word=target)
####去掉list的形式
aa=unlist(aa)
####对这些短句进行分词
res=cutter[aa]
####将分词后的结果从list转换为matrix格式
res=lapply(res,as.matrix)


##寻找关注点
####将每行文本的分词结果逐一排列起来
res = as.data.frame(do.call(rbind, res), stringsAsFactors = F) 
freq = as.data.frame(table(res),stringsAsFactors = F) 
####按词频个数降序排列           
freq = freq[order(-freq[,2]),]  
####挑选前100个高频词，有时可能需要列出给多的词                               
top100 = freq[1:100,]
select_words=c("态度","在线","售后","退换补","保价","赠品","发票")


##整合相近的词汇
####提取所有包含target的评论
tnum=grep(target,comments)
comments2=comments[tnum]
####统计个数
N=length(tnum)
####将相近的词汇进行替代，可能是相近的名词，也可能是该名词的形容词
comments2=gsub(pattern = "不理", replacement = "在线", comments2)
comments2=gsub(pattern = "不回话", replacement = "在线", comments2)
comments2=gsub(pattern = "不在", replacement = "在线", comments2)
comments2=gsub(pattern = "联系不上", replacement = "在线", comments2)
comments2=gsub(pattern = "没人", replacement = "在线", comments2)
comments2=gsub(pattern = "等待", replacement = "在线", comments2)
comments2=gsub(pattern = "及时", replacement = "在线", comments2)
comments2=gsub(pattern = "不回复", replacement = "在线", comments2)
comments2=gsub(pattern = "很慢", replacement = "在线", comments2)
comments2=gsub(pattern = "很快", replacement = "在线", comments2)
comments2=gsub(pattern = "没有回复", replacement = "在线", comments2)
comments2=gsub(pattern = "半天", replacement = "在线", comments2)
comments2=gsub(pattern = "第二天", replacement = "在线", comments2)
comments2=gsub(pattern = "几天", replacement = "在线", comments2)
comments2=gsub(pattern = "电话", replacement = "态度", comments2)
comments2=gsub(pattern = "回复", replacement = "态度", comments2)
comments2=gsub(pattern = "答复", replacement = "态度", comments2)
comments2=gsub(pattern = "敷衍", replacement = "态度", comments2)
comments2=gsub(pattern = "耐心", replacement = "态度", comments2)
comments2=gsub(pattern = "无奈", replacement = "态度", comments2)
comments2=gsub(pattern = "一句", replacement = "态度", comments2)
comments2=gsub(pattern = "太差", replacement = "态度", comments2)
comments2=gsub(pattern = "沟通", replacement = "态度", comments2)
comments2=gsub(pattern = "检测", replacement = "售后", comments2)
comments2=gsub(pattern = "修", replacement = "售后", comments2)
comments2=gsub(pattern = "补货", replacement = "换", comments2)
comments2=gsub(pattern = "差价", replacement = "保价", comments2)
comments2=gsub(pattern = "价保", replacement = "保价", comments2)
comments2=gsub(pattern = "降价", replacement = "保价", comments2)
comments2=gsub(pattern = "赠", replacement = "赠品", comments2)


##统计在出现target的评论中，出现某个关注点的评论
####统计在出现target的评论中，出现某个关注点的评论标号
num1=grep("态度",comments2)
num2=grep("在线",comments2)
num3=grep("售后",comments2)
num4=grep("换",comments2)
num5=grep("保价",comments2)
num6=grep("赠品",comments2)
num7=grep("发票",comments2)
####统计出现target+关注点的评论数比例
n1=length(num1)/N
n2=length(num2)/N
n3=length(num3)/N
n4=length(num4)/N
n5=length(num5)/N
n6=length(num6)/N
n7=length(num7)/N
####计算所有包含这些关注点的评论占总评论的个数
length(unique(c(num1,num2,num3,num4,num5,num6,num7)))/N
####将各个比例放入一个向量中，并命名
kefu=c(n1,n2,n3,n4,n5,n6,n7)
names(kefu)=select_words
####作图
barplot(kefu,col=brewer.pal(7, 'Blues'),ylim=c(0,0.4),ylab="在客服评论中占比")


##计算每个关注点的得分
####所有包含target的评论的平均分
theratings=ratings[tnum]
####包含target+某个关注点的评论的平均分
rr1=mean(theratings[num1])
rr2=mean(theratings[num2])
rr3=mean(theratings[num3])
rr4=mean(theratings[num4])
rr5=mean(theratings[num5])
rr6=mean(theratings[num6])
rr7=mean(theratings[num7])
####将所有平均分放入一个向量，并命名
rr=mean(theratings)
ruse=c(rr1,rr2,rr3,rr4,rr5,rr6,rr7,rr)
names(ruse)=c(paste("客服+",names(kefu),sep=""),"客服")
####计算行业标准，即所有评论的平均分
themean=mean(ratings)
####作图进行对比
barplot(ruse,col=c(brewer.pal(7, 'Greens'),"dark red"),ylim=c(0,5),ylab="平均分")
abline(h=themean,lwd=2)
####最后，将需要的结果用物流的名字进行保存，以便和之后的结果进行区分
ruse_kefu=ruse
com_kefu=comments2
num_kefu=tnum
rat_kefu=theratings


############################4.电池（这部分程序的思路和物流部分相同，差别仅在于提取的关注点）
target="电池"
####在sentence中返回所有包含target的短句
aa=lapply(sentence,find_word,word=target)
####去掉list的形式
aa=unlist(aa)
####对这些短句进行分词
res=cutter[aa]
####将分词后的结果从list转换为matrix格式
res=lapply(res,as.matrix)


##寻找关注点
####将每行文本的分词结果逐一排列起来
res = as.data.frame(do.call(rbind, res), stringsAsFactors = F) 
freq = as.data.frame(table(res),stringsAsFactors = F) 
####按词频个数降序排列           
freq = freq[order(-freq[,2]),]  
####挑选前100个高频词，有时可能需要列出给多的词                               
top100 = freq[1:100,]
####提炼关注点
select_words=c("容量","续航","充电","更换","发热")


##整合相近的词汇
####提取所有包含target的评论
tnum=grep("电池",comments)
comments2=comments[tnum]
####统计个数
N=length(tnum)
####将相近的词汇进行替代，可能是相近的名词，也可能是该名词的形容词
comments2=gsub(pattern = "耐用", replacement = "容量", comments2)
comments2=gsub(pattern = "电量", replacement = "容量", comments2)
comments2=gsub(pattern = "太小", replacement = "容量", comments2)
comments2=gsub(pattern = "有点小", replacement = "容量", comments2)
comments2=gsub(pattern = "小了", replacement = "容量", comments2)
comments2=gsub(pattern = "毫安", replacement = "容量", comments2)
comments2=gsub(pattern = "耗电", replacement = "续航", comments2)
comments2=gsub(pattern = "费电", replacement = "续航", comments2)
comments2=gsub(pattern = "小时", replacement = "续航", comments2)
comments2=gsub(pattern = "半天", replacement = "续航", comments2)
comments2=gsub(pattern = "一天", replacement = "续航", comments2)
comments2=gsub(pattern = "一整天", replacement = "续航", comments2)
comments2=gsub(pattern = "没电", replacement = "续航", comments2)
comments2=gsub(pattern = "够用", replacement = "续航", comments2)
comments2=gsub(pattern = "经用", replacement = "续航", comments2)
comments2=gsub(pattern = "禁用", replacement = "续航", comments2)
comments2=gsub(pattern = "抗用", replacement = "续航", comments2)
comments2=gsub(pattern = "用电", replacement = "续航", comments2)
comments2=gsub(pattern = "时间", replacement = "续航", comments2)
comments2=gsub(pattern = "省电", replacement = "续航", comments2)
comments2=gsub(pattern = "待机", replacement = "续航", comments2)
comments2=gsub(pattern = "慢充", replacement = "充电", comments2)
comments2=gsub(pattern = "充满", replacement = "充电", comments2)
comments2=gsub(pattern = "烫", replacement = "发热", comments2)
comments2=gsub(pattern = "两块", replacement = "换", comments2)


##统计在出现target的评论中，出现某个关注点的评论
####统计在出现target的评论中，出现某个关注点的评论标号
num1=grep("容量",comments2)
num2=grep("续航",comments2)
num3=grep("充电",comments2)
num4=grep("换",comments2)
num5=grep("发热",comments2)
####计算所有包含这些关注点的评论占总评论的个数
length(unique(c(num1,num2,num3,num4,num5)))/N
####统计出现target+关注点的评论数比例
n1=length(num1)/N
n2=length(num2)/N
n3=length(num3)/N
n4=length(num4)/N
n5=length(num5)/N
####将各个比例放入一个向量中，并命名
dianchi=c(n1,n2,n3,n4,n5)
names(dianchi)=select_words
####作图
barplot(dianchi,col=brewer.pal(5, 'Blues'),ylab="在电池评论中占比",ylim=c(0,0.5))


##计算每个关注点的得分
####所有包含target的评论的平均分
theratings=ratings[tnum]
rr1=mean(theratings[num1])
rr2=mean(theratings[num2])
rr3=mean(theratings[num3])
rr4=mean(theratings[num4])
rr5=mean(theratings[num5])
rr=mean(theratings)
####将所有平均分放入一个向量，并命名
ruse=c(rr1,rr2,rr3,rr4,rr5,rr)
names(ruse)=c(paste("电池+",names(dianchi),sep=""),"电池")
####计算行业标准，即所有评论的平均分
themean=mean(ratings)
####作图进行对比
barplot(ruse,col=c(brewer.pal(5, 'Greens'),"dark red"),ylim=c(0,5),ylab="平均分")
abline(h=themean)
####最后，将需要的结果用物流的名字进行保存，以便和之后的结果进行区分
ruse_dianchi=ruse
com_dianchi=comments2
num_dianchi=tnum
rat_dianchi=theratings



##########################################5.运行（这部分程序的思路和物流部分相同，差别仅在于提取的关注点）
target="运行"
####在sentence中返回所有包含target的短句
aa=lapply(sentence,find_word,word=target)
####去掉list的形式
aa=unlist(aa)
####对这些短句进行分词
res=cutter[aa]
####将分词后的结果从list转换为matrix格式
res=lapply(res,as.matrix)


##寻找关注点
####将每行文本的分词结果逐一排列起来
res = as.data.frame(do.call(rbind, res), stringsAsFactors = F) 
freq = as.data.frame(table(res),stringsAsFactors = F) 
####按词频个数降序排列           
freq = freq[order(-freq[,2]),]  
####挑选前100个高频词，有时可能需要列出给多的词                               
top100 = freq[1:100,]
####提炼关注点
select_words=c("速度","占用内存","游戏","开机","后台")


##整合相近的词汇
####提取所有包含target的评论
tnum=grep(target,comments)
comments2=comments[tnum]
####统计个数
N=length(tnum)
####将相近的词汇进行替代，可能是相近的名词，也可能是该名词的形容词
comments2=gsub(pattern = "很快", replacement = "速度", comments2)
comments2=gsub(pattern = "挺快", replacement = "速度", comments2)
comments2=gsub(pattern = "超快", replacement = "速度", comments2)
comments2=gsub(pattern = "够快", replacement = "速度", comments2)
comments2=gsub(pattern = "慢", replacement = "速度", comments2)
comments2=gsub(pattern = "卡", replacement = "速度", comments2)
comments2=gsub(pattern = "流畅", replacement = "速度", comments2)
comments2=gsub(pattern = "顺畅", replacement = "速度", comments2)


##统计在出现target的评论中，出现某个关注点的评论
####统计在出现target的评论中，出现某个关注点的评论标号
num1=grep("速度",comments2)
num2=grep("内存",comments2)
num3=grep("游戏",comments2)
num4=grep("开机",comments2)
num5=grep("后台",comments2)
####统计出现target+关注点的评论数比例
n1=length(num1)/N
n2=length(num2)/N
n3=length(num3)/N
n4=length(num4)/N
n5=length(num5)/N
####计算所有包含这些关注点的评论占总评论的个数
length(unique(c(num1,num2,num3,num4,num5)))/N
####将各个比例放入一个向量中，并命名
yunxing=c(n1,n2,n3,n4,n5)
names(yunxing)=c("速度","占用内存","游戏","开机","后台")
####作图
barplot(yunxing,col=brewer.pal(5, 'Blues'),ylim=c(0,0.8),ylab="在运行评论中占比")


##计算每个关注点的得分
####所有包含target的评论的平均分
theratings=ratings[tnum]
####包含target+某个关注点的评论的平均分
rr1=mean(theratings[num1])
rr2=mean(theratings[num2])
rr3=mean(theratings[num3])
rr4=mean(theratings[num4])
rr5=mean(theratings[num5])
rr=mean(theratings)
####将所有平均分放入一个向量，并命名
ruse=c(rr1,rr2,rr3,rr4,rr5,rr)
names(ruse)=c(paste("运行+",names(yunxing),sep=""),"运行")
####计算行业标准，即所有评论的平均分
themean=mean(ratings)
####作图进行对比
barplot(ruse,col=c(brewer.pal(5, 'Greens'),"dark red"),ylim=c(0,5),ylab="平均分")
abline(h=themean,lwd=2)
####最后，将需要的结果用物流的名字进行保存，以便和之后的结果进行区分
ruse_yunxing=ruse
com_yunxing=comments2
num_yunxing=tnum
rat_yunxing=theratings



################################6.进行产品画像

##设置X手机的标号，N手机的标号为"3102991"。这里仅以X手机为例进行说明
theid="3553571"
##存储四个热评词的得分
total=rep(0,4)

##以物流为例，分析思路和上述相同，只是查找范围仅为该手机包含的评论中
####找出所有包含物流的评论对应的手机编号
phone_wuliu=phoneid[num_wuliu]
####进一步在里面找出该手机对应的评论编号
search=which(phone_wuliu==theid)
####找出该手机所有包含物流的评论
thecom=com_wuliu[search]
####找出该手机所有包含物流的评论，对应的评分
therat=rat_wuliu[search]
####在该手机所有包含物流的评论中查找各个关注点所在评论
num1=grep("速度",thecom)
num2=grep("服务",thecom)
num3=grep("包装",thecom)
####计算该手机在各个关注点的得分，即对应评论的平均分
rr1=mean(therat[num1])
rr2=mean(therat[num2])
rr3=mean(therat[num3])
####计算该手机所有包含物流的评论的平均分
rr=mean(therat)
####保留结果
it_wl=c(rr1,rr2,rr3)
total[1]=rr


##对于“客服”，分析思路同上，注释简略
phone_kefu=phoneid[num_kefu]
search=which(phone_kefu==theid)
thecom=com_kefu[search]
therat=rat_kefu[search]
num1=grep("态度",thecom)
num2=grep("在线",thecom)
num3=grep("售后",thecom)
num4=grep("换",thecom)
num5=grep("发热",thecom)
num6=grep("赠品",thecom)
num7=grep("发票",thecom)
rr1=mean(therat[num1])
rr2=mean(therat[num2])
rr3=mean(therat[num3])
rr4=mean(therat[num4])
rr5=mean(therat[num5])
rr6=mean(therat[num6])
rr7=mean(therat[num7])
rr=mean(therat)
it_kf=c(rr1,rr2,rr3,rr4,rr5,rr6,rr7)
total[2]=rr


##对于“电池”，分析思路同上，注释简略
phone_dianchi=phoneid[num_dianchi]
search=which(phone_dianchi==theid)
thecom=com_dianchi[search]
therat=rat_dianchi[search]
num1=grep("容量",thecom)
num2=grep("续航",thecom)
num3=grep("充电",thecom)
num4=grep("换",thecom)
num5=grep("发热",thecom)
rr1=mean(therat[num1])
rr2=mean(therat[num2])
rr3=mean(therat[num3])
rr4=mean(therat[num4])
rr5=mean(therat[num5])
it_dc=c(rr1,rr2,rr3,rr4,rr5)
total[3]=mean(therat)


##对于“运行”，分析思路同上，注释简略
phone_yunxing=phoneid[num_yunxing]
search=which(phone_yunxing==theid)
thecom=com_yunxing[search]
therat=rat_yunxing[search]
num1=grep("速度",thecom)
num2=grep("内存",thecom)
num3=grep("游戏",thecom)
num4=grep("开机",thecom)
num5=grep("后台",thecom)
rr1=mean(therat[num1])
rr2=mean(therat[num2])
rr3=mean(therat[num3])
rr4=mean(therat[num4])
rr5=mean(therat[num5])
rr=mean(therat)
it_yx=c(rr1,rr2,rr3,rr4,rr5)
total[4]=rr


##为所得结果进行重命名
names(it_wl)=c(paste("物流+",names(wuliu),sep=""))
names(it_kf)=c(paste("客服+",names(kefu),sep=""))
names(it_dc)=c(paste("电池+",names(dianchi),sep=""))
names(it_yx)=c(paste("运行+",names(yunxing),sep=""))
names(total)=c("物流","客服","电池","运行")


##显示该手机在各个关注点的得分，用于excel作图
it_wl
it_kf
it_dc
it_yx
total


##注1：如果计算结果中存在取值NaN，说明该手机的评论中并不包含该关注点，因此在该关注点的得分可以取行业均值
##注2：ruse_wuliu,ruse_kefu,ruse_dianchi,ruse_yunxing中的最后一个值表示物流、客服、电池、运行的行业均值
##注3：ruse_wuliu,ruse_kefu,ruse_dianchi,ruse_yunxing中除最后一个值之外的值，为各个关注点的行业均值