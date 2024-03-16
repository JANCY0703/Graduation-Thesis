############说明：该程序用于处理评论数据，提取热评词###################


############################1.寻找热评词
rm(list=ls())
##设置路径
setwd("C:\\Users\\star\\Desktop\\案例分析")  
####根据您文件存放位置相应改变
##加载R包                                                                                            
library(jiebaRD)
library(jiebaR)                                                       


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
comments=com_data$评论内容
words=words[-num]
ratings=com_data$评论得分
phoneid=com_data$手机编号


##对全部评论进行分词
####设置停用词所在目录
stoppath="./data/stopwords_utf8.txt"
####初始化分词器，即能分词，又可以进行词性标注，并且在分词的时候去停用词
cutter = worker('tag',bylines = TRUE,stop_word=stoppath) 
####进行分词,这步会比较慢                                      
res = cutter[comments]                


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
####对res中每行的分词结果进行筛选，这步会比较慢哦
res_tag=lapply(res,select)
####提取每个词的词性
tag = lapply(res_tag,names)


##只对好评进行处理
####找出所有好评所在的下标
num_good=which(ratings>=4)
####找出好评中的分词结果
res_good=res_tag[num_good]
####将text从list转换为matrix格式
text_good = lapply(res_good,as.matrix)
####将每行文本的分词结果逐一排列起来
text_good = as.data.frame(do.call(rbind, text_good), stringsAsFactors = F) 


##只对差评进行处理
####找出所有好评所在的下标
num_bad=which(ratings<=2)
####找出好评中的分词结果
res_bad=res_tag[num_bad]
####将text从list转换为matrix格式
text_bad = lapply(res_bad,as.matrix)
####将每行文本的分词结果逐一排列起来
text_bad = as.data.frame(do.call(rbind, text_bad), stringsAsFactors = F)


##在好评的结果中，提取高频词作为关键词
####进行词频统计
freq_good = as.data.frame(table(text_good),stringsAsFactors = F) 
####按词频个数降序排列           
freq_good = freq_good[order(-freq_good[,2]),]  
####挑选前50个高频词                                
top50_good = freq_good[1:50,]
####对top50的每行进行标号
rownames(top50_good)=seq(1,50)

####自己找出前50个词中有明确含义的手机属性词，其标号分别为
######【注意！】不同版本软件跑出来的结果和顺序可能不同，我们需要找到的关键词是：
###### 速度、物流、电池、系统、性价比、质量、外观、快递、拍照、功能、运行、送货、
###### 充电、声音、包装，因此根据这些关键词，需要大家从分词结果里面选择对应的编号

sele_num_good=c(7,9,10,12,14,17,20,21,27,29,32,34,39,40,49)
####找出key_words
key_words_good=top50_good[sele_num_good,1]
####挑选前100个高频词用于词云制作                                                     
top100_good = freq_good[1:100,]                  
##输出高频词数据用于制作tagxedo词云标签                                                   
write.table(top100_good, file = "./results/Top100_good_utf8.txt", quote = F, row.names = F, col.names = F)


##在差评的结果中，提取高频词绘制词云
####进行词频统计
freq_bad = as.data.frame(table(text_bad),stringsAsFactors = F) 
####按词频个数降序排列           
freq_bad = freq_bad[order(-freq_bad[,2]),]  
####挑选前50个高频词                                
top50_bad = freq_bad[1:50,]
####对top50的每行进行标号
rownames(top50_bad)=seq(1,50)

####自己找出前50个词中有明确含义的手机属性词，其标号分别为
######【注意！】不同版本软件跑出来的结果和顺序可能不同，我们需要找到的关键词是：
###### 客服、屏幕、售后、电池、质量、充电、耳机、系统、信号、声音、快递、发票、开机、软件
###### 因此根据这些关键词，需要大家从分词结果里面选择对应的编号

sele_num_bad=c(2,4,7,8,12,13,20,23,24,27,28,37,40,44)
####找出key_words
key_words_bad=top50_bad[sele_num_bad,1]
####挑选前100个高频词用于词云制作                                                     
top100_bad = freq_bad[1:100,] 
##输出高频词数据用于制作tagxedo词云标签                                                   
write.table(top100_bad, file = "./results/Top100_bad.txt", quote = F, row.names = F, col.names = F)

##把好评词和差评词合并，然后分为“手机”和“服务”两个方面
key_words=unique(c(key_words_good,key_words_bad)) #23个
#### 找出服务特征方面的词所在下标
#### 速度、物流、送货、快递、包装、客服、售后、发票
num=c(1,2,8,12,15,16,18,21)
####提取服务特征方面的热评词
key_service=key_words[num]
####提取手机特征方面的热评词
key_phone=key_words[-num]

############################2.对热评词进行T检验

##看服务类热评词
####计算所有评论总数
N=length(ratings)
####计算服务类热评词的个数
K1=length(key_service)
####初始化结果矩阵，判断每个热评词是否在每条评论中出现，如果出现记为1，不出现记为0
key_mat1=matrix(0,nrow=N,ncol=K1)
####初始化结果矩阵，保留T检验的p值
pvalue_service=rep(0,K1)
####对每个服务类热评词进行循环
for(k in 1:K1)
{
 theword=key_service[k]   #找到该热评词
 num=grep(theword,comments)  #判断出现该热评词的评论标号
 x=ratings[num]   #将出现该热评词的评论的得分记为x组
 y=ratings[-num]  #将没有出现该热评词的评论的得分记为y组
 test=t.test(x,y)  #检验x组和y组的评分是否有显著差异
 pvalue_service[k]=test$p.value   #保留该检验的T值
 key_mat1[num,k]=1  #记录那些评论包括该热评词
}
####以0.05为标准判断热评词的显著性
num1=which(pvalue_service<0.05)
####保留显著的热评词
key_ser_sig=key_service[num1]
####为其命名
colnames(key_mat1)=key_service
####只在key_mat1中保留显著的热评词
key_mat1=key_mat1[,num1]


##看手机类热评词
####计算手机类热评词的个数
K2=length(key_phone)
####初始化结果矩阵，判断每个热评词是否在每条评论中出现，如果出现记为1，不出现记为0
key_mat2=matrix(0,nrow=N,ncol=K2)
####初始化结果矩阵，保留T检验的p值
pvalue_phone=rep(0,K2)
####对每个服务类热评词进行循环
for(k in 1:K2)
{
 theword=key_phone[k]   #找到该热评词
 num=grep(theword,comments)	#判断出现该热评词的评论标号
 x=ratings[num]	#将出现该热评词的评论的得分记为x组
 y=ratings[-num]	#将没有出现该热评词的评论的得分记为y组
 test=t.test(x,y)	#检验x组和y组的评分是否有显著差异
 pvalue_phone[k]=test$p.value	  #保留该检验的T值
 key_mat2[num,k]=1	#记录那些评论包括该热评词
}
####以0.05为标准判断热评词的显著性
num2=which(pvalue_phone<0.05)
####保留显著的热评词
key_pho_sig=key_phone[num2]  
####为其命名
colnames(key_mat2)=key_phone
####只在key_mat2中保留显著的热评词
key_mat2=key_mat2[,num2]



#####################3.把基于评论的结果整合到每部手机上

##统计每款手机出现的评论中，平均words，平均Lsen
####统计每部手机的平均words
avg_words=tapply(words,phoneid,mean)
####统计每部手机的评论总数
NO_comments=table(phoneid)
####找到所有的手机ID
iid=names(avg_words)
####计算手机ID的个数
I=length(iid)


##统计每部手机中出现每个热评词的频率
####初始化结果矩阵，每行为一部手机，每列为这部手机出现该热评词的频率
service_freq=matrix(0,nrow=I,ncol=length(num1))
phone_freq=matrix(0,nrow=I,ncol=length(num2))
####为每个服务类热评词计算频率
for(k in 1:length(num1))
{
 service_freq[,k]=tapply(key_mat1[,k],phoneid,sum)/NO_comments  #统计每部手机中，第k个热评词的频率
}
####为每个手机类热评词计算频率
for(k in 1:length(num2))
{
 phone_freq[,k]=tapply(key_mat2[,k],phoneid,sum)/NO_comments	#统计每部手机中，第k个热评词的频率
}
####为两个结果矩阵进行命名
colnames(service_freq)=key_ser_sig
colnames(phone_freq)=key_pho_sig

##将生成的衍生指标放在一个数据里
com_reg=cbind(as.numeric(iid),avg_words,service_freq,phone_freq)
##将数据输出
write.csv(com_reg, file = "./results/com_reg.csv", quote = F, row.names = F)


