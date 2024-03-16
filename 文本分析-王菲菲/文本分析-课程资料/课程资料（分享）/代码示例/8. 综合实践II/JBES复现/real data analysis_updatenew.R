#思路：
#每个样本是一部手机

############################1.寻找热评词
rm(list=ls())
##设置路径
#setwd("E://my workfiles//project//accepted papers//review analysis//")  
##加载R包                                                                                                          
library(jiebaRD)
library(jiebaR)                                                       


##读入comments数据
com_data=read.csv("./comments_infor.csv",fileEncoding = "GBK", header = T, stringsAsFactors = F)


##对com_data中的comments进行简单的文本统计
####提取评论所在的列
comments=com_data$评论内容
####统计词数
words=nchar(comments)
####看字数的分布情况
summary(words)
####发现字数中有0，所以我们去掉字数为0的评论
########找出哪些行是0字符的
num=which(words<=2)
#num=which(words<=3)
#length(unique(comments[num]))
#unique(comments[num])
########在评论数据中去掉字数为0的行
com_data=com_data[-num,]
comments=com_data$评论内容
words=words[-num]
ratings=com_data$评论得分
phoneid=com_data$手机编号
####统计句子，我们按照“，。！？”来表明一个句子
sentence=strsplit(comments,"，|。|！|？")


##在所有review中寻找热评词
####设置停用词所在目录
stoppath="./stopwords.txt"
dictpath="./add_dict.txt"
####初始化分词器，即能分词，又可以进行词性标注，并且在分词的时候去停用词
cutter = worker('tag',bylines = TRUE,stop_word=stoppath,user = dictpath) 
res=cutter[comments]
cutter2 = worker('tag',bylines = TRUE,stop_word=stoppath,user = dictpath,symbol=T) 
res2=cutter2[comments]

##对分词结果按2照词性进行筛选
####读入选取的词性
choose_tags=read.csv("./choose_tags.csv", header = F,fileEncoding = "GBK", stringsAsFactors = F) 
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

##把所有词混在一起
text_all = lapply(res_tag,as.matrix)
tag_all = lapply(tag,as.matrix)
####将每行文本的分词结果逐一排列起来
text_all = as.data.frame(do.call(rbind, text_all), stringsAsFactors = F) 
tag_all = as.data.frame(do.call(rbind, tag_all), stringsAsFactors = F) 
####进行词频统计
freq_all = as.data.frame(table(text_all),stringsAsFactors = F) 
####按词频个数降序排列           
freq_all = freq_all[order(-freq_all[,2]),]  



###########################描述分析
#统计每个review的长度，画直方图
#words_all = freq_all[,2]
#barplot(words_all[-1])
LL = lapply(res_tag,length)
LL=unlist(LL)
hist(LL,breaks=seq(0,420,5),col="grey",xlab="review length",main="",cex.axis=0.9,cex.lab=0.9)

#统计每个词出现的频率，画直方图
freqLL = freq_all[,2]
freqLL2=freqLL[-c(1:5)]
hist(freqLL2,breaks=seq(0,10000,100),col="grey",xlab="review length",main="",cex.axis=0.9,cex.lab=0.9)
hist(log(freqLL),breaks=seq(0,11,0.5),col="grey",xlab="review length",main="",cex.axis=0.9,cex.lab=0.9)

#统计不同ratings下review的长度分布
table(ratings)
boxplot(log(LL+1)~ratings,varwidth=T,xlab="the rating score",ylab="log(review length)",col="grey")



####挑选前100个高频词制作词云图
####如何寻找后向词，在高频词中的“形容词”和“副词”                              
top100_all = freq_all[1:100,]
words1=top100_all[,1]
write.csv(top100_all,"./top100.csv",row.names=F)



###########################建模，在某个q下进行筛选
####################
#select_word1函数判断词长为1的词
select_word1=function(x,target)
{
 num1=which(x==target)  #判断x包含前向词的短句标号
 out=0
 if(length(num1)>0)
 {out=1}
 return(out)
}

#select_word2函数判断词长为2的词
select_word2=function(x,firstW,secondW)
{
 num1=which(x==firstW)  #判断x包含前向词的短句标号
 sss=x[num1+1]   #判断x包含后向词的短句标号
 out=0
 if(length(which(sss==secondW))>0)
 {out=1}
 return(out)
}

#select_word3函数判断词长为3的词
select_word3=function(x,firstW,secondW,thirdW)
{
 num1=which(x==firstW)  #判断x包含前向词的短句标号
 num2=which(x==secondW)  #判断x包含前向词的短句标号
 num3=intersect(num1+1,num2)
 sss=x[num3+1]   #判断x包含后向词的短句标号
 out=0
 if(length(which(sss==thirdW))>0)
 {out=1}
 return(out)
}

#与select_word1的区别是，前者只输出0/1，后者输出被包含的次数
select_word1_sum=function(x,target)
{
 num1=which(x==target)  #判断x包含前向词的短句标号
 out=0
 if(length(num1)>0)
 {out=length(num1)}
 return(out)
}

select_word2_sum=function(x,firstW,secondW)
{
 num1=which(x==firstW)  #判断x包含前向词的短句标号
 sss=x[num1+1]   #判断x包含后向词的短句标号
 out=0
 if(length(which(sss==secondW))>0)
 {out=length(which(sss==secondW))}
 return(out)
}

select_word3_sum=function(x,firstW,secondW,thirdW)
{
 num1=which(x==firstW)  #判断x包含前向词的短句标号
 num2=which(x==secondW)  #判断x包含前向词的短句标号
 num3=intersect(num1+1,num2)
 sss=x[num3+1]   #判断x包含后向词的短句标号
 out=0
 if(length(which(sss==thirdW))>0)
 {out=length(which(sss==thirdW))}
 return(out)
}

#计算BIC的值
BIC_hd=function(model,n,p)
{
 #model is the output from a linear regression, n is the sample size, p is the number of all x's
 #get variance
 thesigma=summary(model)$sigma
 #get model size
 size=length(coef(model))
 value=log((thesigma)^2)+n^{-1}*size*(log(n)+2*log(p))
 return(value)
}

#调用BIC进行向后筛选，这步会很慢
Backward_BIC=function(Y,mat,n,p)
{
 base=lm(Y~.,data=mat)
 #the first step
 base_bic=BIC_hd(base,n,p)
 newmat=mat
 q=dim(newmat)[2]
 if(q>1)
 {
 bic_vec=rep(0,q)
 for(ll in 1:q)
 {
   themat=newmat[,-ll]
   themat=as.data.frame(themat)
   themodel=lm(Y~.,data=themat)
   bic_vec[ll]=BIC_hd(themodel,n,p)
 }
 indi=sum((base_bic-bic_vec)<0)==q
 themodel=base
 while(indi==FALSE) #need to kick out variable
 {
   del_num=which.max(base_bic-bic_vec)
   base_bic=bic_vec[del_num]
   thenames=colnames(newmat)
   newmat=newmat[,-del_num]
   #print(thenames[del_num])
   thenames=thenames[-del_num]
   if(class(newmat)!="numeric")
   {
    colnames(newmat)=thenames
    q=dim(newmat)[2]
    bic_vec=rep(0,q)
    for(ll in 1:q)
    {
    themat=newmat[,-ll]
    themat=as.data.frame(themat)
    themodel=lm(Y~.,data=themat)
    bic_vec[ll]=BIC_hd(themodel,n,p)
    }
    #check whether to continue or not
    indi=sum((base_bic-bic_vec)<0)==q
   }
   else
   {
    q=1
    indi=TRUE
    newmat=as.data.frame(newmat)
    names(newmat)=thenames
   }
 }
 }
 res=lm(Y~.,data=newmat)
 return(res)
}



##################用前500个高频词作词典
words1=freq_all[1:500,1]
res_use=res_tag
NN=length(res_use)


####统计每部手机的平均words
avg_words=tapply(words,phoneid,mean)
avg_ratings=tapply(ratings,phoneid,mean)

####统计每部手机的评论总数
NO_comments=table(phoneid)
####找到所有的手机ID
iid=names(avg_words)
####计算手机ID的个数
I=length(iid)

Y=avg_ratings
n=length(Y)
q=3
d=65
words_curr=""
mat_curr=rep(0,NN)
freq_curr=rep(0,I)


###########################第一次比较，直接建立回归模型
 K1=length(words1)
 first_mat=matrix(0,nrow=NN,ncol=K1)
 first_freq=matrix(0,nrow=I,ncol=K1)

 for(i in 1:K1)
 {
  target=words1[i]
  num=lapply(res_use,select_word1,target=target)
  first_mat[,i]=unlist(num)
  first_freq[,i]=tapply(unlist(num),phoneid,sum)/NO_comments
 }
 colnames(first_mat)=colnames(first_freq)=words1
 res1=apply(first_freq,2,function(x){cor(x,Y)})

 ####根据结果再次筛选词典，这步也可以在最开始建立词典的时候完成，即将无意义的词放入stopwords
 num_sel_1=(order(abs(res1),decreasing=TRUE))[1:300]
 del_num=c(1,10,11,12,13,19,21,22,25:31,37,38,40,41,43:45,49,50,52:56,59:68,73:78,80:82,84:86,
88:90,94,95:100,104,106:111,113:116,118:121,125:126,128,132,139,140:141,146:147,150,153,155:160,
164,165,168:181,183:187,194,198,200:213,215:217,219:230,232:240,243:253,255:258,262,265:268,272,
276:278,279,282:289,291:294,297:298,300)
 num_sel_1=num_sel_1[-del_num]
 num_sel_1=num_sel_1[1:100]
 words11=words1[num_sel_1]
 words11[99]=paste(words11[62],words11[99])
 words11_freq=first_freq[,num_sel_1]  
 words11_mat=first_mat[,num_sel_1]
 K11=length(words11)
 colnames(words11_freq)=words11
 first_mat11=first_mat[,num_sel_1]
 first_freq11=first_freq[,num_sel_1]
 words_curr=c(words_curr,words11)
 mat_curr=cbind(mat_curr,words11_mat)
 freq_curr=cbind(freq_curr,words11_freq)
 print("K11=")
 print(K11)

#############词长为1的词组，使用Backward
  words_curr=words_curr[-1]
  mat_curr=mat_curr[,-1]
  freq_curr=freq_curr[,-1]

  ####去重
   del_num=which(duplicated(words_curr)==TRUE)
   if(length(del_num)>0)
   {
    mat_curr=mat_curr[,-del_num]
    freq_curr=freq_curr[,-del_num]
    words_curr=words_curr[-del_num]
   }
  freq_curr=as.data.frame(freq_curr)
  model_curr=lm(Y~.,data=freq_curr)
  out=summary(model_curr)  #289
  c1=length(coef(model_curr))
  c2=length(na.omit(coef(model_curr)))
  if(c1!=c2)
  {
   del_num=which(is.na(coef(model_curr))==TRUE)-1
   mat_curr=mat_curr[,-del_num]
   words_curr=words_curr[-del_num]
   freq_curr=freq_curr[,-del_num]
  }
  model_curr=lm(log(Y)~.,data=freq_curr)
  ####if use backward for model selection
  model_curr_back=Backward_BIC(log(Y),freq_curr,I,50)
  out=summary(model_curr_back)  #
  c1=length(names(coef(model_curr_back)))
  print("Backward is")
  print(c1)
  print(names(coef(model_curr_back)))

########################以下是结果
结果（1）
一共100个词
使用backward选出9个词，以下是结果：
[1] "(Intercept)" "太差"        "差"          "退"          "坏"         
 [6] "死机"        "不错"        "假货"        "欺骗"        "清晰"



 ###########################第二次比较，任意两个词的组合，看是否在文本中出现
 #前向词希望是名词，后向次希望是副词或者形容词，因此对词长为1的词进行了筛选，这步可以通过在jieba分词中显示词性来完成
 sel_before=c(4,5,17,26,29,62,69,75,76,79,83,97)
 sel_after=c(1,2,8,10,13,32,38,59,65,88,96,99,100)
 left=setdiff(1:100,c(sel_before,sel_after))
 before=c(sel_before,left)
 after=c(sel_after,left)
 K11b=length(before)
 K11a=length(after)
 second_mat=rep(0,NN)
 second_freq=rep(0,I)
 words2=rep("",2)
 for(b in 1:K11b)
 {
  i=before[b]
  firstW=words11[i]
  thenum1=which(words11_mat[,i]>0)
  useset=setdiff(after,i)
  for(s in 1:length(useset))
  {
   j=useset[s]
   secondW=words11[j]
   thenum2=which(words11_mat[,j]>0)
   thenum=intersect(thenum1,thenum2)
   theres=res_use[thenum]
   aa=lapply(theres,select_word2,firstW=firstW,secondW=secondW)
   if(length(which(unlist(aa)==1))>50)
   {
    thevec=rep(0,NN)
    thevec[thenum]=unlist(aa)
    thevec2=tapply(thevec,phoneid,sum)/NO_comments
    second_mat=cbind(second_mat,thevec)
    second_freq=cbind(second_freq,thevec2)
    words2=rbind(words2,c(firstW,secondW))
   }
  }
 }

  words2_phrase=paste(words2[,1],"+",words2[,2],sep="")
  second_mat=second_mat[,-1]
  second_freq=second_freq[,-1]
  words2=words2[-1,]
  words2_phrase=words2_phrase[-1]
  K2=length(words2_phrase)

  ####去重
  del_num=which(duplicated(words2_phrase)==TRUE)
  if(length(del_num)>0)
  {
   second_mat=second_mat[,-del_num]
   second_freq=second_freq[,-del_num]
   words2=words2[-del_num,]
   words2_phrase=words2_phrase[-del_num]
  }
  K2=length(words2_phrase)
  #colnames(second_mat)=words2_phrase
  ####SIS, calculate the correlation,选出前n-1个词
  res1=apply(second_freq,2,function(x){cor(x,Y)})
  num2=(order(abs(res1),decreasing=TRUE))
  if(length(num2)<100)
  {
   num_sel_2=num2
  }else{
   num_sel_2=num2[1:100]
  }

  K22=length(num_sel_2)
  words22=words2[num_sel_2,]
  words22_mat=second_mat22=second_mat[,num_sel_2]
  second_freq22=words22_freq=second_freq[,num_sel_2]
  words22_phrase=words2_phrase[num_sel_2]


  #去掉明显无意义的词，同样可以通过stopwords文件去掉
  del_num=c(1,2,4,7,11,14,17,19,20,21,22,23,28,30,31,32,36,40,41,50,56,57:58)
  words22=words22[-del_num,]
  words22_mat=second_mat22=second_mat22[,-del_num]
  second_freq22=words22_freq=second_freq22[,-del_num]
  words22_phrase=words22_phrase[-del_num]
  K22=length(words22_phrase)
  colnames(words22_mat)=colnames(words22_freq)=words22_phrase
  words_curr=c(words11[-c(6,87,93)],words22_phrase)
  freq_curr=cbind(words11_freq[,-c(6,87,93)],words22_freq)
  mat_curr=cbind(words11_mat[,-c(6,87,93)],words22_mat)

 #############词长为2的词组，使用Backward
  ####去重
   del_num=which(duplicated(words_curr)==TRUE)
   if(length(del_num)>0)
   {
    mat_curr=mat_curr[,-del_num]
    freq_curr=freq_curr[,-del_num]
    words_curr=words_curr[del_num]
   }
  freq_curr=as.data.frame(freq_curr)
  model_curr=lm(Y~.,data=freq_curr)
  out=summary(model_curr)  #289
  c1=length(coef(model_curr))
  c2=length(na.omit(coef(model_curr)))
  if(c1!=c2)
  {
   del_num=which(is.na(coef(model_curr))==TRUE)-1
   words_curr=words_curr[-del_num]
   freq_curr=freq_curr[,-del_num]
  }
  model_curr=lm(log(Y)~.,data=freq_curr)
  ####if use backward for model selection
  thek=dim(freq_curr)[2]
  model_curr_back=Backward_BIC(log(Y),freq_curr,I,18)
  out=summary(model_curr_back)  #
  c1=length(names(coef(model_curr_back)))
  print("Backward is")
  print(c1)
  print(names(coef(model_curr_back)))
  
######################以下是结果
结果（2）
增加71个词，共：100+71
使用backward选出12个词，以下是结果：
 [1] "(Intercept)" "太差"        "差"          "售后"        "不错"       
 [6] "假货"        "欺骗"        "显示"        "手感"        "清晰"       
[11] "`检测+坏`"   "`电池+换`"   "`物流+不错`"

  #######################################第三步
  #####first get word phrases
  third_mat=rep(0,NN)
  third_freq=rep(0,I)

  words3=rep("",3)
  for(i in 1:K22)
  {
   firstW=words22[i,1]
   secondW=words22[i,2]
   thenum1=which(words22_mat[,i]>0)
   for(j in 1:K11)
   {
    thirdW=words11[j]
    thenum2=which(words11_mat[,j]>0)
    thenum=intersect(thenum1,thenum2)
    if(length(thenum)>0)
    {
     theres=res[thenum]
     aa1=lapply(theres,select_word3,firstW=firstW,secondW=secondW,thirdW=thirdW)
     if(length(which(unlist(aa1)==1))>2)
     {
      thevec=rep(0,NN)
      thevec[thenum]=unlist(aa1)
      thevec2=tapply(thevec,phoneid,sum)/NO_comments
      third_freq=cbind(third_freq,thevec2)
      third_mat=cbind(third_mat,thevec)
      words3=rbind(words3,c(firstW,secondW,thirdW))
     }
     aa2=lapply(theres,select_word3,firstW=thirdW,secondW=firstW,thirdW=secondW)
     if(length(which(unlist(aa2)==1))>2)
     {
      thevec=rep(0,NN)
      thevec[thenum]=unlist(aa2)
      thevec2=tapply(thevec,phoneid,sum)/NO_comments
      third_freq=cbind(third_freq,thevec2)
      third_mat=cbind(third_mat,thevec)
      words3=rbind(words3,c(thirdW,firstW,secondW))
     }
    }
   }
  }
   #####then regression
   words3_phrase=paste(words3[,1],"+",words3[,2],"+",words3[,3],sep="")
   third_mat=third_mat[,-1]
   third_freq=third_freq[,-1]
   words3=words3[-1,]
   words3_phrase=words3_phrase[-1]
   K3=length(words3_phrase)
   #去掉明显无意义的词组，同样可以通过stopwords文件去掉
   del_num=c(3,4,10,14,16,26,29,30,34,40,42,43,44,47,51:52,64,73,76,78:80,86,87)
   num_sel_3=setdiff(1:K3,del_num)
   K33=length(num_sel_3)
   words33_mat=third_mat33=third_mat[,num_sel_3]
   words33_freq=third_freq33=third_freq[,num_sel_3]
   words33_phrase=words3_phrase[num_sel_3]
   colnames(words33_freq)=words33_phrase
   K33=length(words33_phrase)
   words_curr=c(words_curr,words33_phrase)
   mat_curr=cbind(mat_curr,words33_mat)
   freq_curr=cbind(freq_curr,words33_freq)
   print("K33=")
   print(K33)
  
  
  #############词长为3的词组，使用Backward进行筛选
  ####去重
   del_num=which(duplicated(words_curr)==TRUE)
   if(length(del_num)>0)
   {
    mat_curr=mat_curr[,-del_num]
    freq_curr=freq_curr[,-del_num]
    words_curr=words_curr[-del_num]
   }
  freq_curr=as.data.frame(freq_curr)
  model_curr=lm(Y~.,data=freq_curr)
  out=summary(model_curr)  #289
  c1=length(coef(model_curr))
  c2=length(na.omit(coef(model_curr)))
  if(c1!=c2)
  {
   del_num=which(is.na(coef(model_curr))==TRUE)-1
   words_curr=words_curr[-del_num]
   freq_curr=freq_curr[,-del_num]
  }
  freq_curr2=freq_curr[,-c(9,38,50,52,84,81,80,85,41,22,155)]
  model_curr=lm(log(Y)~.,data=freq_curr)
  ####if use backward for model selection
  thek=dim(freq_curr)[2]
  model_curr_back=Backward_BIC(log(Y),freq_curr2,I,60)
  out=summary(model_curr_back)  #
  c1=length(names(coef(model_curr_back)))
  print("Backward is")
  print(c1)
  print(names(coef(model_curr_back)))


####################以下是结果
结果（3）
增加69个词，共有：100+71+69
使用backward选出11个词或10个词（差和差劲可以看成同义词），以下是结果：
 [1] "(Intercept)"      "太差"             "差"              
 [4] "差劲"             "申请"             "不错"            
 [7] "主板"             "假货"             "碎"              
[10] "`物流 完美`"      "`检测+坏`"        "`退货+屏幕+划痕`"


          