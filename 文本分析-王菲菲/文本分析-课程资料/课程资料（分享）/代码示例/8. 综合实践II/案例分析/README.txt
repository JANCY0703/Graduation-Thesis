运行顺序为：
1.comments.R
2.phone.R
3.analysis.R
4.further explore.R

注意！
【1】请先将编码格式调整为GB18030后再打开文件
【2】请使用/data中 后缀为_utf8的数据


data文件夹里面是用于分析的数据，各个文件的含义如下：
comments_infor.csv：用户评论数据
phone_infor.csv:手机数据，其中几个定性变量的取值含义如下
【指纹识别】0-不支持；1-支持；2-其他
【GPS】0-不支持；1-支持；2-其他
【促销信息】0-无，1-有
choose_tags.csv:用于筛选的词性
stopwords.txt：用于剔除的停用词


results文件夹是运行过程中输出结果所在目录