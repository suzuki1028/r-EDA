

##ツールを使用する前に


#R version 3.3.2 以上の使用を推奨します.
#本ツールを使用していただく際に入力を必要とする部分があります.
#引用符内にメッセージがあるのでそちらを参照してください.
#引用符内への入力は原則として引用符を取ってお願いします.
#データフレームやベクトル, 関数の引数に複数個入力するものがある場合はカンマで区切ってください(例：(dat$a,dat$b)).


#準備パート-------------------------------------------------------------


##パッケージの準備


#-----Rのバージョンの確認-----

R.version.string


#-----パッケージのインストール-----

install.packages("rio")
#様々な形式のデータファイルの読み込み・書き出しを可能にするパッケージ

install.packages("plyr")
#データの分割,適用,結合のためのパッケージ

install.packages("stargazer")
#整ったフォーマットで回帰分析の結果表のLaTeXコード,HTMLコード,ASCIIテキストを作成するパッケージ


#-----パッケージの呼び出し-----

library(rio)
library(plyr)
library(stargazer)
library(nnet)
#順伝播型ニューラルネットワーク及び多項対数線形モデルのためのパッケージ


#パッケージの確認

search()




#----------------------------------------------------------


##データの読み込み


#-----作業ディレクトリの確認-----

getwd()

#作業ディレクトリはデータがある場所に設定してください.


#-----作業ディレクトリの変更-----

setwd("変更後のディレクトリ名を入力")


#-----データの読み込み-----

dat <- import("読み込みたいデータ名を入力", stringsAsFactors = TRUE)

#これ以降データの名前は"dat"で統一しています.




##データセットの作成


#-----データ内容の確認-----

#データの表示

dat

#正しくデータが読み込まれているか,どの変数が分析に必要か確認してください.


#データの構造をコンパクトに表示

str(dat)

#行数と列数,そして変数の型が表示されているので確認してください.
#変数の型は,
#numeric:実数,integer:整数,complex:複素数,factor:順序なし因子,ordered:順序付き因子
#となります.


#データ内容の要約

summary(dat)

#各変数の最小値・最大値,中央値,平均値,四分位数などが表示される.




##欠損値の処理


#-----欠損値の変換-----

#Rでは欠損値に対してNAという論理型が存在するので,
#データ内の欠損値表示が異なる場合(99や.など)は変換してください.
#ピリオド等の記号や文字列の場合は引用符つきでお願いします.


missingFixer <- function(dataframe, na.values) {
  
  n <- length(na.values)
  
  for(i in 1:n){
    
    dataframe[dataframe == na.values[i]] <- NA
    
  }
  
  return(dataframe)

}

missingValues <- c("欠損値を入力")

dat <- missingFixer(dat, missingValues)

dat


#-----欠損値を含むサンプルの削除(リストワイズ法)-----

dat <- na.omit(dat)

dat




##変数の型の変換


#-----連続変数からカテゴリ変数への型変換-----

ntof <- data.frame("型変換したい変数を入力")

ntof <- lapply(ntof,as.factor)

str(ntof)


#-----カテゴリ変数から連続変数への型変換-----

fton <- data.frame("型変換したい変数を入力")

fton <- lapply(fton,as.character)

fton <- lapply(fton,as.numeric)

str(fton)




##変数の加工


#-----変数の合成(複数の連続変数の平均をとる変数の作成)-----

"新しい変数名を入力" <- (dat$"変数名を入力" + dat$"変数名を入力")/2

#例：new <- (dat$a + dat$b + dat$c)/3


#-----整数値をとる変数を反転-----

reverser <- function(dataframe){
  
  n <- length(dataframe)
  
  for(i in 1:n){
    
    dataframe[,i] <- (max(dataframe[,i], na.rm = TRUE) 
                      + min(dataframe[,i], na.rm = TRUE) - dataframe[,i])
    
  }
  
  return(dataframe)

}

rev <- data.frame("変数名を入力")

reverser(rev)


#-----連続変数をカテゴリ変数として作成-----

dat$"変更したい変数名を引用符なしで入力" <- cut(dat$"変更したい変数名を入力",breaks = c("境界となる数値を入力"),
                                            labels = c("新しいカテゴリ名を引用符付きで入力"))

#例：境界(区切り)となる数値をc(0,3,6,10), 新しいカテゴリ名をc("A","B","C")とした場合,
#    A(0<=A<=3),B(3<B<=6),C(6<C<=10)でカテゴリ化される.


#-----カテゴリ内容の変更(カテゴリ変数を別のカテゴリ変数として作成)-----

oldValues <- c("変更前のカテゴリ名を引用符付きで入力")

newValues <- factor(c("変更後のカテゴリ名を引用符付きで入力"))

dat$"変数名を入力" <- newValues[ match(dat$"変数名を入力", oldValues)]

#例：変更前のカテゴリ名をc("A-c","D-F","G-I"), 変更後のカテゴリ名をc("A-F","A-F","G-I")とした場合,
#    "A-F","G-I"の2カテゴリにまとめ直される.


#-----カテゴリ内容の確認-----

#カテゴリ変数の項数が2以下の変数名を表示
#関数作動中に""という警告が出たら使用してください.

levelsChecker <- function(dataframe){
  
  n <- length(names(dataframe))
  
  for(i in 1:n){
    
    level <- length(levels(factor(dataframe[,names(dataframe)[i]])))
    
    if(level < 2 ){
      
      print(names(dataframe)[i])
      
    }
  }
}

levelsChecker(dat)


#カテゴリ変数内の使用しないカテゴリを削除

dat <- droplevels(dat)

#データ処理後にもう使われないカテゴリが出来たときに使用してください.




##データフレームの作成


#-----データフレームの作成-----

dat0 <- data.frame("データセットに含めたい変数名を入力")

dat0

#これ以降データの名前は"dat0"で統一しています.


#-----変数の削除-----

deleterows <- c("変数の列番号を入力")

dat0 <- dat[, -deleterows]




##変数やカテゴリのラベルの変更


#-----変数名の変更-----

#データセット内の全ての変数をリネームする

names(dat0) <- c("新しい変数名を全て引用符付きで入力")

#変数の数と同じだけ入力してください(変数名を変更しないものも入力してください).


#一部の変数をリネームする

names(dat0)[names(dat0) == "変更前の名前を引用符付きで入力"] <- c("変更後の名前を引用符付きで入力")

#位置番号で列を指定することもできます.

names(dat0)["位置番号を入力"] <- "変更後の名前を入力"


#-----カテゴリ名の変更-----

dat0$"変更したい変数名を入力" <- revalue(dat0$"変更したい変数名を入力"
                                    , c("変更前のカテゴリ名を引用符付きで入力" = "変更後のカテゴリ名を引用符付きで入力"))


#-----変数名のみでの変数の呼び出しとその解除-----

attach(dat0)

detach(dat0)

#データの内容を更新した場合はdetach()してからattach()する.





#解析ツール----------------------------------------------------------

##解析ツールを使用する際の注意


#デフォルトではstargazerの出力はConsoleにきれいに表示されるようになっています(type="text").
#ただし,type="html"でHTMLの形で,typeについて何も入力しなければLaTeXのコードの形で
#出力されるので必要に応じて変更してください.

#分析においてカテゴリ変数は全て,最初のカテゴリが基準カテゴリとして扱われます.

#----------------------------------------------------------

##以下,解析ツールのコードです.



variables <- c("変数名を引用符付きで入力")    #被説明変数として扱わない変数の指定




analyzer <- function(vector, dataframe){
  
  n <- length(dataframe)    #data.frame内の変数の個数を取得
  
  no.col <- 1:n    #ローテーション用ベクトル
  
  n1 <- n-1    #ループエンド用
  
  stargazer(dataframe, type = "text")    #記述統計をstargazerで表示
  
  for(i in 0:n1){
    
    k = i+1
    
    length0 = length(levels(dataframe[,k]))　  #(factor型の)変数の項数を取得
    
    x <- no.col[1]
    
    no.col[1] <- no.col[i+1]
    
    no.col[i+1] <- x    #ベクトルのrotate
    
    dataframe0 <- dataframe[,no.col]    #データフレーム列のrotate
    
    variable = names(dataframe0)
    
    xnam = variable[2:n]    #説明変数のラベル
    
    ynam = variable[1]    #被説明変数のラベル
    
    if(is.na(match(ynam,vector))){    #vector中に入力された変数の除外
      
      model = as.formula(paste(paste(ynam,"~") , paste(xnam, collapse= "+")))    #回帰式formulaの記述
      
      if (is.factor(dataframe[,k])){    #変数の型がfactor型かどうかで条件分岐
        
        if (length0 == 2){    #項数が二項かどうかで条件分岐
          
          out0 = glm(model, data = dataframe0, binomial)    #二項ロジスティック回帰
          
          out1 <- step(out0, trace = 0)    #AIC基準でモデル選択
          
          print(paste(paste("Outcome is", ynam) 
                      , paste("(Applying glm)")))    #被説明変数と適用モデル関数の表示
          
        } else {    #項数が三項以上の場合
          
          out1 = multinom(model, data = dataframe0, MaxNWts = 10000)    #多項ロジスティック回帰
          
          print(paste(paste("Outcome is", ynam) 
                      , paste("(Applying multinom)")))
          
        }
        
      } else {
        
        out0 = lm(model, data = dataframe0)    #線形回帰
        
        out1 <- step(out0, trace = 0)    #AIC基準でモデル選択
        
        print(paste(paste("Outcome is", ynam) 
                    , paste("(Applying lm)")))
        
      }
      
      stargazer(out1,type = "text", single.row = TRUE)    #分析結果をstargazerで表示
      
    } else {}
    
  }
  
}


analyzer(variables,dat0)

