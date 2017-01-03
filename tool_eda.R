

##�c�[�����g�p����O��


#R version 3.3.2 �ȏ�̎g�p�𐄏����܂�.
#�{�c�[�����g�p���Ă��������ۂɓ��͂�K�v�Ƃ��镔��������܂�.
#���p�����Ƀ��b�Z�[�W������̂ł�������Q�Ƃ��Ă�������.
#���p�����ւ̓��͂͌����Ƃ��Ĉ��p��������Ă��肢���܂�.
#�f�[�^�t���[����x�N�g��, �֐��̈����ɕ������͂�����̂�����ꍇ�̓J���}�ŋ�؂��Ă�������(��F(dat$a,dat$b)).


#�����p�[�g-------------------------------------------------------------


##�p�b�P�[�W�̏���


#-----R�̃o�[�W�����̊m�F-----

R.version.string


#-----�p�b�P�[�W�̃C���X�g�[��-----

install.packages("rio")
#�l�X�Ȍ`���̃f�[�^�t�@�C���̓ǂݍ��݁E�����o�����\�ɂ���p�b�P�[�W

install.packages("plyr")
#�f�[�^�̕���,�K�p,�����̂��߂̃p�b�P�[�W

install.packages("stargazer")
#�������t�H�[�}�b�g�ŉ�A���͂̌��ʕ\��LaTeX�R�[�h,HTML�R�[�h,ASCII�e�L�X�g���쐬����p�b�P�[�W


#-----�p�b�P�[�W�̌Ăяo��-----

library(rio)
library(plyr)
library(stargazer)
library(nnet)
#���`�d�^�j���[�����l�b�g���[�N�y�ё����ΐ����`���f���̂��߂̃p�b�P�[�W


#�p�b�P�[�W�̊m�F

search()




#----------------------------------------------------------


##�f�[�^�̓ǂݍ���


#-----��ƃf�B���N�g���̊m�F-----

getwd()

#��ƃf�B���N�g���̓f�[�^������ꏊ�ɐݒ肵�Ă�������.


#-----��ƃf�B���N�g���̕ύX-----

setwd("�ύX��̃f�B���N�g���������")


#-----�f�[�^�̓ǂݍ���-----

dat <- import("�ǂݍ��݂����f�[�^�������", stringsAsFactors = TRUE)

#����ȍ~�f�[�^�̖��O��"dat"�œ��ꂵ�Ă��܂�.




##�f�[�^�Z�b�g�̍쐬


#-----�f�[�^���e�̊m�F-----

#�f�[�^�̕\��

dat

#�������f�[�^���ǂݍ��܂�Ă��邩,�ǂ̕ϐ������͂ɕK�v���m�F���Ă�������.


#�f�[�^�̍\�����R���p�N�g�ɕ\��

str(dat)

#�s���Ɨ�,�����ĕϐ��̌^���\������Ă���̂Ŋm�F���Ă�������.
#�ϐ��̌^��,
#numeric:����,integer:����,complex:���f��,factor:�����Ȃ����q,ordered:�����t�����q
#�ƂȂ�܂�.


#�f�[�^���e�̗v��

summary(dat)

#�e�ϐ��̍ŏ��l�E�ő�l,�����l,���ϒl,�l���ʐ��Ȃǂ��\�������.




##�����l�̏���


#-----�����l�̕ϊ�-----

#R�ł͌����l�ɑ΂���NA�Ƃ����_���^�����݂���̂�,
#�f�[�^���̌����l�\�����قȂ�ꍇ(99��.�Ȃ�)�͕ϊ����Ă�������.
#�s���I�h���̋L���╶����̏ꍇ�͈��p�����ł��肢���܂�.


missingFixer <- function(dataframe, na.values) {
  
  n <- length(na.values)
  
  for(i in 1:n){
    
    dataframe[dataframe == na.values[i]] <- NA
    
  }
  
  return(dataframe)

}

missingValues <- c("�����l�����")

dat <- missingFixer(dat, missingValues)

dat


#-----�����l���܂ރT���v���̍폜(���X�g���C�Y�@)-----

dat <- na.omit(dat)

dat




##�ϐ��̌^�̕ϊ�


#-----�A���ϐ�����J�e�S���ϐ��ւ̌^�ϊ�-----

ntof <- data.frame("�^�ϊ��������ϐ������")

ntof <- lapply(ntof,as.factor)

str(ntof)


#-----�J�e�S���ϐ�����A���ϐ��ւ̌^�ϊ�-----

fton <- data.frame("�^�ϊ��������ϐ������")

fton <- lapply(fton,as.character)

fton <- lapply(fton,as.numeric)

str(fton)




##�ϐ��̉��H


#-----�ϐ��̍���(�����̘A���ϐ��̕��ς��Ƃ�ϐ��̍쐬)-----

"�V�����ϐ��������" <- (dat$"�ϐ��������" + dat$"�ϐ��������")/2

#��Fnew <- (dat$a + dat$b + dat$c)/3


#-----�����l���Ƃ�ϐ��𔽓]-----

reverser <- function(dataframe){
  
  n <- length(dataframe)
  
  for(i in 1:n){
    
    dataframe[,i] <- (max(dataframe[,i], na.rm = TRUE) 
                      + min(dataframe[,i], na.rm = TRUE) - dataframe[,i])
    
  }
  
  return(dataframe)

}

rev <- data.frame("�ϐ��������")

reverser(rev)


#-----�A���ϐ����J�e�S���ϐ��Ƃ��č쐬-----

dat$"�ύX�������ϐ��������p���Ȃ��œ���" <- cut(dat$"�ύX�������ϐ��������",breaks = c("���E�ƂȂ鐔�l�����"),
                                            labels = c("�V�����J�e�S���������p���t���œ���"))

#��F���E(��؂�)�ƂȂ鐔�l��c(0,3,6,10), �V�����J�e�S������c("A","B","C")�Ƃ����ꍇ,
#    A(0<=A<=3),B(3<B<=6),C(6<C<=10)�ŃJ�e�S���������.


#-----�J�e�S�����e�̕ύX(�J�e�S���ϐ���ʂ̃J�e�S���ϐ��Ƃ��č쐬)-----

oldValues <- c("�ύX�O�̃J�e�S���������p���t���œ���")

newValues <- factor(c("�ύX��̃J�e�S���������p���t���œ���"))

dat$"�ϐ��������" <- newValues[ match(dat$"�ϐ��������", oldValues)]

#��F�ύX�O�̃J�e�S������c("A-c","D-F","G-I"), �ύX��̃J�e�S������c("A-F","A-F","G-I")�Ƃ����ꍇ,
#    "A-F","G-I"��2�J�e�S���ɂ܂Ƃߒ������.


#-----�J�e�S�����e�̊m�F-----

#�J�e�S���ϐ��̍�����2�ȉ��̕ϐ�����\��
#�֐��쓮����""�Ƃ����x�����o����g�p���Ă�������.

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


#�J�e�S���ϐ����̎g�p���Ȃ��J�e�S�����폜

dat <- droplevels(dat)

#�f�[�^������ɂ����g���Ȃ��J�e�S�����o�����Ƃ��Ɏg�p���Ă�������.




##�f�[�^�t���[���̍쐬


#-----�f�[�^�t���[���̍쐬-----

dat0 <- data.frame("�f�[�^�Z�b�g�Ɋ܂߂����ϐ��������")

dat0


#-----�ϐ��̍폜-----

deleterows <- c("�ϐ��̗�ԍ������")

dat0 <- dat[, -deleterows]




##�ϐ���J�e�S���̃��x���̕ύX


#-----�ϐ����̕ύX-----

#�f�[�^�Z�b�g���̑S�Ă̕ϐ������l�[������

names(dat0) <- c("�V�����ϐ�����S�Ĉ��p���t���œ���")

#�ϐ��̐��Ɠ����������͂��Ă�������(�ϐ�����ύX���Ȃ����̂����͂��Ă�������).


#�ꕔ�̕ϐ������l�[������

names(dat0)[names(dat0) == "�ύX�O�̖��O�����p���t���œ���"] <- c("�ύX��̖��O�����p���t���œ���")

#�ʒu�ԍ��ŗ���w�肷�邱�Ƃ��ł��܂�.

names(dat0)["�ʒu�ԍ������"] <- "�ύX��̖��O�����"


#-----�J�e�S�����̕ύX-----

dat0$"�ύX�������ϐ��������" <- revalue(dat0$"�ύX�������ϐ��������"
                                    , c("�ύX�O�̃J�e�S���������p���t���œ���" = "�ύX��̃J�e�S���������p���t���œ���"))


#-----�ϐ����݂̂ł̕ϐ��̌Ăяo���Ƃ��̉���-----

attach(dat0)

detach(dat0)

#�f�[�^�̓��e���X�V�����ꍇ��detach()���Ă���attach()����.





#��̓c�[��----------------------------------------------------------

##��̓c�[�����g�p����ۂ̒���


#�f�t�H���g�ł�stargazer�̏o�͂�Console�ɂ��ꂢ�ɕ\�������悤�ɂȂ��Ă��܂�(type="text").
#������,type="html"��HTML�̌`��,type�ɂ��ĉ������͂��Ȃ����LaTeX�̃R�[�h�̌`��
#�o�͂����̂ŕK�v�ɉ����ĕύX���Ă�������.

#���͂ɂ����ăJ�e�S���ϐ��͑S��,�ŏ��̃J�e�S������J�e�S���Ƃ��Ĉ����܂�.

#----------------------------------------------------------

##�ȉ�,��̓c�[���̃R�[�h�ł�.



variables <- c("�ϐ��������p���t���œ���")    #������ϐ��Ƃ��Ĉ���Ȃ��ϐ��̎w��




analyzer <- function(vector, dataframe){
  
  n <- length(dataframe)    #data.frame���̕ϐ��̌����擾
  
  no.col <- 1:n    #���[�e�[�V�����p�x�N�g��
  
  n1 <- n-1    #���[�v�G���h�p
  
  stargazer(dataframe, type = "text")    #�L�q���v��stargazer�ŕ\��
  
  for(i in 0:n1){
    
    k = i+1
    
    length0 = length(levels(dataframe[,k]))�@  #(factor�^��)�ϐ��̍������擾
    
    x <- no.col[1]
    
    no.col[1] <- no.col[i+1]
    
    no.col[i+1] <- x    #�x�N�g����rotate
    
    dataframe0 <- dataframe[,no.col]    #�f�[�^�t���[�����rotate
    
    variable = names(dataframe0)
    
    xnam = variable[2:n]    #�����ϐ��̃��x��
    
    ynam = variable[1]    #������ϐ��̃��x��
    
    if(is.na(match(ynam,vector))){    #vector���ɓ��͂��ꂽ�ϐ��̏��O
      
      model = as.formula(paste(paste(ynam,"~") , paste(xnam, collapse= "+")))    #��A��formula�̋L�q
      
      if (is.factor(dataframe[,k])){    #�ϐ��̌^��factor�^���ǂ����ŏ�������
        
        if (length0 == 2){    #�������񍀂��ǂ����ŏ�������
          
          out0 = glm(model, data = dataframe0, binomial)    #�񍀃��W�X�e�B�b�N��A
          
          out1 <- step(out0, trace = 0)    #AIC��Ń��f���I��
          
          print(paste(paste("Outcome is", ynam) 
                      , paste("(Applying glm)")))    #������ϐ��ƓK�p���f���֐��̕\��
          
        } else {    #�������O���ȏ�̏ꍇ
          
          out1 = multinom(model, data = dataframe0, MaxNWts = 10000)    #�������W�X�e�B�b�N��A
          
          print(paste(paste("Outcome is", ynam) 
                      , paste("(Applying multinom)")))
          
        }
        
      } else {
        
        out0 = lm(model, data = dataframe0)    #���`��A
        
        out1 <- step(out0, trace = 0)    #AIC��Ń��f���I��
        
        print(paste(paste("Outcome is", ynam) 
                    , paste("(Applying lm)")))
        
      }
      
      stargazer(out1,type = "text", single.row = TRUE)    #���͌��ʂ�stargazer�ŕ\��
      
    } else {}
    
  }
  
}


analyzer(variables,dat0)
