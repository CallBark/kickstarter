Testing<-read.csv("https://raw.githubusercontent.com/Dereklip/GSBA576/main/Testing_v2.csv")
Training<-read.csv("https://raw.githubusercontent.com/Dereklip/GSBA576/main/Training_v2.csv")
Validation<-read.csv("https://raw.githubusercontent.com/Dereklip/GSBA576/main/Validation_v2.csv")

M1<-lm(USD.Pledged~ . -Category..parent.only., Training)
summary(M1)

M2<-lm(USD.Pledged~Category..parent.only., Training)
summary(M2)

M3<-lm(USD.Pledged~Category, Training)
summary(M3)

#creates dummy variables for categories
Training$TTGames[Training$Category=='games tabletop games']<-1
Training$TTGames[Training$Category!='games tabletop games']<-0

Training$Tech[Training$Category=='technology']<-1
Training$Tech[Training$Category!='technology']<-0

Training$DIYE[Training$Category=='technology diy electronics']<-1
Training$DIYE[Training$Category!='technology diy electronics']<-0

Training$Camera[Training$Category=='technology camera equipment']<-1
Training$Camera[Training$Category!='technology camera equipment']<-0

Training$Gadget[Training$Category=='technology gadgets']<-1
Training$Gadget[Training$Category!='technology gadgets']<-0

Training$Hardware[Training$Category=='technology hardware']<-1
Training$Hardware[Training$Category!='technology hardware']<-0

Training$Sound[Training$Category=='technology sound']<-1
Training$Sound[Training$Category!='technology sound']<-0

#creates dummy variables for categories
Testing$TTGames[Testing$Category=='games tabletop games']<-1
Testing$TTGames[Testing$Category!='games tabletop games']<-0

Testing$Tech[Testing$Category=='technology']<-1
Testing$Tech[Testing$Category!='technology']<-0

Testing$DIYE[Testing$Category=='technology diy electronics']<-1
Testing$DIYE[Testing$Category!='technology diy electronics']<-0

Testing$Camera[Testing$Category=='technology camera equipment']<-1
Testing$Camera[Testing$Category!='technology camera equipment']<-0

Testing$Gadget[Testing$Category=='technology gadgets']<-1
Testing$Gadget[Testing$Category!='technology gadgets']<-0

Testing$Hardware[Testing$Category=='technology hardware']<-1
Testing$Hardware[Testing$Category!='technology hardware']<-0

Testing$Sound[Testing$Category=='technology sound']<-1
Testing$Sound[Testing$Category!='technology sound']<-0

#creates dummy variables for categories
Validation$TTGames[Validation$Category=='games tabletop games']<-1
Validation$TTGames[Validation$Category!='games tabletop games']<-0

Validation$Tech[Validation$Category=='technology']<-1
Validation$Tech[Validation$Category!='technology']<-0

Validation$DIYE[Validation$Category=='technology diy electronics']<-1
Validation$DIYE[Validation$Category!='technology diy electronics']<-0

Validation$Camera[Validation$Category=='technology camera equipment']<-1
Validation$Camera[Validation$Category!='technology camera equipment']<-0

Validation$Gadget[Validation$Category=='technology gadgets']<-1
Validation$Gadget[Validation$Category!='technology gadgets']<-0

Validation$Hardware[Validation$Category=='technology hardware']<-1
Validation$Hardware[Validation$Category!='technology hardware']<-0

Validation$Sound[Validation$Category=='technology sound']<-1
Validation$Sound[Validation$Category!='technology sound']<-0

M4<-lm(USD.Pledged~TTGames+Tech+DIYE+Camera+Gadget+Hardware+Staff.Pick+Spotlight, Training)
summary(M4)

M5<-lm(USD.Pledged~Backers.Count+TTGames+Tech+DIYE+Camera+Hardware, Training)
summary(M5)

M6<-lm(USD.Pledged~Backers.Count, Training)
summary(M6)

#Testing
predictions<-predict(M5, Testing)

#Root Mean Square Prediction Error out of sample
RMSE_OUT<-sqrt(sum((predictions-Testing$USD.Pledged)^2)/length(Testing$USD.Pledged))
RMSE_OUT

##Logistic Regression 1
Mlog1<-glm(State ~ Category..parent.only.+Backers.Count, data = Training, family = "binomial")
summary(Mlog1)

#displays summary analysis of confusion matrix in-sample
confusionMatrix(table(predict(Mlog1, Training, type="response") >= 0.5, Training$State == 1), positive = 'TRUE')

#builds the confusion matrix to look at accuracy on testing data out-of-sample
confusionMatrix(table(predict(Mlog1, Testing, type="response") >= 0.5, Testing$State == 1), positive = 'TRUE')

#creates dummy variables in Training for parent categories
Training$Techparent[Training$Category..parent.only.=='technology']<-1
Training$Techparent[Training$Category..parent.only.!='technology']<-0

Training$Designparent[Training$Category..parent.only.=='design']<-1
Training$Designparent[Training$Category..parent.only.!='design']<-0

Training$Fashionparent[Training$Category..parent.only.=='fashion']<-1
Training$Fashionparent[Training$Category..parent.only.!='fashion']<-0

Training$Filmparent[Training$Category..parent.only.=='film & video']<-1
Training$Filmparent[Training$Category..parent.only.!='film & video']<-0

Training$Foodparent[Training$Category..parent.only.=='food']<-1
Training$Foodparent[Training$Category..parent.only.!='food']<-0

Training$Gamesparent[Training$Category..parent.only.=='games']<-1
Training$Gamesparent[Training$Category..parent.only.!='games']<-0

Training$Journalismparent[Training$Category..parent.only.=='journalism']<-1
Training$Journalismparent[Training$Category..parent.only.!='journalism']<-0

Training$Musicparent[Training$Category..parent.only.=='music']<-1
Training$Musicparent[Training$Category..parent.only.!='music']<-0

Training$Photographyparent[Training$Category..parent.only.=='photography']<-1
Training$Photographyparent[Training$Category..parent.only.!='photography']<-0

Training$Publishingparent[Training$Category..parent.only.=='publishing']<-1
Training$Publishingparent[Training$Category..parent.only.!='publishing']<-0

#creates dummy variables in Testing for parent categories
Testing$Techparent[Testing$Category..parent.only.=='technology']<-1
Testing$Techparent[Testing$Category..parent.only.!='technology']<-0

Testing$Designparent[Testing$Category..parent.only.=='design']<-1
Testing$Designparent[Testing$Category..parent.only.!='design']<-0

Testing$Fashionparent[Testing$Category..parent.only.=='fashion']<-1
Testing$Fashionparent[Testing$Category..parent.only.!='fashion']<-0

Testing$Filmparent[Testing$Category..parent.only.=='film & video']<-1
Testing$Filmparent[Testing$Category..parent.only.!='film & video']<-0

Testing$Foodparent[Testing$Category..parent.only.=='food']<-1
Testing$Foodparent[Testing$Category..parent.only.!='food']<-0

Testing$Gamesparent[Testing$Category..parent.only.=='games']<-1
Testing$Gamesparent[Testing$Category..parent.only.!='games']<-0

Testing$Journalismparent[Testing$Category..parent.only.=='journalism']<-1
Testing$Journalismparent[Testing$Category..parent.only.!='journalism']<-0

Testing$Musicparent[Testing$Category..parent.only.=='music']<-1
Testing$Musicparent[Testing$Category..parent.only.!='music']<-0

Testing$Photographyparent[Testing$Category..parent.only.=='photography']<-1
Testing$Photographyparent[Testing$Category..parent.only.!='photography']<-0

Testing$Publishingparent[Testing$Category..parent.only.=='publishing']<-1
Testing$Publishingparent[Testing$Category..parent.only.!='publishing']<-0

#creates dummy variables in Validation for parent categories
Validation$Techparent[Validation$Category..parent.only.=='technology']<-1
Validation$Techparent[Validation$Category..parent.only.!='technology']<-0

Validation$Designparent[Validation$Category..parent.only.=='design']<-1
Validation$Designparent[Validation$Category..parent.only.!='design']<-0

Validation$Fashionparent[Validation$Category..parent.only.=='fashion']<-1
Validation$Fashionparent[Validation$Category..parent.only.!='fashion']<-0

Validation$Filmparent[Validation$Category..parent.only.=='film & video']<-1
Validation$Filmparent[Validation$Category..parent.only.!='film & video']<-0

Validation$Foodparent[Validation$Category..parent.only.=='food']<-1
Validation$Foodparent[Validation$Category..parent.only.!='food']<-0

Validation$Gamesparent[Validation$Category..parent.only.=='games']<-1
Validation$Gamesparent[Validation$Category..parent.only.!='games']<-0

Validation$Journalismparent[Validation$Category..parent.only.=='journalism']<-1
Validation$Journalismparent[Validation$Category..parent.only.!='journalism']<-0

Validation$Musicparent[Validation$Category..parent.only.=='music']<-1
Validation$Musicparent[Validation$Category..parent.only.!='music']<-0

Validation$Photographyparent[Validation$Category..parent.only.=='photography']<-1
Validation$Photographyparent[Validation$Category..parent.only.!='photography']<-0

Validation$Publishingparent[Validation$Category..parent.only.=='publishing']<-1
Validation$Publishingparent[Validation$Category..parent.only.!='publishing']<-0

#Second logistic model
Mlog2<-glm(State ~ Backers.Count+Techparent+Designparent+Fashionparent+Foodparent+Gamesparent+Musicparent+Photographyparent+Publishingparent, data = Training, family = "binomial")
summary(Mlog2)

#display summary analysis of confusion matrix in-sample
confusionMatrix(table(predict(Mlog2, Training, type="response") >= 0.5, Training$State == 1), positive = 'TRUE')

#builds the confusion matrix to look at accuracy on testing data out-of-sample
confusionMatrix(table(predict(Mlog2, Testing, type="response") >= 0.5, Testing$State == 1), positive = 'TRUE')

#builds the confusion matrix to look at accuracy on testing data out-of-sample
confusionMatrix(table(predict(Mlog2, Validation, type="response") >= 0.5, Validation$State == 1), positive = 'TRUE')


