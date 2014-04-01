# Nagato 
A Naive Bayes classifier for Japanese text.

###なにこれ
NagatoはHaskellで書かれたナイーブベイズ分類器です。とりあえず適当に実装していますが目的は戦艦長門とその擬人化版と某ライトノベルのキャラクターに関する説明文を分類することです(少なくとも当初は)。  

これからの機能追加予定として、品詞による学習内容のフィルタを予定しています。→しました。
あとComplement Naive Bayesにも対応しました。
###Usage
Setting file is JSON format.
```json
{"classes" : [
    {"className" : "nagato_yuki", "dataSource" : "nagato_yuki.txt"},
    {"className" : "nagato_kancolle", "dataSource" : "nagato_kancolle.txt"},
    {"className" : "nagato_city", "dataSource" : "nagato_city.txt"}
    ]
}
```
Start training.
```haskell
Text.Nagato.Train.doTrain "[SETTINGFILE]" "[SAVEFILE]"
--For complement Naive Bayes
Text.Nagato.Train_complement.doTrain "[SETTINGFILE]" "[SAVEFILE]"

```
Classify
```haskell
import Text.Nagato.NagatoIO as NagatoIO
import qualified Text.Nagato.Classify as Classifier

--Load classes from file

classes <- NagatoIO.readFromFile "[CLASSFILE]"
classesComplement <- NagatoIO.readFromFile "[CLASSFILE_FOR_COMPLEMENT]"

Classifier.classify 
Classifier.classifyComplement
```
###注意事項
MeCabの辞書はUTF-8でないと正常に動作しません
