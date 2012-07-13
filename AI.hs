---import System.IO.UTF8

main = do 
  str <- getContents 
  mapM putStrLn $filter (/="") $three_gram $ filter (/='\n') str
 -- print $filter (/="") $three_gram str
  

ngram :: String -> Int -> [String]
ngram [] n = []
ngram ['。'] n = []
ngram [x] n = [[x]]
ngram str n = take n str : ngram str2 n
              where (x:str2) = str

three_gram :: String ->  [String]
three_gram [] = []
three_gram ['。'] = []
three_gram [x] = [[x]]
three_gram str = three_gram_i(take 3 str) : three_gram str2 
              where (x:str2) = str


three_gram_i :: [Char] -> String
three_gram_i ('。':xs) = []
three_gram_i (x:'。':y) = [x]
three_gram_i (x:y:'。':[]) = x:y:[]
three_gram_i ('、':xs) = []
three_gram_i (x:'、':y) = [x]
three_gram_i (x:y:'、':[]) = x:y:[]
three_gram_i ('　':xs) = []
three_gram_i (x:'　':y) = [x]
three_gram_i (x:y:'　':[]) = x:y:[]
three_gram_i xs = xs
