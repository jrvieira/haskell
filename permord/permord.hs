import Shut

perm = [5,9,6,8,3,4,1,0,2,7]
list = "abcdefghij"

permute = map . (!!)

main :: IO ()
main = test [
   unit "reverse" (permute list $ reverse [0..9]) (reverse list)
   ,
   unit "permute" (permute list perm) "fjgidebach"
   ]
   
