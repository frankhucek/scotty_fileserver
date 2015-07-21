module Types where

--                Name    Time    Size
type FileEntry = (String, String, String)

fst' = \(x,_,_) -> x
snd' = \(_,x,_) -> x
trd' = \(_,_,x) -> x
