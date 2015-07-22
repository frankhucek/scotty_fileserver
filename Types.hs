module Types where



data FileEntry2 = FileEntry2 { feName         :: String
                             , feLastModified :: String
                             , feSize         :: String
                             }


--                Name    Time    Size
type FileEntry = (String, String, String)

fst' = \(x,_,_) -> x
snd' = \(_,x,_) -> x
trd' = \(_,_,x) -> x
