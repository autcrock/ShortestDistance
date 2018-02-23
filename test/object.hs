module Object
where

    import Data.Aeson
    
o =
Right (
    Map { map = [
        fromList [("A", Object (
            fromList [
                ("B",Number 100.0),
                ("C",Number 30.0)]
                ))]
       ,fromList [("B",Object (
           fromList [
               ("F",Number 300.0)]
               ))]
       ,fromList [("C",Object (
           fromList [
               ("D",Number 200.0)]
               ))]
       ,fromList [("D",Object (
           fromList [
               ("E",Number 80.0),
               ("H",Number 90.0)]
               ))]
       ,fromList [("E",Object (
           fromList [
               ("G",Number 150.0),
               ("H",Number 30.0),
               ("F",Number 50.0)]
               ))]
       ,fromList [("F",Object (
           fromList [
               ("G",Number 70.0)]
               ))]
       ,fromList [("G",Object (
           fromList [
               ("H",Number 50.0)]
               ))]
        ]})
