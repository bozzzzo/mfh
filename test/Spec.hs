import Test.Hspec
import Test.QuickCheck
import Data.List
import Lib

dot = piece "x" [(0,0,0)]

main :: IO ()
main = hspec $ do
  describe "empty" $ do
     it "can be 2D 1x1" $ do
       empty [1,1] `shouldBe` Space' [0,0,0] [[[" "]]]
     it "can be 2D 1x2" $ do
       empty [1,2] `shouldBe` Space' [0,1,0] [[[" "]
                                             ,[" "]]]
     it "can be 3D 1x1x1" $ do
       empty [1,1,1] `shouldBe` Space' [0,0,0] [[[" "]]]

     it "can be 3D 1x2x3" $ do
       empty [1,2,3] `shouldBe` Space' [0,1,2] [[[" "]
                                               ,[" "]]
                                              ,[[" "]
                                               ,[" "]]
                                              ,[[" "]
                                               ,[" "]]]

  describe "rotations" $ do
    let p = piece "x" [(0,0,0),(1,0,0),(0,2,1)]
    describe "rotz" $ do
      it "rotates clockwise" $ do
        rotz p `shouldBe` piece "x" [(2,0,0),(2,1,0),(0,0,1)]
      it "comes around" $ do
        (rotz . rotz . rotz . rotz) p `shouldBe` p
    describe "roty" $ do
      it "rotates clockwise" $ do
        roty p `shouldBe` piece "x" [(1,0,0),(1,0,1),(0,2,0)]
      it "comes around" $ do
        (roty . roty . roty . roty) p `shouldBe` p
    describe "rotx" $ do
      it "rotates clockwise" $ do
        rotx p `shouldBe` piece "x" [(0,0,2),(1,0,2),(0,1,0)]
      it "comes around" $ do
        (rotx . rotx . rotx . rotx) p `shouldBe` p
    describe "rotations" $ do
      it "leaves dot alone" $ do
        rotations dot  `shouldBe` [dot]
      it "has 3 images for stick" $ do
        let stick = piece "x" [(0,0,0),(0,1,0)]
        rotations stick `shouldBe` sort [rotx stick, roty stick, rotz stick]

  describe "shadow" $ do
    it "has no shadow under dot" $ do
      shadow dot `shouldBe` piece "x'" []
    it "has shadow under raised dot" $ do
      let r = piece "x" [(0,0,1)]
      shadow r `shouldBe` piece "x'" [(0,0,0)]
    it "has shadow under raised stick" $ do
      let r = piece "x" [(0,0,1),(0,0,2)]
      shadow r `shouldBe` piece "x'" [(0,0,0)]
    it "has shadow under z" $ do
      let z = piece "z" [(0,0,1),(1,0,1),(1,0,0),(2,0,0)]
      shadow z `shouldBe` piece "z'" [(0,0,0)]
    it "has shadow under raised z" $ do
      let z = piece "z" [(0,0,3),(0,1,3),(0,1,2),(0,2,2)]
      shadow z `shouldBe` piece "z'" [(0,0,2),(0,1,1),(0,2,1)]

  describe "move" $ do
    it "can do nothing" $ do
      move (0,0,0) dot `shouldBe` dot
    it "can move in x" $ do
      move (1,0,0) dot `shouldBe` piece "x" [(1,0,0)]
    it "can move in y" $ do
      move (0,1,0) dot `shouldBe` piece "x" [(0,1,0)]
    it "can move in z" $ do
      move (0,0,1) dot `shouldBe` piece "x" [(0,0,1)]
    it "can move in xyz" $ do
      move (3,2,1) dot `shouldBe` piece "x" [(3,2,1)]

  describe "moves" $ do
    it "can do nothing" $ do
      moves [0,0,0] dot `shouldBe` [dot]
    it "can move dot" $ do
      sort (moves [1,1,1] dot) `shouldBe` sort [ move (x,y,z) dot
                                                 | x <- [0..1],
                                                   y <- [0..1],
                                                   z <- [0..1]]

  describe "flail" $ do
    it "can do nothing" $ do
      flail [0,0,0] dot `shouldBe` [dot]
    it "can move dot" $ do
      flail [1,1,1] dot `shouldBe` moves [1,1,1] dot
    it "can move stick" $ do
      let stick = piece "x" [(0,0,0),(0,1,0)]
          s = [2,2,2]
      sort (flail s stick) `shouldBe` (sort $ concatMap (moves s) [
        stick, (rotx stick), (rotz stick)])
