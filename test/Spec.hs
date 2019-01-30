import BuildGraph
import Test.Hspec

testStore :: String
testStore = "test/store/"

storeInfoFile :: String
storeInfoFile = "test/store-info.txt"

main :: IO ()
main = hspec $ do
  describe "Prelude.read" $ do
    it "can list files in the store" $ do
        listAll testStore

    it "can write a dotFile" $ do
        writeDot storeInfoFile testStore

    it "can read a storeInfoFile" $ do
        infoMap <- readStoreInfo storeInfoFile
        print infoMap
