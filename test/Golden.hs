module Golden where

import Control.Monad (forM_)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile)
import Data.Default
import Data.List (isSuffixOf)

import qualified Fio2Hledger
import qualified Fio2Hledger.Types
import qualified System.Directory

test_derivation :: TestTree
test_derivation =
  testGroup "golden" $
    [ testSample
        "sample-out"
        "fio-czk"
        "all in one sample"
        (Fio2Hledger.sample Fio2Hledger.Types.czConfig)
    ] ++ map
           (\my ->
             testSample
               "sample-split-out"
               ("fio-czk" ++ my)
               ("split sample " ++  my)
               (Fio2Hledger.splitSample def))
            [ "-2021", "-2022", "" ]
       ++ map
           (\my ->
             testSample
               "sample-split-up-out"
               ("fio-czk" ++ my)
               ("updating split sample " ++  my)
               (do
                  -- wipe old journals so we don't update multiple times
                  fs <- System.Directory.listDirectory "."
                  forM_ (filter (\x -> ".journal" `Data.List.isSuffixOf` x) fs) $ \f ->
                    System.Directory.removeFile f
                  Fio2Hledger.splitSample def
                  Fio2Hledger.updateSample def
               ))
           [ "-2021", "-2022", "-2023", "" ]
 where
  testSample
    :: FilePath
    -> FilePath
    -> String
    -> IO ()
    -> TestTree
  testSample dir f desc act =
    goldenVsFile
      desc
      (dir ++ "/" ++ f ++ ".journal.golden")
      (dir ++ "/" ++ f ++ ".journal")
      $ do
    System.Directory.withCurrentDirectory
          dir
          act
