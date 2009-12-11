import Test.Framework (defaultMain)

import qualified Data.Convertible.Instances.String

main :: IO ()
main = defaultMain
    [ Data.Convertible.Instances.String.testSuite
    ]
