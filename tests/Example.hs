module Example where

import Prelude ()
import Prelude.Compat

import Example.CreateAtom (createAtom)
import Test.HUnit (Assertion)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

exampleTests :: Test
exampleTests =
  testGroup "Examples" [testCase "example code to create an atom feed typechecks" typeCheckAtom]

typeCheckAtom :: Assertion
typeCheckAtom =
  case createAtom of
    _:_ -> return ()
    _ -> error "createAtom returned an empty String"
