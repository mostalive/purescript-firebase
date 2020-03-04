module Test.Authorization where

import Data.Either (either)
import Effect.Aff (Aff, attempt)
import Effect.Exception (message)
import Foreign (unsafeToForeign)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Prelude (Unit, bind, discard, ($), (=<<))
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (expectError, shouldEqual, fail)
import Web.Firebase (EventType(ChildMoved, ChildChanged, ChildRemoved, ChildAdded))
import Web.Firebase.Aff (child, database, on, onceValue, push, rootRefFor, set) as DBA
import Web.Firebase.Testing (DatabaseName(..), initializeAdminApp, initializeAnonymousTestApp, loadDatabaseRules)
import Web.Firebase.Types (DatabaseImpl)
import Web.Firebase.Types as FBT


authorizationSpec :: FBT.Firebase -> Spec Unit
authorizationSpec _ = (before $ setForbiddenEntry')  do
    describe "Authorization"    do
      describe "Writing" do
        it "with Aff push on forbidden location throws an error" $ \forbidden -> do
          let newValue = {success: "push Aff"}
          expectError $ DBA.push (unsafeToForeign newValue) forbidden
      describe "once() on forbidden location" do
        it "with Aff throws an error" $ \forbidden -> do
           e <- attempt $ DBA.onceValue forbidden  -- catch error thrown and assert
           either (\err -> (message err) `shouldEqual` "permission_denied at /forbidden: Client doesn't have permission to access the desired data.\n | firebase code: | \n PERMISSION_DENIED") (\_ -> "expected an error to be thrown" `shouldEqual` "but was not") e
      describe "on() at forbidden location" do
        it "ChildAdded with Aff throws an error" $ \forbidden -> do
          expectError $ DBA.on ChildAdded forbidden
        it "ChildRemoved with Aff throws an error" $ \forbidden ->   do
          expectError $ DBA.on ChildRemoved forbidden
        it "ChildChanged with Aff throws an error" $ \forbidden -> do
          expectError $ DBA.on ChildChanged forbidden
        it "ChildMoved with Aff throws an error" $ \forbidden -> do
          expectError $ DBA.on ChildMoved forbidden
      it "set() with Aff at forbidden location throws an error" $ \forbidden ->  do
        let newValue = {success: "set Aff"}
        e <- attempt $ DBA.set (unsafeToForeign newValue) forbidden
        either (\err -> (message err) `shouldEqual` "PERMISSION_DENIED: Permission denied\n | firebase code: | \n PERMISSION_DENIED")
               (\_ -> fail "expected an error to be thrown but was not")
               e

setForbiddenEntry' :: Aff DatabaseImpl
setForbiddenEntry' = do
  let dbName = (DatabaseName "AuthorizationApp")
  let forbiddenKey = "forbidden"
  arrange dbName forbiddenKey
  DBA.child forbiddenKey =<< appRoot =<< initializeAnonymousTestApp dbName
  where
    arrange dbName forbiddenKey = do
      root <- appRoot =<< initializeAdminApp dbName
      fbRules <- readTextFile UTF8 "./test/rules.json"
      let rules = {databaseName: dbName,
                   rules: fbRules }
      forbidden <- DBA.child forbiddenKey root
      pathRoot <- DBA.child "firstchild" forbidden
      DBA.set (unsafeToForeign "something") pathRoot
      loadDatabaseRules rules

appRoot::FBT.FirebaseAppImpl -> Aff DatabaseImpl
appRoot app = DBA.rootRefFor =<< DBA.database app
