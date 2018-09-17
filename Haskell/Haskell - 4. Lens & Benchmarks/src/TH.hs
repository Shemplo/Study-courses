{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module TH where

import Language.Haskell.TH
import Data.Text (Text, append, pack, intercalate)
import Control.Monad (replicateM)

chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices len inds = do
    ns <- replicateM len (newName "a")
    lamE [tupP $ map varP ns] $ tupE $ map (map varE ns !!) inds

class ShowAsText a where
    show :: a -> Text

showWithPrefix :: Show a => Text -> a -> Text
showWithPrefix prefix obj = append prefix $ pack (Prelude.show obj) 

deriveShowAsText :: Name -> Q [Dec]
deriveShowAsText name = do
    (TyConI inf) <- reify name
    let cons = case inf of
         DataD    _ _ _ _ cons' _ -> cons'
         NewtypeD _ _ _ _ cons' _ -> [cons']
         _ -> fail "Fail"
    res <- genShowAsText cons
    pure [InstanceD Nothing [] (AppT (ConT ''ShowAsText) (ConT name)) [res]]
    where
        genShowAsText :: [Con] -> Q Dec
        genShowAsText cons = funD 'TH.show (map genShowAsTextClause cons)

        genShowAsTextClause :: Con -> Q Clause
        genShowAsTextClause con = do
            let (nm, flds, start, end, sep) = case con of
                 (RecC name' flds') -> 
                    (name', map (\(n, _, _) -> nameBase n ++ " = ") flds', " {", "}", ", ")
                 (NormalC name' flds') ->
                    (name', replicate (length flds') " ", "", "", "")
                 _ -> undefined
            fldsNames <- replicateM (length flds) (newName "a")

            let _insideF1 = AppE (VarE 'intercalate) (LitE $ StringL sep)
            let _insideF2 (a, b) = AppE (AppE (VarE 'showWithPrefix) (LitE $ StringL b)) (VarE a)
            let _insideF3 = LitE $ StringL (nameBase nm ++ start)

            let zippedNames = zip fldsNames flds
            let resultFold = AppE _insideF1 (ListE (map _insideF2 zippedNames))
            let namedResult = AppE (AppE (VarE 'append) _insideF3) resultFold
            let result = AppE (AppE (VarE 'append) namedResult) $ LitE $ StringL end
            pure $ Clause [ConP nm (map VarP fldsNames)] (NormalB result) []