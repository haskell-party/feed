{-# LANGUAGE TupleSections #-}

--------------------------------------------------------------------
-- |
-- Module    : Text.Atom.Feed.Validate
-- Copyright : (c) Galois, Inc. 2008,
--             (c) Sigbjorn Finne 2009-
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------
module Text.Atom.Feed.Validate
  ( VTree(..)
  , ValidatorResult
  , advice
  , demand
  , valid
  , mkTree
  , flattenT
  , validateEntry
  , checkEntryAuthor
  , checkCats
  , checkContents
  , checkContributor
  , checkContentLink
  , checkLinks
  , checkId
  , checkPublished
  , checkRights
  , checkSource
  , checkSummary
  , checkTitle
  , checkUpdated
  , checkCat
  , checkContent
  , checkTerm
  , checkAuthor
  , checkPerson
  , checkName
  , checkEmail
  , checkUri
  ) where

import Prelude.Compat

import Data.XML.Types
import Text.Atom.Feed.Import

import Data.List.Compat
import Data.Maybe

data VTree a
  = VNode [a] [VTree a]
  | VLeaf [a]
  deriving (Eq, Show)

type ValidatorResult = VTree (Bool, String)

advice :: String -> ValidatorResult
advice s = VLeaf [(False, s)]

demand :: String -> ValidatorResult
demand s = VLeaf [(True, s)]

valid :: ValidatorResult
valid = VLeaf []

mkTree :: [(Bool, String)] -> [ValidatorResult] -> ValidatorResult
mkTree = VNode

flattenT :: VTree a -> [a]
flattenT (VLeaf xs) = xs
flattenT (VNode as bs) = as ++ concatMap flattenT bs

validateEntry :: Element -> ValidatorResult
validateEntry e =
  mkTree
    []
    [ checkEntryAuthor e
    , checkCats e
    , checkContents e
    , checkContributor e
    , checkId e
    , checkContentLink e
    , checkLinks e
    , checkPublished e
    , checkRights e
    , checkSource e
    , checkSummary e
    , checkTitle e
    , checkUpdated e
    ]

-- Sec 4.1.2, check #1
checkEntryAuthor :: Element -> ValidatorResult
checkEntryAuthor e =
  case pNodes "author" (elementChildren e) of
    [] -- required
     ->
      case pNode "summary" (elementChildren e) of
        Nothing -> demand "Required 'author' element missing (no 'summary' either)"
        Just e1 ->
          case pNode "author" (elementChildren e1) of
            Just a -> checkAuthor a
            _ -> demand "Required 'author' element missing"
    xs -> mkTree [] $ map checkAuthor xs

-- Sec 4.1.2, check #2
checkCats :: Element -> ValidatorResult
checkCats e = mkTree [] $ map checkCat (pNodes "category" (elementChildren e))

checkContents :: Element -> ValidatorResult
checkContents e =
  case pNodes "content" (elementChildren e) of
    [] -> valid
    [c] -> mkTree [] [checkContent c]
    cs ->
      mkTree
        (flattenT
           (demand
              ("at most one 'content' element expected inside 'entry', found: " ++ show (length cs))))
        (map checkContent cs)

checkContributor :: Element -> ValidatorResult
checkContributor _e = valid

checkContentLink :: Element -> ValidatorResult
checkContentLink e =
  case pNodes "content" (elementChildren e) of
    [] ->
      case pNodes "link" (elementChildren e) of
        [] ->
          demand
            "An 'entry' element with no 'content' element must have at least one 'link-rel' element"
        xs ->
          case filter (== "alternate") $ mapMaybe (pAttr "rel") xs of
            [] ->
              demand
                "An 'entry' element with no 'content' element must have at least one 'link-rel' element"
            _ -> valid
    _ -> valid

checkLinks :: Element -> ValidatorResult
checkLinks e =
  case pNodes "link" (elementChildren e) of
    xs ->
      case map fst $
           filter (\(_, n) -> n == "alternate") $ mapMaybe (\ex -> (ex, ) <$> pAttr "rel" ex) xs of
        xs1 ->
          let jmb (Just x) (Just y) = Just (x, y)
              jmb _ _ = Nothing
           in case mapMaybe (\ex -> pAttr "type" ex `jmb` pAttr "hreflang" ex) xs1 of
                xs2 ->
                  if any (\x -> length x > 1) (group xs2)
                    then demand
                           "An 'entry' element cannot have duplicate 'link-rel-alternate-type-hreflang' elements"
                    else valid

checkId :: Element -> ValidatorResult
checkId e =
  case pNodes "id" (elementChildren e) of
    [] -> demand "required field 'id' missing from 'entry' element"
    [_] -> valid
    xs -> demand ("only one 'id' field expected in 'entry' element, found: " ++ show (length xs))

checkPublished :: Element -> ValidatorResult
checkPublished e =
  case pNodes "published" (elementChildren e) of
    [] -> valid
    [_] -> valid
    xs ->
      demand
        ("expected at most one 'published' field in 'entry' element, found: " ++ show (length xs))

checkRights :: Element -> ValidatorResult
checkRights e =
  case pNodes "rights" (elementChildren e) of
    [] -> valid
    [_] -> valid
    xs ->
      demand ("expected at most one 'rights' field in 'entry' element, found: " ++ show (length xs))

checkSource :: Element -> ValidatorResult
checkSource e =
  case pNodes "source" (elementChildren e) of
    [] -> valid
    [_] -> valid
    xs ->
      demand ("expected at most one 'source' field in 'entry' element, found: " ++ show (length xs))

checkSummary :: Element -> ValidatorResult
checkSummary e =
  case pNodes "summary" (elementChildren e) of
    [] -> valid
    [_] -> valid
    xs ->
      demand
        ("expected at most one 'summary' field in 'entry' element, found: " ++ show (length xs))

checkTitle :: Element -> ValidatorResult
checkTitle e =
  case pNodes "title" (elementChildren e) of
    [] -> demand "required field 'title' missing from 'entry' element"
    [_] -> valid
    xs -> demand ("only one 'title' field expected in 'entry' element, found: " ++ show (length xs))

checkUpdated :: Element -> ValidatorResult
checkUpdated e =
  case pNodes "updated" (elementChildren e) of
    [] -> demand "required field 'updated' missing from 'entry' element"
    [_] -> valid
    xs ->
      demand ("only one 'updated' field expected in 'entry' element, found: " ++ show (length xs))

checkCat :: Element -> ValidatorResult
checkCat e = mkTree [] [checkTerm e, checkScheme e, checkLabel e]
  where
    checkScheme e' =
      case pAttrs "scheme" e' of
        [] -> valid
        (_:xs)
          | null xs -> valid
          | otherwise ->
            demand ("Expected at most one 'scheme' attribute, found: " ++ show (1 + length xs))
    checkLabel e' =
      case pAttrs "label" e' of
        [] -> valid
        (_:xs)
          | null xs -> valid
          | otherwise ->
            demand ("Expected at most one 'label' attribute, found: " ++ show (1 + length xs))

checkContent :: Element -> ValidatorResult
checkContent e =
  mkTree
    (flattenT (mkTree [] [type_valid, src_valid]))
    [ case ty of
        "text" ->
          case elementChildren e of
            [] -> valid
            _ -> demand "content with type 'text' cannot have child elements, text only."
        "html" ->
          case elementChildren e of
            [] -> valid
            _ -> demand "content with type 'html' cannot have child elements, text only."
        "xhtml" ->
          case elementChildren e of
            [] -> valid
            [_] -> valid -- ToDo: check that it is a 'div'.
            _ds -> demand "content with type 'xhtml' should only contain one 'div' child."
        _ -> valid
    ]
  where
    types = pAttrs "type" e
    (ty, type_valid) =
      case types of
        [] -> ("text", valid)
        [t] -> checkTypeA t
        (t:ts) ->
          (t, demand ("Expected at most one 'type' attribute, found: " ++ show (1 + length ts)))
    src_valid =
      case pAttrs "src" e of
        [] -> valid
        [_] ->
          case types of
            [] -> advice "It is advisable to provide a 'type' along with a 'src' attribute"
            (_:_) -> valid
        ss -> demand ("Expected at most one 'src' attribute, found: " ++ show (length ss))
    checkTypeA v
      | v `elem` std_types = (v, valid)
      | otherwise = (v, valid)
      where
        std_types = ["text", "xhtml", "html"]

{-
      case parseMIMEType ty of
        Nothing -> valid
        Just mt
          | isXmlType mt -> valid
          | otherwise ->
            case onlyElems (elContent e) of
              [] -> valid -- check
              _  -> demand ("content with MIME type '" ++ ty ++ "' must only contain base64 data")]
-}
{-
            case parseMIMEType t of
              Just{} -> valid
              _      -> demand "The 'type' attribute must be a valid MIME type"
-}
{-
        case parseMIMEType v of
          Nothing -> ("text", demand ("Invalid/unknown type value " ++ v))
          Just mt ->
            case mimeType mt of
              Multipart{} -> ("text", demand "Multipart MIME types not a legal 'type'")
              _ -> (v, valid)
-}
checkTerm :: Element -> ValidatorResult
checkTerm e =
  case pNodes "term" (elementChildren e) of
    [] -> demand "required field 'term' missing from 'category' element"
    [_] -> valid
    xs ->
      demand ("only one 'term' field expected in 'category' element, found: " ++ show (length xs))

checkAuthor :: Element -> ValidatorResult
checkAuthor = checkPerson

checkPerson :: Element -> ValidatorResult
checkPerson e = mkTree (flattenT $ checkName e) [checkEmail e, checkUri e]

checkName :: Element -> ValidatorResult
checkName e =
  case pNodes "name" (elementChildren e) of
    [] -> demand "required field 'name' missing from 'author' element"
    [_] -> valid
    xs -> demand ("only one 'name' expected in 'author' element, found: " ++ show (length xs))

checkEmail :: Element -> ValidatorResult
checkEmail e =
  case pNodes "email" (elementChildren e) of
    [] -> valid
    (_:xs)
      | null xs -> valid
      | otherwise ->
        demand ("at most one 'email' expected in 'author' element, found: " ++ show (1 + length xs))

checkUri :: Element -> ValidatorResult
checkUri e =
  case pNodes "email" (elementChildren e) of
    [] -> valid
    (_:xs)
      | null xs -> valid
      | otherwise ->
        demand ("at most one 'uri' expected in 'author' element, found: " ++ show (1 + length xs))
