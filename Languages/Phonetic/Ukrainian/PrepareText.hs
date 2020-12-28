-- |
-- Module      :  Languages.Phonetic.Ukrainian.PrepareText
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to order the 7 or less Ukrainian words (or their concatenations)
-- to obtain (to some extent) suitable for poetry or music text.
-- Earlier it has been a module DobutokO.Poetry.Ukrainian.PrepareText
-- from the @dobutokO-poetry@ package.
-- In particular, this module can be used to prepare the Ukrainian text
-- by applying the most needed grammar to avoid misunderstanding
-- for the produced text. The attention is paid to the prepositions, pronouns, conjunctions
-- and particles that are most commonly connected (or not) in a significant way
-- with the next text.
-- Uses the information from:
-- https://uk.wikipedia.org/wiki/%D0%A1%D0%BF%D0%BE%D0%BB%D1%83%D1%87%D0%BD%D0%B8%D0%BA
-- and
-- https://uk.wikipedia.org/wiki/%D0%A7%D0%B0%D1%81%D1%82%D0%BA%D0%B0_(%D0%BC%D0%BE%D0%B2%D0%BE%D0%B7%D0%BD%D0%B0%D0%B2%D1%81%D1%82%D0%B2%D0%BE)
--

module Languages.Phonetic.Ukrainian.PrepareText (
  -- * Basic functions
  prepareText
  , prepareTextN
  , complexWords
  , splitLines
  , splitLinesN
  , auxiliary1
  , isPreposition
  , isConcatenated
  , isSpC
  , concatenated2
  , jottedConv
  -- * Used to transform after convertToProperUkrainian from mmsyn6ukr package
  , aux4
  , aux0
) where

import CaseBi (getBFst')
import Data.List.InnToOut.Basic (mapI)
import Data.Char (isAlpha,toLower)
import qualified Data.Vector as V

-- | Is used to convert a Ukrainian text into list of 'String' each of which is ready to be
-- used by the functions from the other modules in the package.
-- It applies minimal grammar links and connections between the most commonly used Ukrainian
-- words that \"should\" be paired and not dealt with separately
-- to avoid the misinterpretation and preserve maximum of the semantics for the
-- \"phonetic\" language on the Ukrainian basis.
prepareText :: String -> [String]
prepareText = filter (any isUkrainianL) . splitLines . map (unwords . concatenated2. auxiliary1 . complexWords . words . filter (\t -> isAlpha t || isSpC t)) . filter (not . null) . lines

-- | Concatenates complex words in Ukrainian so that they are not separated further by possible words order rearrangements (because they are treated
-- as a single word). This is needed to preserve basic grammar in phonetic languages.
complexWords :: [String] -> [String]
complexWords (xs:ys:zs:ts:xss) =
 getBFst' (xs:complexWords (ys:zs:ts:xss),V.fromList [("\1074",
    if ys == "\1084\1110\1088\1091" && zs == "\1090\1086\1075\1086" &&
    ts == "\1103\1082" then (xs ++ ys ++ zs ++ ts ++ (concat . take 1 $ xss)):
    complexWords (drop 1 xss) else (xs ++ ys):complexWords (zs:ts:xss)),
    ("\1076\1072\1088\1084\1072", if ys == "\1097\1086"
    then (xs ++ ys ++ zs):complexWords (ts:xss)
    else (xs ++ ys):complexWords (zs:ts:xss)), ("\1076\1083\1103",
    if ys == "\1090\1086\1075\1086" && zs == "\1097\1086\1073"
    then (xs ++ ys ++ zs ++ ts):complexWords xss
    else (xs ++ ys):complexWords (zs:ts:xss)), ("\1079",
      case ys of
        "\1090\1080\1084" -> if zs == "\1097\1086\1073"
          then (xs ++ ys ++ zs):complexWords (ts:xss)
          else (xs ++ ys):complexWords (zs:ts:xss)
        "\1090\1086\1075\1086" -> if zs == "\1095\1072\1089\1091" && ts == "\1103\1082"
          then (xs ++ ys ++ zs ++ "\1081\1072\1082" ++
          (concat . take 1 $ xss)):complexWords (drop 1 xss)
          else (xs ++ ys):complexWords (zs:ts:xss)
        _ -> (xs ++ ys):complexWords (zs:ts:xss)), ("\1079\1072\1084\1110\1089\1090\1100",
    if ys == "\1090\1086\1075\1086" && zs == "\1097\1086\1073"
    then (xs ++ ys ++ zs ++ ts):complexWords xss
    else (xs ++ ys):complexWords (zs:ts:xss)), ("\1087\1086\1087\1088\1080",
    if ys == "\1090\1077" && zs == "\1097\1086" then (xs ++ ys ++ zs ++ ts):complexWords xss
    else (xs ++ ys):complexWords (zs:ts:xss)), ("\1087\1088\1080",
    if ys == "\1094\1100\1086\1084\1091" then (xs ++ ys ++ zs):complexWords (ts:xss)
    else (xs ++ ys):complexWords (zs:ts:xss)), ("\1087\1110\1089\1083\1103",
    if ys == "\1090\1086\1075\1086" && zs == "\1103\1082" then (xs ++ ys ++ zs ++ ts):complexWords xss
    else (xs ++ ys):complexWords (zs:ts:xss)), ("\1090\1072\1082", if ys == "\1097\1086"
    then (xs ++ ys ++ zs):complexWords (ts:xss) else (xs ++ ys):complexWords (zs:ts:xss)),
    ("\1090\1080\1084\1095\1072\1089\1086\1084", if ys == "\1103\1082"
    then (xs ++ ys ++ zs):complexWords (ts:xss) else (xs ++ ys):complexWords (zs:ts:xss)),
    ("\1090\1086\1084\1091", if ys == "\1103\1082" then (xs ++ "\1081\1072\1082" ++ zs):complexWords (ts:xss)
    else (xs ++ ys):complexWords (zs:ts:xss)), ("\1091", if ys == "\1084\1110\1088\1091" &&
    zs == "\1090\1086\1075\1086" && ts == "\1103\1082" then (xs ++ ys ++ zs ++ ts ++
    (concat . take 1 $ xss)):complexWords (drop 1 xss) else (xs ++ ys):complexWords (zs:ts:xss)),
    ("\1093\1086\1095", if ys == "\1073\1080" then (xs ++ ys ++ zs):complexWords (ts:xss)
    else (xs ++ ys):complexWords (zs:ts:xss)), ("\1093\1086\1095", if ys == "\1073\1080"
    then (xs ++ ys ++ zs):complexWords (ts:xss) else (xs ++ ys):complexWords (zs:ts:xss)),
    ("\1095\1077\1088\1077\1079", if ys == "\1090\1077" && zs == "\1097\1086"
    then (xs ++ ys ++ zs ++ ts):complexWords xss else (xs ++ ys):complexWords (zs:ts:xss))]) xs
complexWords (xs:ys:zs:_) =
 getBFst' ([xs,ys,zs],V.fromList [
    ("\1076\1072\1088\1084\1072", if ys == "\1097\1086"
    then [xs ++ ys ++ zs]
    else [xs ++ ys,zs]), ("\1079",
      case ys of
        "\1090\1080\1084" -> if zs == "\1097\1086\1073"
          then [xs ++ ys ++ zs]
          else [xs ++ ys,zs]
        _ -> [xs ++ ys,zs]), ("\1087\1088\1080",
    if ys == "\1094\1100\1086\1084\1091" then [xs ++ ys ++ zs]
    else [xs ++ ys,zs]), ("\1090\1072\1082", if ys == "\1097\1086"
    then [xs ++ ys ++ zs] else [xs ++ ys,zs]),     ("\1090\1080\1084\1095\1072\1089\1086\1084",
    if ys == "\1103\1082" then [xs ++ ys ++ zs] else [xs ++ ys, zs]),
    ("\1090\1086\1084\1091", if ys == "\1103\1082" then [xs ++ "\1081\1072\1082" ++ zs]
    else [xs ++ ys,zs]), ("\1093\1086\1095", if ys == "\1073\1080" then [xs ++ ys ++ zs]
    else [xs ++ ys,zs]), ("\1093\1086\1095", if ys == "\1073\1080"
    then [xs ++ ys ++ zs] else [xs ++ ys,zs])]) xs
complexWords xss = xss

-- | Since 0.2.1.0 version the function is recursive and is applied so that all returned elements ('String') are no longer than 7 words in them.
splitLines :: [String] -> [String]
splitLines xss
 | null xss = []
 | otherwise = mapI (\xs -> compare (length . words $ xs) 7 == GT) (\xs -> let yss = words xs in
     splitLines . map unwords . (\(q,r) -> [q,r]) . splitAt (length yss `quot` 2) $ yss) $ xss

-- | A generalized variant of the 'splitLines' with the arbitrary maximum number of the words in the lines given as the first argument.
splitLinesN :: Int -> [String] -> [String]
splitLinesN n xss
 | null xss || n <= 0 = []
 | otherwise = mapI (\xs -> compare (length . words $ xs) n == GT) (\xs -> let yss = words xs in
     splitLines . map unwords . (\(q,r) -> [q,r]) . splitAt (length yss `quot` 2) $ yss) $ xss

-- | A generalized variant of the 'prepareText' with the arbitrary maximum number of the words in the lines given as the first argument.
prepareTextN :: Int -> String -> [String]
prepareTextN n = filter (any isUkrainianL) . splitLinesN n . map (unwords . concatenated2. auxiliary1 . complexWords . words . filter (\t -> isAlpha t || isSpC t)) . filter (not . null) . lines


auxiliary1 :: [String] -> [String]
auxiliary1 (xs:ys:zs:xss)
  | isConcatenated ys || isPreposition ys =
      auxiliary1 (xs:auxiliary1 ((ys ++ (drop 1 . jottedConv $ ' ':zs)):xss))
  | isConcatenated xs || isPreposition xs = auxiliary1 ((xs ++ (drop 1 . jottedConv $ ' ':ys)):zs:xss)
  | otherwise = xs:auxiliary1 (ys:zs:xss)
auxiliary1 x@(xs:ys:xss)
  | isConcatenated xs || isPreposition xs = auxiliary1 ((xs ++ (drop 1 . jottedConv $ ' ':ys)):xss)
  | otherwise = x
auxiliary1 xss = xss

isPreposition :: String -> Bool
isPreposition ts =
  getBFst' (False, V.fromList .
   zip ["\1030\1079", "\1041\1077\1079", "\1041\1110\1083\1103", "\1042",
    "\1042\1110\1076", "\1044\1083\1103", "\1044\1086", "\1047",
     "\1047\1072", "\1047\1072\1088\1072\1076\1080", "\1047\1110",
      "\1050", "\1050\1086\1083\1086", "\1050\1088\1110\1079\1100",
       "\1050\1088\1110\1084", "\1052\1077\1078", "\1052\1077\1078\1080",
        "\1052\1110\1078", "\1053\1072", "\1053\1072\1076", "\1054",
         "\1054\1073", "\1054\1076", "\1054\1082\1088\1110\1084",
          "\1055\1077\1088\1077\1076", "\1055\1086", "\1055\1088\1080",
           "\1055\1088\1086", "\1055\1088\1086\1090\1080",
            "\1055\1110\1076", "\1055\1110\1089\1083\1103",
             "\1057\1077\1088\1077\1076", "\1057\1077\1088\1077\1076\1080",
              "\1059", "\1063\1077\1088\1077\1079", "\1073\1077\1079",
               "\1073\1110\1083\1103", "\1074", "\1074\1110\1076",
                "\1076\1083\1103", "\1076\1086", "\1079", "\1079\1072",
                 "\1079\1072\1088\1072\1076\1080", "\1079\1110",
                  "\1082", "\1082\1086\1083\1086", "\1082\1088\1110\1079\1100",
                   "\1082\1088\1110\1084", "\1084\1077\1078", "\1084\1077\1078\1080",
                    "\1084\1110\1078", "\1085\1072", "\1085\1072\1076", "\1086",
                     "\1086\1073", "\1086\1076", "\1086\1082\1088\1110\1084",
                      "\1087\1077\1088\1077\1076", "\1087\1086", "\1087\1088\1080",
                       "\1087\1088\1086", "\1087\1088\1086\1090\1080", "\1087\1110\1076",
                        "\1087\1110\1089\1083\1103", "\1089\1077\1088\1077\1076",
                         "\1089\1077\1088\1077\1076\1080", "\1091",
                          "\1095\1077\1088\1077\1079", "\1110\1079"] $
                           replicate 200 True) ts
{-# INLINE isPreposition #-}

-- | Since the dobutokO-poetry version 0.16.3.0 the (||) operator has been changed to the (&&).
-- The idea is that these words are the ones that are pronouns and they \"should\" be treated
-- (by the author's understanding) as independent words.
isConcatenated :: String -> Bool
isConcatenated ts
 | null ts = False
 | otherwise = compare (length ts) 2 /= GT && getBFst' (True, V.fromList .
     zip ["\1028", "\1042\1080", "\1052\1080", "\1058\1080", "\1058\1110",
       "\1062\1110", "\1071", "\1074\1080", "\1084\1080", "\1090\1080", "\1090\1110",
         "\1094\1110", "\1103", "\1108"] $ replicate 14 False) ts &&
           (head ts `notElem` "\1031\1111")
{-# INLINE isConcatenated #-}

concatenated2 :: [String] -> [String]
concatenated2 (xs:ys:xss) =
 getBFst' (xs:concatenated2 (ys:xss), V.fromList . zip ["\1040\1073\1086","\1040\1076\1078\1077",
 "\1040\1083\1077","\1040\1085\1110\1078","\1041\1086\1076\1072\1081",
 "\1041\1091\1094\1110\1084\1090\1086","\1042\1078\1077","\1042\1080\1082\1083\1102\1095\1085\1086",
 "\1042\1083\1072\1089\1085\1077","\1042\1090\1110\1084","\1044\1072\1074\1072\1081",
 "\1047\1072\1090\1077","\1050\1086\1083\1080","\1051\1077\1076\1074\1077","\1051\1080\1096\1077",
 "\1052\1072\1081\1078\1077","\1052\1086\1074","\1052\1086\1074\1073\1080",
 "\1052\1086\1074\1073\1080\1090\1086","\1053\1072\1074\1110\1090\1100",
 "\1053\1072\1089\1082\1110\1083\1100\1082\1080","\1053\1072\1095\1077","\1053\1072\1095\1077\1073",
 "\1053\1072\1095\1077\1073\1090\1086","\1053\1077\1074\1078\1077","\1053\1077\1084\1086\1074",
 "\1053\1077\1084\1086\1074\1073\1080","\1053\1077\1084\1086\1074\1073\1080\1090\1086",
 "\1053\1077\1085\1072\1095\1077","\1053\1077\1085\1072\1095\1077\1073\1090\1086",
 "\1053\1077\1093\1072\1081","\1053\1090\1078\1077","\1053\1110\1073\1080",
 "\1053\1110\1073\1080\1090\1086","\1053\1110\1078","\1054\1090\1086\1078",
 "\1055\1088\1080\1090\1086\1084\1091","\1055\1088\1080\1090\1110\1084",
 "\1055\1088\1080\1095\1086\1084\1091","\1055\1088\1080\1095\1110\1084",
 "\1055\1088\1086\1090\1077","\1057\1072\1084\1077","\1057\1077\1073\1090\1086",
 "\1058\1072\1082\1080","\1058\1086\1073\1090\1086","\1058\1110\1083\1100\1082\1080",
 "\1061\1072\1081","\1061\1086\1095","\1061\1110\1073\1072","\1062\1077\1073\1090\1086",
 "\1065\1086\1073","\1071\1082\1073\1080","\1071\1082\1088\1072\1079","\1071\1082\1097\1086",
 "\1072\1073\1086","\1072\1076\1078\1077","\1072\1083\1077","\1072\1085\1110\1078",
 "\1073\1086\1076\1072\1081","\1073\1091\1094\1110\1084\1090\1086","\1074\1078\1077",
 "\1074\1080\1082\1083\1102\1095\1085\1086","\1074\1083\1072\1089\1085\1077",
 "\1074\1090\1110\1084","\1076\1072\1074\1072\1081","\1079\1072\1090\1077","\1082\1086\1083\1080",
 "\1083\1077\1076\1074\1077","\1083\1080\1096\1077","\1084\1072\1081\1078\1077","\1084\1086\1074",
 "\1084\1086\1074\1073\1080","\1084\1086\1074\1073\1080\1090\1086","\1085\1072\1074\1110\1090\1100",
 "\1085\1072\1089\1082\1110\1083\1100\1082\1080","\1085\1072\1095\1077","\1085\1072\1095\1077\1073",
 "\1085\1072\1095\1077\1073\1090\1086","\1085\1077\1074\1078\1077","\1085\1077\1084\1086\1074",
 "\1085\1077\1084\1086\1074\1073\1080","\1085\1077\1084\1086\1074\1073\1080\1090\1086",
 "\1085\1077\1085\1072\1095\1077","\1085\1077\1085\1072\1095\1077\1073\1090\1086",
 "\1085\1077\1093\1072\1081","\1085\1110\1073\1080","\1085\1110\1073\1080\1090\1086",
 "\1085\1110\1078","\1086\1090\1078\1077","\1086\1090\1086\1078","\1087\1088\1080\1090\1086\1084\1091",
 "\1087\1088\1080\1090\1110\1084","\1087\1088\1080\1095\1086\1084\1091","\1087\1088\1080\1095\1110\1084",
 "\1087\1088\1086\1090\1077","\1089\1072\1084\1077","\1089\1077\1073\1090\1086","\1090\1072\1082\1080",
 "\1090\1086\1073\1090\1086","\1090\1110\1083\1100\1082\1080","\1093\1072\1081","\1093\1086\1095",
 "\1093\1110\1073\1072","\1094\1077\1073\1090\1086","\1097\1086\1073","\1103\1082\1073\1080",
 "\1103\1082\1088\1072\1079","\1103\1082\1097\1086"] $ replicate 200 ((xs ++ (drop 1 . jottedConv $ ' ':ys)):concatenated2 xss)) xs
concatenated2 xss = xss

isSpC :: Char -> Bool
isSpC x = x == '\'' || x == ' ' || x == '\x2019' || x == '\x02BC' || x == '-'
{-# INLINE isSpC #-}

jottedConv :: String -> String
jottedConv (x:y:xs)
  | isSpC x = x:(getBFst' (jottedConv (y:xs), V.fromList
     [('\1028', '\1049':'\1077':jottedConv xs),
      ('\1031', '\1049':'\1110':jottedConv xs),
      ('\1070', '\1049':'\1091':jottedConv xs),
      ('\1071', '\1049':'\1072':jottedConv xs),
      ('\1102', '\1081':'\1091':jottedConv xs),
      ('\1103', '\1081':'\1072':jottedConv xs),
      ('\1108', '\1081':'\1077':jottedConv xs),
      ('\1111', '\1081':'\1110':jottedConv xs)]) y)
  | otherwise = x:jottedConv (y:xs)
jottedConv xs = xs

-- | Can be used to prepare the text after 'convertToProperUkrainian' from 'Melodics.Ukrainian' module from @mmsyn6ukr@ package so that all the 'String' can
-- be represented as unique 'Char'.
aux4 :: String -> Char
aux4 xs
  | xs == "\1076\1078" = 'j'
  | xs == "\1076\1079" = 'z'
  | xs == "\1089\1100" = 's'
  | xs == "\1094\1100" = 'c'
  | null xs = error "Languages.Phonetic.Ukrainian.PrepareText.aux4: Empty String. "
  | otherwise = head xs

-- | Can be used to prepare the text after 'convertToProperUkrainian' from 'Melodics.Ukrainian' module from @mmsyn6ukr@ package so that all the 'String' can
-- be represented as unique 'Char'. Afterwards, all the palatalized or semi-palatalized sounds are represented just the same as not palatalized ones.
aux0 :: V.Vector String -> V.Vector Char
aux0 v
  | V.null v = V.empty
  | otherwise = V.map head . V.filter (not. null) . V.map (\xs ->
     case xs of
      "\1089\1100" -> "\1089"
      "\1094\1100" -> "\1094"
      "\1076\1078" -> "j"
      "\1076\1079" -> "z"
      _            -> xs) . V.filter (/= "\1100") $ v

-- | Is taken from the @mmsyn6ukr@ package version 0.8.1.0 so that the amount of dependencies are reduced (and was slightly modified).
isUkrainianL :: Char -> Bool
isUkrainianL y | (y >= '\1040' && y <= '\1065') || (y >= '\1070' && y <= '\1097') = True
              | otherwise = getBFst' (False, V.fromList . map (\x -> (x, True)) $ "\1028\1030\1031\1068\1100\1102\1103\1108\1110\1111\1168\1169\8217") y
