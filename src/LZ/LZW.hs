{- |
  Module      : LZ.LZW
  Description : An implementation of LZW method
  Maintainer  : Estéban DARTUS
-}
module LZ.LZW(compress, uncompress) where
import LZ.Dictionaries(ascii)
import Data.Maybe
import Data.List
import Debug.Trace (trace)

type Dictionary = [String]

-- | LZW compress method
compress :: String -> [Int]
compress [] = [] -- Si le texte est vide, on revoie un tableau vide (Optimisation)
compress texte = compressHelper texte [] ascii []            -- Le texte qu'on veut compresser, le morceau de texte à trouver, la table ascii et un accumulateur de récursion.


compressHelper :: String -> String -> Dictionary -> [Int] -> [Int]

compressHelper [] [] _ acc = acc -- Ne devrait jamais être utilisé mais on ne sait jamais. Sortie de secours en quelque sorte

--Condition de sortie standard. A mettre avant la boucle récursive sinon problème de liste vide
compressHelper [] morceau dico acc -- Si on a atteint la fin du texte à compresser . . .
  | morceau == [] -- . . . Si le morceau à rechercher est vide, on retourne l'accumulateur
    = acc 
  | otherwise -- . . . Sinon on concatène l'index du dernier morceau à l'accumulateur
    = (acc ++ [fromJust preIndex]) 
  where
    preIndex = findIndex (\x -> x == morceau) dico --Calcul de l'index du dernier morceau

-- Boucle récursive principale
compressHelper texte morceau dico acc -- Si on n'a pas encore compressé tout le texte
  | isNothing index 
    = compressHelper texte [] (dico ++ [nouvMorceau]) (acc ++ [fromJust preIndex]) -- Si le morceau recherché n'est pas dans le dictionnaire, alors on le rajoute et on récupère l'index du morceau moins son dernier caractère
  | otherwise 
    = compressHelper (tail texte) nouvMorceau dico acc -- Sinon si le morceau recherché existe, on concatène le caractère suivant et on recherche le tout
  where
    index = findIndex (\x -> x == nouvMorceau) dico
    nouvMorceau = trace ("texte: "++ show texte ++ "  morceau: " ++ show morceau ++ "  acc: " ++ show acc) $ morceau ++ [head texte]
    temp = findIndex (\x -> x == morceau) dico -- recherche de l'index d'un morceau
    preIndex = if isNothing temp 
               then Just (length ascii) 
               else temp


-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> Maybe String
uncompress [] = Nothing -- Si le code donné est vide, on retourne rien (Optimisation)
uncompress code = uncompressHelper code 0 ascii (Just []) -- Code à décompresser, l'index de l'élément précédent, Dictionnaire de décompression et l'accumulateur de récursion

--Fonction de décompression dans le cas où la liste d'entier n'est pas vide
uncompressHelper :: [Int] -> Int -> Dictionary -> Maybe String -> Maybe String

--Condition de sortie standard
uncompressHelper [] _ _ acc = acc

--Boucle de récursion princiaple
uncompressHelper (index:code) preIndex dico acc
  | index < 0 || length dico < index || isNothing acc || (length dico == index && length (fromJust acc) == 0) --Si la décompression n'est pas possible ...
    = Nothing -- ... on renvoie un texte vide ...
  | otherwise -- ... sinon on lance la décompression
    = uncompressHelper code index nouvDico (Just (maybeAcc ++ char)) -- On continue la décompression avec le reste du code, l'index du caractère qui vient d'être décodé, le dico avec le nouveau morceau et le résultat concaténé dans l'accumulateur
  where
    maybeAcc = fromJust acc
    char = nouvDico !! index --récupération du caractère dans le nouveau dictionnaire à l'emplacement index
    preChar = dico !! preIndex  -- idem mais pour le caractère précédent dans le dictionnaire précédent
    -- Création du nouveau dictionnaire
    nouvDico = if length maybeAcc == 0 -- Si l'accumulateur est vide (donc si on est au début de la récursion), on retourne le dictionnaire de base
               then dico
               else if index < (length dico) --Sinon si l'index peut se trouver dans le dico ...
                    then dico ++ [(preChar ++ [head char])] -- ... on concatène le caractère précédemment mit dans l'accumulateur ainsi que le PREMIER caractère de la nouvelle chaine
                    else dico ++ [(preChar ++ [head preChar])] -- sinon on prend seulement le caractère précédent (gestion de cas avec des chaînes comme "aaaaa")
