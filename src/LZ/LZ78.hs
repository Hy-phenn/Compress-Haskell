{- |
  Module      : LZ.LZ78
  Description : An implementation of LZ78 method
  Maintainer  : Julian Gosselin
-}
module LZ.LZ78 (compress, decompress) where

import LZ.Dictionaries (empty, zeroAsChar)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)

-- | La fonction principale de compression LZ78.
compress :: String -> [(Int, Char)]
compress input = compressHelper input empty 1

-- | Helper fonction pour la compression LZ78 avec gestion récursive du texte, du dictionnaire et de l'index actuel.
compressHelper :: String -> [String] -> Int -> [(Int, Char)]
compressHelper [] _ _ = []  -- Cas de base : si le texte est vide, retourne une liste vide de paires encodées
compressHelper (c:cs) dictionary index =
    let prefix = longestPrefix [c] cs dictionary  -- Trouve le plus long préfixe à partir du dictionnaire
    in if prefix == [c]  -- Si le préfixe est égal au caractère actuel,
       then (0, c) : compressHelper cs (dictionary ++ [prefix]) (index + 1)  -- ajoute une paire encodée avec index 0 et le caractère au résultat, puis continue avec le reste du texte
       else let maybeIndex = findIndex (== init prefix) dictionary  -- Sinon, trouve l'index du préfixe dans le dictionnaire
                foundIndex = fromMaybe (-1) maybeIndex + 1
                newDictionary = dictionary ++ [prefix]  -- Met à jour le dictionnaire avec le nouveau préfixe
            in (foundIndex, last prefix) : compressHelper (drop (length prefix - 1) cs) newDictionary (index + 1)  -- Ajoute une paire encodée avec l'index trouvé et le dernier caractère du préfixe au résultat, puis continue avec le reste du texte


-- | Trouve le plus long préfixe qui peut être formé à partir du dictionnaire, en commençant par un caractère donné.
longestPrefix :: String -> String -> [String] -> String
longestPrefix currentPrefix [] _ = currentPrefix  -- Cas de base : si la liste de suffixes est vide, retourne le préfixe actuel
longestPrefix currentPrefix (c:cs) dictionary =
    let newPrefix = currentPrefix ++ [c]  -- Ajoute le caractère actuel au préfixe actuel pour former un nouveau préfixe
    in if newPrefix `elem` dictionary  -- Si le nouveau préfixe est dans le dictionnaire,
       then longestPrefix newPrefix cs dictionary  -- appelle récursivement avec le reste des caractères et le même dictionnaire
       else if any (== init newPrefix) dictionary  -- Sinon, si le préfixe sans le dernier caractère est dans le dictionnaire,
            then newPrefix  -- retourne le nouveau préfixe
            else currentPrefix  -- Sinon, retourne le préfixe actuel, ce qui signifie qu'il ne peut pas être étendu davantage


-- | La fonction de décompression pour LZ78.
decompress :: [(Int, Char)] -> String
decompress encoded = decompressHelper encoded empty

-- | Helper fonction pour la décompression LZ78 avec gestion récursive des paires encodées et du dictionnaire.
decompressHelper :: [(Int, Char)] -> [String] -> String
decompressHelper [] _ = ""  -- Cas de base : si la liste d'entrée est vide, retourne une chaîne vide
decompressHelper ((index, char):rest) dictionary =
    let previous = if index == 0 then "" else dictionary !! (index - 1)  -- Récupère la chaîne correspondant à l'index, ou une chaîne vide si l'index est 0
        newString = previous ++ [char]  -- Crée une nouvelle chaîne en concaténant la chaîne précédente avec le caractère actuel
        newDictionary = dictionary ++ [newString]  -- Met à jour le dictionnaire avec la nouvelle chaîne
    in newString ++ decompressHelper rest newDictionary  -- Renvoie la nouvelle chaîne suivie de la décompression récursive du reste de la liste avec le nouveau dictionnaire

    --

