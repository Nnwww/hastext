module TestDict where

import WordEmbedding.HasText.Dict

testWordsFromFile = wordsFromFile (\c _ -> c + 1) 0
