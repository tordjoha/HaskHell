module Colors (armyGreen, groundColor, blackTranslucent) where

import Graphics.Gloss(Color, makeColorI)

armyGreen :: Color
armyGreen = makeColorI 78 91 49 255

groundColor :: Color
groundColor = makeColorI 107 68 35 255

blackTranslucent :: Color
blackTranslucent = makeColorI 0 0 0 128
