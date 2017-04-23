module MatrixFont exposing (intToChar, fontIntMin, fontIntMax)
import Dict exposing (..)

fontIntMin: Int
fontIntMin = 0

fontIntMax: Int
fontIntMax = (List.length charList) - 1

charList: List Char
charList = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
            'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
            'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
            '$', '+', '-', '*', '/', '=', '%', '"', '\'', '#',
            '&', '_', '(', ')', ',', '.', ';', ':', '?', '!', 
            '\\', '|', '{', '}', '<', '>', '[', ']', '^', '~']

intCharMap: Dict Int Char
intCharMap = Dict.fromList (List.indexedMap (\i c -> (i, c)) charList)

intToChar: Int -> Char
intToChar anInt = Maybe.withDefault ' ' (Dict.get anInt intCharMap)


