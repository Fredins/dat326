Bool       : {False, True}
Maybe Bool : {Just True, Just False, Nothing}

1. Bool -> Maybe Bool
2 -> 3
{ Just } 1

2. Maybe Bool -> Bool
3 -> 2
{isJust, isNothing, fromJust} 3

3. Maybe (Bool, Maybe (Bool, Maybe Bool))

(Bool, Maybe Bool) = { (True, Just True)
                     , (True, Just False)
                     , (True, Nothing)
                     , (False, Just True)
                     , (False, Just False)
                     , (False, Nothing)
                     } 
2 * 3 = 6

(Bool, Maybe (Bool, Maybe Bool) = { (True, Just (True, Just True))
                                  , (True, Just (True, Just False))
                                  , (True, Just (True, Nothing))
                                  , (True, Just (False, Just True))
                                  , (True, Just (False, Just False))
                                  , (True, Just (False, Nothing))
                                  , (True, Nothing)
                                  , (False, Just (True, Just True))
                                  , (False, Just (True, Just False))
                                  , (False, Just (True, Nothing))
                                  , (False, Just (False, Just True))
                                  , (False, Just (False, Just False))
                                  , (False, Just (False, Nothing))
                                  , (False, Nothing)
                                  } 
2 * (6 + 1) = 14

Maybe (Bool, Maybe (Bool, Maybe Bool) = { Just (True, Just (True, Just True))
                                        , Just (True, Just (True, Just False))
                                        , Just (True, Just (True, Nothing))
                                        , Just (True, Just (False, Just True))
                                        , Just (True, Just (False, Just False))
                                        , Just (True, Just (False, Nothing))
                                        , Just (True, Nothing)
                                        , Just (False, Just (True, Just True))
                                        , Just (False, Just (True, Just False))
                                        , Just (False, Just (True, Nothing))
                                        , Just (False, Just (False, Just True))
                                        , Just (False, Just (False, Just False))
                                        , Just (False, Just (False, Nothing))
                                        , Just (False, Nothing)
                                        , Nothing
                                        } 
14 + 1 = 15
