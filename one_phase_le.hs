extractLabels :: String -> (LabelMap, String)
extractLabels p = (lm, unlines pl)
    where ls = lines p
          extractLabel :: LabelMap -> ProgramCounter -> String -> (LabelMap, String)
          extractLabel lm pc isn
              | islbl = (Map.insert lbl pc lm, isn')
              | otherwise = (lm, isn)
              where wi = words isn
                    cl = head wi
                    islbl = (last cl == ':')
                    lbl = init cl
                    isn' = unwords $ tail wi
          transform :: LabelMap -> ProgramCounter -> [String] -> (LabelMap, [String])
          transform lm pc (i:is) = (lm'', il')
              where (lm', i') = extractLabel lm pc i
                    (lm'', is') = transform lm' (pc + 1) is
                    il' = i' : is'
          transform lm _ [] = (lm, [])
          (lm, pl) = transform Map.empty 0 ls
